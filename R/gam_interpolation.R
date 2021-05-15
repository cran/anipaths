#' GAM interpolation using \code{mgcv:gam()}. 
#'
#' @param formula optionally specify formula for \code{mgcv::gam()} using \code{y} as response and \code{time} as predictor.
#' @param y observations
#' @param time times for observations 
#' @param pred_times prediction times
#' @param se.fit logical default is \code{TRUE}; should standard pointwise errors be computed for interpolation
#' @param s_args Arguments to \code{mgcv::s()} can be passed using a named list/vector.
#'
#' @importFrom mgcv gam s predict.gam
#'
#' @return interpolated values
gam_interp <- function(formula = NULL, y, time, pred_times, se.fit = T, s_args = NULL){
  s_args <- unlist(s_args)
  df <- data.frame(y = y, time = as.numeric(time))
  if(is.null(formula)){
    formula <- y ~ s(time)
    if(!is.null(s_args)){
      formula <- as.formula(paste0("y ~ s(time, ", paste(names(s_args), "=", s_args, collapse = ", "), ")"))
    }
  }
  fit <- gam(formula = formula, data = data.frame(y = y, time = as.numeric(time)))
  pred <- predict.gam(fit, newdata = data.frame(time = as.numeric(pred_times)), se.fit = se.fit)
  if(se.fit){
    pred <- cbind(fit = pred$fit, se.fit = pred$se.fit)
  }
  return(pred)
}

#' Synchronous GAM interpolation of all paths
#'
#' @param paths lists of data.frames containing positions, times, and covariate for each individual
#' @param coord two-vector of character strings giving names of x and y coordinates in data.frames
#' @param Time.name character string giving name of time variable in data.frames
#' @param time.grid grid of possible times to use for interpolation (individuals will only be interpolated to times within the range of observation times)
#' @param s_args Arguments to \code{mgcv::s()} can be passed using a named list/vector.
#'
#' @return list of interpolated paths by individual
paths_gam_interp <- function(paths, coord, Time.name, time.grid, s_args = NULL){
  paths.interp <- lapply(paths, function(path.i){
    t.i <- as.numeric(path.i[, Time.name])
    time.grid.i <- time.grid[time.grid >= min(t.i) & time.grid <= max(t.i)]
    pred_d <- vapply(1:2, function(d){
      gam_interp(y = path.i[, coord[d]], time = t.i, pred_times = time.grid.i, se.fit = T, s_args = s_args)
    }, FUN.VALUE = matrix(0, length(time.grid.i), 2))
    pred <- matrix(NA, nrow = length(time.grid), ncol = 4)
    pred[time.grid %in% time.grid.i, ] <- matrix(pred_d, nrow = length(time.grid.i))
    colnames(pred) <- c("mu.x", "se.mu.x", "mu.y", "se.mu.y")
    return(pred)
  })
  return(paths.interp)
}

#' Synchronous interpolation of covariate using either GAM (same as paths) or piece-wise constant if covariate is a factor
#'
#' @param paths lists of data.frames containing positions, times, and covariate for each individual
#' @param covariate character string giving name of covariate variable in data.frames
#' @param Time.name character string giving name of time variable in data.frames
#' @param time.grid grid of possible times to use for interpolation (individuals will only be interpolated to times within the range of observation times)
#' @param s_args arguments to \code{mgcv::s()} for GAM interpolation method
#'
#' @return list of interpolated covariate by individual
covariate_interp <- function(paths, covariate = NULL, Time.name, time.grid, s_args){
  if(is.null(covariate)) return(NULL)
  covariate.factors <- NULL
  if(all(sapply(paths, function(x) !is.numeric(x[, covariate])))){
    covariate.factors <- levels(as.factor(unlist(sapply(paths, function(x) x[, covariate]))))
  }
  covariate.interp <- lapply(paths, function(path.i){
    covariate.interp.i <- rep(NA, length(time.grid))
    t.i <- as.numeric(path.i[, Time.name])
    time.grid.i <- time.grid[time.grid >= min(t.i) & time.grid <= max(t.i)]
    if(is.numeric(path.i[, covariate])){
      covariate.interp.i[time.grid %in% time.grid.i] <- 
        gam_interp(y = path.i[, covariate], time = t.i, pred_times = time.grid.i, s_args = s_args, se.fit = F)
    } else {
      covariate.interp.i <- factor(covariate.interp.i, levels = covariate.factors)
        pred.covariate <- approx(
          x = t.i, y = as.factor(path.i[, covariate]),
          xout = as.numeric(time.grid.i), method = "constant"
        )$y
        covariate.interp.i[time.grid %in% time.grid.i] <- 
          factor(covariate.factors[pred.covariate], levels = covariate.factors)
    }
      return(covariate.interp.i)
    })
  return(list(covariate.interp = covariate.interp, covariate.factors = covariate.factors))
}