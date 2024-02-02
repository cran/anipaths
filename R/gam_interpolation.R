#' GAM interpolation using \code{mgcv:gam()}. 
#'
#' @param formula optionally specify formula for \code{mgcv::gam()} using \code{y} as response and \code{time} as predictor.
#' @param y observations
#' @param time times for observations 
#' @param pred_times prediction times
#' @param se.fit logical default is \code{TRUE}; should standard pointwise errors be computed for interpolation
#' @param s_args Arguments to \code{mgcv::s()} can be passed using a named list/vector.
#' @param uncertainty.type State what type of uncertainty plot 1 is default for tails more than 1 is amount of predicted trajectories for each unique individual and blurs for blur plot
#' @param verbose logical; \code{TRUE} prints messages about fitting details
#'
#' @importFrom mgcv gam s predict.gam print.gam
#' @importFrom mvtnorm rmvnorm
#'
#' @return interpolated values
gam_interp <- function(formula = NULL, y, time, pred_times, se.fit = T, s_args = NULL, uncertainty.type, verbose = F){
  s_args <- unlist(s_args)
  df <- data.frame(y = y, time = as.numeric(time))
  if(length(time) == 2){
    return(cbind(fit = approx(x = time, y = y, xout = pred_times)$y, se.fit = NA))
  }
  if(is.null(formula)){
    if(!is.null(s_args)){
      formula <- as.formula(paste0("y ~ s(time, ", 
                                   paste(names(s_args), "=", s_args, collapse = ", "), ")"))
    } else {
      k <- floor(min(length(time) - 2, max(length(pred_times) / 4, 20), 200))
      formula <- y ~ s(time, k = k)
      if(isTRUE(verbose)) message("s_args is NULL. Setting k to ", k)
    }
  }
  fit <- gam(formula = formula, data = data.frame(y = y, time = as.numeric(time)))
  if(isTRUE(verbose)) print.gam(fit)
  if(uncertainty.type > 1 & is.numeric(uncertainty.type)){
    pred <- predict.gam(fit, newdata = data.frame(time = as.numeric(pred_times)), se.fit = se.fit, type = "lpmatrix")
    beta <- rmvnorm(n = uncertainty.type, mean = fit$coefficients, sigma = fit$Vp)
    f <- pred %*% t(beta) ## compute all possible paths
    return(f)
  } else{
    pred <- predict.gam(fit, newdata = data.frame(time = as.numeric(pred_times)), se.fit = se.fit)
    if(se.fit){
      pred <- cbind(fit = pred$fit, se.fit = pred$se.fit)
    }
  }
  return(pred)
}

#' Synchronous GAM interpolation of all paths
#'
#' @param paths lists of data.frames containing positions, times, and covariate for each individual
#' @param coord two-vector of character strings giving names of x and y coordinates in data.frames
#' @param Time.name character string giving name of time variable in data.frames
#' @param time.grid grid of possible times to use for interpolation (individuals will only be interpolated to times within the range of observation times)
#' @param s_args List of arguments to \code{mgcv::s()} the same length as number of unique individuals. Each entry in the list should be a named list/vector.
#' @param uncertainty.type State what type of uncertainty plot 1 is default for tails more than 1 is amount of predicted trajectories for each unique individual and blurs for blur plot
#' @param verbose logical; \code{TRUE} prints messages about fitting details
#'
#' @return list of interpolated paths by individual
paths_gam_interp <- function(paths, coord, Time.name, time.grid, s_args = NULL, uncertainty.type, verbose = F){
  paths.interp <- lapply(1:length(paths), function(i){
    message("Beginning interpolation for individual ", i, " of ", length(paths), ".")
    path.i <- paths[[i]]
    t.i <- as.numeric(path.i[, Time.name])
    time.grid.i <- time.grid[time.grid >= min(t.i) & time.grid <= max(t.i)]
    if(uncertainty.type > 1 & is.numeric(uncertainty.type)){
      pred_d <- vapply(1:2, function(d){
        gam_interp(y = path.i[, coord[d]], time = t.i, pred_times = time.grid.i, 
                   se.fit = F, s_args = s_args[[i]], uncertainty.type = uncertainty.type, verbose = verbose)
      }, FUN.VALUE = array(0, dim = c(length(time.grid.i), uncertainty.type)))
      pred <- array(NA, dim = c(length(time.grid), uncertainty.type, 4), 
                    dimnames = list(time.grid, 1:uncertainty.type, 
                                    c("mu.x", "se.mu.x", "mu.y", "se.mu.y")))
      pred[time.grid %in% time.grid.i,,c("mu.x", "mu.y")] <- 
        array(pred_d, dim = c(length(time.grid.i), uncertainty.type, 2))
    } else {
      pred_d <- vapply(1:2, function(d){
        gam_interp(y = path.i[, coord[d]], time = t.i, pred_times = time.grid.i, 
                   se.fit = T, s_args = s_args[[i]], uncertainty.type = uncertainty.type, verbose = verbose)
      }, FUN.VALUE = array(0, dim = c(length(time.grid.i), 2)))
      pred <- array(NA, dim = c(length(time.grid), 1, 4), 
                    dimnames = list(time.grid, 1,c("mu.x", "se.mu.x", "mu.y", "se.mu.y")))
      pred[time.grid %in% time.grid.i, ,c("mu.x", "se.mu.x", "mu.y", "se.mu.y")] <- 
        array(pred_d, dim = c(length(time.grid.i), 2, 2))
    }
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