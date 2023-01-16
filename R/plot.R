#' Plot animation path interpolation
#'
#' This is mainly intended as a way to check that the interpolations used in the animation are working as expected.
#'
#' @param x \code{paths_animation} object as created through a call to \code{animate_paths()}.
#' @param ... additional arguments passed to \code{plot}.
#' @param i index of individual to plot (corresponds to index in \code{unique(paths[, 'ID.name')}).
#' @param level confidence level for error bands. \code{NA} removes bands.
#' @param type either \code{"path"} (default) for two marginal interpolation plots, or \code{"covariate"} for a single interpolation plot
#' @param ylim_x y-axis limits for marginal plots (x, easting, etc.)
#' @param ylim_y y-axis limits for marginal plots (y, northing, etc.)
#'
#' @return NULL
#' @rdname plot
#' @method plot paths_animation
#' @export
#' @importFrom stats qnorm
#' @importFrom graphics layout lines matlines rug
#'
#' @examples
#' vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#' vultures_paths <- vultures[vultures$POSIX > as.POSIXct("2009-03-22", origin = "1970-01-01") &
#'   vultures$POSIX < as.POSIXct("2009-04-05", origin = "1970-01-01"), ]
#' interpolated_paths <-
#'   animate_paths(
#'     paths = vultures_paths,
#'     delta.t = 3600 * 6,
#'     coord = c("location.long", "location.lat"),
#'     Time.name = "POSIX",
#'     ID.name = "individual.local.identifier",
#'     s_args = rep(list(list(k = 10)), 6),
#'     return.paths = TRUE
#'   )
#' plot(interpolated_paths, i = 2)
plot.paths_animation <- function(x, ..., i = 1, level = 0.05, type = "path",
                                 ylim_x = NULL, ylim_y = NULL) {
  if(type == "path"){
    layout(matrix(1:2, 2, 1))
    par(mar = c(2, 2, 1, 1))
    for(d in c("x", "y")){
      center_line <- x$paths.interp[[i]][, 1, paste0('mu.', d)]
      ## crawl SEs not working yet because of projection
      ylim <- get(paste0("ylim_", d))
      if(x$interpolation_type == "gam"){
        margin <- qnorm(level / 2) * x$paths.interp[[i]][, 1, paste0('se.mu.', d)]
        upper_line <- center_line + margin
        lower_line <- center_line - margin
        alt_lines <- matrix(NA, length(center_line), 1)
        if(is.null(ylim)){
          ylim <- range(x$paths[[i]][, x$coord[which(d == c("x", "y"))]], 
                        upper_line, lower_line, na.rm = T)
        } 
      } else if(x$interpolation_type == "crawl"){
        upper_line <- lower_line <- rep(NA, length(center_line))
        ## check for more realizations
        realization_columns <- grep(paste0(d, ".samp"), names(x$paths.interp[[i]][, 1, ]))
        alt_lines <- x$paths.interp[[i]][, realization_columns]
        if(is.null(ylim)){
          ylim <- range(x$paths[[i]][, x$coord[which(d == c("x", "y"))]], 
                        alt_lines, na.rm = T)
        } 
      }
      plot(x$paths[[i]][, x$Time.name], x$paths[[i]][, x$coord[which(d == c("x", "y"))]], 
           ylim = ylim, xlab = "time", ylab = d, ...)
      suppressWarnings(rug(x$time.grid))
      lines(x$time.grid, center_line)
      lines(x$time.grid, upper_line, lty = 2)
      lines(x$time.grid, lower_line, lty = 2)
      if(x$interpolation_type == "crawl"){
        matlines(x$time.grid, alt_lines, lty = 2, col = "gray")
      }
    }
  } else if(type == "covariate"){
    if(is.factor(x$covariate.interp[[i]])){
      x$paths[[i]][, x$covariate] <- as.factor(x$paths[[i]][, x$covariate])
    }
    plot(x$paths[[i]][, x$Time.name], x$paths[[i]][, x$covariate], ...) 
    lines(x$time.grid, x$covariate.interp[[i]])
  }
}
