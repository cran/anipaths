#' Plot animation path interpolation
#' 
#' This is mainly intended as a way to check that the interpolations used in the animation are working as expected.
#'
#' @param x \code{paths_animation} object as created through a call to \code{animate_paths()}.
#' @param ... additional arguments passed to \code{plot}.
#' @param i index of individual to plot (corresponds to index in \code{unique(paths[, 'ID.name')}).
#' @param level confidence level for error bands. \code{NA} removes bands.
#' @param ylim_x y-axis limits for marginal plots (x, easting, etc.)
#' @param ylim_y y-axis limits for marginal plots (y, northing, etc.)
#'
#' @return NULL
#' @rdname plot
#' @method plot paths_animation
#' @export
#' @importFrom stats qnorm
#' @importFrom graphics layout lines
#' 
#' @examples 
#' vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#' vultures_paths <- vultures[vultures$POSIX > as.POSIXct("2009-03-22", origin = "1970-01-01") & 
#'                              vultures$POSIX < as.POSIXct("2009-04-05", origin = "1970-01-01"), ]
#' interpolated_paths <- 
#' animate_paths(paths = vultures_paths,
#'               delta.t = 3600*6,
#'               coord = c("location.long", "location.lat"),
#'               Time.name = "POSIX",
#'               ID.name = "individual.local.identifier", 
#'               max.knots = 13,
#'               return.paths = TRUE)
#' interpolated_paths_gp <- 
#' animate_paths(paths = vultures_paths,
#'               delta.t = 3600*6,
#'               coord = c("location.long", "location.lat"),
#'               Time.name = "POSIX",
#'               ID.name = "individual.local.identifier", 
#'               max.knots = 3*13,
#'               return.paths = TRUE)
#' plot(interpolated_paths, i = 2)
#' plot(interpolated_paths_gp, i = 2, level = 0.01)
plot.paths_animation <- function(x, ..., i = 1, level = 0.05, 
                                 ylim_x = NULL, ylim_y = NULL){
  list2env(x, envir = environment())
  layout(matrix(1:2, 2, 1)); par(mar = c(2, 2, 1, 1))
  plot(x$paths[[i]][, x$Time.name], x$paths[[i]][, x$coord[1]], ylim = ylim_x, ...)
  lines(x$times, x$paths.interp[[i]][, 1])
  lines(x$times, x$paths.interp[[i]][, 1] + qnorm(level/2) * x$paths.interp[[i]][, 3], lty = 2)
  lines(x$times, x$paths.interp[[i]][, 1] + qnorm(1-level/2) * x$paths.interp[[i]][, 3], lty = 2)
  plot(x$paths[[i]][, x$Time.name], x$paths[[i]][, x$coord[2]], ylim = ylim_y, ...)
  lines(x$times, x$paths.interp[[i]][, 2])
  lines(x$times, x$paths.interp[[i]][, 2] + qnorm(level/2) * x$paths.interp[[i]][, 4], lty = 2)
  lines(x$times, x$paths.interp[[i]][, 2] + qnorm(1-level/2) * x$paths.interp[[i]][, 4], lty = 2)
  return(NULL)
}