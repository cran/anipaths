#' Animation Expression
#' @keywords internal
#'
#' @param bg a list object for the ggmap background if background object is supplied
#' @param bg.axes logical: should animation place axis labels when using a background image (default is \code{TRUE}). If \code{RGoogleMaps} is used to produce background, labels will be "northing" and "easting". Otherwise, the strings given to \code{coord} will be used.
#' @param bg.misc Character string which will be executed as \code{R} code after generating the background, and before adding trajectories, etc.
#' @param bg.opts Options passed to \code{plot()} function call that makes background in each frame. For example, this could be used to specify blue ocean and gray landcover if \code{background} is a \code{SpatialPolygonsDataFrame} and \code{bg.opts = list(bg = "dodgerblue4", col = "gray", border = "gray")}.
#' @param blur.size a integer of the size for blur points; default is 8
#' @param cliques A list of colors for network projections
#' @param color_covariate_function a function to generate color for covariate interpolation
#' @param coord A character vector of length 2 giving the names of the longitude/easting and latitude/northing columns in the \code{paths} \code{data.frame} (in that order). This is required if \code{paths} is not a \code{SpatialPointsDataFrame}.
#' @param covariate The name of the column in \code{paths} that identifies the covariate to be mapped to a ring of color around each point.
#' @param covariate.factors factor levels for covariate interpolation
#' @param covariate.interp interpolation for covariate projections
#' @param covariate.legend.loc either the location of the covariate legend, or \code{NA} if no legend is desired
#' @param covariate.name name of covariate interpolation
#' @param covariate.range the range of covariate interpolation
#' @param covariate.thresh if changed from its default value of \code{NULL}, the interpolated value of the covariate will be binarized based on this numeric value.
#' @param covariate.ticks 
#' @param crawl.mu.color color for the main predictions for crawl interpolation; default is black
#' @param crawl.plot.type a character string of what type of the plot you wish to generate when \code{interpolation_type = "crawl"}. Default is "point.tail" for points with tails; input "point" for point plot and input "blur" for blur point plot; input "blur.point" for blur point with tails
#' @param cur.time start time of animation
#' @param date.col default is \code{"black"}
#' @param delta.t The gap in time between each frame in the animation. Specify one of \code{delta.t} or \code{n.frames}. If both are specified, \code{delta.t} is used.
#' @param dev.opts Options passed to \code{png()} before creating each frame.
#' @param dimmed Numeric vector of individuals to "dim" in the animation. Order corresponds to the order of the ID.name variable, or order of paths list.
#' @param ID_names a list of names for each animal in the data
#' @param interpolation_type a character string of the type of interpolation. Default is "gam" for a generalized addictive model. Input "crawl" to interpolate using \code{crawl} package
#' @param interval Seconds per frame in animation. Default is 1/12 (or 12 frames per second).
#' @param legend.loc passed to first argument of \code{legend()} function. Default is \code{"topright"}. \code{NA} removes legend.
#' @param main Title for each frame. SOON: support for changing titles to allow for, say, dates.
#' @param method either \code{"html"} (default) or \code{"mp4"}. The latter requires the user has installed \code{ffmpeg} (see \code{?animation::saveVideo()}).
#' @param n.frames The number of frames used to animate the complete time domain of the data.
#' @param network Array of dimensions (# individuals, # individuals, \code{n.frames}) that gives a dyanmic network structure among the individuals.
#' @param network.interp interpolated network of dimension \code{(n.indiv, n.indiv, n.frames)}
#' @param network.ring.trans transparency of network segments (default is 1)
#' @param network.ring.wt thickness of network rings (default is 3)
#' @param network.segment.wt thickness of network segments (default is 3)
#' @param network.segment.trans 
#' @param network.thresh 
#' @param par.opts Options passed to \code{par()} before creating each frame.
#' @param paths A list of all paths from each animals stored in a \code{data.frame} or \code{SpatialPointsDataFrame} object.
#' @param paths.interp a path animation object that contains all predicted and simulated paths for all animals
#' @param plot.date Logical variable toggling date text at the time center of the animation.
#' @param pt.alpha alpha value for the points
#' @param pt.cex A numeric value giving the character expansion (size) of the points for each individual. Default is 1.
#' @param pt.colors A vector of colors to be used for each individual in the animation. Default values come from Color Brewer palettes. When a network is provided, this is ignored and individuals are all colored black. If \code{NA}, no plot colors are chosen to distinguish individuals. This can be useful when making animations involving a covariate. Consider also setting \code{legend.loc} to \code{NA} in this case.
#' @param pt.wd size of the points; default is 1
#' @param res Resolution of images in animation. Increase this for higher quality (and larger) images.
#' @param scale 
#' @param simulation logical. Generate simulation predictions to have multiple projects for the animal paths
#' @param simulation.iter an integer of how many paths the crawl model will generate
#' @param tail.alpha alpha value for the tails
#' @param tail.colors default is "gray87". Can be single color or vector of colors.
#' @param tail.length Length of the tail trailing each individual.
#' @param tail.wd Thickness of tail trailing behind each individual. Default is 1.
#' @param theme_map plot theme for ggplot, default is NULL
#' @param time.grid A vector of time interval.
#' @param Time.name The name of the columns in \code{paths} gving the observation times. This column must be of class \code{POSIXt}, or numeric.
#' @param uncertainty.level value in (0, 1) corresponding to \code{level} at which to draw uncertainty ellipses. \code{NA} (default) results in no ellipses.
#' @param uncertainty.type State what type of uncertainty plot 1 is default for tails more than 1 is amount of predicted trajectories for each unique individual and blurs for blur plot
#' @param whole.path logical. If \code{TRUE} (default = \code{FALSE}), the complete interpolated trajectories will be plotted in the background of the animation. If \code{whole.path = TRUE}, consider also setting \code{tail.length = 0}.
#' @param xlim Boundaries for plotting. If left undefined, the range of the data will be used.
#' @param ylim Boundaries for plotting. If left undefined, the range of the data will be used.
#' @param ... other arguments to be passed to \code{ani.options} to animation options such as the time interval between image frames.
#'
#' @return animation for different methods and different interpolation types
#' @import ellipse
#' @import ggplot2
#' @import ggmap ggmap
#' @importFrom scales alpha
#' @importFrom grDevices rgb
#' @importFrom animation saveHTML saveVideo ani.options
#' @importFrom dplyr filter
#' @importFrom lubridate as_date
#'
animation_expression <- function(bg, bg.axes, bg.misc, bg.opts, blur.size, cliques, 
                                 color_covariate_function, coord, covariate, 
                                 covariate.factors, covariate.interp, covariate.legend.loc, 
                                 covariate.name, covariate.range, covariate.thresh, covariate.ticks,
                                 crawl.mu.color, crawl.plot.type, cur.time, date.col, delta.t, 
                                 dev.opts, dimmed, ID_names, interpolation_type, 
                                 interval, legend.loc, main, method, n.frames, network, network.interp,
                                 network.ring.trans, network.ring.wt, network.segment.trans, 
                                 network.thresh, network.segment.wt, 
                                 par.opts, paths, paths.interp, plot.date, pt.alpha, 
                                 pt.cex, pt.colors, pt.wd, res, scale, simulation, simulation.iter, 
                                 tail.alpha, tail.colors, tail.length, tail.wd, theme_map, 
                                 time.grid, Time.name, uncertainty.level, uncertainty.type, 
                                 whole.path, xlim, ylim, ...) {
  
  # set option for animation frame and start time
  ani.options(interval = interval, ani.width = res * 480, ani.height = res * 480, verbose = F)
  
  ## expression for animation for gam model
  if (interpolation_type == "gam" & method == "html") {
    saveHTML(expr = {
      for (frame in 1:n.frames) {
        ## device ----
        do.call(png, c(
          list(
            filename = sprintf(ani.options("img.fmt"), frame),
            width = ani.options("ani.width"), height = ani.options("ani.height")
          ),
          dev.opts
        ))
        ## set par options ----
        do.call(par, par.opts)
        ## add background ----
        if (inherits(bg[[frame]], "ggmap")) {
          par(mar = c(0.1, 0.1, 0.1, 0.1))
          if (bg.axes) {
            par(mar = c(4.1, 4.1, 0.1, 0.1))
          }
          do.call(plot, c(list("x" = bg[[frame]]), xlab = "", ylab = "", bg.opts))
          if (bg.axes) {
            mtext(text = "easting [m]", side = 1, line = 2.6)
            axis(1, at = seq(0, 1280, l = 5), signif(seq(0, 1280 / scale[1], l = 5), 3))
            mtext(text = "northing [m]", side = 2, line = 2.6)
            axis(2, at = seq(0, 1280, l = 5), signif(seq(0, 1280 / scale[2], l = 5), 3))
          }
        } else {
          par(mar = c(0.1, 0.1, 0.1, 0.1))
          if (bg.axes) {
            par(mar = c(4.1, 4.1, 0.1, 0.1))
          }
          if(inherits(bg[[frame]], "SpatialPolygonsDataFrame")){
            do.call(sp::plot, c(
              list(
                "x" = bg[[frame]], xlab = "", ylab = "",
                "xlim" = xlim, "ylim" = ylim, "main" = main
              ),
              bg.opts
            ))
          } else {
            do.call(plot, c(
              list(
                "x" = bg[[frame]], xlab = "", ylab = "",
                "xlim" = xlim, "ylim" = ylim, "main" = main
              ),
              bg.opts
            ))
          }
          if (bg.axes) {
            mtext(text = coord[1], side = 1, line = 2.6)
            mtext(text = coord[2], side = 2, line = 2.6)
          }
        }
        if (!is.null(bg.misc)) {
          eval(parse(text = bg.misc))
        }
        ## whole path ----
        if (isTRUE(whole.path)) {
          for (id in 1:length(paths)) {
            lines(x = paths.interp[[id]][ , , c("mu.x", "mu.y")], 
                  col = id)
          }
        }
        ## add blur ellipse ----
        if(uncertainty.type == "blur"){
          for(id in 1:length(paths)){
            points(paths.interp[[id]][frame, ,"mu.x"], paths.interp[[id]][frame, ,"mu.y"])
            points(blur_point(x=matrix(c(1.96*paths.interp[[id]][frame, ,"se.mu.x"],0,0,
                                         1.96*paths.interp[[id]][frame, ,"se.mu.y"]), 2,2),
                              center = c(paths.interp[[id]][frame, ,"mu.x"], paths.interp[[id]][frame, ,"mu.y"]),
                              col = tail.colors, alpha_mult = new_alpha(paths.interp[[id]][frame, ,"se.mu.x"],paths.interp[[id]][frame, ,"se.mu.y"])))
          }
        } else if(uncertainty.type > 1){
          ## multiple trajectories ----
          multi_col <- tail.colors
          for(id in 1:length(paths)){
            if("unique" %in% tail.colors){
              multi_col <- id
            }
            suppressWarnings(matlines(paths.interp[[id]][max(1, frame - tail.length):frame, ,"mu.x"],
                                      paths.interp[[id]][max(1, frame - tail.length):frame, ,"mu.y"], 
                                      col = multi_col, lty = 1, lwd = 6 * tail.wd * res))
          }
        } else{
          ## add tails ----
          for (id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])) {
            if (frame > 1 & tail.length > 0) {
              segments(
                x0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), ,"mu.x"],
                x1 = paths.interp[[id]][max(2, frame - tail.length):frame, ,"mu.x"],
                y0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), ,"mu.y"],
                y1 = paths.interp[[id]][max(2, frame - tail.length):frame, ,"mu.y"],
                col = tail.colors[(id - 1) %% length(tail.colors) + 1], lwd = 6 * tail.wd * res
              )
            }
          }
        }
        ## clique-wise network segments ----
        if (!is.null(network.interp)) {
          if (!is.na(cliques$cliques[[frame]][1])) {
            for (cl in 1:length(cliques$cliques[[frame]])) {
              for (id in as.numeric(cliques$cliques[[frame]][[cl]])) {
                for (id2 in as.numeric(cliques$cliques[[frame]][[cl]])[-id]) {
                  segments(
                    x0 = paths.interp[[id]][frame, , "mu.x"],
                    y0 = paths.interp[[id]][frame, , "mu.y"],
                    x1 = paths.interp[[id2]][frame, , "mu.x"],
                    y1 = paths.interp[[id2]][frame, , "mu.y"],
                    col = alpha(cliques$colors[[frame]][cl], network.segment.trans),
                    lwd = network.segment.wt * res
                  )
                }
              }
            }
          }
        }
        ## add points and clique-wise network rings ----
        radius <- rep(1.2 * res, length(paths))
        for (id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])) {
          covariate.ring <- pt.colors[id, 1 + id %in% dimmed]
          lwd <- 1 / 1.5
          if (!is.null(covariate)) {
            covariate.ring <- NA
            if (!is.na(covariate.interp[[id]][frame])) {
              if (!is.null(covariate.thresh)) {
                covariate.normalized.value <- (covariate.interp[[id]][frame] >= covariate.thresh)
              } else {
                if (!is.factor(covariate.interp[[id]])) {
                  covariate.normalized.value <-
                    (covariate.interp[[id]][frame] - covariate.range[1]) / diff(covariate.range)
                } else {
                  covariate.normalized.value <-
                    which(covariate.interp[[id]][frame] == levels(unlist(covariate.interp))) /
                    nlevels(unlist(covariate.interp))
                }
                covariate.ring <-
                  rgb(color_covariate_function(covariate.normalized.value), maxColorValue = 255)
                lwd <- 2.5 / 1.5
              }
            }
          }
          if (is.na(pt.colors[1])) {
            points(matrix(paths.interp[[id]][frame, , c("mu.x", "mu.y")], ncol = 2),
                   col = covariate.ring, pch = 19, cex = pt.cex * res * 0.85
            )
            if (all(!is.na(paths.interp[[id]][frame, , "se.mu.x"])) & !is.na(uncertainty.level)) {
              lines(ellipse(
                x = diag(paths.interp[[id]][frame, , c("se.mu.x", "se.mu.y")]^2),
                centre = paths.interp[[id]][frame, , c("mu.x", "mu.y")], level = uncertainty.level
              ),
              lty = 3
              )
            }
          } else {
            points(matrix(paths.interp[[id]][frame, , c("mu.x", "mu.y")], ncol = 2),
                   col = covariate.ring, bg = pt.colors[id, 1 + id %in% dimmed],
                   pch = 21, lwd = lwd * res, cex = pt.cex * res * 0.85
            )
            if (all(!is.na(paths.interp[[id]][frame, , "se.mu.x"])) & !is.na(uncertainty.level)) {
              lines(ellipse(
                x = diag(paths.interp[[id]][frame, , c("se.mu.x", "se.mu.y")]^2),
                centre = paths.interp[[id]][frame, , c("mu.x", "mu.y")], level = uncertainty.level
              ),
              lty = 3
              )
            }
          }
        }
        if (!is.null(network)) {
          if (!is.na(cliques$cliques[[frame]][1])) {
            for (cl in 1:length(cliques$cliques[[frame]])) {
              for (id in as.numeric(cliques$cliques[[frame]][[cl]])) {
                for (id2 in (1:length(paths))[-id]) {
                  points(matrix(paths.interp[[id]][frame, , c("mu.x", "mu.y")], ncol = 2),
                         col = alpha(colour = cliques$colors[[frame]][cl], network.ring.trans),
                         cex = radius[id], lwd = network.ring.wt * res
                  )
                }
                radius[id] <- radius[id] + pt.cex * res * 0.85 * (network.interp[id, id2, frame] > network.thresh)
              }
            }
          }
        }
        ## add clique-wise legend ----
        if (is.null(network)) {
          legend.pt.colors <- pt.colors[, 1]
          legend.pt.colors[(1:nrow(pt.colors)) %in% dimmed] <- pt.colors[(1:nrow(pt.colors)) %in% dimmed, 2]
          legend(legend.loc,
                 pch = 19, pt.cex = 0.8 * res, col = legend.pt.colors, legend = ID_names,
                 box.lwd = 0, bty = "n", text.col = "gray60", cex = res
          )
        } else {
          if (!is.na(cliques$cliques[[frame]][1])) {
            legend(legend.loc,
                   pch = 1, lwd = 3 * res, lty = NA, pt.cex = 0.8 * res, cex = res,
                   col = cliques$colors[[frame]],
                   legend = gsub(")", "", gsub("c(", "", lapply(cliques$cliques[[frame]], as.numeric), fixed = T)),
                   box.lwd = 0
            )
          }
        }
        if (!is.null(covariate)) {
          if (is.null(covariate.thresh)) {
            if (!is.null(covariate.factors[1])) {
              color_ticks <- seq(0, 1, l = length(covariate.factors))
              legend(covariate.legend.loc,
                     pch = 21 - 2 * is.na(pt.colors[1]), lwd = 2.5 / 1.5 * res,
                     col = rgb(color_covariate_function(color_ticks), maxColorValue = 255),
                     legend = covariate.factors, cex = res,
                     box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name
              )
            } else {
              color_ticks <- (covariate.ticks - covariate.range[1]) / diff(covariate.range)
              legend(covariate.legend.loc,
                     pch = 21 - 2 * is.na(pt.colors[1]), pt.cex = pt.cex * res * 0.85, lwd = 2.5 / 1.5 * res,
                     col = rgb(color_covariate_function(color_ticks), maxColorValue = 255),
                     legend = covariate.ticks, cex = res,
                     box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name
              )
            }
          } else {
            legend(covariate.legend.loc,
                   pch = 21 - 2 * is.na(pt.colors[1]), pt.cex = pt.cex * res * 0.85, lwd = 2.5 / 1.5 * res,
                   col = rgb(color_covariate_function(0:1), maxColorValue = 255),
                   legend = paste(c("<", ">="), covariate.thresh), cex = res,
                   box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name
            )
          }
        }
        ## add date/time ----
        if (plot.date) {
          if ("POSIXt" %in% class(time.grid[frame])) {
            # mtext(text = as.Date(time.grid[frame]), side = 3, line = -3, cex = res)
            mtext(text = (time.grid[frame]), side = 3, line = -3, cex = res, col = date.col)
          } else {
            mtext(text = signif(time.grid[frame], 6), side = 3, line = -3, cex = res, col = date.col)
          }
        }
        ## timing ----
        if (n.frames >= 10 & frame %% floor(n.frames / 10) == 0) {
          message(paste("frame ", frame, " out of ", n.frames, " (", round(frame / n.frames, 3) * 100, "%)",
                        " [", round(as.numeric(Sys.time() - cur.time), 2), " seconds]",
                        sep = ""
          ))
          cur.time <- Sys.time()
        }
        ## dev.off ----
        dev.off()
      }
    }, use.dev = F, ...)
  } else if (interpolation_type == "gam" & method == "mp4") {
    saveVideo(expr = {
      for (frame in 1:n.frames) {
        ## device ----
        png(
          filename = sprintf(ani.options("img.fmt"), frame),
          width = ani.options("ani.width"), height = ani.options("ani.height")
        )
        ## set par options ----
        do.call(par, par.opts)
        ## add background ----
        if (class(bg[[frame]])[1] == "ggmap") {
          par(mar = c(0.1, 0.1, 0.1, 0.1))
          if (bg.axes) {
            par(mar = c(4.1, 4.1, 0.1, 0.1))
          }
          do.call(plot, c(list("x" = bg[[frame]]), xlab = "", ylab = "", bg.opts))
          if (bg.axes) {
            mtext(text = "easting [m]", side = 1, line = 2.6)
            axis(1, at = seq(0, 1280, l = 5), signif(seq(0, 1280 / scale[1], l = 5), 3))
            mtext(text = "northing [m]", side = 2, line = 2.6)
            axis(2, at = seq(0, 1280, l = 5), signif(seq(0, 1280 / scale[2], l = 5), 3))
          }
        } else {
          par(mar = c(0.1, 0.1, 0.1, 0.1))
          if (bg.axes) {
            par(mar = c(4.1, 4.1, 0.1, 0.1))
          }
          do.call(plot, c(
            list(
              "x" = bg[[frame]], xlab = "", ylab = "",
              "xlim" = xlim, "ylim" = ylim, "main" = main
            ),
            bg.opts
          ))
          if (bg.axes) {
            mtext(text = coord[1], side = 1, line = 2.6)
            mtext(text = coord[2], side = 2, line = 2.6)
          }
        }
        if (!is.null(bg.misc)) {
          eval(parse(text = bg.misc))
        }
        ## whole path ----
        if (isTRUE(whole.path)) {
          for (id in 1:length(paths)) {
            lines(x = paths.interp[[id]][ , , c("mu.x", "mu.y")])
          }
        }
        if(uncertainty.type == "blur"){
          ## add blur ellipse ----
          if(!all(sapply(bg, is.null))){
            warning("Blur with backgrounds not ready yet but can use backgrounds with multiple trajectories")
            break
          }
          for(id in 1:length(paths)){
            points(paths.interp[[id]][frame, ,"mu.x"], paths.interp[[id]][frame, ,"mu.y"])
            points(blur_point(x=matrix(c(1.96*paths.interp[[id]][frame, ,"se.mu.x"],0,0,
                                         1.96*paths.interp[[id]][frame, ,"se.mu.y"]), 2,2),
                              center = c(paths.interp[[id]][frame, ,"mu.x"], paths.interp[[id]][frame, ,"mu.y"]),
                              col = tail.colors, alpha_mult = new_alpha(paths.interp[[id]][frame, ,"se.mu.x"],paths.interp[[id]][frame, ,"se.mu.y"])))
          }
        } else if(uncertainty.type > 1){
          ## multiple trajectories ----
          for(id in 1:length(paths)){
            suppressWarnings(matlines(paths.interp[[id]][max(1, frame - tail.length):frame, ,"mu.x"],
                                      paths.interp[[id]][max(1, frame - tail.length):frame, ,"mu.y"], 
                                      col = tail.colors, lty = 1, lwd = 6 * tail.wd * res))
          }
        } else{
          ## add tails ----
          for (id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])) {
            if (frame > 1 & tail.length > 0) {
              segments(
                x0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), ,"mu.x"],
                x1 = paths.interp[[id]][max(2, frame - tail.length):frame, ,"mu.x"],
                y0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), ,"mu.y"],
                y1 = paths.interp[[id]][max(2, frame - tail.length):frame, ,"mu.y"],
                col = tail.colors[(id - 1) %% length(tail.colors) + 1], lwd = 6 * tail.wd * res
              )
            }
          }
        }
        ## clique-wise network segments ----
        if (!is.null(network)) {
          if (!is.na(cliques$cliques[[frame]][1])) {
            for (cl in 1:length(cliques$cliques[[frame]])) {
              for (id in as.numeric(cliques$cliques[[frame]][[cl]])) {
                for (id2 in as.numeric(cliques$cliques[[frame]][[cl]])[-id]) {
                  segments(
                    x0 = paths.interp[[id]][frame, , "mu.x"],
                    y0 = paths.interp[[id]][frame, , "mu.y"],
                    x1 = paths.interp[[id2]][frame, , "mu.x"],
                    y1 = paths.interp[[id2]][frame, , "mu.y"],
                    col = alpha(cliques$colors[[frame]][cl], network.segment.trans),
                    lwd = network.segment.wt * res
                  )
                }
              }
            }
          }
        }
        ## add points and clique-wise network rings ----
        radius <- rep(1.2 * res, length(paths))
        for (id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])) {
          covariate.ring <- pt.colors[id, 1 + id %in% dimmed]
          lwd <- 1 / 1.5
          if (!is.null(covariate)) {
            covariate.ring <- NA
            if (!is.na(covariate.interp[[id]][frame])) {
              if (!is.null(covariate.thresh)) {
                covariate.normalized.value <- (covariate.interp[[id]][frame] >= covariate.thresh)
              } else {
                if (!is.factor(covariate.interp[[id]])) {
                  covariate.normalized.value <-
                    (covariate.interp[[id]][frame] - covariate.range[1]) / diff(covariate.range)
                } else {
                  covariate.normalized.value <-
                    which(covariate.interp[[id]][frame] == levels(unlist(covariate.interp))) /
                    nlevels(unlist(covariate.interp))
                }
                covariate.ring <-
                  rgb(color_covariate_function(covariate.normalized.value), maxColorValue = 255)
                lwd <- 2.5 / 1.5
              }
            }
          }
          if (is.na(pt.colors[1])) {
            points(matrix(paths.interp[[id]][frame, , c("mu.x", "mu.y")], ncol = 2),
                   col = covariate.ring, pch = 19, cex = pt.cex * res * 0.85
            )
            if (all(!is.na(paths.interp[[id]][frame, , "se.mu.x"])) & !is.na(uncertainty.level)) {
              lines(ellipse(
                x = diag(paths.interp[[id]][frame, , c("se.mu.x", "se.mu.y")]^2),
                centre = paths.interp[[id]][frame, , c("mu.x", "mu.y")], level = uncertainty.level
              ),
              lty = 3
              )
            }
          } else {
            points(matrix(paths.interp[[id]][frame, , c("mu.x", "mu.y")], ncol = 2),
                   col = covariate.ring, bg = pt.colors[id, 1 + id %in% dimmed],
                   pch = 21, lwd = lwd * res, cex = pt.cex * res * 0.85
            )
            if (all(!is.na(paths.interp[[id]][frame, , "se.mu.x"])) & !is.na(uncertainty.level)) {
              lines(ellipse(
                x = diag(paths.interp[[id]][frame, , c("se.mu.x", "se.mu.y")]^2),
                centre = paths.interp[[id]][frame, , c("mu.x", "mu.y")], level = uncertainty.level
              ),
              lty = 3
              )
            }
          }
        }
        if (!is.null(network)) {
          if (!is.na(cliques$cliques[[frame]][1])) {
            for (cl in 1:length(cliques$cliques[[frame]])) {
              for (id in as.numeric(cliques$cliques[[frame]][[cl]])) {
                for (id2 in (1:length(paths))[-id]) {
                  points(matrix(paths.interp[[id]][frame, , c("mu.x", "mu.y")], ncol = 2),
                         col = alpha(colour = cliques$colors[[frame]][cl], network.ring.trans),
                         cex = radius[id], lwd = network.ring.wt * res
                  )
                }
                radius[id] <- radius[id] + pt.cex * res * 0.85 * (network.interp[id, id2, frame] > network.thresh)
              }
            }
          }
        }
        ## add clique-wise legend ----
        if (is.null(network)) {
          legend.pt.colors <- pt.colors[, 1]
          legend.pt.colors[(1:nrow(pt.colors)) %in% dimmed] <- pt.colors[(1:nrow(pt.colors)) %in% dimmed, 2]
          legend(legend.loc,
                 pch = 19, pt.cex = 0.8 * res, col = legend.pt.colors, legend = ID_names,
                 box.lwd = 0, bty = "n", text.col = "gray60", cex = res
          )
        } else {
          if (!is.na(cliques$cliques[[frame]][1])) {
            legend(legend.loc,
                   pch = 1, lwd = 3 * res, lty = NA, pt.cex = 0.8 * res, cex = res,
                   col = cliques$colors[[frame]],
                   legend = gsub(")", "", gsub("c(", "", lapply(cliques$cliques[[frame]], as.numeric), fixed = T)),
                   box.lwd = 0
            )
          }
        }
        if (!is.null(covariate)) {
          if (is.null(covariate.thresh)) {
            if (!is.null(covariate.factors[1])) {
              color_ticks <- seq(0, 1, l = length(covariate.factors))
              legend(covariate.legend.loc,
                     pch = 21 - 2 * is.na(pt.colors[1]), lwd = 2.5 / 1.5 * res,
                     col = rgb(color_covariate_function(color_ticks), maxColorValue = 255),
                     legend = covariate.factors, cex = res,
                     box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name
              )
            } else {
              color_ticks <- (covariate.ticks - covariate.range[1]) / diff(covariate.range)
              legend(covariate.legend.loc,
                     pch = 21 - 2 * is.na(pt.colors[1]), pt.cex = pt.cex * res * 0.85, lwd = 2.5 / 1.5 * res,
                     col = rgb(color_covariate_function(color_ticks), maxColorValue = 255),
                     legend = covariate.ticks, cex = res,
                     box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name
              )
            }
          } else {
            legend(covariate.legend.loc,
                   pch = 21 - 2 * is.na(pt.colors[1]), pt.cex = pt.cex * res * 0.85, lwd = 2.5 / 1.5 * res,
                   col = rgb(color_covariate_function(0:1), maxColorValue = 255),
                   legend = paste(c("<", ">="), covariate.thresh), cex = res,
                   box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name
            )
          }
        }
        ## add date/time ----
        if (plot.date) {
          if ("POSIXt" %in% class(time.grid[frame])) {
            # mtext(text = as.Date(time.grid[frame]), side = 3, line = -3, cex = res)
            mtext(text = (time.grid[frame]), side = 3, line = -3, cex = res, col = date.col)
          } else {
            mtext(text = signif(time.grid[frame], 6), side = 3, line = -3, cex = res, col = date.col)
          }
        }
        ## timing ----
        if (n.frames >= 10 & frame %% floor(n.frames / 10) == 0) {
          message(paste("frame ", frame, " out of ", n.frames, " (", round(frame / n.frames, 3) * 100, "%)",
                        " [", round(as.numeric(Sys.time() - cur.time), 2), " seconds]",
                        sep = ""
          ))
          cur.time <- Sys.time()
        }
        ## dev.off ----
        dev.off()
      }
    }, use.dev = F, ...)
  } else if (interpolation_type == "crawl") {
    n.frames <- length(time.grid)
    # add theme if didn't specified
    if (is.null(theme_map)) {
      theme_map <- function(base_size = 9, base_family = "") {
        # require(grid)
        theme_bw(base_size = base_size, base_family = base_family) %+replace%
          theme(
            axis.title.x = element_text(vjust = 0),
            axis.title.y = element_text(angle = 90, vjust = 1.25),
            axis.text.y = element_text(angle = 90),
            axis.ticks = element_line(colour = "black", size = 0.25),
            legend.background = element_rect(fill = NA, colour = NA),
            legend.direction = "vertical",
            legend.key = element_rect(fill = NA, colour = "white"),
            legend.text = element_text(),
            legend.title = element_text(face = "bold", hjust = 0),
            panel.border = element_rect(fill = NA, colour = "black"),
            panel.grid.major = element_line(colour = "grey92", size = 0.3, linetype = 1),
            panel.grid.minor = element_blank(),
            plot.title = element_text(vjust = 1),
            strip.background = element_rect(fill = "grey90", colour = "black", size = 0.3),
            strip.text = element_text()
          )
      }
    }
    # generate animation based on method html or mp4 
    if(method == "html"){
      # plot crawl package using ggplot
      saveHTML(expr = {
        par(mar = c(4.1, 4.1, 0.1, 0.1))
        for (current_time in seq_along(time.grid)) {
          # find current time
          time_point <- time.grid[[current_time]]
          
          # initiate print list for data and index
          df_list <- list()
          df_today_list <- list()
          k <- 1
          
          # find tail length for each animal and each simulation
          for (id in ID_names) {
            for (samp in unique(paths.interp[[1]]$key)) {
              # filter df to each animal
              df_indiv <- paths.interp[[1]] %>%
                filter(ID == id & key == samp)
              
              # find data for all dates by delta
              df_indiv %<>% filter(!!as.symbol(Time.name) %in% time.grid)
              
              # filter df to have date equal to current time or less
              df_indiv %<>%
                filter(!!as.symbol(Time.name) <= time_point)
              
              # filter the data to have all the time before current time
              df <- df_indiv
              
              # filter the data to have current time only
              df_today <- df_indiv %>%
                filter(!!as.symbol(Time.name) == time_point)
              
              # assign df and df_today to print list
              df_list[[k]] <- df
              df_today_list[[k]] <- df_today
              # df_sline[[k]] <- ps
              k <- k + 1
            }
          }
          # combine df and df_today list
          df <- df_list %>%
            bind_rows()
          df$ID %<>% factor(levels = as.character(ID_names))
          
          df_today <- df_today_list %>%
            bind_rows()
          df_today$ID %<>% factor(levels = as.character(ID_names))
          
          # add background if exist
          if (class(bg[[1]])[1] == "ggmap") {
            p <- ggmap(bg[[1]])
          } else {
            p <- ggplot()
          }
          
          # plot
          if (uncertainty.type == 1) {
            if (nrow(df) > 1 & length(unique(df$ID)) > 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = interaction(key, ID), colour = ID)
                ) + 
                geom_path(
                  data = df %>% 
                    filter(key == "mu"),  
                  aes(x = mu.x, y = mu.y, group = interaction(key, ID)), 
                  linetype="dotted", color = crawl.mu.color, size = 1
                )
            } else if (nrow(df) > 1 & length(unique(df$ID)) == 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = key, colour = ID)
                ) + 
                geom_path(
                  data = df %>% 
                    filter(key == "mu"),  
                  aes(x = mu.x, y = mu.y, group = key), 
                  linetype="dotted", color = crawl.mu.color, size = 1
                )
            }
            p <- p +
              geom_point(
                data = df_today,
                aes(x = mu.x, y = mu.y, colour = ID), alpha = pt.alpha, size = 3
              )
          } else if (uncertainty.type == 1 & tail.length == 0) {
            p <- p +
              geom_point(
                data = df_today %>% filter(key == "mu"),
                aes(x = mu.x, y = mu.y, colour = ID),
                alpha = pt.alpha, size = pt.wd
              )
          } else if (uncertainty.type == "blur" & tail.length == 0) {
            p <- p + 
              geom_blurry(
                data = df_today, aes(x = mu.x, y = mu.y, colour = ID, size = se.mu.y), alpha = pt.alpha
              ) +
              scale_size(guide = NULL)
          } else if (uncertainty.type == "blur" & tail.length > 0){
            if (nrow(df) > 1 & length(unique(df$ID)) > 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = interaction(key, ID)), 
                  color = crawl.mu.color, alpha = 0.01, size = tail.wd + 3
                )
            } else if (nrow(df) > 1 & length(unique(df$ID)) == 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = key), 
                  color = crawl.mu.color, alpha = 0.01, size = tail.wd + 3
                ) 
            }
            p <- p + 
              geom_blurry(
                data = df_today, aes(x = mu.x, y = mu.y, colour = ID, size = se.mu.y), alpha = pt.alpha
              ) +
              scale_size(guide = NULL)
          }
          
          # add xlim, ylim, and theme if there is no ggmap background
          if (class(bg[[1]])[1] != "ggmap") {
            p <- p +
              xlim(xlim) +
              ylim(ylim) +
              theme_map()
          }
          
          p <- p +
            labs(title = time.grid[[current_time]]) +
            scale_colour_manual(values = pt.colors, drop = FALSE) +
            theme(legend.title = element_blank())
          
          print(p)
          
          ## timing ----
          frame <- current_time
          if (n.frames >= 10 & frame %% floor(n.frames / 10) == 0) {
            message(paste("frame ", frame, " out of ", n.frames, " (", round(frame / n.frames, 3) * 100, "%)",
                          " [", round(as.numeric(Sys.time() - cur.time), 2), " seconds]",
                          sep = ""
            ))
            cur.time <- Sys.time()
          }
        }
      })
    } else if (method == "mp4"){
      # plot crawl package using ggplot
      saveVideo(expr = {
        par(mar = c(4.1, 4.1, 0.1, 0.1))
        for (current_time in seq_along(time.grid)) {
          # find current time
          time_point <- time.grid[[current_time]]
          
          # initiate print list for data and index
          df_list <- list()
          df_today_list <- list()
          k <- 1
          
          # find tail length for each animal and each simulation
          for (id in ID_names) {
            for (samp in unique(paths.interp[[1]]$key)) {
              # filter df to each animal
              df_indiv <- paths.interp[[1]] %>%
                filter(ID == id & key == samp)
              
              # find data for all dates by delta
              df_indiv %<>% filter(!!as.symbol(Time.name) %in% time.grid)
              
              # filter df to have date equal to current time or less
              df_indiv %<>%
                filter(!!as.symbol(Time.name) <= time_point)
              
              # filter the data to have all the time before current time
              df <- df_indiv
              
              # filter the data to have current time only
              df_today <- df_indiv %>%
                filter(!!as.symbol(Time.name) == time_point)
              
              # assign df and df_today to print list
              df_list[[k]] <- df
              df_today_list[[k]] <- df_today
              # df_sline[[k]] <- ps
              k <- k + 1
            }
          }
          # combine df and df_today list
          df <- df_list %>%
            bind_rows()
          df$ID %<>% factor(levels = as.character(ID_names))
          
          df_today <- df_today_list %>%
            bind_rows()
          df_today$ID %<>% factor(levels = as.character(ID_names))
          
          # add background if exist
          if (class(bg[[1]])[1] == "ggmap") {
            p <- ggmap(bg[[1]])
          } else {
            p <- ggplot()
          }
          
          # plot
          if (uncertainty.type == 1) {
            if (nrow(df) > 1 & length(unique(df$ID)) > 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = interaction(key, ID), colour = ID)
                ) + 
                geom_path(
                  data = df %>% 
                    filter(key == "mu"),  
                  aes(x = mu.x, y = mu.y, group = interaction(key, ID)), 
                  linetype="dotted", color = crawl.mu.color, size = 1
                )
            } else if (nrow(df) > 1 & length(unique(df$ID)) == 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = key, colour = ID)
                ) + 
                geom_path(
                  data = df %>% 
                    filter(key == "mu"),  
                  aes(x = mu.x, y = mu.y, group = key), 
                  linetype="dotted", color = crawl.mu.color, size = 1
                )
            }
            p <- p +
              geom_point(
                data = df_today,
                aes(x = mu.x, y = mu.y, colour = ID), alpha = pt.alpha, size = 3
              )
          } else if (uncertainty.type == 1 & tail.length == 0) {
            p <- p +
              geom_point(
                data = df_today %>% filter(key == "mu"),
                aes(x = mu.x, y = mu.y, colour = ID),
                alpha = pt.alpha, size = pt.wd
              )
          } else if (uncertainty.type == "blur" & tail.length == 0) {
            p <- p + 
              geom_blurry(
                data = df_today, aes(x = mu.x, y = mu.y, colour = ID, size = se.mu.y), alpha = pt.alpha
              ) +
              scale_size(guide = NULL)
          } else if (uncertainty.type == "blur" & tail.length >0){
            if (nrow(df) > 1 & length(unique(df$ID)) > 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = interaction(key, ID)), 
                  color = crawl.mu.color, alpha = 0.01, size = tail.wd + 3
                )
            } else if (nrow(df) > 1 & length(unique(df$ID)) == 1) {
              p <- p +
                geom_path(
                  data = df,
                  aes(x = mu.x, y = mu.y, group = key), 
                  color = crawl.mu.color, alpha = 0.01, size = tail.wd + 3
                ) 
            }
            p <- p + 
              geom_blurry(
                data = df_today, aes(x = mu.x, y = mu.y, colour = ID, size = se.mu.y), alpha = pt.alpha
              ) +
              scale_size(guide = NULL)
          }
          
          # add xlim, ylim, and theme if there is no ggmap background
          if (class(bg[[1]])[1] != "ggmap") {
            p <- p +
              xlim(xlim) +
              ylim(ylim) +
              theme_map()
          }
          
          p <- p +
            labs(title = time.grid[[current_time]]) +
            scale_colour_manual(values = pt.colors, drop = FALSE) +
            theme(legend.title = element_blank())
          
          print(p)
          
          ## timing ----
          frame <- current_time
          if (n.frames >= 10 & frame %% floor(n.frames / 10) == 0) {
            message(paste("frame ", frame, " out of ", n.frames, " (", round(frame / n.frames, 3) * 100, "%)",
                          " [", round(as.numeric(Sys.time() - cur.time), 2), " seconds]",
                          sep = ""
            ))
            cur.time <- Sys.time()
          }
          
        }
      })
    }
  } 
}

globalVariables(c("ID", "key", "mu.x", "mu.y", "se.mu.x.", "se.mu.y"), "anipaths")
