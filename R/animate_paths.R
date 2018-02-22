################################################################################ ----
################################################################################ ----
## Henry Scharf
##
## This script makes an "animation function" that takes paths, images, and a 
## network, and makes a useful animation. I hope it can be useful to others 
## besides myself.
################################################################################ ----
################################################################################ ----
#' animate paths
#' 
#' Animates telemetry data for the purposed of EDA using smoothing splines to interpolate the observed locations. The animations are particularly useful when examining multiple simultaneous trajectories. The output of the call to \code{animate_paths()} should bring up a browser window that shows the animation. Additionally, the images generated in \code{images/} (or else the value set for \code{imgdir}) may be used with ffmpeg, latex, or other presentation software that can build animations directly from a sequence of images.
#'
#' @param paths Either a \code{data.frame} with longitudes/eastings, latitudes/northings, IDs, and times (see \code{coord}, \code{ID.name}, and \code{Time.name}), a \code{SpatialPointsDataFrame} with IDs and times, or a list of data.frames containing the longitudes, latitudes, and times for each individual (with names provided).
#' @param times If all paths are already synchornous, another option for passing the data is to define \code{paths} as a list of 2-column matrices, all with the same number of rows, and to specify the times separately via this argument. This is intended to reduce redundancy slightly, but is probably not the most common way to use this package. 
#' @param delta.t The gap in time between each frame in the animation. Specify one of \code{delta.t} or \code{n.frames}. If both are specified, \code{delta.t} is used.
#' @param n.frames The number of frames used to animate the complete time domain of the data.
#' @param interval Seconds per frame in animation. Default is 1/12 (or 12 frames per second).
#' @param paths.proj PROJ.4 string corresponding to the projection of the data. Default is "+proj=longlat". 
#' @param coord A character vector of length 2 giving the names of the longitude/easting and latitude/northing columns in the \code{paths} \code{data.frame} (in that order). This is required if \code{paths} is not a \code{SpatialPointsDataFrame}.  
#' @param Time.name The name of the columns in \code{paths} gving the observation times. This column must be of class \code{POSIXt}, or numeric.
#' @param ID.name The name of the column in \code{paths} that identifies each individual. If left as \code{NULL} (default), a single individual is assumed.
#' @param covariate.name The name of the column in \code{paths} that identifies the covariate to be mapped to a ring of color around each point.
#' @param covariate.colors vector of colors which will be used in their given order to make a color ramp (see \code{colorRamp()})
#' @param covariate.thresh if changed from its default value of \code{NULL}, the interpolated value of the covariate will be binarized based on this numeric value.
#' @param covariate.legend.loc either the location of the covariate legend, or \code{NA} if no legend is desired
#' @param background Three possibilities: (1) A single background image over which animation will be overlayed, or a list of images corresponding to each frame. (2) A list with values \code{location} (long/lat), \code{zoom}, and \code{maptype} (see \code{ggmap::get_map()}) which will be used to generate a background for the animation based on Google maps tiles. (3) A logical value of \code{TRUE}, which will cue the function to get the best Google Map tile combination it can come up with. Note: \code{ggmap} must be installed for (2) and (3).
#' @param bg.axes logical: should animation place axis labels when using a background image (default is \code{TRUE}). If \code{RGoogleMaps} is used to produce background, labels will be "northing" and "easting". Otherwise, the strings given to \code{coord} will be used.
#' @param method either \code{"html"} (default) or \code{"mp4"}. The latter requires the user has installed \code{ffmpeg} (see \code{?animation::saveVideo()}).
#' @param pt.colors A vector of colors to be used for each individual in the animation. Default values come from Color Brewer palettes. When a network is provided, this is ignored and individuals are all colored black. If \code{NA}, no plot colors are chosen to distinguish individuals. This can be useful when making animations involving a covariate. Consider also setting \code{legend.loc} to \code{NA} in this case.
#' @param dimmed Numeric vector of individuals to "dim" in the animation. Order corresponds to the order of the ID.name variable, or order of paths list.
#' @param network Array of dimensions (# individuals) \eqn{\times} (# individuals) \eqn{\times} \code{n.frames} that gives a dyanmic network structure among the individuals. 
#' @param network.times Numeric vector. If network time grid doesn't match \code{n.frames}, supply the times at which the network has been evaluated so it can be interpolated using smoothing splines.
#' @param network.thresh Network structure is summarized in the animation in a binary way, regardless of whether or not the \code{network} is continuously weighted or not. The value of \code{network.thresh} determines the level below which no connection is shown, and above which an active connection is shown via colored rings and connecting segments.
#' @param network.colors A symmetric matrix of dimension \code{length(paths)} \eqn{\times} \code{length(paths)} giving the colors associated with each pairwise relationship.
#' @param plot.date Logical variable toggling date text at the time center of the animation.
#' @param legend.loc passed to first argument of \code{legend()} function. Default is \code{"topright"}. \code{NA} removes legend.
#' @param tail.length Length of the tail trailing each individual.
#' @param xlim Boundaries for plotting. If left undefined, the range of the data will be used.
#' @param ylim Boundaries for plotting. If left undefined, the range of the data will be used.
#' @param main Title for each frame. SOON: support for changing titles to allow for, say, dates.
#' @param bg.opts Options passed to \code{plot()} function call that makes background in each frame. For example, this could be used to specify blue ocean and gray landcover if \code{background} is a \code{SpatialPolygonsDataFrame} and \code{bg.opts = list(bg = "dodgerblue4", col = "gray", border = "gray")}.
#' @param res Resolution of images in animation. Increase this for higher quality (and larger) images.
#' @param override Logical variable toggling where or not to override warnings about how long the animation procedure will take.
#' @param bs default is \code{"'tp'"} (thin plate splines), but this can be any spline basis supported by \code{s()} in the \code{mgcv} package.
#' @param max.knots maximum number of allowed knots. This actual number of knots used in the fitting will be \code{min(max.knots, #observations_i)}.
#' @param uncertainty.level value in \eqn{(0, 1)} corresponding to \code{level} at which to draw uncertainty ellipses. \code{NA} (default) results in no ellipses.
#' @param return.paths logical. Default is \code{FALSE}, but if \code{TRUE} then the interpolated paths are returned and no animation is produced.
#' @param ... other arguments to be passed to \code{ani.options} to animation options such as the time interval between image frames.
#'
#' @return video file, possibly a directory containing the individual images, or interpolated paths.
#' @export
#' @importFrom mgcv gam s
#' @importFrom stats as.formula predict
#' @importFrom animation ani.options
#' @importFrom graphics par plot mtext axis segments points legend
#' 
#' @examples ##
#' vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#' vultures_paths <- vultures[vultures$POSIX > as.POSIXct("2009-01-01", origin = "1970-01-01") & 
#'                              vultures$POSIX < as.POSIXct("2009-04-05", origin = "1970-01-01"), ]
#' animate_paths(paths = vultures_paths,
#'               delta.t = "month",
#'               coord = c("location.long", "location.lat"),
#'               Time.name = "POSIX",
#'               ID.name = "individual.local.identifier")
#' \donttest{
#' readline("Press [enter] to continue.")
#' background <- list(location = c(-90, 10),
#'                    zoom = 3,
#'                    maptype = "satellite")
#' COVARIATE <- cos(as.numeric(vultures_paths$timestamp) / 
#'                    diff(range(as.numeric(vultures_paths$timestamp))) * 4 * pi)
#' animate_paths(paths = cbind(vultures_paths, COVARIATE),
#'               delta.t = "week",
#'               coord = c("location.long", "location.lat"),
#'               Time.name = "POSIX", covariate.name = "COVARIATE", 
#'               covariate.colors = RColorBrewer::brewer.pal(n = 9, "RdYlGn"),
#'               ID.name = "individual.local.identifier", 
#'               background = background)
#' }
animate_paths <- function(paths, times = NULL, delta.t = NULL, n.frames = 12*30, interval = 1/12,
                          paths.proj = "+proj=longlat", coord = c("x", "y"), Time.name = "time", 
                          covariate.name = NULL, covariate.colors = c("black", "white"), 
                          covariate.thresh = NULL, covariate.legend.loc = "right",
                          ID.name = NULL, background = NULL, bg.axes = TRUE, method = "html", 
                          pt.colors = NULL, dimmed = NULL, res = 1.5, plot.date = TRUE, legend.loc = "topright",
                          network = NULL, network.times = NULL, network.thresh = 0.5, network.colors = NULL,
                          tail.length = 5, xlim = NULL, ylim = NULL, main = NULL, bg.opts = NULL,
                          bs = "'tp', fx=T", max.knots = 20, uncertainty.level = NA, override = FALSE, return.paths = FALSE, ...){
  ## SpatialPointsDataFrame ----
  if(class(paths) == "SpatialPointsDataFrame"){
    message("\n SpatialPointsDataFrame object detected.")
    coord <- sp::coordnames(paths)
    paths <- as.data.frame(paths)
    ID_names <- unique(paths[, ID.name])
  }
  ## take data from "raw" df form to lists organized by individual ----
  if(is.data.frame(paths)){
    paths.df <- paths
    ## get individual's names
    ID_names <- unique(paths[, ID.name])
    if(is.null(ID.name)){
      paths.df <- cbind(paths.df, "ID" = rep(1, nrow(paths.df)))
      ID.name <- "ID"
    }
    paths <- vector("list", length(unique(paths.df[, ID.name])))
    paths <- sapply(unique(unique(paths.df[, ID.name])), function(id){
      paths.df[paths.df[, ID.name] == id, c(coord, Time.name, covariate.name)]
    }, simplify = F)
    time.range <- range(paths.df[, Time.name])
  } else {
    ID_names <- 1:length(paths)
    if(!is.null(times)){
      time.range <- range(times)
    } else {
      if(!(Time.name %in% names(paths[[1]]))){
        stop("Argument 'times' and/or 'Time.name' arguments not/misspecified.")
      }
      time.range <- range(unlist(lapply(paths, function(path){range(path[, Time.name])})))
    }
  }
  n.indiv <- length(paths)
  ## if 'times' supplied, but paths is a list of matrices/data.frames, then need to append the times
  if(!is.null(times) & all(lapply(paths, nrow) == nrow(paths[[1]]))){
    paths <- lapply(paths, function(path.i){
      df <- as.data.frame(cbind(path.i, times))
      names(df) <- c(coord, "time"); Time.name <- "time"
      return(df)
    })
  } 
  ## interpolation ----
  ## timing 
  if(!is.null(delta.t)){
    time.grid <- seq(time.range[1], time.range[2], by = delta.t)
  }
  if(is.null(delta.t)){
    if(is.null(n.frames)){
      if(!is.null(times)){
        delta.t <- mean(diff(times))
        time.grid <- seq(time.range[1], time.range[2], by = delta.t)
      } else {
        stop("One of 'delta.t,' 'n.frames,' or 'times' must be supplied.")
      }
    }
    time.grid <- seq(time.range[1], time.range[2], l = n.frames)
    delta.t <- diff(time.range) / n.frames
  }
  n.frames <- length(time.grid)
  ## warn if this animation might take a long time
  if(!override){
    if(length(paths) > 25 || n.frames > 1000){
      readline(prompt=paste(length(paths), "individuals detected using", n.frames, "frames. This could take a while. Press [enter] to continue or [esc] to stop now."))
      message("Okay, here we go!")
    }
  }
  ## do interpolation
  paths.interp <- lapply(paths, function(path.i){
    time.grid.i <- time.grid[time.grid >= min(path.i[, Time.name]) & 
                               time.grid <= max(path.i[, Time.name])]
    t.i <- as.numeric(path.i[, Time.name])
    n.knots <- min(max.knots, length(t.i))
    spline.fit.x <- mgcv::gam(as.formula(paste0("path.i[, coord[1]] ~ s(t.i, bs = ", bs, ", k = ", n.knots, ")")),
                              knots = data.frame(t.i=t.i))
    spline.fit.y <- mgcv::gam(as.formula(paste0("path.i[, coord[2]] ~ s(t.i, bs = ", bs, ", k = ", n.knots, ")")),
                              knots = data.frame(t.i=t.i))
    pred.x <- predict(spline.fit.x, data.frame(t.i = as.numeric(time.grid.i)), se.fit = T)
    pred.y <- predict(spline.fit.y, data.frame(t.i = as.numeric(time.grid.i)), se.fit = T)
    out <- matrix(NA, nrow = length(time.grid), ncol = 4)
    out[time.grid %in% time.grid.i, ] <- cbind(pred.x$fit, pred.y$fit, pred.x$se.fit, pred.y$se.fit)
    return(out)
  })
  ## do covariate interpolation if needed
  if(!is.null(covariate.name)){
    covariate.interp <- lapply(paths, function(path.i){
      time.grid.i <- time.grid[time.grid >= min(path.i[, Time.name]) & time.grid <= max(path.i[, Time.name])]
      spline.fit.covariate <- mgcv::gam(as.formula(paste0("path.i[, covariate.name] ~ s(t.i, bs = ", bs, ", k = ", n.knots, ")")),
                                knots = data.frame(t.i=t.i))
      out <- rep(NA, length(time.grid))
      out[time.grid %in% time.grid.i] <- predict(spline.fit.x, data.frame(t.i = as.numeric(time.grid.i)), se.fit = T)
      return(out)
    })
    covariate.ticks <- pretty(unlist(lapply(covariate.interp, range, na.rm = T)))
    covariate.range <- range(covariate.ticks)
  }
  ## check for single points
  for(i in 1:length(paths.interp)){
    if(sum(!is.na(paths.interp[[i]])) < 4){
      stop(paste("Current framing yields an interpolation with one or fewer points for individual: ", 
                 ID_names[i], ". Add more frames or reduce delta.t.", sep = ""))
    }
  }
  if(!is.null(network)){
    if(dim(network)[3] != n.frames & !is.null(network.times)){
      network.interp <- array(t(sapply(1:n.indiv, function(id){
        t(sapply(1:n.indiv, function(id2){
          predict(
            mgcv::gam(as.formula(paste0("network[id, id2, ] ~ s(t.i, bs = ", bs, ", k = ", n.knots, ")")),
                      knots = data.frame(t.i=t.i)), data.frame(t.i = as.numeric(time.grid.i)))
        }))
      })), dim = c(n.indiv, n.indiv, n.frames))
      network <- network.interp
    }
  }
  ## if return.paths, then stop ----
  if(return.paths){
    paths_animation <- list("paths.interp" = paths.interp, "times" = time.grid, "paths" = paths,
                            "coord" = coord, "Time.name" = Time.name)
    class(paths_animation) <- "paths_animation"
    return(paths_animation)
  }
  ## make list of background images for each frame ----
  if(isTRUE(background)){
    bounding_box <- c(apply(matrix(unlist(lapply(paths.interp, function(x) range(x, na.rm = T))), nrow = 2), 1, range))
    center <- diag(matrix(bounding_box, 2, 2) %*% matrix(c(0.5, 0.5, 0.4, 0.6), 2, 2))
    url <- ggmap::get_map(location = bounding_box, maptype = "terrain", urlonly = T)
    zoom <- as.numeric(substr(url, regexpr("zoom=", url)[1] + 5, regexpr("zoom=", url)[1] + 5))
    background <- list("location" = center, "zoom" = zoom, "maptype" = "hybrid")
  }
  if(sum(names(background) %in% c("location", "zoom", "maptype")) == 3){
    background <- ggmap::get_map(location = background$location, zoom = background$zoom, maptype = background$maptype)
    # class(background) <- "google"
  }
  if(length(background) == 1 || !class(background) == "list"){
    if(!class(background)[1] == "list"){
      background <- list(background)
    }
    bg <- sapply(1:n.frames, function(x) return(background[[1]]), simplify = F)
  }
  ## get fixed plot limits ----
  if(is.null(xlim)){
    xlim <- range(lapply(paths, function(path.i){range(path.i[, coord[[1]]])}))
  }
  if(is.null(ylim)){
    ylim <- range(lapply(paths, function(path.i){range(path.i[, coord[[2]]])}))
  }
  ## pt + covariate colors ----
  if(is.null(network)){
    if(is.null(pt.colors)){
      pt.colors <- c(RColorBrewer::brewer.pal(9, "Set1"),
                     RColorBrewer::brewer.pal(9, "GnBu")[3:9],
                     RColorBrewer::brewer.pal(9, "OrRd")[3:9],
                     RColorBrewer::brewer.pal(9, "BuPu")[3:9],
                     RColorBrewer::brewer.pal(9, "YlGn")[3:9],
                     RColorBrewer::brewer.pal(9, "PuBu")[3:9],
                     RColorBrewer::brewer.pal(9, "YlOrBr")[3:9],
                     RColorBrewer::brewer.pal(9, "YlGnBu")[3:9],
                     RColorBrewer::brewer.pal(9, "PuRd")[3:9],
                     RColorBrewer::brewer.pal(9, "PuBuGn")[3:9],
                     RColorBrewer::brewer.pal(9, "Purples")[3:9],
                     RColorBrewer::brewer.pal(9, "Oranges")[3:9])
    }
    pt.colors <- cbind(pt.colors[(1:length(paths) - 1) %% length(pt.colors) + 1], scales::alpha("lightgray", 0.5))
  } else {
    if(is.null(pt.colors)){
      pt.colors <- "black"
    }
    pt.colors <- cbind(pt.colors[(1:length(paths) - 1) %% length(pt.colors) + 1], scales::alpha("lightgray", 0.5))
  }
  if(!is.null(covariate.name)){
    color_covariate_function <- grDevices::colorRamp(covariate.colors)
  }
  ## network colors ----
  if(!is.null(network)){
    cliques <- get.network.colors(binary.network = network > network.thresh)
    # if(is.null(network.colors)){
    #   network.colors <- matrix("wheat3", length(paths), length(paths))
    #   for(t in 1:dim(network)[3]){
    #     cliques.t <- igraph::max_cliques(graph_from_adjacency_matrix(network[, , t] > network.thresh, 
    #                                                                 mode = "undirected", diag = F, min = 2))
    #     for(clique in cliques.t){
    #       
    #       network.colors[, , t]
    #     }
    #   }
    #   mean.network <- apply(network, 1:2, max)
    #   RCBn <- 8
    #   network.colors[rev(order(mean.network))[-(1:length(paths))]][1:(2*RCBn)] <- 
    #     rep(RColorBrewer::brewer.pal(RCBn, "Dark2"), rep(2, RCBn))
    # }
  }
  # for(dim in dimmed){
  #   for(dim2 in dimmed){
  #     network.colors[dim, dim2] <- network.colors[dim2, dim] <- scales::alpha(network.colors[dim, dim2], 0.5)
  #   }
  # }
  ## adjust center + scale for google map ----
  center <- c(0, 0); scale <- c(1, 1)
  if(class(bg[[1]])[1] == "ggmap"){
    bb.map <- as.data.frame(t(matrix(as.numeric(attr(bg[[1]], "bb")), 2, 2)))
    sp::coordinates(bb.map) = c(2, 1); sp::proj4string(bb.map) <- sp::CRS("+proj=longlat")
    bb <- sp::spTransform(bb.map, CRSobj = sp::CRS(paste("+proj=merc +a=6378137 +b=6378137", 
                                                         "+lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0", 
                                                         "+k=1.0 +units=m +nadgrids=@null +no_defs")))
    bb <- sp::coordinates(bb)
    center <- bb[1, ];
    scale <- 1280 / apply(bb, 2, diff)
    paths.interp <- sapply(1:length(paths.interp), function(i){
      path.i <- paths.interp[[i]]
      if(!is.null(paths.proj)){
        path.i.sp.ind <- which(!is.na(path.i[, 1]))
        path.i.sp <- as.data.frame(path.i[path.i.sp.ind, ])
        path.i.sp[path.i.sp[, 2] < -90, 2] <- -89
        sp::coordinates(path.i.sp) <- c(1, 2)
        sp::proj4string(path.i.sp) <- sp::CRS(paths.proj)
        path.i[path.i.sp.ind, ] <- 
          sp::spTransform(path.i.sp, CRSobj = sp::CRS(paste("+proj=merc +a=6378137 +b=6378137", 
                                                            "+lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0", 
                                                            "+k=1.0 +units=m +nadgrids=@null +no_defs")))@coords
      }
      a <- t((t(path.i) - center) * scale)
    }, simplify = F)
  }
  ## animate ----
  animation::ani.options(interval = interval, ani.width = res*480, ani.height = res*480, verbose = F)
  start.time <- cur.time <- Sys.time()
  if(method == "html"){
    animation::saveHTML(expr = {
    for(frame in 1:n.frames){
      ## add background ----
      if(class(bg[[frame]])[1] == "ggmap"){
        par(mar = c(0.1, 0.1, 0.1, 0.1))
        if(bg.axes){
          par(mar = c(4.1, 4.1, 0.1, 0.1))
        }
        do.call(plot, c(list("x" = bg[[frame]]), xlab = "", ylab = "", bg.opts))
        if(bg.axes){
          mtext(text = "easting [m]", side = 1, line = 2.6)
          axis(1, at = seq(0, 1280, l=5), signif(seq(0, 1280/scale[1], l=5), 3))
          mtext(text = "northing [m]", side = 2, line = 2.6)
          axis(2, at = seq(0, 1280, l=5), signif(seq(0, 1280/scale[2], l=5), 3))
        }
      } else {
        par(mar = c(0.1, 0.1, 0.1, 0.1))
        if(bg.axes){
          par(mar = c(4.1, 4.1, 0.1, 0.1))
        }
        if(length(grep("Spatial", class(bg[[frame]])) == 1)){
          do.call(sp::plot, c(list("x" = bg[[frame]], xlab = "", ylab = "",
                               "xlim" = xlim, "ylim" = ylim, "main" = main),
                          bg.opts))
        } else {
          do.call(plot, c(list("x" = bg[[frame]], xlab = "", ylab = "",
                               "xlim" = xlim, "ylim" = ylim, "main" = main),
                          bg.opts))
        }
        if(bg.axes){
          mtext(text = coord[1], side = 1, line = 2.6)
          mtext(text = coord[2], side = 2, line = 2.6)
        }
      }
      ## add tails and (pairwise) network segments ----
      for(id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])){
        if(frame > 1){
          segments(x0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), 1],
                   x1 = paths.interp[[id]][max(2, frame - tail.length):frame, 1],
                   y0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), 2],
                   y1 = paths.interp[[id]][max(2, frame - tail.length):frame, 2],
                   col = "gray87", lwd = 6*res)
        }
        # ## pairwise network approach ----
        # if(!is.null(network)){
        #   for(id2 in (1:length(paths))[-id]){
        #     segments(x0 = paths.interp[[id]][frame, 1],
        #              y0 = paths.interp[[id]][frame, 2],
        #              x1 = paths.interp[[id2]][frame, 1],
        #              y1 = paths.interp[[id2]][frame, 2],
        #              col = scales::alpha(network.colors[id, id2], 0.5),
        #              lwd = 4*(network[id, id2, frame] > network.thresh))
        #   }
        # }
      }
      ## clique network approach for segments ----
      if(!is.null(network)){
        if(!is.na(cliques$cliques[[frame]][1])){
          for(cl in 1:length(cliques$cliques[[frame]])){
            for(id in as.numeric(cliques$cliques[[frame]][[cl]])){
              for(id2 in as.numeric(cliques$cliques[[frame]][[cl]])[-id]){
                segments(x0 = paths.interp[[id]][frame, 1],
                         y0 = paths.interp[[id]][frame, 2],
                         x1 = paths.interp[[id2]][frame, 1],
                         y1 = paths.interp[[id2]][frame, 2],
                         col = scales::alpha(cliques$colors[[frame]][cl], 0.5),
                         lwd = 3*res)
              }
            }
          }
        }
      }
      ## add points and network rings ----
      radius <- rep(1.2*res, length(paths))
      ## clique-wise approach ----
      for(id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])){
        covariate.ring <- pt.colors[id, 1 + id %in% dimmed]; lwd <- 1/1.5
        if(!is.null(covariate.name)){
          covariate.ring <- NA
          if(!is.na(covariate.interp[[id]][frame])){
            if(!is.null(covariate.thresh)){
              covariate.normalized.value <- (covariate.interp[[id]][frame] >= covariate.thresh)
            } else {
              covariate.normalized.value <-
                (covariate.interp[[id]][frame] - covariate.range[1]) / diff(covariate.range)
            }
            covariate.ring <-
              grDevices::rgb(color_covariate_function(covariate.normalized.value), maxColorValue = 255)
            lwd = 2.5/1.5
          }
        }
        if(is.na(pt.colors[1])){
          points(matrix(paths.interp[[id]][frame, 1:2], ncol = 2),
                 col = covariate.ring, pch = 19, cex = 0.85*res)
          if(!is.na(paths.interp[[id]][frame, 3]) & !is.na(uncertainty.level)){
            lines(ellipse::ellipse(x = diag(paths.interp[[id]][frame, 3:4]^2),
                          centre = paths.interp[[id]][frame, 1:2], level = uncertainty.level),
                  lty = 3)
          }
        } else {
          points(matrix(paths.interp[[id]][frame, 1:2], ncol = 2),
                 col = covariate.ring, bg = pt.colors[id, 1 + id %in% dimmed],
                 pch = 21, lwd = lwd*res, cex = 0.85*res)
          if(!is.na(paths.interp[[id]][frame, 3]) & !is.na(uncertainty.level)){
            lines(ellipse::ellipse(x = diag(paths.interp[[id]][frame, 3:4]^2),
                          centre = paths.interp[[id]][frame, 1:2], level = uncertainty.level),
                  lty = 3)
          }
        }
      }
      if(!is.null(network)){
        if(!is.na(cliques$cliques[[frame]][1])){
          for(cl in 1:length(cliques$cliques[[frame]])){
            for(id in as.numeric(cliques$cliques[[frame]][[cl]])){
              for(id2 in (1:length(paths))[-id]){
                points(matrix(paths.interp[[id]][frame, ], ncol = 2),
                       col = scales::alpha(colour = cliques$colors[[frame]][cl], 1),
                       cex = radius[id], lwd = 3*res)
              }
              radius[id] <- radius[id] + 0.85*res*(network[id, id2, frame] > network.thresh)
            }
          }
        }
      }
      # # pairwise approach ----
      # for(id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])){
      #   for(id2 in (1:length(paths))[-id]){
      #     # points(matrix(paths.interp[[id]][frame, ], ncol = 2), col = pt.colors[id2], cex = 2.5*network[id, id2, frame],
      #     #        lwd = 5*network[id, id2, frame])
      #     points(matrix(paths.interp[[id]][frame, ], ncol = 2), col = scales::alpha(network.colors[id, id2], 1),
      #            cex = radius[id],
      #            lwd = 4*(network[id, id2, frame] > network.thresh))
      #     radius[id] <- radius[id] + 1.25*(network[id, id2, frame] > network.thresh)
      #   }
      # }
      # ## add pairwise legend ----
      # if(is.null(network)){
      #   legend("bottomleft", pch = 19, pt.cex = 1.2, col = pt.colors[, 1], legend = 1:length(paths), bty = "n")
      # } else {
      #   legend.boundaries <- c(par()$usr[1] + diff(par()$usr[1:2]) * 0.05, par()$usr[2] - diff(par()$usr[1:2]) * 0.8,
      #                          par()$usr[3] + diff(par()$usr[3:4]) * 0.05, par()$usr[4] - diff(par()$usr[3:4]) * 0.8)
      #   xs <- matrix(seq(legend.boundaries[1], legend.boundaries[2], l=length(paths)), length(paths), length(paths), byrow = T)
      #   ys <- matrix(rev(seq(legend.boundaries[3], legend.boundaries[4], l=length(paths))), length(paths), length(paths))
      #   points(xs[lower.tri(xs)], ys[lower.tri(ys)], lwd = 4*0.67, cex = 1.75*0.67,
      #          col = network.colors[lower.tri(network.colors)], xpd = T)
      #   points(rep(xs[1, 1] - diff(xs[1, 1:2]), length(paths) - 1), ys[-1, 1],
      #          pch = as.character(2:length(paths)), xpd = T)
      #   points(xs[1, -length(paths)], rep(ys[length(paths), 1] + diff(ys[1:2, 1]), length(paths) - 1),
      #          pch = as.character(1:(length(paths) - 1)), xpd = T)
      # }
      ## add cliaue-wise legend ----
      if(is.null(network)){
        legend(legend.loc, pch = 19, pt.cex = 0.8*res, col = pt.colors[, 1], legend = ID_names,
               box.lwd = 0, bty = "n", text.col = "gray60")
      } else {
        if(!is.na(cliques$cliques[[frame]][1])){
          legend(legend.loc, pch = 1, lwd = 3*res, lty = NA, pt.cex = 0.8*res, cex = res, col = cliques$colors[[frame]],
                 legend = gsub(")", "", gsub("c(", "", lapply(cliques$cliques[[frame]], as.numeric), fixed = T)), box.lwd = 0)
        }
      }
      if(!is.null(covariate.name)){
        if(is.null(covariate.thresh)){
        color_ticks <- (covariate.ticks - covariate.range[1])/diff(covariate.range)
        legend(covariate.legend.loc, pch = 21 - 2 * is.na(pt.colors[1]), lwd = 2.5/1.5*res,
               col = grDevices::rgb(color_covariate_function(color_ticks), maxColorValue = 255),
               legend = covariate.ticks,
               box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name)
        } else {
          legend(covariate.legend.loc, pch = 21 - 2 * is.na(pt.colors[1]), lwd = 2.5/1.5*res,
                 col = grDevices::rgb(color_covariate_function(0:1), maxColorValue = 255),
                 legend = paste(c("<", ">="), covariate.thresh),
                 box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name)
        }
      }
      ## add date/time ----
      if("POSIXt" %in% class(time.grid[frame])){
        # mtext(text = as.Date(time.grid[frame]), side = 3, line = -3, cex = res)
        mtext(text = (time.grid[frame]), side = 3, line = -3, cex = res)
      }
      ## timing ----
      if(n.frames >= 10 & frame %% floor(n.frames / 10) == 0){
        message(paste("\n frame ", frame, " out of ", n.frames, " (", round(frame/n.frames, 3)*100, "%)",
                      " [", round(as.numeric(Sys.time() - cur.time), 2), " seconds]", sep = ""))
        cur.time <- Sys.time()
      }
    }
  }, ...)
  }
  if(method == "mp4"){
    animation::saveVideo(expr = {
      for(frame in 1:n.frames){
        ## add background ----
        if(class(bg[[frame]])[1] == "ggmap"){
          par(mar = c(0.1, 0.1, 0.1, 0.1))
          if(bg.axes){
            par(mar = c(4.1, 4.1, 0.1, 0.1))
          }
          do.call(plot, c(list("x" = bg[[frame]]), xlab = "", ylab = "", bg.opts))
          if(bg.axes){
            mtext(text = "easting [m]", side = 1, line = 2.6)
            axis(1, at = seq(0, 1280, l=5), signif(seq(0, 1280/scale[1], l=5), 3))
            mtext(text = "northing [m]", side = 2, line = 2.6)
            axis(2, at = seq(0, 1280, l=5), signif(seq(0, 1280/scale[2], l=5), 3))
          }
        } else {
          par(mar = c(0.1, 0.1, 0.1, 0.1))
          if(bg.axes){
            par(mar = c(4.1, 4.1, 0.1, 0.1))
          }
          if(length(grep("Spatial", class(bg[[frame]])) == 1)){
            do.call(sp::plot, c(list("x" = bg[[frame]], xlab = "", ylab = "",
                                     "xlim" = xlim, "ylim" = ylim, "main" = main),
                                bg.opts))
          } else {
            do.call(plot, c(list("x" = bg[[frame]], xlab = "", ylab = "",
                                 "xlim" = xlim, "ylim" = ylim, "main" = main),
                            bg.opts))
          }
          if(bg.axes){
            mtext(text = coord[1], side = 1, line = 2.6)
            mtext(text = coord[2], side = 2, line = 2.6)
          }
        }
        ## add tails and (pairwise) network segments ----
        for(id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])){
          if(frame > 1){
            segments(x0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), 1],
                     x1 = paths.interp[[id]][max(2, frame - tail.length):frame, 1],
                     y0 = paths.interp[[id]][max(1, frame - tail.length - 1):(frame - 1), 2],
                     y1 = paths.interp[[id]][max(2, frame - tail.length):frame, 2],
                     col = "gray87", lwd = 6*res)
          }
          # ## pairwise network approach ----
          # if(!is.null(network)){
          #   for(id2 in (1:length(paths))[-id]){
          #     segments(x0 = paths.interp[[id]][frame, 1],
          #              y0 = paths.interp[[id]][frame, 2],
          #              x1 = paths.interp[[id2]][frame, 1],
          #              y1 = paths.interp[[id2]][frame, 2],
          #              col = scales::alpha(network.colors[id, id2], 0.5),
          #              lwd = 4*(network[id, id2, frame] > network.thresh))
          #   }
          # }
        }
        ## clique network approach for segments ----
        if(!is.null(network)){
          if(!is.na(cliques$cliques[[frame]][1])){
            for(cl in 1:length(cliques$cliques[[frame]])){
              for(id in as.numeric(cliques$cliques[[frame]][[cl]])){
                for(id2 in as.numeric(cliques$cliques[[frame]][[cl]])[-id]){
                  segments(x0 = paths.interp[[id]][frame, 1],
                           y0 = paths.interp[[id]][frame, 2],
                           x1 = paths.interp[[id2]][frame, 1],
                           y1 = paths.interp[[id2]][frame, 2],
                           col = scales::alpha(cliques$colors[[frame]][cl], 0.5),
                           lwd = 3*res)
                }
              }
            }
          }
        }
        ## add points and network rings ----
        radius <- rep(1.2*res, length(paths))
        ## clique-wise approach ----
        for(id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])){
          covariate.ring <- pt.colors[id, 1 + id %in% dimmed]; lwd <- 1/1.5
          if(!is.null(covariate.name)){
            covariate.ring <- NA
            if(!is.na(covariate.interp[[id]][frame])){
              if(!is.null(covariate.thresh)){
                covariate.normalized.value <- (covariate.interp[[id]][frame] >= covariate.thresh)
              } else {
                covariate.normalized.value <-
                  (covariate.interp[[id]][frame] - covariate.range[1]) / diff(covariate.range)
              }
              covariate.ring <-
                grDevices::rgb(color_covariate_function(covariate.normalized.value), maxColorValue = 255)
              lwd = 2.5/1.5
            }
          }
          if(is.na(pt.colors[1])){
            points(matrix(paths.interp[[id]][frame, 1:2], ncol = 2),
                   col = covariate.ring, pch = 19, cex = 0.85*res)
            if(!is.na(paths.interp[[id]][frame, 3]) & !is.na(uncertainty.level)){
              lines(ellipse::ellipse(x = diag(paths.interp[[id]][frame, 3:4]^2),
                                     centre = paths.interp[[id]][frame, 1:2], level = uncertainty.level),
                    lty = 3)
            }
          } else {
            points(matrix(paths.interp[[id]][frame, 1:2], ncol = 2),
                   col = covariate.ring, bg = pt.colors[id, 1 + id %in% dimmed],
                   pch = 21, lwd = lwd*res, cex = 0.85*res)
            if(!is.na(paths.interp[[id]][frame, 3]) & !is.na(uncertainty.level)){
              lines(ellipse::ellipse(x = diag(paths.interp[[id]][frame, 3:4]^2),
                                     centre = paths.interp[[id]][frame, 1:2], level = uncertainty.level),
                    lty = 3)
            }
          }
        }
        if(!is.null(network)){
          if(!is.na(cliques$cliques[[frame]][1])){
            for(cl in 1:length(cliques$cliques[[frame]])){
              for(id in as.numeric(cliques$cliques[[frame]][[cl]])){
                for(id2 in (1:length(paths))[-id]){
                  points(matrix(paths.interp[[id]][frame, ], ncol = 2),
                         col = scales::alpha(colour = cliques$colors[[frame]][cl], 1),
                         cex = radius[id], lwd = 3*res)
                }
                radius[id] <- radius[id] + 0.85*res*(network[id, id2, frame] > network.thresh)
              }
            }
          }
        }
        # # pairwise approach ----
        # for(id in c(dimmed, (1:length(paths))[!(1:length(paths)) %in% dimmed])){
        #   for(id2 in (1:length(paths))[-id]){
        #     # points(matrix(paths.interp[[id]][frame, ], ncol = 2), col = pt.colors[id2], cex = 2.5*network[id, id2, frame],
        #     #        lwd = 5*network[id, id2, frame])
        #     points(matrix(paths.interp[[id]][frame, ], ncol = 2), col = scales::alpha(network.colors[id, id2], 1),
        #            cex = radius[id],
        #            lwd = 4*(network[id, id2, frame] > network.thresh))
        #     radius[id] <- radius[id] + 1.25*(network[id, id2, frame] > network.thresh)
        #   }
        # }
        # ## add pairwise legend ----
        # if(is.null(network)){
        #   legend("bottomleft", pch = 19, pt.cex = 1.2, col = pt.colors[, 1], legend = 1:length(paths), bty = "n")
        # } else {
        #   legend.boundaries <- c(par()$usr[1] + diff(par()$usr[1:2]) * 0.05, par()$usr[2] - diff(par()$usr[1:2]) * 0.8,
        #                          par()$usr[3] + diff(par()$usr[3:4]) * 0.05, par()$usr[4] - diff(par()$usr[3:4]) * 0.8)
        #   xs <- matrix(seq(legend.boundaries[1], legend.boundaries[2], l=length(paths)), length(paths), length(paths), byrow = T)
        #   ys <- matrix(rev(seq(legend.boundaries[3], legend.boundaries[4], l=length(paths))), length(paths), length(paths))
        #   points(xs[lower.tri(xs)], ys[lower.tri(ys)], lwd = 4*0.67, cex = 1.75*0.67,
        #          col = network.colors[lower.tri(network.colors)], xpd = T)
        #   points(rep(xs[1, 1] - diff(xs[1, 1:2]), length(paths) - 1), ys[-1, 1],
        #          pch = as.character(2:length(paths)), xpd = T)
        #   points(xs[1, -length(paths)], rep(ys[length(paths), 1] + diff(ys[1:2, 1]), length(paths) - 1),
        #          pch = as.character(1:(length(paths) - 1)), xpd = T)
        # }
        ## add cliaue-wise legend ----
        if(is.null(network)){
          legend(legend.loc, pch = 19, pt.cex = 0.8*res, col = pt.colors[, 1], legend = ID_names,
                 box.lwd = 0, bty = "n", text.col = "gray60")
        } else {
          if(!is.na(cliques$cliques[[frame]][1])){
            legend(legend.loc, pch = 1, lwd = 3*res, lty = NA, pt.cex = 0.8*res, cex = res, col = cliques$colors[[frame]],
                   legend = gsub(")", "", gsub("c(", "", lapply(cliques$cliques[[frame]], as.numeric), fixed = T)), box.lwd = 0)
          }
        }
        if(!is.null(covariate.name)){
          if(is.null(covariate.thresh)){
            color_ticks <- (covariate.ticks - covariate.range[1])/diff(covariate.range)
            legend(covariate.legend.loc, pch = 21 - 2 * is.na(pt.colors[1]), lwd = 2.5/1.5*res,
                   col = grDevices::rgb(color_covariate_function(color_ticks), maxColorValue = 255),
                   legend = covariate.ticks,
                   box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name)
          } else {
            legend(covariate.legend.loc, pch = 21 - 2 * is.na(pt.colors[1]), lwd = 2.5/1.5*res,
                   col = grDevices::rgb(color_covariate_function(0:1), maxColorValue = 255),
                   legend = paste(c("<", ">="), covariate.thresh),
                   box.lwd = 0, bty = "n", text.col = "gray60", lty = NA, title = covariate.name)
          }
        }
        ## add date/time ----
        if("POSIXt" %in% class(time.grid[frame])){
          # mtext(text = as.Date(time.grid[frame]), side = 3, line = -3, cex = res)
          mtext(text = (time.grid[frame]), side = 3, line = -3, cex = res)
        }
        ## timing ----
        if(n.frames >= 10 & frame %% floor(n.frames / 10) == 0){
          message(paste("\n frame ", frame, " out of ", n.frames, " (", round(frame/n.frames, 3)*100, "%)",
                        " [", round(as.numeric(Sys.time() - cur.time), 2), " seconds]", sep = ""))
          cur.time <- Sys.time()
        }
      }
    }, ...)
  }
  message(paste("Total time", round(as.numeric(Sys.time()) - as.numeric(start.time), 2), "seconds."))
  return(NULL)
}

globalVariables(c("n.knots", "t.i", "spline.fit.x", "spline.fit.y", "time.grid.i"), "anipaths")

#' get.network.colors()
#' Finds all maximal cliques in the network at each time point and tries to assign them a useful coloring
#'
#' @param binary.network a 3D array giving the time-varying adjecency matrix of a dynamic network.
#'
#' @return a list of two elements: a list of the maximal cliques at each time, and c list with colors for each clique at each time 
#' @export
get.network.colors <- function(binary.network){
  ## unique cliques ----
  cliques <- sapply(1:dim(binary.network)[3], function(t){
    rev(lapply(igraph::max_cliques(igraph::graph_from_adjacency_matrix(binary.network[, , t], 
                                                                       mode = "undirected", diag = F), min = 2), sort))}, 
    simplify = F)
  unique.cliques <- NULL
  clique.lengths <- unlist(lapply(cliques, length))
  for(t in (1:dim(binary.network)[3])[which(clique.lengths > 0)]){
    unique.cliques <- unique(c(unique.cliques, cliques[[t]]))
  }
  network.color.options <- unique(c(RColorBrewer::brewer.pal(8, "Dark2"),
                                    RColorBrewer::brewer.pal(9, "GnBu")[3:9],
                                    RColorBrewer::brewer.pal(9, "OrRd")[3:9],
                                    RColorBrewer::brewer.pal(9, "BuPu")[3:9],
                                    RColorBrewer::brewer.pal(9, "YlGn")[3:9],
                                    RColorBrewer::brewer.pal(9, "PuBu")[3:9],
                                    RColorBrewer::brewer.pal(9, "YlOrBr")[3:9],
                                    RColorBrewer::brewer.pal(9, "YlGnBu")[3:9],
                                    RColorBrewer::brewer.pal(9, "PuRd")[3:9],
                                    RColorBrewer::brewer.pal(9, "PuBuGn")[3:9],
                                    RColorBrewer::brewer.pal(9, "Purples")[3:9],
                                    RColorBrewer::brewer.pal(9, "Oranges")[3:9]))
  if(length(unique.cliques) > length(network.color.options)){
    network.color.options <- c(network.color.options, 
                               rep("wheat3", length(unique.cliques) - length(network.color.options)))
  }
  available.colors <- network.color.options
  clique.colors <-vector("list", dim(binary.network)[3])
  ## paint network ----
  for(t in (1:dim(binary.network)[3])){
    if(t == 1){
      clique.colors[[t]] <- rep("black", length(cliques[[t]]))
      for(c in 1:length(cliques[[t]])){
        clique.colors[[1]][c] <- available.colors[1]
        available.colors <- available.colors[-1]
      }
    }
    else {
      if(clique.lengths[t] == 0){
        cliques[[t]] <- NA
        clique.colors[[t]] <- NA
        next
      }
      available.colors <- available.colors[!(available.colors %in% clique.colors[[t-1]])]
      clique.colors[[t]] <- rep("black", length(cliques[[t]]))
      for(c in 1:length(cliques[[t]])){
        from.b4 <- lapply(cliques[[t-1]], function(c.tm1){
          intersect(as.numeric(c.tm1), as.numeric(cliques[[t]][[c]]))
        })
        ## (1) check to see if this clique is nested inside a previous one ----
        nested.b4 <- which(unlist(lapply(from.b4, function(intersection){
          identical(as.numeric(intersection), as.numeric(cliques[[t]][[c]]))
        })))
        nested.b4 <- nested.b4[which(!(clique.colors[[t-1]][nested.b4] %in% clique.colors[[t]]))]
        if(length(nested.b4) > 0){
          clique.colors[[t]][c] <- clique.colors[[t-1]][min(nested.b4)]
        } else {
          ## (2) check to see if a previous clique is nested inside this one ----
          rev.nested.b4 <- which(sapply(1:length(from.b4), function(cl){
            identical(as.numeric(from.b4[[cl]]), as.numeric(cliques[[t - 1]][[cl]]))}))
          rev.nested.b4 <- rev.nested.b4[which(!(clique.colors[[t-1]][rev.nested.b4] %in% clique.colors[[t]]))]
          if(length(rev.nested.b4) > 0){
            clique.colors[[t]][c] <- clique.colors[[t-1]][min(rev.nested.b4)]
          } else {
            ## laste resort: add a new color ----
            if(!is.na(available.colors[1])){
              clique.colors[[t]][c] <- available.colors[1]
              available.colors <- available.colors[-1]
            } else {
              ## if I ran out of colors, use "wheat3"
              clique.colors[[t]][c] <- "wheat3"
            }
          }
        }
      }
    }
  }
  return(list("colors" = clique.colors, "cliques" = cliques))
}