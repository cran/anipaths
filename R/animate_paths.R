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
#' If all paths are already synchronous, another option for passing the data is to define \code{paths} as a list of matrices, all with the same number of rows, and to specify the times separately via the next argument. This situation might arise when, for example, locations the user wishes to animated correspond to realizations/sampler from a discrete-time movement model. Covariates may be provided as named columns of the matrices in \code{paths}.
#' @param coord A character vector of length 2 giving the names of the longitude/easting and latitude/northing columns in the \code{paths} \code{data.frame} (in that order). This is required if \code{paths} is not a \code{SpatialPointsDataFrame}.
#' @param Time.name The name of the columns in \code{paths} gving the observation times. This column must be of class \code{POSIXt}, or numeric.
#' @param background Three possibilities: (1) A single background image over which animation will be overlayed, or a SpatRaster objects with one layers corresponding to each frame. (2) A list with values \code{center} (long/lat), \code{zoom}, and \code{maptype} (see \code{ggmap::get_googlemap()}) which will be used to generate a background for the animation based on Google maps tiles. Additional arguments may be added which will be passed to \code{ggmap::get_googlemap()}. (3) A logical value of \code{TRUE}, which will cue the function to get the best Google Map tile combination it can come up with. Note: \code{ggmap} must be installed for (2) and (3). Note: if you are calling \code{animate_paths()} several times in a short period of time you may get an error from Google for trying to pull tiles too often (e.g., \code{Error in download.file(url, destfile = tmp, quiet = !messaging, mode = "wb") : cannot open URL 'http://maps.googleapis...'}). Waiting a minute or so usually solves this.
#' @param bg.axes logical: should animation place axis labels when using a background image (default is \code{TRUE}). If \code{RGoogleMaps} is used to produce background, labels will be "northing" and "easting". Otherwise, the strings given to \code{coord} will be used.
#' @param bg.misc Character string which will be executed as \code{R} code after generating the background, and before adding trajectories, etc.
#' @param bg.opts Options passed to \code{plot()} function call that makes background in each frame. For example, this could be used to specify blue ocean and gray land cover if \code{background} is a \code{MULTIPOLYGON} simple features object and \code{bg.opts = list(bg = "dodgerblue4", col = "gray", border = "gray")}.
#' @param blur.size a integer of the size for blur points; default is 8
#' @param covariate The name of the column in \code{paths} that identifies the covariate to be mapped to a ring of color around each point.
#' @param covariate.colors vector of colors which will be used in their given order to make a color ramp (see \code{colorRamp()})
#' @param covariate.legend.loc either the location of the covariate legend, or \code{NA} if no legend is desired
#' @param covariate.thresh if changed from its default value of \code{NULL}, the interpolated value of the covariate will be binarized based on this numeric value.
#' @param crawl.mu.color color for the main predictions for crawl interpolation; default is black
#' @param crawl.plot.type a character string of what type of the plot you wish to generate when \code{interpolation_type = "crawl"}. Default is "point.tail" for points with tails; input "point" for point plot and input "blur" for blur point plot; ; input "blur.point" for blur point with tails.
#' @param date.col default is \code{"black"}
#' @param delta.t The gap in time between each frame in the animation. Specify one of \code{delta.t} or \code{n.frames}. If both are specified, \code{delta.t} is used.
#' @param dev.opts Options passed to \code{png()} before creating each frame.
#' @param dimmed Numeric vector of individuals to "dim" in the animation. Order corresponds to the order of the ID.name variable, or order of paths list.
#' @param ID.name The name of the column in \code{paths} that identifies each individual. If left as \code{NULL} (default), a single individual is assumed.
#' @param interpolation_type a character string of the type of interpolation. Default is "gam" for a generalized addictive model. Use "crawl" to interpolate using \code{crawl} package. Note: due to the ongoing shift in PROJ4/6 standards, warning about CRS comments may appear.
#' @param interval Seconds per frame in animation. Default is 1/12 (or 12 frames per second).
#' @param legend.loc passed to first argument of \code{legend()} function. Default is \code{"topright"}. \code{NA} removes legend.
#' @param main Title for each frame. 
#' @param max_refit_attempts an integer of number of resampling when the fit for crawl failed to run; default is 10
#' @param method either \code{"html"} (default) or \code{"mp4"}. The latter requires the user has installed \code{ffmpeg} (see \code{?animation::saveVideo()}).
#' @param n.frames The number of frames used to animate the complete time domain of the data.
#' @param network Array of dimensions (# individuals, # individuals, \code{n.frames}) that gives a dyanmic network structure among the individuals.
#' @param network.colors A symmetric matrix of dimension \code{length(paths)} \eqn{\times} \code{length(paths)} giving the colors associated with each pairwise relationship.
#' @param network.ring.trans transparency of network segments (default is 1)
#' @param network.ring.wt thickness of network rings (default is 3)
#' @param network.segment.trans transparency of network segments (default is 0.5)
#' @param network.segment.wt thickness of network segments (default is 3)
#' @param network.thresh Network structure is summarized in the animation in a binary way, regardless of whether or not the \code{network} is continuously weighted or not. The value of \code{network.thresh} determines the level below which no connection is shown, and above which an active connection is shown via colored rings and connecting segments.
#' @param network.times Numeric vector. If network time grid doesn't match \code{n.frames}, supply the times at which the network has been evaluated so it can be interpolated using smoothing splines.
#' @param override Logical variable toggling where or not to override warnings about how long the animation procedure will take.
#' @param par.opts Options passed to \code{par()} before creating each frame.
#' @param paths.proj PROJ.4 string corresponding to the projection of the data. Default is "+proj=longlat".
#' @param paths.tranform.crs a PROJ.4 string of coordinate projection transformation based on the animals' location; default is "+proj=aea +lat_1=30 +lat_2=70".
#' @param plot.date Logical variable toggling date text at the time center of the animation.
#' @param pt.alpha alpha value for the points
#' @param pt.cex A numeric value giving the character expansion (size) of the points for each individual. Default is 1.
#' @param pt.colors A vector of colors to be used for each individual in the animation. Default values come from Color Brewer palettes. When a network is provided, this is ignored and individuals are all colored black. If \code{NA}, no plot colors are chosen to distinguish individuals. This can be useful when making animations involving a covariate. Consider also setting \code{legend.loc} to \code{NA} in this case.
#' @param pt.wd size of the points; default is 1
#' @param res Resolution of images in animation. Increase this for higher quality (and larger) images.
#' @param return.paths logical. Default is \code{FALSE}, but if \code{TRUE} then the interpolated paths are returned and no animation is produced.
#' @param s_args Default is \code{NULL}, in which case \code{anipaths} attempts to select a reasonable number of knots for the GAM interpolation. Alternatively, the user can provide a list of arguments to \code{mgcv::s()} the same length and order as number of unique individuals (i.e., \code{unique(paths[, ID.name])}). Each entry in the list should be a named list/vector (e.g., \code{s_args = list(list(k = 10), list(k = 12), ...)}).
#' @param simulation logical. Generate simulation predictions to have multiple projects for the animal paths; default is \code{FALSE}.
#' @param simulation.iter an integer of how many paths the crawl model will generate; default is 5.
#' @param tail.alpha alpha value for the tails
#' @param tail.colors default is \code{"gray87"}. Can be single color or vector of colors.
#' @param tail.length Length of the tail trailing each individual.
#' @param tail.wd Thickness of tail trailing behind each individual. Default is 1.
#' @param theme_map plot theme for \code{ggplot}, default is \code{NULL}
#' @param times If all paths are already synchronous, another option for passing the data is to define \code{paths} as a list of matrices, all with the same number of rows, and to specify the times separately via this argument.
#' @param uncertainty.level value in (0, 1) corresponding to \code{level} at which to draw uncertainty ellipses. \code{NA} (default) results in no ellipses.
#' @param uncertainty.type State what type of uncertainty plot 1 is default for tails more than 1 is amount of predicted trajectories for each unique individual and blurs for blur plot
#' @param whole.path logical. If \code{TRUE} (default = \code{FALSE}), the complete interpolated trajectories will be plotted in the background of the animation. If \code{whole.path = TRUE}, consider also setting \code{tail.length = 0}.
#' @param verbose logical; \code{TRUE} prints messages about fitting details
#' @param xlim Boundaries for plotting. If left undefined, the range of the data will be used.
#' @param ylim Boundaries for plotting. If left undefined, the range of the data will be used.
#' @param ... other arguments to be passed to \code{ani.options} to animation options such as the time interval between image frames.
#' 
#' @return video file, possibly a directory containing the individual images, or interpolated paths.
#' @export
#' @importFrom mgcv gam s
#' @importFrom stats as.formula predict approx reshape
#' @importFrom animation ani.options saveHTML saveVideo
#' @importFrom graphics par plot mtext axis segments points legend
#' @importFrom sf st_as_sf st_transform st_crs st_drop_geometry st_coordinates
#' @importFrom grDevices png dev.off colorRamp adjustcolor
#' @importFrom ggmap get_googlemap calc_zoom
#' @importFrom mvtnorm rmvnorm
#'
#' @examples ##
#' vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#' vultures_paths <- vultures[vultures$POSIX > as.POSIXct("2009-03-01", origin = "1970-01-01") &
#'   vultures$POSIX < as.POSIXct("2009-05-01", origin = "1970-01-01"), ]
#' animate_paths(
#'   paths = vultures_paths,
#'   delta.t = "week",
#'   coord = c("location.long", "location.lat"),
#'   Time.name = "POSIX",
#'   ID.name = "individual.local.identifier"
#' )
#' \dontrun{
#' background <- list(
#'   center = c(-90, 10),
#'   zoom = 3,
#'   maptype = "satellite"
#' )
#' library(ggmap)
#' library(RColorBrewer)
#' COVARIATE <- cos(as.numeric(vultures_paths$timestamp) /
#'   diff(range(as.numeric(vultures_paths$timestamp))) * 4 * pi)
#' animate_paths(
#'   paths = cbind(vultures_paths, COVARIATE),
#'   delta.t = "week",
#'   coord = c("location.long", "location.lat"),
#'   Time.name = "POSIX", covariate = "COVARIATE",
#'   covariate.colors = brewer.pal(n = 9, "RdYlGn"),
#'   ID.name = "individual.local.identifier",
#'   background = background
#' )
#' 
#'# animation using crawl interpolation
#' animate_paths(
#'   paths = vultures_paths,
#'   delta.t = "week",
#'   coord = c("location.long", "location.lat"),
#'   Time.name = "POSIX",
#'   ID.name = "individual.local.identifier",
#'   interpolation_type = "crawl"
#' )
#'}
#'
#' # Run to remove files generated by this function
#' system("rm -r js; rm -r css; rm -r images; rm index.html")
#' 
animate_paths <- function(paths, coord = c("x", "y"), Time.name = "time", 
                          background = NULL, bg.axes = TRUE, bg.misc = NULL, 
                          bg.opts = NULL, blur.size = 8, 
                          covariate = NULL, covariate.colors = c("black", "white"),
                          covariate.legend.loc = "bottomright", covariate.thresh = NULL, 
                          crawl.mu.color = "black", crawl.plot.type = "point.tail", 
                          date.col = "black", delta.t = NULL, dev.opts = list(), 
                          dimmed = NULL, ID.name = NULL, interpolation_type = "gam", 
                          interval = 1/12, legend.loc = "topright", main = NULL, 
                          max_refit_attempts = 10, method = "html", 
                          n.frames = NULL, network = NULL, network.colors = NULL, 
                          network.thresh = 0.5, network.times = NULL, 
                          network.ring.trans = 1, network.ring.wt = 3, network.segment.trans = 0.5, 
                          network.segment.wt = 3, override = FALSE, par.opts = list(), 
                          paths.proj = "+proj=longlat", paths.tranform.crs = "+proj=aea", 
                          plot.date = TRUE, pt.alpha = 0.4, pt.cex = 1, pt.colors = NULL, 
                          pt.wd = 1, res = 1.5, return.paths = FALSE, s_args = NULL, 
                          simulation = FALSE, simulation.iter = 12, tail.alpha = 0.6, 
                          tail.colors = "gray87", tail.length = 5, tail.wd = 1, 
                          theme_map = NULL, times = NULL, uncertainty.level = NA, uncertainty.type = 1, 
                          whole.path = FALSE, xlim = NULL, ylim = NULL, verbose = FALSE, ...) {
  
  ## permission to overwrite. If not granted, suggestions given to save animations
  check <- check_overwrite(method=method, return.paths=return.paths, ...)
  if(isFALSE(check)){
    return(
      message(paste(
        "Interupting anipaths to prevent overwriting existing files.",
        "Some options for viewing your animation:",
        "Option 1: Create and set a new directory to save animation files. See setwd() and dir.create()",
        "Option 2: Set argument img.name to add your animation to existing index.html link",
        "Option 3: Set argument method = 'mp4' and video.name to new value for stand-alone animation.", 
        sep = "\n"))
    )
  }
  ## Check for incompatible arguments ----
  if(uncertainty.type == "blur" & !is.null(background) & !isFALSE(background)){
    stop(paste("Blur with backgrounds is not yet available. \n",
               "If you would like to use a background, we suggest",
               "setting uncertainty.type to a positive integer."))
  }
  ## SpatialPointsDataFrame ----
  if (inherits(paths, "SpatialPointsDataFrame")) {
    stop("\n SpatialPointsDataFrame object detected. Support for sp package-type objects no longer available in anipaths.")
  }
  if(inherits(paths, 'sf')){
    message("\n sf object detected.")
    coord <- c('X', 'Y')
    paths.proj <- st_crs(paths)
    paths <- cbind(st_drop_geometry(paths), st_coordinates(paths))
    ID_names <- unique(paths[, ID.name])
  }
  ## warn about covariate/network + uncertainty ----
  if(any(!is.null(covariate), !is.null(network)) & 
     any(!is.null(simulation), crawl.plot.type == "blur.tail")){
    message("Covariate/network interpolation and visualization not yet compatible with uncertainty visualization.")
  }
  ## take data from "raw" df form to lists organized by individual ----
  ## get covariate.interp from covariate
  covariate.name <- covariate
  if (is.data.frame(paths)) {
    paths.df <- paths
    
    ## get individual's names
    ID_names <- unique(paths[, ID.name])
    if (is.null(ID.name)) {
      paths.df <- cbind(paths.df, "ID" = rep(1, nrow(paths.df)))
      ID.name <- "ID"
      legend.loc <- NA
      ID_names <- NA
    }
    paths <- vector("list", length(unique(paths.df[, ID.name])))
    paths <-
      sapply(ID_names, function(id) {
        paths.df[paths.df[, ID.name] == id, c(coord, Time.name, covariate)]
      }, simplify = F)
    time.range <- range(paths.df[, Time.name])
    covariate.interp <- NULL
  } else if(is.list(paths)) {
    paths.interp <- paths
    ID_names <- 1:length(paths)
    if (!is.null(names(paths))) {
      ID_names <- names(paths)
    }
    if (!is.null(times)) {
      time.range <- range(times)
    } else {
      if (!(Time.name %in% colnames(paths[[1]]))) {
        stop("Argument 'times' and/or 'Time.name' not/misspecified.")
      }
      time.range <-
        range(unlist(lapply(paths, function(path) {
          range(path[, Time.name])
        })))
    }
    if (is.list(covariate) & length(covariate) == length(ID_names)) {
      covariate.interp <- covariate
      covariate.name <- names(covariate)[1]
      if (is.numeric(covariate.interp[[1]])) {
        covariate.factors <- NA
      } else if (is.character(covariate.interp[[1]])) {
        
      } else {
        covariate.factors <- levels(as.factor(covariate.interp[[1]]))
      }
      covariate.ticks <- pretty(unlist(lapply(covariate.interp, function(x) range(as.numeric(x), na.rm = T))))
      if(!is.null(covariate.ticks)){
        covariate.range <- range(covariate.ticks)
      }
    } else {
      covariate.interp <- NULL
    }
  }
  
  n.indiv <- length(paths)
  
  ## timing ----
  ## if 'times' supplied, but paths is a list of matrices/data.frames, then need to append the times
  if (!is.null(times) &
      all(lapply(paths, nrow) == nrow(paths[[1]]))) {
    paths <- lapply(paths, function(path.i) {
      df <- as.data.frame(cbind(path.i, times))
      names(df) <- c(coord, "time")
      Time.name <- "time"
      return(df)
    })
    time.grid <- times
  }
  if (!is.null(delta.t)) {
    time.grid <- seq(time.range[1], time.range[2], by = delta.t)
  }
  if (is.null(delta.t)) {
    if (is.null(n.frames)) {
      if (!is.null(times)) {
        if (!exists("time.grid")) {
          delta.t <- mean(diff(times))
          time.grid <- times
        }
      } else {
        stop("One of 'delta.t,' 'n.frames,' or 'times' must be supplied.")
      }
    } else {
      time.grid <- seq(time.range[1], time.range[2], l = n.frames)
      delta.t <- diff(time.range) / n.frames
    }
  }
  n.frames <- length(time.grid)
  
  ## warn if this animation might take a long time
  if (!override) {
    if (length(paths) > 20 || n.frames > 1000) {
      readline(
        prompt = paste(
          length(paths),
          "individuals detected using",
          n.frames,
          "frames. This could take a while. Press [enter] to continue or [esc] to stop now."
        )
      )
      message("Okay, here we go!")
    }
  }
  ## covariate interpolation ----
  if(is.null(covariate.interp)){
    covariate.interp.and.factors <- covariate_interp(paths = paths, covariate = covariate, Time.name = Time.name, 
                                                     time.grid = time.grid, s_args = s_args)
    covariate.interp <- covariate.interp.and.factors$covariate.interp
    covariate.factors <- covariate.interp.and.factors$covariate.factors
    covariate.ticks <- pretty(unlist(lapply(covariate.interp, function(x) range(as.numeric(x), na.rm = T))))
    if(!is.null(covariate.ticks)){
      covariate.range <- range(covariate.ticks)
    }
  }
  ## network interpolation ----
  network.interp <- network_interp(network = network, network.times = network.times, time.grid = time.grid)
  ## paths interpolation ----
  if(!exists("paths.interp")){
    if (interpolation_type == "gam") {
      paths.interp <- paths_gam_interp(paths = paths, coord = coord, Time.name = Time.name,
                                       time.grid = time.grid, s_args = s_args, 
                                       uncertainty.type = uncertainty.type, verbose = verbose)
      paths.interp.out <- paths.interp
    } else if (interpolation_type == "crawl") {
      crawl_paths <- crawl_interpolation(
        coord = coord, delta.t = delta.t, ID_names = ID_names, 
        max_refit_attempts = max_refit_attempts, n.indiv = n.indiv, paths = paths, 
        paths.proj = paths.proj, paths.tranform.crs = paths.tranform.crs,
        simulation = simulation, simulation.iter = simulation.iter, 
        time.grid = time.grid, Time.name = Time.name)
      paths.interp <- crawl_paths
      if(inherits(paths.interp, "data.frame")) paths.interp <- list(paths.interp)
      ## return.paths = TRUE formatting
      paths.interp.out <- vector("list", length(ID_names))
      for(id in ID_names){
        i <- which(id == ID_names)
        # names(paths.interp.out[[i]]) <- c('POSIX', 'mu.x', 'mu.y', 'se.mu.x', 'se.mu.y')
        crawl.i <- reshape(crawl_paths[crawl_paths$ID == id, ], v.names = c("mu.x", "mu.y"), 
                           idvar = "POSIX", timevar = "key", drop = c("ID", "se.mu.x", "se.mu.y"), 
                           direction = "wide")
        paths.interp.out[[i]] <- data.frame(matrix(NA, length(time.grid), 1 + 2 * length(unique(crawl_paths$key))))
        names(paths.interp.out[[i]]) <- names(crawl.i)
        names(paths.interp.out[[i]])[1:3] <- c('POSIX', 'mu.x', 'mu.y')
        paths.interp.out[[i]][time.grid %in% crawl.i$POSIX, ] <- crawl.i
      }
    }
  } else {
    paths.interp.out <- paths.interp
  }
  ## paths_animation_object ----
  paths_animation <- list(paths = paths, paths.interp = paths.interp.out, 
                          covariate.interp = covariate.interp, 
                          network.interp = network.interp,
                          Time.name = Time.name, covariate = covariate, 
                          time.grid = time.grid, coord = coord, 
                          interpolation_type = interpolation_type)
  class(paths_animation) <- "paths_animation"
  ## if return.paths, then stop
  if (return.paths) {
    return(paths_animation)
  }
  ## make list of background images for each frame ----
  if ((isTRUE(background) || sum(names(background) %in% c("center", "zoom", "maptype")) == 3) &
      is.null(getOption("ggmap"))) {
    stop(paste(
      "Google maps now requires an API key. Once you have registered",
      "an account with Google here (https://cloud.google.com/maps-platform/),",
      "you can provide the API key via the ggmap function",
      "register_google(key = 'YOUR_API_KEY')."
    ))
  }
  if (isTRUE(background)) {
    n_interpolations <- as.numeric(uncertainty.type == "blur")
    if(is.numeric(uncertainty.type)){
      n_interpolations <- uncertainty.type
    }
    bounding_boxes <- matrix(unlist(lapply(paths.interp, function(x) {
      apply(array(x[, , c("mu.x", "mu.y")], dim = c(dim(x)[1], n_interpolations, 2)), 3, range, na.rm = T)
    })), nrow = 4) ## left, right, bottom, top
    bounding_box <- matrix(c(
      min(bounding_boxes[1, ]), max(bounding_boxes[2, ]),
      min(bounding_boxes[3, ]), max(bounding_boxes[4, ])
    ), 2, 2)
    if(length(grep("+proj=longlat", paths.proj)) == 0) {
      bounding_box_sf <- st_as_sf(as.data.frame(bounding_box), coords = c("V1", "V2"), crs = paths.proj)
      bounding_box_ll <- st_transform(bounding_box_sf, crs = "+proj=longlat")
      bounding_box <- st_coordinates(bounding_box_ll)
    }
    center <- colMeans(bounding_box)
    zoom <- calc_zoom(bounding_box[, 1], bounding_box[, 2]) - 1
    background <- list("center" = center, "zoom" = zoom, "maptype" = "hybrid")
  }
  if (sum(names(background) %in% c("center", "zoom", "maptype")) == 3) {
    background <- do.call(what = get_googlemap, args = background)
  }
  if (length(background) == 1 || !inherits(background, "list")) {
    if (!inherits(background, "list")) {
      background <- list(background)
    }
    bg <- sapply(1:n.frames, function(x) {
      return(background[[1]])
    }, simplify = F)
  }
  if (length(background) == n.frames) {
    bg <- background
  }
  if (inherits(background, "RasterStack")) {
    stop("RasterStack background detected. raster package-type objects no longer supported in anipaths.")
  }
  if (inherits(background, "SpatRaster")){
    message("SpatRaster background detected.")
    if (length(background) == n.frames) {
      bg <- background
    }
  }
  ## get fixed plot limits ----
  if (is.null(xlim)) {
    xlim <- range(lapply(paths, function(path.i) {
      range(path.i[, coord[[1]]], na.rm = T)
    }))
  }
  if (is.null(ylim)) {
    ylim <- range(lapply(paths, function(path.i) {
      range(path.i[, coord[[2]]], na.rm = T)
    }))
  }
  ## pt + covariate colors ----
  if (is.null(pt.colors)) {
    if (!is.null(network)) {
      pt.colors <- "black"
    } else {
      pt.colors <- c(
        brewer.pal(9, "GnBu")[3:9],
        brewer.pal(9, "OrRd")[3:9],
        brewer.pal(9, "BuPu")[3:9],
        brewer.pal(9, "YlGn")[3:9],
        brewer.pal(9, "PuBu")[3:9],
        brewer.pal(9, "YlOrBr")[3:9],
        brewer.pal(9, "YlGnBu")[3:9],
        brewer.pal(9, "PuRd")[3:9],
        brewer.pal(9, "PuBuGn")[3:9],
        brewer.pal(9, "Purples")[3:9],
        brewer.pal(9, "Oranges")[3:9]
      )
    }
  }
  pt.colors <- cbind(
    pt.colors[(1:length(paths) - 1) %% length(pt.colors) + 1],
    adjustcolor("lightgray", 0.5)
  )
  if (!is.null(covariate)) {
    color_covariate_function <- colorRamp(covariate.colors)
  }
  ## network colors ----
  if (!is.null(network)) {
    cliques <- get_network_colors(binary.network = network.interp > network.thresh, 
                                  network.color.options = network.colors)
  }
  ## adjust center + scale for google map ----
  if(interpolation_type == "gam"){
    if(inherits(bg[[1]], "ggmap")){
      for(i in 1:length(paths.interp)){
        for(rep in 1:dim(paths.interp[[i]])[2]){
          paths.i.rep.df <- as.data.frame(paths.interp[[i]][, rep, c('mu.x', 'mu.y')])
          paths.i.rep.df.na.ind <- unique(which(is.na(paths.i.rep.df), arr.ind = T)[, 1])
          if(length(paths.i.rep.df.na.ind) > 0){
            paths.interp[[i]][-paths.i.rep.df.na.ind, rep, c('mu.x', 'mu.y')] <- 
              googlemap_proj(st_as_sf(paths.i.rep.df[-paths.i.rep.df.na.ind, ], 
                                      coords = c('mu.x', 'mu.y'), crs = paths.proj), bg[[1]])
          } else {
            paths.interp[[i]][, rep, c('mu.x', 'mu.y')] <- 
              googlemap_proj(st_as_sf(paths.i.rep.df, coords = c('mu.x', 'mu.y'), crs = paths.proj), 
                             bg[[1]])
            
          }
        }
      }
      scale <- get_googlemap_min_scale(bg[[1]])$scale
      if (bg.axes) {
        message(paste0(
          "Note: Due to complications implemeting Google Maps tiles, ",
          "axes labels are not appropriately centered, but they are to scale."
        ))
      }
    }
  }
  ## animate ----
  # set start time
  message("Interpolation complete. Buildling frames.")
  start.time <- cur.time <- Sys.time()
  
  # generate animation
  animation_expression(
    bg = bg, bg.axes = bg.axes, bg.misc = bg.misc, bg.opts = bg.opts, blur.size = blur.size, 
    cliques = cliques, color_covariate_function = color_covariate_function, coord = coord, 
    covariate = covariate, covariate.factors = covariate.factors, covariate.interp = covariate.interp, 
    covariate.legend.loc = covariate.legend.loc, covariate.name = covariate.name, 
    covariate.range = covariate.range, covariate.thresh = covariate.thresh, 
    covariate.ticks = covariate.ticks, crawl.mu.color = crawl.mu.color, 
    crawl.plot.type = crawl.plot.type, cur.time = cur.time, date.col = date.col, delta.t = delta.t, 
    dev.opts = dev.opts, dimmed = dimmed, ID_names = ID_names, interpolation_type = interpolation_type, 
    interval = interval, legend.loc = legend.loc, main = main, method = method, n.frames = n.frames, 
    network = network, network.interp = network.interp, network.ring.trans = network.ring.trans, 
    network.ring.wt = network.ring.wt, network.segment.trans = network.segment.trans, 
    network.thresh = network.thresh, network.segment.wt = network.segment.wt, par.opts = par.opts, 
    paths = paths, paths.interp = paths.interp, plot.date = plot.date, pt.alpha = pt.alpha, 
    pt.cex = pt.cex, pt.colors = pt.colors, pt.wd = pt.wd, res = res, scale = scale, 
    simulation = simulation, simulation.iter = simulation.iter, tail.alpha = tail.alpha, 
    tail.colors = tail.colors, tail.length = tail.length, tail.wd = tail.wd, theme_map = theme_map, 
    time.grid = time.grid, Time.name = Time.name, uncertainty.level = uncertainty.level, 
    uncertainty.type = uncertainty.type, whole.path = whole.path, xlim = xlim, ylim = ylim, ...
  )
  
  message(paste("Total time", round(as.numeric(Sys.time()) - as.numeric(start.time), 2), "seconds."))
  if (method == "html") {
    message(
      "\n To view animation, open generated .html file (default is index.html) in any browser.",
      "\n Thanks for using anipaths!"
    )
  }
  return(NULL)
}

globalVariables(c("n.knots", "t.i", "spline.fit.x", "spline.fit.y", "time.grid.i"), "anipaths")
