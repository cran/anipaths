## ---- include = F-------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(eval = F)

## ---- fig.show='hold', fig.cap="test"-----------------------------------------
#  library(anipaths)
#  vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ] ## limit attention to 2009
#  animate_paths(paths = vultures_paths,
#                delta.t = "day",
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier")

## ----map_logical--------------------------------------------------------------
#  library(ggmap)
#  animate_paths(paths = vultures_paths,
#                delta.t = 2 * 24 * 60 * 60, ## number of seconds in two days
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = TRUE, img.name = "background_TRUE")

## ----map_google---------------------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009:2010, ]
#  background <- list(center = c(-90, 10),
#                     zoom = 3,
#                     maptype = "satellite")
#  animate_paths(paths = vultures_paths,
#                delta.t = 3 * 24 * 60 * 60, ## number of seconds in three days
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background, img.name = "background_google")

## ----map_simple---------------------------------------------------------------
#  background <- rworldmap::getMap(resolution = "coarse")
#  sp::proj4string(background) ## matches default projection in animate_paths()
#  # background <- data.frame(x = range(vultures_paths$location.long)[c(1, 2, 2, 1, 1)],
#  #                          y = range(vultures_paths$location.lat)[c(1, 1, 2, 2, 1)])
#  animate_paths(paths = vultures_paths,
#                delta.t = "week",
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background, img.name = "background_user")

## ----make_covariates----------------------------------------------------------
#  behaviors <- c("exploratory", "directed", "stationary")
#  set.seed(1)
#  vultures_paths$behavior <-
#    unlist(sapply(unique(vultures_paths$individual.local.identifier), function(id){
#      v_id <- vultures_paths[vultures_paths$individual.local.identifier == id, ]
#      switches <- c(0, sort(sample(1:nrow(v_id), 2)), nrow(v_id))
#      rep(behaviors[sample(1:3, 3)], diff(switches))
#    }))

## ----plot_covariates----------------------------------------------------------
#  delta.t <- "day"
#  background <- rworldmap::getMap(resolution = "coarse")
#  sp::proj4string(background)
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                covariate = "behavior", covariate.colors = viridis::viridis(3),
#                ID.name = "individual.local.identifier",
#                background = background, img.name = "covariates")

## -----------------------------------------------------------------------------
#  interp <- animate_paths(paths = vultures_paths,
#                          delta.t = "day",
#                          coord = c("location.long", "location.lat"),
#                          Time.name = "POSIX",
#                          ID.name = "individual.local.identifier",
#                          s_args = rep(list(list(k = 10)), 10),
#                          return.paths = T)
#  plot(interp, i = 2)

## -----------------------------------------------------------------------------
#  obs_counts <- merge(data.frame(individual.local.identifier = unique(vultures_paths$individual.local.identifier)),
#                      aggregate(timestamp ~ individual.local.identifier, data = vultures_paths, FUN = length),
#                      by = "individual.local.identifier", sort = F)
#  obs_counts
#  s_args <- lapply(obs_counts$timestamp, function(x) c(k = floor(min(x / 4, 306))))
#  interp <- animate_paths(paths = vultures_paths,
#                          delta.t = "day",
#                          coord = c("location.long", "location.lat"),
#                          Time.name = "POSIX",
#                          ID.name = "individual.local.identifier",
#                          s_args = s_args,
#                          return.paths = T)
#  plot(interp, i = 2)

## -----------------------------------------------------------------------------
#  interp <- animate_paths(paths = vultures_paths,
#                          delta.t = "day",
#                          coord = c("location.long", "location.lat"),
#                          Time.name = "POSIX",
#                          ID.name = "individual.local.identifier",
#                          return.paths = T, verbose = T)
#  plot(interp, i = 2)

## ----whole_path_anim----------------------------------------------------------
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background,
#                whole.path = TRUE, tail.length = 0, img.name = "whole_traj")

## ----dim_anim-----------------------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009 &
#                               vultures$location.lat > 32, ]
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background, dimmed = c(1, 3, 5), img.name = "dim")

## ----make_network-------------------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ]
#  set.seed(1)
#  n_indiv <- length(unique(vultures_paths$individual.local.identifier))
#  change_pts <- 5
#  network.times <- seq(min(vultures_paths$POSIX), max(vultures_paths$POSIX), l = change_pts + 2)
#  network <- array(NA, dim = c(n_indiv, n_indiv, length(network.times)))
#  for(time_i in 2:length(network.times)){
#    network_mat <- matrix(sample(1:0, n_indiv^2, prob = c(0.1, 0.9), replace = T),
#                          n_indiv, n_indiv)
#    network_mat[lower.tri(network_mat)] <- t(network_mat)[lower.tri(network_mat)]
#    diag(network_mat) <- 1
#    network[, , (time_i - 1):time_i] <- network_mat
#  }

## ----network_anim-------------------------------------------------------------
#  delta.t <- 3600*24*2
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background, network = network, network.times = network.times,
#                img.name = "network")

## ----remove files, eval = F---------------------------------------------------
#  system("rm -r js; rm -r css; rm -r images; rm index.html")

