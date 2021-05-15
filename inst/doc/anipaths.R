## ---- include = F-------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(eval = F)

## ---- fig.show='hold', fig.cap="test"-----------------------------------------
#  library(anipaths)
#  vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ] ## limit attention to 2009
#  delta.t <- "day"
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier")

## ----map_logical--------------------------------------------------------------
#  library(ggmap)
#  delta.t <- "week"
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = TRUE)

## ----map_google---------------------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009:2010, ]
#  delta.t <- 3600*24*2 ## number of seconds in two days
#  background <- list(center = c(-90, 10),
#                     zoom = 3,
#                     maptype = "satellite")
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background)

## ----map_simple---------------------------------------------------------------
#  delta.t <- "week"
#  background <- rworldmap::getMap(resolution = "coarse")
#  sp::proj4string(background) ## matches default projection in animate_paths()
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background)

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
#                background = background, s_args = c(k = 80))

## ----generate_synchronous-----------------------------------------------------
#  delta.t <- 3600*24*3 ## 3 days between synchronized observations
#  background <- rworldmap::getMap(resolution = "coarse")
#  sp::proj4string(background)
#  behaviors <- c("exploratory", "directed", "stationary")
#  set.seed(1)
#  vultures_paths$behavior <-
#    unlist(sapply(unique(vultures_paths$individual.local.identifier), function(id){
#      v_id <- vultures_paths[vultures_paths$individual.local.identifier == id, ]
#      switches <- c(0, sort(sample(1:nrow(v_id), 2)), nrow(v_id))
#      rep(behaviors[sample(1:3, 3)], diff(switches))
#    }))
#  synchro_paths <- animate_paths(paths = vultures_paths, s_args = c(k = 70),
#                                 delta.t = delta.t, covariate = "behavior",
#                                 coord = c("location.long", "location.lat"),
#                                 Time.name = "POSIX", ID.name = "individual.local.identifier",
#                                 return.paths = T)

## ----animate_sync_covariates--------------------------------------------------
#  names(synchro_paths$covariate.interp) <- "behavior"
#  animate_paths(paths = synchro_paths$paths.interp, times = synchro_paths$time.grid,
#                covariate = synchro_paths$covariate.interp,
#                covariate.colors = 1:3, background = background)

## ----uncertainty_anim---------------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009 &
#                               vultures$location.lat > 32 &
#                               vultures$individual.local.identifier != "Mark", ]
#  delta.t <- "day"
#  background <- rworldmap::getMap(resolution = "coarse")
#  sp::proj4string(background)
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background, uncertainty.level = 0.99)

## ----whole_path_anim----------------------------------------------------------
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background,
#                whole.path = TRUE, tail.length = 0)

## ----dim_anim-----------------------------------------------------------------
#  vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009 &
#                               vultures$location.lat > 32, ]
#  animate_paths(paths = vultures_paths,
#                delta.t = delta.t,
#                coord = c("location.long", "location.lat"),
#                Time.name = "POSIX",
#                ID.name = "individual.local.identifier",
#                background = background, dimmed = c(1, 3, 5))

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
#                background = background, network = network, network.times = network.times)

## ----remove files, include = F------------------------------------------------
#  system("rm -r js; rm -r css; rm -r images; rm index.html")

