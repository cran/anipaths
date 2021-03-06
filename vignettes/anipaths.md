---
title: "anipaths"
author: "Henry Scharf"
date: "2019-06-14"
output: 
  rmarkdown::html_vignette:
    fig_caption: yes
    toc: true
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Animating animal trajectories

The package `anipaths` contains a collection of telemetry observations for turkey vultures originally analyzed in:

> Dodge S, Bohrer G, Bildstein K, Davidson SC, Weinzierl R, Mechard MJ, Barber D, Kays R, Brandes D, Han J (2014) Environmental drivers of variability in the movement ecology of turkey vultures (Cathartes aura) in North and South America. Philosophical Transactions of the Royal Society B 20130195.

To animate the locations, we first need to create a time stamp variable of class `numeric` or `POSIX`. One advantage to using the `POSIX` class is that we can specify the gaps in the interpolation (`delta.t`) using convenient character strings like `"hour"` or `"week"`.


```r
library(anipaths)
vultures$POSIX <- as.POSIXct(vultures$timestamp, tz = "UTC")
vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ] ## limit attention to 2009
delta.t <- "day"
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier")
```

### Using background maps

Often it may be useful to add a map of the relevant study area. There are lots of ways to incorporate a map. The simplest way is to set `background` to `TRUE`, in which case `anipaths` will do the best it can to select a map based on the data. In the next example, we've changed the time step to help the animations load a little faster.

```r
delta.t <- "week"
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", 
              background = TRUE)
```

You can also give a long/lat `location`, `zoom` level (3-21; see `?ggmap::get_map()`), and `maptype` (`satellite`, `terrain`, `hybrid`) to be passed to `ggmap::get_map()`, and `anipaths` will make a background for you. 

As shown below, the value of `delta.t` can be specified as a numeric, which will be interpreted in whatever units used by `as.numeric(paths['Time.name'])` (in our case, this is seconds). 

```r
vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009:2010, ]
delta.t <- 3600*24*2 ## number of seconds in two days
background <- list(location = c(-90, 10), 
                   zoom = 3, 
                   maptype = "satellite")
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", 
              background = background)
```

You can also supply your own background image. The projection and units should match the data.

```r
delta.t <- "week"
background <- rworldmap::getMap(resolution = "coarse")
sp::proj4string(background) ## matches default projection in animate_paths()
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", 
              background = background)
```

The function `animate_paths()` must project the data to match Google's map tiles, so if your data aren't in long/lat format, make sure you update the `paths.proj` variable with the correct string.

### Adding individual-level covariate information

If you have a covariate of interest for each individual, `animate_paths()` can display that information as a colored ring around each individual. We don't have any natural individual-level covariates available for the vultures, so we'll make one up for the purposed of demonstration. The following code assigns each individual a random interval for each of three behaviors: *exploratory*, *directed*, and *stationary*.


```r
behaviors <- c("exploratory", "directed", "stationary")
set.seed(1)
vultures_paths$behavior <- 
  unlist(sapply(unique(vultures_paths$individual.local.identifier), function(id){
    v_id <- vultures_paths[vultures_paths$individual.local.identifier == id, ]
    switches <- c(0, sort(sample(1:nrow(v_id), 2)), nrow(v_id))
    rep(behaviors[sample(1:3, 3)], diff(switches))
  }))
```

The covariates have now been appended to the `paths` data frame. We can let `animate_paths()` know we would like it to display this information by setting the argument `covariate` to match the name of the appropriate column in `paths` (i.e., `behavior`). The default colors are a gray scale. Any collection of colors can be provided (e.g., `covariate.colors = viridis::viridis(3)`) which will be turned into a palette to match the support of the covariate. 


```r
delta.t <- "day"
background <- rworldmap::getMap(resolution = "coarse")
sp::proj4string(background)
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              covariate = "behavior", covariate.colors = viridis::viridis(3),
              ID.name = "individual.local.identifier",
              background = background, max.knots = 80)
```

*Note*: if a covariate is represented numerically but should be treated as a factor, changing the class using `as.factor()` should prevent `animate_paths()` from interpolating to "intermediate" values (e.g., if male and female are coded as 0 and 1, defining the column of the data frame as a factor will constrain individuals to only take on values of 0 and 1).

### Animating synchronous data

This situation may arise, for example, when a user wishes to employ `anipaths` to produce visualizations of realizations generated from a discrete-time movement model. To demonstrate how to use `animate_paths()` to animate already synchronous paths, bypassing the spline-based interpolation, we can use `animate_paths()` to produce synchronized paths (setting `return.paths = TRUE`), and then pretend we had those synchronized paths from the beginning. In this case, we will also pretend we have covariates associated with those synchronized times.


```r
delta.t <- 3600*24*3 ## 3 days between synchronized observations
background <- rworldmap::getMap(resolution = "coarse")
sp::proj4string(background)
behaviors <- c("exploratory", "directed", "stationary")
set.seed(1)
vultures_paths$behavior <- 
  unlist(sapply(unique(vultures_paths$individual.local.identifier), function(id){
    v_id <- vultures_paths[vultures_paths$individual.local.identifier == id, ]
    switches <- c(0, sort(sample(1:nrow(v_id), 2)), nrow(v_id))
    rep(behaviors[sample(1:3, 3)], diff(switches))
  }))
synchro_paths <- animate_paths(paths = vultures_paths, max.knots = 70,
                               delta.t = delta.t, covariate = "behavior",
                               coord = c("location.long", "location.lat"),
                               Time.name = "POSIX", ID.name = "individual.local.identifier", 
                               return.paths = T)
```

Now we input our synchronized paths (a list of matrices all with the same number of rows) into the `paths` argument, the synchronized times in the `times` argument, and the synchronized covariates in the `covariate` argument. Naming the first element of the `covariate` list passes the label through to the legend in the animation.


```r
names(synchro_paths$covariate.interp) <- "behavior"
animate_paths(paths = synchro_paths$paths.interp, times = synchro_paths$times, 
              covariate = synchro_paths$covariate.interp, 
              covariate.colors = 1:3, background = background)
```

### Output formats

The default format for displaying the animation is an `index.html` file. This is great for broad functionality, and the generated images may be used as the basis of a variety of other animation formats (e.g., the "animate" package in LaTeX).  If you have `ffmpeg` installed on your system, you can also set `method = "mp4"` to create a stand-alone video that is easy to share with others. For more information, see https://www.ffmpeg.org/. 

## Checking the interpolation

As a way to check that `anipaths` is producing reasonable interpolations of the telemetry observations, a generic `plot()` function is provided that takes an argument of class `paths_animation` produced by calling `animate_paths()` with `return.paths = TRUE`. See `example(plot.paths_animation)`. Adjusting the parameters of the interpolation can be done by modifying the `bs` and `max.knots` arguments. As a general rule, if the interpolated paths look "too smooth", try increasing `max.knots`.

## Additional features
### Showing location uncertainty (beta)

Uncertainty about the depicted location of each individual is computed as part of the interpolation procedure. Error ellipses representing a Gaussian approximation to the uncertainty can be added by specifying a quantile level for the argument `uncertainty.level`. These ellipses should really only be interpreted qualitatively, but may be useful for communicating when there is significant uncertainty about the location of an individual (perhaps due to sparse observations in time). 


```r
vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009 & 
                             vultures$location.lat > 32 & 
                             vultures$individual.local.identifier != "Mark", ]
delta.t <- "day"
background <- rworldmap::getMap(resolution = "coarse")
sp::proj4string(background) 
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", 
              background = background, uncertainty.level = 0.99)
```

### Showing complete trajectories

Set `whole.path = TRUE`. It is not required, but probably clearer visually, if we also set `tail.length = 0`.


```r
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", 
              background = background, 
              whole.path = TRUE, tail.length = 0)
```

### Dimming selected individuals


```r
vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009 & 
                             vultures$location.lat > 32, ]
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", 
              background = background, dimmed = c(1, 3, 5))
```

### Showing relational information (beta)

It is also possible to visualize dynamic pair-wise relational information with `anipaths`. This information needs to be coded as an array of adjacency matrices (binary and weighted edges allowed, no support yet for directed information). If the network information is on a different time scale than the position information, it will be interpolated using smoothing splines to match.

We don't have existing relationships among the vultures to display, so we'll make some up for the purposes of demonstration. 


```r
vultures_paths <- vultures[format(vultures$POSIX, "%Y") == 2009, ]
set.seed(1)
n_indiv <- length(unique(vultures_paths$individual.local.identifier))
change_pts <- 5
network_times <- seq(min(vultures_paths$POSIX), max(vultures_paths$POSIX), l = change_pts + 2)
network <- array(NA, dim = c(n_indiv, n_indiv, length(network_times)))
for(time_i in 2:length(network_times)){
  network_mat <- matrix(sample(1:0, n_indiv^2, prob = c(0.1, 0.9), replace = T), 
                        n_indiv, n_indiv)
  network_mat[lower.tri(network_mat)] <- t(network_mat)[lower.tri(network_mat)]
  diag(network_mat) <- 1
  network[, , (time_i - 1):time_i] <- network_mat
}
```


```r
delta.t <- 3600*24*2
animate_paths(paths = vultures_paths, 
              delta.t = delta.t,
              coord = c("location.long", "location.lat"),
              Time.name = "POSIX",
              ID.name = "individual.local.identifier", max.knots = 50,
              background = background, network = network, network.times = network_times)
```
