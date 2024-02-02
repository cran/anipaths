#' Single and Multiple Realizations Using crawl Package
#' @keywords internal
#' 
#' @param coord A character vector of length 2 giving the names of the longitude/easting and latitude/northing columns in the \code{paths} \code{data.frame} (in that order). This is required if \code{paths} is not a \code{SpatialPointsDataFrame}.
#' @param delta.t The gap in time between each frame in the animation. Specify one of \code{delta.t} or \code{n.frames}. If both are specified, \code{delta.t} is used.
#' @param ID_names a list of names for each animal in the data
#' @param max_refit_attempts an integer of number of attempts per individual to fit crawl model
#' @param n.indiv an integer of the total number of unique animals in the data
#' @param paths A list of all paths from each animals stored in a \code{data.frame} or \code{SpatialPointsDataFrame} object.
#' @param paths.proj PROJ.4 string corresponding to the projection of the data
#' @param paths.tranform.crs a character string of coordinate projection transformation based on the animals' location.
#' @param simulation logical. Generate simulation predictions to have multiple projects for the animal paths
#' @param simulation.iter an integer of how many paths the crawl model will generate
#' @param time.grid grid for synchronized interpolations 
#' @param Time.name The name of the columns in \code{paths} gving the observation times. This column must be of class \code{POSIXt}, or numeric.
#'
#' @return interpolation values from crawl package 
#' @importFrom crawl crwMLE crwPredict crwSimulator crwPostIS
#' @importFrom dplyr mutate bind_rows select inner_join 
#' @importFrom magrittr %<>% %>%
#' @importFrom lubridate ceiling_date floor_date
#' @importFrom grDevices colorRampPalette
#' @importFrom tidyr gather
#' @importFrom sf st_as_sf st_transform st_crs st_drop_geometry st_coordinates st_bbox
#' @importFrom stringr str_remove
#' @importFrom tidyselect all_of contains
#' @importFrom stats median
crawl_interpolation <- function(coord, delta.t, ID_names, max_refit_attempts, n.indiv, paths, paths.proj, 
                                paths.tranform.crs, simulation, simulation.iter, time.grid, Time.name) {
  
  # initiate print list for predictions
  print_fits <- vector("list", length = n.indiv)
  
  # add a loop to generate data, fit, and predictions for all objects
  for (paths.i in 1:n.indiv) {
    #### change class of paths to be spatial point data frame + project ----
    if (!inherits(paths[[paths.i]], "sf")) {
      # convert to sf object to project
      paths[[paths.i]] <- st_as_sf(paths[[paths.i]], coords = coord, crs = st_crs(paths.proj))
    }
    
    ## adjust duplicate times or else crawl will haunt your dreams
    duplicates <- which(diff(st_drop_geometry(paths[[paths.i]])[, Time.name]) == 0)
    paths[[paths.i]][duplicates, Time.name] <- paths[[paths.i]][duplicates, Time.name] + 1e-6
    
    # transform spatial data so the long/lat can be in a suitable format for crawl
    paths.tranform.crs <- paste0(
      paths.tranform.crs, " +lat_1=", st_bbox(paths[[paths.i]])["ymin"],
      " +lat_2=", st_bbox(paths[[paths.i]])["ymax"]
    )
    
    paths[[paths.i]] <- st_transform(paths[[paths.i]], paths.tranform.crs)
    
    #### prepare to fit the model ----
    # 20210429HRS an attempt at better initial values. 
    time_steps <- diff(as.numeric(st_drop_geometry(paths[[paths.i]])[, Time.name])) / 3600
    ln_tau_init <- log(median(sqrt(rowSums((apply(st_coordinates(paths[[paths.i]]), 2, diff))^2)) / time_steps))
    theta <- c(
      'ln tau' = ln_tau_init,
      'ln sigma' = ln_tau_init + log(0.25),
      'ln beta' = 0
    )
    # set time frame to predict based on each object
    obs_window <- range(st_drop_geometry(paths[[paths.i]])[, Time.name])
    predTime <- time.grid[time.grid > obs_window[1] & time.grid < obs_window[2]]

    # show progress for user
    message("Beginning crawl fit for: ", ID_names[[paths.i]])
    
    ## ----
    ## allow for multiple attempts ----
    n_attempts <- 1; success <- c(crwMLE = F, crwPredict = F, crwSimulator = T, crwPostIS = T)
    while(n_attempts <= max_refit_attempts & min(success) == 0){
      tryCatch({
        
        ## crwMLE ----
        ## inner loop of attempts on fitting
        n_fit_attempts <- 1; fit <- NULL
        while(any(is.null(fit), is.na(c(fit$se, fit$par, fit$estPar)), min(diag(fit$Cmat)) < 0) & n_fit_attempts <= 10) {
          suppressMessages(
            suppressWarnings(
              fit <- crwMLE(
                mov.model = ~1,
                err.model = list(x= ~1),
                data = paths[[paths.i]],
                Time.name = Time.name,
                theta = theta,
                constr = list(lower = theta + log(c(1e-4, 1e-4, 1e-2)), 
                              upper = theta + log(c(1e4, 1e4, 1e2))),
                attempts = 10, need.hess = T
              )
            )
          )
          fit
          n_fit_attempts <- n_fit_attempts + 1
          success['crwMLE'] <- T
        }
        if(any(is.null(fit), is.na(c(fit$se, fit$Cmat, fit$par, fit$estPar))) & n_fit_attempts > 10){
          success['crwMLE'] <- F
        }
        
        ## crwPredict ----
        # best predicted path using the time defined time.grid
        suppressMessages(
          suppressWarnings(
            predObj <- crwPredict(
              object.crwFit = fit,
              predTime,
              return.type = "flat")
          )
        )
        # add column for ID to plot by group later
        predObj$ID <- ID_names[[paths.i]]
        predObj <- predObj[predObj$locType == "p", ]
        class(predObj) <- "data.frame"
        
        ## unless interrupted, assume successful
        success['crwPredict'] <- T
        
        ## crwSimulator + crwPostIS ----
        # Simulation: multiple interpolations
        if (simulation == TRUE) {
          if(min(success[c("crwMLE", "crwPredict")]) == 0){
            next
          }
          success[c("crwSimulator", "crwPostIS")] <- F
          
          # show progress for user
          message("Generating ", simulation.iter, " path realizations for: ", ID_names[[paths.i]])
          
          ## inner loop of attempts at crawl::crwSimulator for one fit
          n_sim_attempts <- 1
          while(min(success[c('crwSimulator', 'crwPostIS')]) == 0 & n_sim_attempts <= 5) {
            
            # initiate print list for simulation
            suppressMessages(
              suppressWarnings(
              simObj <- crwSimulator(fit, predTime)
              )
            )
            ## unless interrupted, mark successful
            success['crwSimulator'] <- T
            
            ## assume it will work until it doesn't
            success['crwPostIS'] <- T
            # set loops to add simulation paths
            for (i in 1:simulation.iter) {
              tryCatch({
                suppressMessages(
                  suppressWarnings(samp <- crwPostIS(simObj, fullPost = TRUE))
                  )
              }, 
              error = function(err){
                err
                success['crwPostIS'] <<- F ## mark unsuccessful if error
              })
              
              predObj %<>%
                mutate(
                  samp.x = samp$alpha.sim[samp$locType == "p", "mu.x"],
                  samp.y = samp$alpha.sim[samp$locType == "p", "mu.y"]
                )
              
              colnames(predObj)[colnames(predObj) == "samp.x"] <- paste0("samp.x", i)
              colnames(predObj)[colnames(predObj) == "samp.y"] <- paste0("samp.y", i)
            }
            n_sim_attempts <- n_sim_attempts + 1
          }
        }
        
      }, 
      ## errors + warnings ----
      error = function(err) {
        n_attempts <<- n_attempts + 1
        message(
          "Errors in crawl::", paste(names(which(!success)), collapse = "(), "), 
          ". Trying again [", n_attempts - 1, "/5]")
      })
    }
    
    # add preds to print list
    print_fits[[paths.i]] <- predObj
  }
  # combine print_fits into a single dataframe
  print_fits %<>% bind_rows() %>% as.data.frame()
  
  # gather data
  print_fits.x <- print_fits %>%
    select(all_of(Time.name), contains(".x"), -nu.x, -se.nu.x, ID) %>%
    gather(key, mu.x, -all_of(Time.name), -se.mu.x, -ID)

  print_fits.x$key %<>%
    str_remove(".x")

  print_fits.y <- print_fits %>%
    select(all_of(Time.name), contains(".y"), -nu.y, -se.nu.y, ID) %>%
    gather(key, mu.y, -all_of(Time.name), -se.mu.y, -ID)

  print_fits.y$key %<>%
    str_remove(".y")

  # combine data
  print_fits <- print_fits.x %>%
    inner_join(print_fits.y, by = c(Time.name, "key", "ID"))

  # filter NA
  print_fits <- print_fits[!is.na(print_fits$mu.x) & !is.na(print_fits$mu.y), ]

  # tranform data back to original projection string
  print_fits <- st_as_sf(print_fits, coords = c("mu.x", "mu.y"), crs = paths.tranform.crs)
  print_fits <- st_transform(print_fits, paths.proj)

  # change print_fits into data.frame object to use in ggplot mapping later
  fits_pts <- st_coordinates(print_fits)
  colnames(fits_pts) <- c("mu.x", "mu.y")
  print_fits <- cbind(st_drop_geometry(print_fits), fits_pts)
  
  # rearrange order of variables 
  print_fits %<>% 
    select(mu.x, mu.y, se.mu.x, se.mu.y, ID, key, all_of(Time.name))

  # return print predictions list or simulation list for each ID name
  return(print_fits)
}

globalVariables(c("nu.x", "se.nu.x", "se.mu.x", "nu.y", "se.nu.y", "median"), "anipaths")
