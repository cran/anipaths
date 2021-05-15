# this is a helper function from @coolbutuselessLogo
# https://coolbutuseless.github.io/2019/03/19/geom_blurry-proof-of-concept/
# here is the repository https://coolbutuseless.github.io/2020/02/11/introducing-the-ggblur-package/
# we put it here because the package is not on CRAN yet
# once it's on CRAN we will make the switch to use the package instead 

#' geom_blurry
#'
#' @param mapping 
#' @param data 
#' @param stat 
#' @param position 
#' @param ... 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#'
#' @return blurry point
#' @keywords internal
#' @noRd
#' 
geom_blurry <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBlurry,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}


GeomBlurry <- ggproto(
  "GeomBlurry", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = aes(
    shape = 19, colour = "black", size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    
    coords <- coord$transform(data, panel_params)
    
    my_alpha <- coords$alpha
    my_alpha[is.na(my_alpha)] <- 1.0
    ggplot2:::ggname(
      "geom_blurry",
      grid::grobTree(
        
        grid::pointsGrob(
          coords$x, coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col      = alpha(coords$colour, my_alpha * 0.1),
            fill     = alpha(coords$fill  , my_alpha * 0.1),
            fontsize = (coords$size + 3) * .pt + coords$stroke * .stroke / 2,
            lwd      = coords$stroke * .stroke / 2
          )
        ),
        
        grid::pointsGrob(
          coords$x, coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col      = alpha(coords$colour, my_alpha * 0.3),
            fill     = alpha(coords$fill  , my_alpha * 0.3),
            fontsize = (coords$size + 2) * .pt + coords$stroke * .stroke / 2,
            lwd      = coords$stroke * .stroke / 2
          )
        ),
        
        grid::pointsGrob(
          coords$x, coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col      = alpha(coords$colour, my_alpha * 0.5),
            fill     = alpha(coords$fill  , my_alpha * 0.5),
            fontsize = (coords$size + 1) * .pt + coords$stroke * .stroke / 2,
            lwd      = coords$stroke * .stroke / 2
          )
        ),
        
        grid::pointsGrob(
          coords$x, coords$y,
          pch = coords$shape,
          gp = grid::gpar(
            col      = alpha(coords$colour, coords$alpha),
            fill     = alpha(coords$fill  , coords$alpha),
            fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
            lwd      = coords$stroke * .stroke / 2
          )
        )
        
      )
    )
  },
  
  draw_key = draw_key_point
)

translate_shape_string <- function(shape_string) {
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }
  
  pch_table <- c(
    "square open"           =  0,
    "circle open"           =  1,
    "triangle open"         =  2,
    "plus"                  =  3,
    "cross"                 =  4,
    "diamond open"          =  5,
    "triangle down open"    =  6,
    "square cross"          =  7,
    "asterisk"              =  8,
    "diamond plus"          =  9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )
  
  shape_match <- charmatch(shape_string, names(pch_table))
  
  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0
  
  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)
    
    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])
    
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    }
    
    stop(
      "Can't find shape name:",
      collapsed_names,
      more_problems,
      call. = FALSE
    )
  }
  
  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)
    
    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )
    
    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )
    
    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    }
    
    stop(
      "Shape names must be unambiguous:",
      collapsed_names,
      more_problems,
      call. = FALSE
    )
  }
  
  unname(pch_table[shape_match])
}