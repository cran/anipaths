#' blur ellipses function
#'
#' @param x An object. In the default method the parameter x should be a correlation between -1 and 1 or a square positive definite matrix at least 2x2 in size. It will be treated as the correlation or covariance of a multivariate normal distribution.
#' @param levels contour levels
#' @param alpha_mult multiplier on transparency level
#' @param col default is black
#' @param center two-vector giving center of ellipse
#' 
#' @import  ellipse
#' @importFrom scales alpha
#' @importFrom graphics polygon
blur_point <- function(x, levels = seq(1e-3, 1-1e-1, l=15),
                       alpha_mult, col = "black", center){
  if ((sum(is.na(x)) + sum(is.na(center))) == 0){
    for(level in levels){
      polygon(ellipse::ellipse(x, level = level, centre = center), border = NA,
              col = scales::alpha(col, min(1, 1/length(levels)*3*alpha_mult/sqrt(prod(diag(x))))))
    }
  }
}

#' Get good alpha_mult
#'
#' @param sd1 standard deviation of longitude
#' @param sd2 standard deviation of latitude
#'
#' @return scalar value to be used for alpha_mult in blur_point()
new_alpha <- function(sd1, sd2){
  x <- (sd1 + sd2)/2
}