% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blur.R
\name{new_alpha}
\alias{new_alpha}
\title{Get good alpha_mult}
\usage{
new_alpha(sd1, sd2)
}
\arguments{
\item{sd1}{standard deviation of longitude}

\item{sd2}{standard deviation of latitude}
}
\value{
scalar value to be used for alpha_mult in blur_point()
}
\description{
Get good alpha_mult
}
