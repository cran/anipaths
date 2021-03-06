% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gam_interpolation.R
\name{gam_interp}
\alias{gam_interp}
\title{GAM interpolation using \code{mgcv:gam()}.}
\usage{
gam_interp(formula = NULL, y, time, pred_times, se.fit = T, s_args = NULL)
}
\arguments{
\item{formula}{optionally specify formula for \code{mgcv::gam()} using \code{y} as response and \code{time} as predictor.}

\item{y}{observations}

\item{time}{times for observations}

\item{pred_times}{prediction times}

\item{se.fit}{logical default is \code{TRUE}; should standard pointwise errors be computed for interpolation}

\item{s_args}{Arguments to \code{mgcv::s()} can be passed using a named list/vector.}
}
\value{
interpolated values
}
\description{
GAM interpolation using \code{mgcv:gam()}.
}
