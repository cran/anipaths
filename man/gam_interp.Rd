% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gam_interpolation.R
\name{gam_interp}
\alias{gam_interp}
\title{GAM interpolation using \code{mgcv:gam()}.}
\usage{
gam_interp(
  formula = NULL,
  y,
  time,
  pred_times,
  se.fit = T,
  s_args = NULL,
  uncertainty.type,
  verbose = F
)
}
\arguments{
\item{formula}{optionally specify formula for \code{mgcv::gam()} using \code{y} as response and \code{time} as predictor.}

\item{y}{observations}

\item{time}{times for observations}

\item{pred_times}{prediction times}

\item{se.fit}{logical default is \code{TRUE}; should standard pointwise errors be computed for interpolation}

\item{s_args}{Arguments to \code{mgcv::s()} can be passed using a named list/vector.}

\item{uncertainty.type}{State what type of uncertainty plot 1 is default for tails more than 1 is amount of predicted trajectories for each unique individual and blurs for blur plot}

\item{verbose}{logical; \code{TRUE} prints messages about fitting details}
}
\value{
interpolated values
}
\description{
GAM interpolation using \code{mgcv:gam()}.
}
