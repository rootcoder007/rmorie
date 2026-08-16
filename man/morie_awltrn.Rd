% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_awltrn}
\alias{morie_awltrn}
\title{Augmented outcome-weighted learning (native R arm)}
\usage{
morie_awltrn(R = NULL, A = NULL, H = NULL, propensity = NULL, method = "aol", prognostic = NULL, shift = NULL, ridge = 1e-06, stages = NULL)
}
\arguments{
\item{R}{Numeric vector of outcomes (single-stage only).}
\item{A}{Integer vector of treatments, -1 or +1 (single-stage only).}
\item{H}{Matrix of covariates (single-stage only).}
\item{propensity}{Randomisation probabilities.}
\item{method}{\code{"aol"} or \code{"owl"} (single-stage only).}
\item{prognostic}{Optional prognostic values (single-stage only).}
\item{shift}{Optional additive constant (single-stage only).}
\item{ridge}{Ridge penalty.}
\item{stages}{Optional list of \code{(R, A, H)} triples for   multi-stage backward induction.}
}
\description{
Native R arm of \code{morie.fn.awltrn}. Dispatches to \code{fit_stages} when \code{stages} is supplied and to \code{fit_aol} otherwise. No random numbers are drawn, so the shared generator is not touched and the two arms are deterministic given the same inputs.
}
\value{
A named list; see \code{fit_aol} and \code{fit_stages}.
}
\keyword{internal}
