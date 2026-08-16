% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causdiddc_twfe_weights}
\alias{.causdiddc_twfe_weights}
\title{The w_{g,t} of Theorem 1, from the residual of D on the two-way}
\usage{
.causdiddc_twfe_weights(D, group, period, weights = NULL)
}
\arguments{
\item{D}{See Usage.}
\item{group}{See Usage.}
\item{period}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
fixed effects.  Alternating projections, iterated to convergence -- the within-transformation solver, not the matrix-form Frisch-Waugh.
}
\value{
A list with \code{weights}, \code{residual}.
}
\keyword{internal}
