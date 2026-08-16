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
\item{D}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{group}{Coerced to vector by the body, with \code{as.vector}.}
\item{period}{Coerced to vector by the body, with \code{as.vector}.}
\item{weights}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
fixed effects.  Alternating projections, iterated to convergence -- the within-transformation solver, not the matrix-form Frisch-Waugh.
}
\value{
A list with \code{weights}, \code{residual}.
}
\keyword{internal}
