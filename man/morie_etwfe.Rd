% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_etwfe}
\alias{morie_etwfe}
\title{The saturated cohort-by-period regression of Sec. 5}
\usage{
morie_etwfe(Y, unit, period, first_treated, X = NULL)
}
\arguments{
\item{first_treated}{N-vector of adoption periods (the period in   which each unit is first treated; \code{NULL} or \code{NA} for   never-treated).}
\item{X}{Optional N-by-p matrix of covariates.}
}
\description{
Every \eqn{(g, t)}{(g, t)} cell with \eqn{t \ge g}{t >= g} gets its own coefficient, so the fitted coefficients **are** the \eqn{ATT(g, t)}{ATT(g, t)}.  Nothing is averaged, so nothing can be averaged with a negative weight.
}
\value{
A named list mirroring the RichResult payload: \code{estimate},   \code{att} (named numeric vector keyed by \code{"<g>|<t>"}),   \code{cells} (list of \code{c(g, t)} pairs), \code{n_cells},   \code{coef}, \code{cohorts}, \code{periods}, \code{n},   \code{method}, \code{note}.
}
\keyword{internal}
\examples{
morie_etwfe(Y = c(1, 2, 3, 4, 5, 6, 7, 8), unit = c(1, 2, 3, 4, 5, 6, 7, 8), period = c(1, 2, 3, 4, 5, 6, 7, 8), first_treated = c(1, 2, 3, 4, 5, 6, 7, 8))
}
