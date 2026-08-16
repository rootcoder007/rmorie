% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc_weights}
\alias{morie_causdiddc_weights}
\title{The w_{g,t} of Theorem 1}
\usage{
morie_causdiddc_weights(D, group, period, weights = NULL)
}
\arguments{
\item{weights}{Optional observation weights.}
}
\description{
The weights of de Chaisemartin & D\'Haultfoeuille (2020) Theorem 1, from the residual of the treatment indicator on the two sets of fixed effects, scaled to sum to one over the treated cells.  They can be NEGATIVE -- the regression everyone runs is not an average treatment effect.
}
\value{
A list with \code{weights} (named numeric keyed by   "(group,period)" strings) and \code{residual} (the within-   transformed treatment).
}
\keyword{internal}
