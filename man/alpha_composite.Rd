% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alpha_composite}
\alias{alpha_composite}
\title{Front-to-back alpha compositing}
\usage{
alpha_composite(colours, alphas, depths = NULL)
}
\arguments{
\item{colours}{Numeric matrix of per-Gaussian RGB (or more channels).}
\item{alphas}{Numeric vector of per-Gaussian opacities in [0, 1].}
\item{depths}{Optional depth vector; absent means input order.}
}
\description{
Front-to-back alpha compositing
}
\value{
List with colour, transmittance, coverage, note.
}
\keyword{internal}
