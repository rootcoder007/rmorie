% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funmix_first_pc}
\alias{.funmix_first_pc}
\title{First principal direction by power iteration -- deterministic start}
\usage{
.funmix_first_pc(C, p)
}
\arguments{
\item{C}{A matrix; passed to \code{nrow}.}
\item{p}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the funmix_native implementation. Called by \code{morie_funmix_functional_mixture}. See the file header for the source the module follows.
}
\value{
A list with \code{score}, \code{pc}.
}
\keyword{internal}
