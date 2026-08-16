% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_add64}
\alias{.ghc_add64}
\title{.ghc_add64}
\usage{
.ghc_add64(a, b)
}
\arguments{
\item{a}{A list; the body reads \code{$hi}, \code{$lo} from it.}
\item{b}{A list; the body reads \code{$hi}, \code{$lo} from it.}
}
\description{
A step of the helpers_ghc_rng implementation. Called by \code{.ghc_int}, \code{.ghc_unif}, \code{.survrsf_rng}. See the file header for the source the module follows.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
