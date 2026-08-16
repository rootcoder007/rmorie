% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_xor64}
\alias{.ghc_xor64}
\title{.ghc_xor64}
\usage{
.ghc_xor64(a, b)
}
\arguments{
\item{a}{A list; the body reads \code{$hi}, \code{$lo} from it.}
\item{b}{A list; the body reads \code{$hi}, \code{$lo} from it.}
}
\description{
A step of the helpers_ghc_rng implementation. Called by \code{.ghc_int}, \code{.ghc_unif}. See the file header for the source the module follows.
}
\value{
A list with \code{hi}, \code{lo}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.ghc_xor64(D, D)
}
