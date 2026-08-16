% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_lcg31_unif}
\alias{.ghc_lcg31_unif}
\title{The uniform the Python arms take from it: st / 2^31 AFTER the step}
\usage{
.ghc_lcg31_unif(env)
}
\arguments{
\item{env}{A list; the body reads \code{$st} from it.}
}
\description{
A step of the helpers_ghc_rng implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.ghc_lcg31_unif(env = list(a = 1, b = 2))
}
