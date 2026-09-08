% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_coverage_by_region}
\alias{morie_coverage_by_region}
\title{morie_coverage_by_region}
\usage{
morie_coverage_by_region(theta, level = 0.95, lower_bound = 0, draws = 20000L, seed = 0, 
    split = NULL) 
}
\arguments{
\item{theta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{level}{Numeric; combined arithmetically in the body. Defaults to \code{0.95}.}
\item{lower_bound}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{draws}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20000L}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{split}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the bndpcb_native implementation. Called by \code{morie_bet_violation}. See the file header for the source the module follows.
}
\value{
A list with \code{marginal_coverage}, \code{subset_coverage}, \code{subset_share}, \code{mean_width}, \code{p_empty}, \code{split}, \code{theta}, \code{draws}.
}
\keyword{internal}
\examples{
morie_coverage_by_region(theta = 0.5)
}
