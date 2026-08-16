% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ErrBound}
\alias{ErrBound}
\title{P_e <= sqrt(P1 P2) exp(-D_B).  NOT FROM THIS BOOK -- the standard}
\usage{
ErrBound(p1, p2, db)
}
\arguments{
\item{p1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{db}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
Kailath bound.  It pairs with GaussOverlap, NOT with Divergence: the divergence does not bound the error this way, and substituting it would give a number that looks like a bound and is not one.  The bound is on the OPTIMAL classifier, a floor no real one can beat.
}
\value{
A list with \code{bound}, \code{priors}, \code{bhattacharyya}, \code{tightest_at_equal_priors}, \code{bounds_the_optimal_classifier_not_yours}, \code{not_from_this_book}, \code{pairs_with_the_overlap_not_with_divergence}, \code{reference}, \code{method}.
}
\keyword{internal}
\examples{
ErrBound(0.5, 0.5, 1.4)
}
