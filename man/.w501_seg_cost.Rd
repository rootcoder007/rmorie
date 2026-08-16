% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w501_seg_cost}
\alias{.w501_seg_cost}
\title{0-based half-open [a, b): R indices a+1 .. b}
\usage{
.w501_seg_cost(tb, a, b, cost)
}
\arguments{
\item{tb}{A list; the body reads \code{$cs}, \code{$css} from it.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
\item{cost}{One of \code{"mean"}, \code{"meanvar"}.}
}
\description{
Part of the w501_changepoint_native implementation; see the file header for the source it follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
