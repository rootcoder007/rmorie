% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.slvgrf_check}
\alias{.slvgrf_check}
\title{.slvgrf_check}
\usage{
.slvgrf_check(scores, priority)
}
\arguments{
\item{scores}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{priority}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the slvgrf_native implementation. Called by \code{qini_curve}, \code{rate_test}, \code{toc_curve}. See the file header for the source the module follows.
}
\value{
A list with \code{g}, \code{s}.
}
\keyword{internal}
