% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sxrhrt_gridmax}
\alias{.sxrhrt_gridmax}
\title{Refinement stops while adjacent grid values still differ by far more}
\usage{
.sxrhrt_gridmax(f, lo, hi, points = 201L, stages = 4L)
}
\arguments{
\item{f}{See Usage.}
\item{lo}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{hi}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{points}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{201L}.}
\item{stages}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4L}.}
}
\description{
than floating-point noise. Going finer would push the comparison back below the noise floor and reintroduce exactly the disagreement this exists to remove.
}
\value{
The value of \code{a}, as built in the body.
}
\keyword{internal}
