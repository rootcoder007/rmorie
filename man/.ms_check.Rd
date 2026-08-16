% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ms_check}
\alias{.ms_check}
\title{.ms_check}
\usage{
.ms_check(time, event)
}
\arguments{
\item{time}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{event}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the survival_more implementation. Called by \code{.ms_baseline}, \code{.ms_fit_lls}, \code{.ms_km} and 8 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{t}, \code{e}.
}
\keyword{internal}
\examples{
.ms_check(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0))
}
