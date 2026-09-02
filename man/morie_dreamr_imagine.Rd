% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dreamr_imagine}
\alias{morie_dreamr_imagine}
\title{morie_dreamr_imagine}
\usage{
morie_dreamr_imagine(state, action_model, transition, reward_model, horizon, value_model = NULL) 
}
\arguments{
\item{state}{Carried through into a list the body builds.}
\item{action_model}{Passed to \code{c}.}
\item{transition}{Passed to \code{c}.}
\item{reward_model}{Passed to \code{c}.}
\item{horizon}{Coerced to integer by the body, with \code{as.integer}.}
\item{value_model}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the dreamr_native implementation. Called by \code{morie_dreamr}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{states}, \code{actions}, \code{rewards}, \code{values}, \code{horizon}, \code{method}.
}
\keyword{internal}
\examples{
am <- function(s) 1
tr <- function(s, a) s * 0.9 + 0.1
rm <- function(s) sum(s)
vm <- function(s) sum(s) * 2
r <- morie_dreamr_imagine(state = c(1, 0.5), am, tr, rm, horizon = 5,
                          value_model = vm)
str(r, max.level = 1)
}
