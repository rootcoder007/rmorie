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
\item{state}{See Usage.}
\item{action_model}{See Usage.}
\item{transition}{See Usage.}
\item{reward_model}{See Usage.}
\item{horizon}{See Usage.}
\item{value_model}{Defaults to \code{NULL}.}
}
\description{
Part of the dreamr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{states}, \code{actions}, \code{rewards}, \code{values}, \code{horizon}, \code{method}.
}
\keyword{internal}
