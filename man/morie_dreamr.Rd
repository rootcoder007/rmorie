% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dreamr}
\alias{morie_dreamr}
\title{morie_dreamr}
\usage{
morie_dreamr(state, action_model, transition, reward_model, value_model, horizon = 15, gamma = 0.99, lam = 0.95, estimator = "lambda", k = 1)
}
\arguments{
\item{state}{See Usage.}
\item{action_model}{See Usage.}
\item{transition}{See Usage.}
\item{reward_model}{See Usage.}
\item{value_model}{See Usage.}
\item{horizon}{Defaults to \code{15}.}
\item{gamma}{Defaults to \code{0.99}.}
\item{lam}{Defaults to \code{0.95}.}
\item{estimator}{Defaults to \code{"lambda"}.}
\item{k}{Defaults to \code{1}.}
}
\description{
Part of the dreamr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{returns}, \code{objective}, \code{value_loss}, \code{residual}, \code{states}, \code{actions}, \code{rewards}, \code{values}, \code{horizon}, \code{gamma}, \code{lam}, \code{estimator}, \code{method}.
}
\keyword{internal}
