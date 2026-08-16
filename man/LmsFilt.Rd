% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsFilt}
\alias{LmsFilt}
\title{LmsFilt}
\usage{
LmsFilt(primary, reference, order = 8, mu = 0.01, variable = FALSE, alpha = 0.02)
}
\arguments{
\item{primary}{See Usage.}
\item{reference}{See Usage.}
\item{order}{Defaults to \code{8}.}
\item{mu}{Defaults to \code{0.01}.}
\item{variable}{Defaults to \code{FALSE}.}
\item{alpha}{Defaults to \code{0.02}.}
}
\description{
Part of the rangayyan_adapt implementation; see the file header for the source it follows.
}
\value{
A list with \code{e}, \code{output}, \code{y}, \code{final_weights}, \code{order}, \code{mu}, \code{variable_step}, \code{step_history}, \code{stable_bound}, \code{within_bound}, \code{input_power}, \code{output_power}, \code{power_reduction}, \code{converges_in_the_mean_only}, \code{method}.
}
\keyword{internal}
