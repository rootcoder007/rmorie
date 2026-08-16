% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sdid}
\alias{sdid}
\title{sdid}
\usage{
sdid(Y, treated, t_post, method = "sdid", zeta = NULL)
}
\arguments{
\item{Y}{See Usage.}
\item{treated}{See Usage.}
\item{t_post}{See Usage.}
\item{method}{Defaults to \code{"sdid"}.}
\item{zeta}{Defaults to \code{NULL}.}
}
\description{
Part of the causscd_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{tau}, \code{unit_weights}, \code{time_weights}, \code{zeta}, \code{delta_treated}, \code{delta_control}, \code{method_name}, \code{n_treated}, \code{n_control}, \code{t_pre}, \code{t_post}, \code{method}, \code{note}.
}
\keyword{internal}
