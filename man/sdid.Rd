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
\item{Y}{Passed to \code{.causscd_grid}.}
\item{treated}{Passed to \code{.causscd_grid}.}
\item{t_post}{A count; the body uses it as \code{seq_len(...)}.}
\item{method}{One of \code{"did"}, \code{"sc"}, \code{"sdid"}. Defaults to \code{"sdid"}.}
\item{zeta}{Defaults to \code{NULL}.}
}
\description{
A step of the causscd_native implementation. Called by \code{causscd}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tau}, \code{unit_weights}, \code{time_weights}, \code{zeta}, \code{delta_treated}, \code{delta_control}, \code{method_name}, \code{n_treated}, \code{n_control}, \code{t_pre}, \code{t_post}, \code{method}, \code{note}.
}
\keyword{internal}
