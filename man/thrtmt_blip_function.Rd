% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{thrtmt_blip_function}
\alias{thrtmt_blip_function}
\title{thrtmt_blip_function}
\usage{
thrtmt_blip_function(y, A, W, V = NULL, ridge = 1e-08)
}
\arguments{
\item{y}{Passed to \code{.thrtmt_vec}.}
\item{A}{Passed to \code{.thrtmt_vec}.}
\item{W}{Optional; may be \code{NULL}. Passed to \code{.thrtmt_mat}.}
\item{V}{Optional; may be \code{NULL}. Passed to \code{.thrtmt_mat}.}
\item{ridge}{Passed to \code{.thrtmt_lstsq}. Defaults to \code{1e-08}.}
}
\description{
A step of the thrtmt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{blip}, \code{info}.
}
\keyword{internal}
