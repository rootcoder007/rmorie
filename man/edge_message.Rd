% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edge_message}
\alias{edge_message}
\title{Eq. (3). Positions enter ONLY as ||x_i - x_j||^2, which is what}
\usage{
edge_message(h_i, h_j, x_i, x_j, phi_e, a_ij = NULL)
}
\arguments{
\item{h_i}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h_j}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x_i}{Passed to \code{.sqdist}.}
\item{x_j}{Passed to \code{.sqdist}.}
\item{phi_e}{See Usage.}
\item{a_ij}{Defaults to \code{NULL}.}
}
\description{
makes the message invariant.
}
\value{
The value of \code{phi_e}.
}
\keyword{internal}
