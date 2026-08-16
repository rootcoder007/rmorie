% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{scalar_vector_message}
\alias{scalar_vector_message}
\title{scalar_vector_message}
\usage{
scalar_vector_message(s_j, v_j, r_ij, phi_s, phi_v, W_rbf)
}
\arguments{
\item{s_j}{Passed to \code{.painn_vec}.}
\item{v_j}{Passed to \code{.painn_mat}.}
\item{r_ij}{Passed to \code{.painn_vec}.}
\item{phi_s}{Accepted by the signature and not used anywhere in the body.}
\item{phi_v}{Accepted by the signature and not used anywhere in the body.}
\item{W_rbf}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the painn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ds}, \code{dv}, \code{note}.
}
\keyword{internal}
