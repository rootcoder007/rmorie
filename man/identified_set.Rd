% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{identified_set}
\alias{identified_set}
\title{identified_set}
\usage{
identified_set(beta, n_pre, n_post, M = 0, family = "SD", l_vec = NULL, grid = NULL)
}
\arguments{
\item{beta}{Passed to \code{.snmtst_split}.}
\item{n_pre}{Passed to \code{.snmtst_split}.}
\item{n_post}{Passed to \code{.snmtst_split}.}
\item{M}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{family}{Compared against \code{"SD"}. Defaults to \code{"SD"}.}
\item{l_vec}{Passed to \code{.snmtst_target}.}
\item{grid}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the snmtst_native implementation. Called by \code{breakdown_value}, \code{fixed_length_ci}, \code{sensitivity_curve}. See the file header for the source the module follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{estimate}, \code{pre_max_change}, \code{bound}, \code{M}, \code{family}, \code{width}, \code{note}.
}
\keyword{internal}
\examples{
beta <- c(-0.05, 0.02, 0.5, 0.55, 0.6)
r <- identified_set(beta, n_pre = 2, n_post = 3, M = 0.1)
str(r, max.level = 1)
}
