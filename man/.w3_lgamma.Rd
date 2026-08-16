% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_lgamma}
\alias{.w3_lgamma}
\title{Lanczos log-gamma. Not lgamma(): Python\\'s is a different routine}
\usage{
.w3_lgamma(z)
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{.bnppvl_log_beta}, \code{.w3_betainc}, \code{.w3_gammcf} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.w3_lgamma(V)
}
