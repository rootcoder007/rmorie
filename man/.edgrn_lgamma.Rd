% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.edgrn_lgamma}
\alias{.edgrn_lgamma}
\title{.edgrn_lgamma}
\usage{
.edgrn_lgamma(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
A step of the edgrn_native implementation. Called by \code{.edgrn_betainc}, \code{.edgrn_chi2_sf}, \code{.edgrn_nb_logpmf}. See the file header for the source the module follows.
}
\value{
The value of \code{lgamma}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.edgrn_lgamma(V)
}
