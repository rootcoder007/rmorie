% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.edgrn_nb_logpmf}
\alias{.edgrn_nb_logpmf}
\title{.edgrn_nb_logpmf}
\usage{
.edgrn_nb_logpmf(y, mu, phi)
}
\arguments{
\item{y}{Numeric; combined arithmetically in the body.}
\item{mu}{Numeric; passed to \code{max}.}
\item{phi}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the edgrn_native implementation. Called by \code{edgrn_exact_test}. See the file header for the source the module follows.
}
\value{
The value of \code{(}.
}
\keyword{internal}
\examples{
.edgrn_nb_logpmf(y = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8), phi = 0.5)
}
