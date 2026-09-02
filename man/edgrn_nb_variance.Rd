% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edgrn_nb_variance}
\alias{edgrn_nb_variance}
\title{edgrn_nb_variance}
\usage{
edgrn_nb_variance(mu, dispersion) 
}
\arguments{
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{dispersion}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the edgrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{variance}, \code{poisson}, \code{biological}, \code{bcv}, \code{note}.
}
\keyword{internal}
\examples{
edgrn_nb_variance(mu = 5L, dispersion = 5L)
}
