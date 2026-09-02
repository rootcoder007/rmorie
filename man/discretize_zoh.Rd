% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{discretize_zoh}
\alias{discretize_zoh}
\title{discretize_zoh}
\usage{
discretize_zoh(delta, A, B, rule = "zoh") 
}
\arguments{
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{B}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rule}{One of \code{"euler"}, \code{"zoh"}. Defaults to \code{"zoh"}.}
}
\description{
A step of the mambss_native implementation. Called by \code{selective_ssm_step}. See the file header for the source the module follows.
}
\value{
A list with \code{Abar}, \code{Bbar}.
}
\keyword{internal}
\examples{
discretize_zoh(delta = 0.5, A = c(1, 2, 3, 4, 5, 6, 7, 8), B = c(1, 2, 3, 4, 5, 6, 7, 8))
}
