% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LogMinPh}
\alias{LogMinPh}
\title{Eq (4.70): log(1 - alpha z^-1) = -sum alpha^n/n z^-n, |z| > |alpha|}
\usage{
LogMinPh(alpha, terms = 20, z = NULL) 
}
\arguments{
\item{alpha}{Coerced to complex by the body, with \code{as.complex}.}
\item{terms}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
\item{z}{Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.}
}
\description{
The coefficients sit at POSITIVE quefrency and decay at least as fast as 1/n: the minimum-phase cepstrum is causal.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LogMinPh(V)
}
