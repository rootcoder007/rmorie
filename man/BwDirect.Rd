% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwDirect}
\alias{BwDirect}
\title{Eq (3.145): specified on the discrete-frequency axis outright, so}
\usage{
BwDirect(omega, omega_c, N) 
}
\arguments{
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega_c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{N}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
there is no warping to prewarp for.  The filter so defined has zero phase, which is only usable with the whole record in hand -- it is not causal and cannot be run sample by sample.
}
\value{
A list with \code{squared_magnitude}, \code{magnitude}, \code{omega}, \code{omega_c}, \code{N}, \code{half_power_at_cutoff}, \code{no_warping}, \code{zero_phase}, \code{not_causal}, \code{method}.
}
\keyword{internal}
\examples{
BwDirect(omega = c(1, 2, 3, 4, 5, 6, 7, 8), omega_c = 5L, N = 5L)
}
