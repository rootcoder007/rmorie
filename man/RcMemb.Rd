% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RcMemb}
\alias{RcMemb}
\title{RcMemb: passive RC membrane potential dynamics}
\usage{
RcMemb(t, I_inj = 0, C_m = 0.2, R_m = 100, V_rest = -65)
}
\arguments{
\item{t}{See Usage.}
\item{I_inj}{Defaults to \code{0}.}
\item{C_m}{Defaults to \code{0.2}.}
\item{R_m}{Defaults to \code{100}.}
\item{V_rest}{Defaults to \code{-65}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{t_ms}, \code{V_mV}, \code{tau_ms}, \code{V_steady_mV}, \code{input_resistance_MOhm}, \code{peak_mV}, \code{final_mV}, \code{V_rest_mV}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
RcMemb(V)
}
