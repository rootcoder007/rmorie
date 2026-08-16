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
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{I_inj}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{C_m}{Numeric; combined arithmetically in the body. Defaults to \code{0.2}.}
\item{R_m}{Numeric; combined arithmetically in the body. Defaults to \code{100}.}
\item{V_rest}{Numeric; combined arithmetically in the body. Defaults to \code{-65}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{t_ms}, \code{V_mV}, \code{tau_ms}, \code{V_steady_mV}, \code{input_resistance_MOhm}, \code{peak_mV}, \code{final_mV}, \code{V_rest_mV}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
RcMemb(V)
}
