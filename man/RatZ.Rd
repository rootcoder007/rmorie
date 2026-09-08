% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RatZ}
\alias{RatZ}
\title{RatZ}
\usage{
RatZ(gain, r, zeros_in, zeros_out, poles_in, poles_out, z = NULL) 
}
\arguments{
\item{gain}{Coerced to complex by the body, with \code{as.complex}.}
\item{r}{Coerced to integer by the body, with \code{as.integer}.}
\item{zeros_in}{Coerced to complex by the body, with \code{as.complex}.}
\item{zeros_out}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles_in}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles_out}{Coerced to complex by the body, with \code{as.complex}.}
\item{z}{Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.}
}
\description{
A step of the rangayyan_cep implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
