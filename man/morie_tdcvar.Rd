% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tdcvar}
\alias{morie_tdcvar}
\title{morie_tdcvar}
\usage{
morie_tdcvar(y, A, L_t, time = NULL, contrast = "cumulative", kind = "binary", 
    stabilize = TRUE, trim = NULL) 
}
\arguments{
\item{y}{Passed to \code{.tdcvar_vec}.}
\item{A}{A list; the body checks with \code{is.list}.}
\item{L_t}{A list; the body checks with \code{is.list}.}
\item{time}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{contrast}{One of \code{"cumulative"}, \code{"everexposed"}, \code{"final"}. Defaults to \code{"cumulative"}.}
\item{kind}{Passed to \code{.tdcvar_ip_weights_history}. Defaults to \code{"binary"}.}
\item{stabilize}{Passed to \code{.tdcvar_ip_weights_history}. Defaults to \code{TRUE}.}
\item{trim}{Passed to \code{.tdcvar_ip_weights_history}.}
}
\description{
A step of the tdcvar_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\references{
----------
Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC. Ch. 20 (treatment-confounder feedback),
Sec. 21.2 (IP weighting for time-varying treatments), Sec. 12.3 (the
stabilized weights and the mean-1 diagnostic).

Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period", Mathematical Modelling
7(9-12), 1393-1512, doi:10.1016/0270-0255(86)90088-6 -- where the
problem and the weighted solution were first set out.
}
\keyword{internal}
