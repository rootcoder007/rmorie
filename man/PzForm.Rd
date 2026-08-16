% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PzForm}
\alias{PzForm}
\title{Eq (3.69): H(z) = prod (1 - z_k z^-1) / prod (1 - p_k z^-1).  A pole}
\usage{
PzForm(zeros, poles, z = NULL, gain = 1)
}
\arguments{
\item{zeros}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles}{Coerced to complex by the body, with \code{as.complex}.}
\item{z}{Optional; may be \code{NULL}. Coerced to complex by the body, with \code{as.complex}.}
\item{gain}{Coerced to complex by the body, with \code{as.complex}. Defaults to \code{1}.}
}
\description{
on the unit circle makes H undefined there; outside it, a causal system is unstable.  Both are reported.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PzForm(V, V)
}
