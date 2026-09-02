% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_rms_norm}
\alias{morie_mistr_rms_norm}
\title{RMSNorm}
\usage{
morie_mistr_rms_norm(x, weight = NULL, eps = 1e-06) 
}
\arguments{
\item{x}{Vector of length d.}
\item{weight}{Optional gain of length d.}
\item{eps}{Numerical guard.}
}
\description{
\code{x / sqrt(mean(x^2) + eps)}, times a gain. No mean subtraction: invariant to the SCALE of its input and, unlike LayerNorm, not to a shift.
}
\value{
The normalised vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_mistr_rms_norm(V)
}
