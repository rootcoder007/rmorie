% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PzFormZ}
\alias{PzFormZ}
\title{Eq (3.70): H(z) = z^(M-N) prod (z - z_k) / prod (z - p_k), the same}
\usage{
PzFormZ(zeros, poles, z = NULL, gain = 1)
}
\arguments{
\item{zeros}{See Usage.}
\item{poles}{See Usage.}
\item{z}{Defaults to \code{NULL}.}
\item{gain}{Defaults to \code{1}.}
}
\description{
function as eq (3.69) rewritten in z.  The z^(M-N) factor is exactly what the change of variable produces; dropping it multiplies H by a pure delay, invisible in the magnitude and fatal to the phase.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
