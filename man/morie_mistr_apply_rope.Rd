% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_apply_rope}
\alias{morie_mistr_apply_rope}
\title{Apply RoPE to a vector}
\usage{
morie_mistr_apply_rope(x, pos, theta = NULL, base = 10000)
}
\arguments{
\item{x}{Vector of length d.}
\item{pos}{Position.}
\item{theta}{Optional pre-computed angles.}
\item{base}{Frequency base.}
}
\description{
Rotate each channel pair (2i, 2i+1) by \code{pos * theta_i}.
}
\value{
The rotated vector.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_mistr_apply_rope(V, V)
}
