% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{adaln_zero}
\alias{adaln_zero}
\title{Adaptive LayerNorm with a ZERO-initialised residual gate}
\usage{
adaln_zero(cond, hidden, W_scale, W_shift, W_alpha, eps = 1e-06) 
}
\arguments{
\item{cond}{   numeric, conditioning vector.}
\item{hidden}{ numeric, hidden activations.}
\item{W_scale}{d x |cond| matrix for gamma.}
\item{W_shift}{d x |cond| matrix for beta.}
\item{W_alpha}{d x |cond| matrix for alpha (initialised to zero).}
\item{eps}{    numeric, variance epsilon.}
}
\description{
Regresses gamma, beta and alpha from the conditioning vector. With W_alpha = 0 every block is the identity at initialisation.
}
\value{
list with modulated, gate, identity_at_init, note.
}
\keyword{internal}
\examples{
adaln_zero(cond = c(1, 2, 3, 4, 5, 6, 7, 8), hidden = c(1, 2, 3, 4, 5, 6, 7, 8),
  W_scale = c(1, 2, 3, 4, 5, 6, 7, 8), W_shift = c(1, 2, 3, 4, 5, 6, 7, 8),
  W_alpha = c(1, 2, 3, 4, 5, 6, 7, 8))
}
