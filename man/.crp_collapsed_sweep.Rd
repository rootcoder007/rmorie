% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.crp_collapsed_sweep}
\alias{.crp_collapsed_sweep}
\title{Neal\\\'s algorithm 3 for a conjugate normal DP mixture.  theta is}
\usage{
.crp_collapsed_sweep(y, alpha, n_iter, mu0, tau2, sigma2, seed)
}
\arguments{
\item{y}{See Usage.}
\item{alpha}{See Usage.}
\item{n_iter}{See Usage.}
\item{mu0}{See Usage.}
\item{tau2}{See Usage.}
\item{sigma2}{See Usage.}
\item{seed}{See Usage.}
}
\description{
integrated out, so a customer joins table k with weight n_k f(y_i | y_k) and opens a new one with weight alpha f(y_i).  The sweep order and the uniform draws are fixed, so the two language arms visit identical states.
}
\value{
A list with \code{z}, \code{counts}, \code{sums}.
}
\keyword{internal}
