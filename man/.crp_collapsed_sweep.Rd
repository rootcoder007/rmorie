% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.crp_collapsed_sweep}
\alias{.crp_collapsed_sweep}
\title{Neal\\'s algorithm 3 for a conjugate normal DP mixture.  theta is}
\usage{
.crp_collapsed_sweep(y, alpha, n_iter, mu0, tau2, sigma2, seed)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{n_iter}{A count; the body uses it as \code{seq_len(...)}.}
\item{mu0}{Numeric; combined arithmetically in the body.}
\item{tau2}{Numeric; combined arithmetically in the body.}
\item{sigma2}{Numeric; combined arithmetically in the body.}
\item{seed}{Passed to \code{.ghc_rng}.}
}
\description{
integrated out, so a customer joins table k with weight n_k f(y_i | y_k) and opens a new one with weight alpha f(y_i).  The sweep order and the uniform draws are fixed, so the two language arms visit identical states.
}
\value{
A list with \code{z}, \code{counts}, \code{sums}.
}
\keyword{internal}
