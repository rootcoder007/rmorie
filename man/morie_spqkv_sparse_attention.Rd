% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_spqkv_sparse_attention}
\alias{morie_spqkv_sparse_attention}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
morie_spqkv_sparse_attention(x, window = 4L, stride = 8L, n_random = 0L, seed = 0L) 
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later ' Child-2019 sparse attention mask ' ' @param x Sequence length (integer) or tensor whose second-to-last '   dim is the sequence length. ' @param window Integer local window radius (default 4). ' @param stride Integer strided-attention period (default 8). ' @param n_random Integer per-row random links (default 0). ' @param seed Integer RNG seed (default 0). ' @return Named list with tensor (additive mask), boolean, density, method. ' @keywords internal
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{window}{Argument \code{window}; see Usage.}
\item{stride}{Argument \code{stride}; see Usage.}
\item{n_random}{Argument \code{n_random}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
}
\value{
A list with components \code{by}, \code{size}, \code{tensor}, \code{boolean}, \code{density}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_spqkv_sparse_attention(V)
}
