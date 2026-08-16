% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{Kvmse}
\alias{Kvmse}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
Kvmse(K, b = 2, seed = 1)
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later ' Quantize every key in a cache and measure the achieved MSE. ' ' The per-key MSE is reported alongside the mean: one badly ' reconstructed key can dominate an attention score even when the ' average looks fine. The rotation is shared across keys. ' ' Formula: for each row k_i, xtilde_i = TurboQuant_mse(k_i, b); '   MSE_i = ||k_i - xtilde_i||^2, relative to ||k_i||^2 ' ' @param K Key cache, one key per row. ' @param b Bits per coordinate. ' @param seed Seed for the shared pinned rotation. ' @return List with \\code\{mse\}, \\code\{relative_mse\}, \\code\{mean_mse\}, '   \\code\{mean_relative\}, \\code\{worst_relative\}, \\code\{bound\}, '   \\code\{within_bound\}, \\code\{n\}, \\code\{d\}, \\code\{b\}. ' @references Zandieh et al., arXiv:2504.19874, Algorithm 1 and Theorem '   1. Fetched from arXiv. NOTE: the worklist filed this row under '   "vdLaan-ICLR2026-arxiv-2504.19874"; arXiv 2504.19874 is Zandieh et '   al.'s TurboQuant, not a van der Laan paper.
}
\value{
A list with components \code{mse}, \code{relative_mse}, \code{mean_mse}, \code{mean_relative}, \code{worst_relative}, \code{bound}, \code{within_bound}, \code{n}, \code{d}, \code{b}, \code{method}.
}
\keyword{internal}
