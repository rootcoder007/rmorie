% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Apxkern}
\alias{Apxkern}
\title{Extended approximate (compressed) kernel model (MVSML eq. 8.13)}
\usage{
Apxkern(X, m_index, Z_u1, Z_E, kernel = "linear", gamma = NULL, tol = 1e-10)
}
\description{
y = mu 1 + Z_E beta_E + P_u1 f + P_u2 l + eps.  Steps 1-7 of the summary on p.296: P = K_{L,m} U S^(-1/2) is the compressed design of (8.12) built from m of the L lines, P_u1 = Z_u1 P expands it to the n records, and P_u2 = P_u1 : Z_E is the row-wise Kronecker interaction with the environment design.
}
\value{
A list with \code{P}, \code{P_u1}, \code{P_u2}, \code{design}, \code{widths}, \code{rank}.
}
\keyword{internal}
