% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.impFB_cost}
\alias{.impFB_cost}
\title{.impFB_cost}
\usage{
.impFB_cost(R, X, Y, alpha = 40, lam = 0.1)
}
\arguments{
\item{R}{See Usage.}
\item{X}{See Usage.}
\item{Y}{See Usage.}
\item{alpha}{Defaults to \code{40}.}
\item{lam}{Defaults to \code{0.1}.}
}
\description{
Part of the impFB_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\references{
----------
Hu, Y., Koren, Y. & Volinsky, C. (2008) "Collaborative Filtering for
Implicit Feedback Datasets", Proceedings of the Eighth IEEE
International Conference on Data Mining (ICDM 2008), 263-272,
doi:10.1109/ICDM.2008.22. Sec. 2 (the four properties of implicit
feedback, in particular that its numerical value indicates confidence
rather than preference). Sec. 4 (the binarised p_ui, the confidence
c_ui = 1 + alpha r_ui with alpha = 40, the cost function of eq. (3)
summed over ALL m*n pairs, the alternating least squares update
eq. (4), and the Y\\'C^u Y = Y\\'Y + Y\\'(C^u - I)Y decomposition giving
O(f^2 N + f^3 m) total time). Sec. 5 (explaining recommendations
through W^u).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", Computer 42(8), 30-37,
doi:10.1109/MC.2009.263. The explicit-feedback factorisation this is
adapted from.
}
\keyword{internal}
