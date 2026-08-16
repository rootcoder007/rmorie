% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.impFB_cheatsheet}
\alias{.impFB_cheatsheet}
\title{.impFB_cheatsheet}
\usage{
.impFB_cheatsheet()
}
\description{
A step of the impFB_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A character value.
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
eq. (4), and the Y\'C^u Y = Y\'Y + Y\'(C^u - I)Y decomposition giving
O(f^2 N + f^3 m) total time). Sec. 5 (explaining recommendations
through W^u).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", Computer 42(8), 30-37,
doi:10.1109/MC.2009.263. The explicit-feedback factorisation this is
adapted from.
}
\keyword{internal}
\examples{
.impFB_cheatsheet()
}
