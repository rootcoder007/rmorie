% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tlbandt_martingale_terms}
\alias{.tlbandt_martingale_terms}
\title{.tlbandt_martingale_terms}
\usage{
.tlbandt_martingale_terms(A, Y, g, Q1, Q0, psi)
}
\arguments{
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{g}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Q1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Q0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{psi}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlbandt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 24 (Chambaz,
Zheng & van der Laan): an infinite i.i.d. sequence
(W_n, Y_n(0), Y_n(1)) sequentially and partially disclosed; the
context W_n revealed first, then a randomized action A_n carried out
with probability g_n(.|W_n) determined by the observations accrued so
far, and only the reward Y_n = Y_n(A_n) of the action taken granted --
the alternative never observed; a possibly high-dimensional context
set and rewards in the open unit interval.

Chambaz, A., Zheng, W. & van der Laan, M. J. (2017) "Targeted
sequential design for targeted learning inference of the optimal
treatment rule and its mean reward", Annals of Statistics 45(6),
2537-2564, doi:10.1214/16-AOS1534.

Lai, T. L. & Robbins, H. (1985) "Asymptotically efficient adaptive
allocation rules", Advances in Applied Mathematics 6(1), 4-22,
doi:10.1016/0196-8858(85)90002-8. The reward-maximising tradition
this departs from.
}
\keyword{internal}
