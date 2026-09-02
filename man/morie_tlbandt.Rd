% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlbandt}
\alias{morie_tlbandt}
\title{morie_tlbandt}
\usage{
morie_tlbandt(W, Y1, Y0, blip_fn, delta = 0.1, seed = 0, greedy = FALSE, burn_in = 20L) 
}
\arguments{
\item{W}{A matrix; passed to \code{as.matrix}.}
\item{Y1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Y0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{blip_fn}{Accepted by the signature and not used anywhere in the body.}
\item{delta}{Passed to \code{.tlbandt_design_probability}. Defaults to \code{0.1}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{greedy}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{burn_in}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20L}.}
}
\description{
A step of the tlbandt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{A}, \code{Y}, \code{g}, \code{history}, \code{greedy}, \code{min_g}, \code{max_g}, \code{note}.
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
\examples{
morie_tlbandt(W = c(1, 2, 3, 4, 5, 6, 7, 8), Y1 = c(1, 2, 3, 4, 5, 6, 7, 8), Y0 = c(1, 2, 3, 4, 5, 6, 7, 8), blip_fn = c(1, 2, 3, 4, 5, 6, 7, 8))
}
