% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_itrgrf}
\alias{morie_itrgrf}
\title{morie_itrgrf}
\usage{
morie_itrgrf(y, W, X, cost = 0, n_trees = 150, min_leaf = 5, seed = 0, evaluate = "split", propensity = NULL, level = 0.95)
}
\arguments{
\item{y}{See Usage.}
\item{W}{See Usage.}
\item{X}{See Usage.}
\item{cost}{Defaults to \code{0}.}
\item{n_trees}{Defaults to \code{150}.}
\item{min_leaf}{Defaults to \code{5}.}
\item{seed}{Defaults to \code{0}.}
\item{evaluate}{Defaults to \code{"split"}.}
\item{propensity}{Defaults to \code{NULL}.}
\item{level}{Defaults to \code{0.95}.}
}
\description{
Part of the itrgrf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{value}, \code{se}, \code{ci}, \code{rule}, \code{tau}, \code{mu1}, \code{mu0}, \code{treated_fraction}, \code{value_treat_all}, \code{value_treat_none}, \code{gain_over_treat_all}, \code{gain_over_treat_none}, \code{scores}, \code{cost}, \code{evaluate}, \code{n}, \code{n_scored}, \code{level}, \code{method}.
}
\references{
----------
Athey, S. & Wager, S. (2021) "Policy Learning With Observational
  Data", Econometrica 89(1), 133-161, doi:10.3982/ECTA15732. The
  doubly robust scores and the regret bound for the learned rule.

Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized Random
  Forests", The Annals of Statistics 47(2), 1148-1178,
  doi:10.1214/18-AOS1709, arXiv:1610.01271. The forest that
  supplies tau-hat.

Zhao, Y., Zeng, D., Rush, A. J. & Kosorok, M. R. (2012)
  "Estimating Individualized Treatment Rules Using Outcome
  Weighted Learning", Journal of the American Statistical
  Association 107(499), 1106-1118, doi:10.1080/01621459.2012.695674.
  The value-maximisation framing.

Manski, C. F. (2004) "Statistical Treatment Rules for
  Heterogeneous Populations", Econometrica 72(4), 1221-1246,
  doi:10.1111/j.1468-0262.2004.00530.x. Treatment rules as the
  object of inference.
}
\keyword{internal}
