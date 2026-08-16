% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.itrgrf_policy_from_tau}
\alias{.itrgrf_policy_from_tau}
\title{.itrgrf_policy_from_tau}
\usage{
.itrgrf_policy_from_tau(tau, cost = 0)
}
\arguments{
\item{tau}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{cost}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the itrgrf_native implementation. Called by \code{morie_itrgrf}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.itrgrf_policy_from_tau(V)
}
