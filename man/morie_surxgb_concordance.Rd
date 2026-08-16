% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_surxgb_concordance}
\alias{morie_surxgb_concordance}
\title{Harrell\'s C for the fit; a larger prediction is a longer life, so}
\usage{
morie_surxgb_concordance(fit, X, times, events)
}
\arguments{
\item{fit}{Passed to \code{morie_surxgb_predict}.}
\item{X}{Passed to \code{morie_surxgb_predict}.}
\item{times}{Passed to \code{morie_survrsf_c_index}.}
\item{events}{Passed to \code{morie_survrsf_c_index}.}
}
\description{
the score is negated before it is ranked.
}
\value{
The value of \code{morie_survrsf_c_index}.
}
\references{
----------
Chen, T. & Guestrin, C. (2016) "XGBoost: A Scalable Tree Boosting
System", KDD \'16, 785-794, doi:10.1145/2939672.2939785. Secs. 2.1
and 2.2, eqs (5)-(7).

Barnwal, A., Cho, H. & Hocking, T. (2022) "Survival Regression with
Accelerated Failure Time Model in XGBoost", Journal of Computational
and Graphical Statistics 31(4), 1292-1302,
doi:10.1080/10618600.2022.2067548 (arXiv:2006.04920). Table 1,
Definitions 1-2, Table 2, Sec. 4.

Ishwaran, H., Kogalur, U. B., Blackstone, E. H. & Lauer, M. S.
(2008) "Random Survival Forests", Annals of Applied Statistics 2(3),
841-860, doi:10.1214/08-AOAS169, Sec. 5.1, for the concordance index
reused here from survrsf.
}
\keyword{internal}
