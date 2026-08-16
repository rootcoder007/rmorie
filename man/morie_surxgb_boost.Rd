% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_surxgb_boost}
\alias{morie_surxgb_boost}
\title{morie_surxgb_boost}
\usage{
morie_surxgb_boost(X, y_lower, y_upper, n_rounds = 50, eta = 0.1, max_depth = 3, lam = 1, gamma = 0, min_child = 5, sigma = 1, dist = "normal", base_score = NULL, derivatives = "analytic")
}
\arguments{
\item{X}{See Usage.}
\item{y_lower}{See Usage.}
\item{y_upper}{See Usage.}
\item{n_rounds}{Defaults to \code{50}.}
\item{eta}{Defaults to \code{0.1}.}
\item{max_depth}{Defaults to \code{3}.}
\item{lam}{Defaults to \code{1}.}
\item{gamma}{Defaults to \code{0}.}
\item{min_child}{Defaults to \code{5}.}
\item{sigma}{Defaults to \code{1}.}
\item{dist}{Defaults to \code{"normal"}.}
\item{base_score}{Defaults to \code{NULL}.}
\item{derivatives}{Defaults to \code{"analytic"}.}
}
\description{
Part of the surxgb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{trees}, \code{eta}, \code{lam}, \code{gamma}, \code{sigma}, \code{dist}, \code{base_score}, \code{derivatives}, \code{loss_history}, \code{prediction}, \code{n_rounds}, \code{max_depth}, \code{method}.
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
\examples{
morie_surxgb_boost(X = c(1, 2, 3, 4, 5, 6, 7, 8), y_lower = c(1, 2, 3, 4, 5, 6, 7, 8), y_upper = c(1, 2, 3, 4, 5, 6, 7, 8))
}
