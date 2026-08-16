% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_surxgb_aft_gradient_hessian}
\alias{morie_surxgb_aft_gradient_hessian}
\title{morie_surxgb_aft_gradient_hessian}
\usage{
morie_surxgb_aft_gradient_hessian(y_lower, y_upper, u, sigma = 1, dist = "normal", method = "analytic", eps = 1e-05)
}
\arguments{
\item{y_lower}{See Usage.}
\item{y_upper}{See Usage.}
\item{u}{See Usage.}
\item{sigma}{Defaults to \code{1}.}
\item{dist}{Defaults to \code{"normal"}.}
\item{method}{Defaults to \code{"analytic"}.}
\item{eps}{Defaults to \code{1e-05}.}
}
\description{
Part of the surxgb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{gradient}, \code{hessian}, \code{loss}, \code{hessian_floored}, \code{derivative_method}.
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
morie_surxgb_aft_gradient_hessian(y_lower = 5L, y_upper = 5L, u = c(1, 2, 3, 4, 5, 6, 7, 8))
}
