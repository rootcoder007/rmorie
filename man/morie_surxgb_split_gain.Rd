% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_surxgb_split_gain}
\alias{morie_surxgb_split_gain}
\title{Equation (7): the loss reduction, net of the leaf price}
\usage{
morie_surxgb_split_gain(GL, HL, GR, HR, lam = 1, gamma = 0)
}
\arguments{
\item{GL}{Numeric; combined arithmetically in the body.}
\item{HL}{Numeric; combined arithmetically in the body.}
\item{GR}{Numeric; combined arithmetically in the body.}
\item{HR}{Numeric; combined arithmetically in the body.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the surxgb_native implementation. Called by \code{.surxgb_build}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
morie_surxgb_split_gain(GL = c(1, 2, 3, 4, 5, 6, 7, 8), HL = c(1, 2, 3, 4, 5, 6, 7, 8), GR = c(1, 2, 3, 4, 5, 6, 7, 8), HR = c(1, 2, 3, 4, 5, 6, 7, 8))
}
