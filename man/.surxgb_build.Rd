% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.surxgb_build}
\alias{.surxgb_build}
\title{.surxgb_build}
\usage{
.surxgb_build(X, g, h, idx, depth, max_depth, lam, gamma, min_child)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{g}{A vector; indexed elementwise.}
\item{h}{A vector; indexed elementwise.}
\item{idx}{A vector; its length is taken and its elements indexed.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.surxgb_build}.}
\item{lam}{Passed to \code{morie_surxgb_leaf_weight}.}
\item{gamma}{Passed to \code{morie_surxgb_split_gain}.}
\item{min_child}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the surxgb_native implementation. Called by \code{morie_surxgb_boost}. See the file header for the source the module follows.
}
\value{
A list with \code{leaf}, \code{variable}, \code{cut}, \code{gain}, \code{left}, \code{right}.
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
