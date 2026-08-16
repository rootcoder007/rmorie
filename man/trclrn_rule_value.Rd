% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{trclrn_rule_value}
\alias{trclrn_rule_value}
\title{trclrn_rule_value}
\usage{
trclrn_rule_value(Y, A, X, rule, propensity = NULL, method = "ipw", outcome_model = NULL, min_propensity = 0.01)
}
\arguments{
\item{Y}{Passed to \code{.trclrn_check}.}
\item{A}{Passed to \code{.trclrn_check}.}
\item{X}{Passed to \code{.trclrn_check}.}
\item{rule}{Accepted by the signature and not used anywhere in the body.}
\item{propensity}{Passed to \code{.trclrn_check}.}
\item{method}{Compared against \code{"ipw"}. Defaults to \code{"ipw"}.}
\item{outcome_model}{Defaults to \code{NULL}.}
\item{min_propensity}{Passed to \code{.trclrn_check}. Defaults to \code{0.01}.}
}
\description{
A step of the trclrn_native implementation. Called by \code{trclrn_fit_tree}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\references{
Laber, E. B. & Zhao, Y. Q. (2015) "Tree-based methods for
individualized treatment regimes", Biometrika 102(3), 501-514,
doi:10.1093/biomet/asv028. Sec. 2.1 (the optimal rule and Assumptions
1-3: positivity, strong ignorability, consistency), Sec. 2.2 (purity
measures for treatment allocation and the rectangular-region
representation), and the treatment of continuous treatments by kernel
smoothing.
Tao, Y. & Wang, L. (2017) "Adaptive Contrast Weighted Learning for
Multi-Stage Multi-Treatment Decision-Making", Biometrics 73,
145-155, doi:10.1111/biom.12539. The adaptive contrast that augments
the weighted-learning objective with an outcome model, implemented
here as method="augmented".
Zhang, B., Tsiatis, A. A., Laber, E. B. & Davidian, M. (2012) "A robust
method for estimating optimal treatment regimes", Biometrics 68(4),
1010-1018, doi:10.1111/j.1541-0420.2012.01763.x. The recasting of
treatment selection as a weighted classification problem that the
tree search rests on.
}
\keyword{internal}
