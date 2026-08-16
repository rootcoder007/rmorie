% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.trclrn_best_treatment}
\alias{.trclrn_best_treatment}
\title{.trclrn_best_treatment}
\usage{
.trclrn_best_treatment(y, a, p, rows, arms, method, Xm, outcome_model)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{a}{A vector; indexed elementwise.}
\item{p}{A vector; indexed elementwise.}
\item{rows}{See Usage.}
\item{arms}{See Usage.}
\item{method}{Compared against \code{"ipw"}.}
\item{Xm}{A matrix; indexed by row and column.}
\item{outcome_model}{See Usage.}
}
\description{
A step of the trclrn_native implementation. Called by \code{trclrn_fit_tree}. See the file header for the source the module follows.
}
\value{
A list with \code{best}, \code{bv}.
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
