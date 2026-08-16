% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{trclrn_predict_rule}
\alias{trclrn_predict_rule}
\title{trclrn_predict_rule}
\usage{
trclrn_predict_rule(tree, X)
}
\arguments{
\item{tree}{See Usage.}
\item{X}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the trclrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
