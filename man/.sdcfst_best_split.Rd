% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sdcfst_best_split}
\alias{.sdcfst_best_split}
\title{Variance-reduction split over the given rows and features. Candidate}
\usage{
.sdcfst_best_split(X, y, rows, feats, min_leaf)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{rows}{A vector; its length is taken and its elements indexed.}
\item{feats}{See Usage.}
\item{min_leaf}{Numeric; combined arithmetically in the body.}
}
\description{
cuts are midpoints between consecutive DISTINCT values -- the only set that can produce different partitions, and putting a cut exactly on a data point is where the two arms\\' comparisons could part company.
}
\value{
The value of \code{best}, as built in the body.
}
\references{
  Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
    C., Newey, W. and Robins, J. (2018) "Double/debiased machine
    learning for treatment and structural parameters." The
    Econometrics Journal 21(1), C1-C68.
  Robinson, P.M. (1988) "Root-N-consistent semiparametric regression."
    Econometrica 56(4), 931-954.
  Robins, J.M., Rotnitzky, A. and Zhao, L.P. (1994) JASA 89(427),
    846-866.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." JASA
    113(523), 1228-1242.
}
\keyword{internal}
