% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sdcfst_grow}
\alias{.sdcfst_grow}
\title{One honest tree: split on struct_rows, fill leaves from leaf_rows}
\usage{
.sdcfst_grow(X, y, struct_rows, leaf_rows, feats_n, min_leaf, max_depth, e, depth = 0L)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{struct_rows}{See Usage.}
\item{leaf_rows}{See Usage.}
\item{feats_n}{See Usage.}
\item{min_leaf}{See Usage.}
\item{max_depth}{See Usage.}
\item{e}{See Usage.}
\item{depth}{Defaults to \code{0L}.}
}
\description{
Part of the sdcfst_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{leaf}, \code{feature}, \code{cut}, \code{left}, \code{right}.
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
