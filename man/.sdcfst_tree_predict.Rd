% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sdcfst_tree_predict}
\alias{.sdcfst_tree_predict}
\title{.sdcfst_tree_predict}
\usage{
.sdcfst_tree_predict(node, x)
}
\arguments{
\item{node}{A list; the body reads \code{$cut}, \code{$feature}, \code{$leaf}, \code{$left}, \code{$right}, \code{$value} from it.}
\item{x}{A vector; indexed elementwise.}
}
\description{
A step of the sdcfst_native implementation. Called by \code{morie_sdcfst_predict}. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
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
