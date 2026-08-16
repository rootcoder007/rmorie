% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tncomp_select}
\alias{.tncomp_select}
\title{.tncomp_select}
\usage{
.tncomp_select(fps, k, objective, seed = NULL, D = NULL)
}
\arguments{
\item{fps}{See Usage.}
\item{k}{See Usage.}
\item{objective}{Compared against \code{"maxmin"}.}
\item{seed}{Passed to \code{.tncomp_seed}.}
\item{D}{Defaults to \code{NULL}.}
}
\description{
A step of the tncomp_native implementation. Called by \code{maxmin_selection}, \code{maxsum_selection}, \code{morie_tncomp}. See the file header for the source the module follows.
}
\value{
A list with \code{chosen}, \code{M}.
}
\references{
----------
Snarey, M., Terrett, N. K., Willett, P. & Wilton, D. J. (1997)
"Comparison of algorithms for dissimilarity-based compound
selection", Journal of Molecular Graphics and Modelling 15(6),
372-385, doi:10.1016/S1093-3263(98)00008-4.  The MaxMin and
MaxSum objectives reproduced above, their greedy implementation,
and the finding that MaxMin covers a collection better.

Willett, P., Barnard, J. M. & Downs, G. M. (1998) "Chemical
similarity searching", Journal of Chemical Information and
Computer Sciences 38(6), 983-996, doi:10.1021/ci9800211, for
the Tanimoto distance the objectives are measured in; see
morie.fn.sasimi.
}
\keyword{internal}
