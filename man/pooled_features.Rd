% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{pooled_features}
\alias{pooled_features}
\title{pooled_features}
\usage{
pooled_features(hyp, src, ref) 
}
\arguments{
\item{hyp}{Passed to \code{.comet_vec}.}
\item{src}{Passed to \code{.comet_vec}.}
\item{ref}{Passed to \code{.comet_vec}.}
}
\description{
A step of the comet_native implementation. Called by \code{estimator_score}. See the file header for the source the module follows.
}
\value{
A list with \code{features}, \code{dim}, \code{hyp_ref_diff}, \code{hyp_src_diff}, \code{note}.
}
\keyword{internal}
\examples{
pooled_features(hyp = c(1, 2, 3, 4, 5, 6, 7, 8), src = c(1, 2, 3, 4, 5, 6, 7, 8),
  ref = c(1, 2, 3, 4, 5, 6, 7, 8))
}
