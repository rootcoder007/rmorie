% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_lcg_seed}
\alias{.tf_lcg_seed}
\title{.tf_lcg_seed}
\usage{
.tf_lcg_seed(seed)
}
\arguments{
\item{seed}{Passed to \code{.tf_seed_limbs}.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{EmdEns}. See the file header for the source the module follows.
}
\value{
The value of \code{.tf_lcg_step}.
}
\keyword{internal}
\examples{
.tf_lcg_seed(seed = 5L)
}
