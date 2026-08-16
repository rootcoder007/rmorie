% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_zero_grads}
\alias{.propinf_zero_grads}
\title{.propinf_zero_grads}
\usage{
.propinf_zero_grads(model)
}
\arguments{
\item{model}{A list; the body reads \code{$phis}, \code{$psis}, \code{$rho} from it.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}. See the file header for the source the module follows.
}
\value{
A list with \code{phis}, \code{psis}, \code{rho}.
}
\keyword{internal}
