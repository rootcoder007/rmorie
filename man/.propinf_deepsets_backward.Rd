% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_deepsets_backward}
\alias{.propinf_deepsets_backward}
\title{.propinf_deepsets_backward}
\usage{
.propinf_deepsets_backward(model, sets, cache, dout, grads)
}
\arguments{
\item{model}{A list; the body reads \code{$context}, \code{$phis}, \code{$psis}, \code{$repr_dim}, \code{$rho} from it.}
\item{sets}{A vector; its length is taken.}
\item{cache}{A list; the body reads \code{$caches}, \code{$racts}, \code{$rpre} from it.}
\item{dout}{Passed to \code{.propinf_mlp_backward}.}
\item{grads}{A list; the body reads \code{$phis}, \code{$psis}, \code{$rho} from it.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}. See the file header for the source the module follows.
}
\value{
The value of \code{for}.
}
\keyword{internal}
