% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_deepsets_forward}
\alias{.propinf_deepsets_forward}
\title{.propinf_deepsets_forward}
\usage{
.propinf_deepsets_forward(model, sets)
}
\arguments{
\item{model}{A list; the body reads \code{$context}, \code{$phis}, \code{$psis}, \code{$repr_dim}, \code{$rho}, \code{$scalers} from it.}
\item{sets}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}, \code{morie_propinf_property_inference}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
