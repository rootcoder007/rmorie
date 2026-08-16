% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cdae_act}
\alias{.cdae_act}
\title{.cdae_act}
\usage{
.cdae_act(name, x)
}
\arguments{
\item{name}{One of \code{"identity"}, \code{"sigmoid"}, \code{"tanh"}.}
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the cdaeRC_native implementation. Called by \code{decode}, \code{encode}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
