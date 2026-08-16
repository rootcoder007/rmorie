% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_mlp_backward}
\alias{.propinf_mlp_backward}
\title{.propinf_mlp_backward}
\usage{
.propinf_mlp_backward(net, acts, pre, dout, grads, final = "relu", hidden_act = "relu")
}
\arguments{
\item{net}{See Usage.}
\item{acts}{See Usage.}
\item{pre}{See Usage.}
\item{dout}{See Usage.}
\item{grads}{See Usage.}
\item{final}{Defaults to \code{"relu"}.}
\item{hidden_act}{Defaults to \code{"relu"}.}
}
\description{
Part of the propinf_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{delta}, as built in the body.
}
\keyword{internal}
