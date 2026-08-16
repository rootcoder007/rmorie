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
\item{net}{A vector; its length is taken and its elements indexed.}
\item{acts}{A vector; indexed elementwise.}
\item{pre}{A vector; indexed elementwise.}
\item{dout}{See Usage.}
\item{grads}{A vector; indexed elementwise.}
\item{final}{One of \code{"linear"}, \code{"sigmoid"}, \code{"tanh"}. Defaults to \code{"relu"}.}
\item{hidden_act}{Compared against \code{"tanh"}. Defaults to \code{"relu"}.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_deepsets_backward}, \code{.propinf_train_vector_meta}. See the file header for the source the module follows.
}
\value{
The value of \code{delta}, as built in the body.
}
\keyword{internal}
