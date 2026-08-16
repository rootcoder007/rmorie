% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_mlp_forward}
\alias{.propinf_mlp_forward}
\title{.propinf_mlp_forward}
\usage{
.propinf_mlp_forward(net, x, final = "relu", hidden_act = "relu")
}
\arguments{
\item{net}{A vector; its length is taken and its elements indexed.}
\item{x}{See Usage.}
\item{final}{One of \code{"linear"}, \code{"sigmoid"}, \code{"tanh"}. Defaults to \code{"relu"}.}
\item{hidden_act}{Compared against \code{"tanh"}. Defaults to \code{"relu"}.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_deepsets_forward}, \code{.propinf_train_vector_meta}, \code{.propinf_vector_meta_predict}. See the file header for the source the module follows.
}
\value{
A list with \code{acts}, \code{pre}.
}
\keyword{internal}
