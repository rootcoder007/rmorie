% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_train_vector_meta}
\alias{.propinf_train_vector_meta}
\title{.propinf_train_vector_meta}
\usage{
.propinf_train_vector_meta(feats, labels, hidden, epochs, lr, seed)
}
\arguments{
\item{feats}{A vector; its length is taken and its elements indexed.}
\item{labels}{A vector; indexed elementwise.}
\item{hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{epochs}{Coerced to integer by the body, with \code{as.integer}.}
\item{lr}{Passed to \code{.propinf_sgd_step}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}. See the file header for the source the module follows.
}
\value{
The value of \code{net}, as built in the body.
}
\keyword{internal}
