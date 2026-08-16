% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_sgd_step}
\alias{.propinf_sgd_step}
\title{.propinf_sgd_step}
\usage{
.propinf_sgd_step(net, grads, lr, scale)
}
\arguments{
\item{net}{A vector; its length is taken and its elements indexed.}
\item{grads}{A vector; indexed elementwise.}
\item{lr}{Numeric; combined arithmetically in the body.}
\item{scale}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}, \code{.propinf_train_vector_meta}. See the file header for the source the module follows.
}
\value{
The value of \code{net}, as built in the body.
}
\keyword{internal}
