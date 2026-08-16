% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_mlp_init}
\alias{.propinf_mlp_init}
\title{.propinf_mlp_init}
\usage{
.propinf_mlp_init(sizes, rnd)
}
\arguments{
\item{sizes}{A vector; its length is taken and its elements indexed.}
\item{rnd}{Passed to \code{.propinf_normal_lcg}.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_deepsets_init}, \code{.propinf_train_vector_meta}. See the file header for the source the module follows.
}
\value{
The value of \code{net}, as built in the body.
}
\keyword{internal}
