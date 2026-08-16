% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_train_set_meta}
\alias{.propinf_train_set_meta}
\title{.propinf_train_set_meta}
\usage{
.propinf_train_set_meta(sets_list, labels, phi_hidden, repr_dim, rho_hidden, epochs, lr, seed, context = "paired")
}
\arguments{
\item{sets_list}{A vector; its length is taken and its elements indexed.}
\item{labels}{A vector; indexed elementwise.}
\item{phi_hidden}{Passed to \code{.propinf_deepsets_init}.}
\item{repr_dim}{Passed to \code{.propinf_deepsets_init}.}
\item{rho_hidden}{Passed to \code{.propinf_deepsets_init}.}
\item{epochs}{Coerced to integer by the body, with \code{as.integer}.}
\item{lr}{Passed to \code{.propinf_sgd_step}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}.}
\item{context}{Passed to \code{.propinf_deepsets_init}. Defaults to \code{"paired"}.}
}
\description{
A step of the propinf_native implementation. Called by \code{morie_propinf_property_inference}. See the file header for the source the module follows.
}
\value{
The value of \code{model}, as built in the body.
}
\keyword{internal}
