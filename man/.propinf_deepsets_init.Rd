% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_deepsets_init}
\alias{.propinf_deepsets_init}
\title{.propinf_deepsets_init}
\usage{
.propinf_deepsets_init(shapes, phi_hidden, repr_dim, rho_hidden, rnd, context = "paired", edge_hidden = NULL)
}
\arguments{
\item{shapes}{A vector; its length is taken and its elements indexed.}
\item{phi_hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{repr_dim}{Numeric; combined arithmetically in the body.}
\item{rho_hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{rnd}{Passed to \code{.propinf_mlp_init}.}
\item{context}{One of \code{"none"}, \code{"paired"}. Defaults to \code{"paired"}.}
\item{edge_hidden}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the propinf_native implementation. Called by \code{.propinf_train_set_meta}. See the file header for the source the module follows.
}
\value{
A list with \code{phis}, \code{psis}, \code{rho}, \code{repr_dim}, \code{shapes}, \code{context}, \code{scalers}.
}
\keyword{internal}
