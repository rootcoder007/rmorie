% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_sae_train_layer}
\alias{.morie_w4d_sae_train_layer}
\title{.morie_w4d_sae_train_layer}
\usage{
.morie_w4d_sae_train_layer(H, k, epochs, lr, seed)
}
\arguments{
\item{H}{A matrix; passed to \code{nrow}.}
\item{k}{A count; the body uses it as \code{numeric(...)}.}
\item{epochs}{A count; the body uses it as \code{seq_len(...)}.}
\item{lr}{Numeric; combined arithmetically in the body.}
\item{seed}{Passed to \code{.morie_w4d_lcg_mat}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_stacked_autoencoder_modules}. See the file header for the source the module follows.
}
\value{
A list with \code{W}, \code{b}, \code{c}, \code{losses}.
}
\keyword{internal}
