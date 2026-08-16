% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_kc_gram}
\alias{.mor_kc_gram}
\title{.mor_kc_gram}
\usage{
.mor_kc_gram(z, kernel, bandwidth)
}
\arguments{
\item{z}{A matrix; passed to \code{nrow}.}
\item{kernel}{One of \code{"gaussian"}, \code{"linear"}.}
\item{bandwidth}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the kcusum_native implementation. Called by \code{morie_kcusum}. See the file header for the source the module follows.
}
\value{
A list with \code{K}, \code{bw}.
}
\keyword{internal}
