% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_idft_re}
\alias{.morie_rg_idft_re}
\title{.morie_rg_idft_re}
\usage{
.morie_rg_idft_re(re, im)
}
\arguments{
\item{re}{A vector; its length is taken.}
\item{im}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_cep implementation. Called by \code{CCepstrum}, \code{Cepstrum}, \code{HomDeconv} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_idft_re(V, V)
}
