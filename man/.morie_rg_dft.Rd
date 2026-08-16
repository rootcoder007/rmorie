% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_dft}
\alias{.morie_rg_dft}
\title{.morie_rg_dft}
\usage{
.morie_rg_dft(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
A step of the rangayyan_cep implementation. Called by \code{CardioResp}, \code{CCepstrum}, \code{Cepstrum} and 11 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{re}, \code{im}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_dft(V)
}
