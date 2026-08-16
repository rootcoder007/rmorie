% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_unwrap}
\alias{.morie_rg_unwrap}
\title{.morie_rg_unwrap}
\usage{
.morie_rg_unwrap(phase)
}
\arguments{
\item{phase}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rangayyan_cep implementation. Called by \code{CCepstrum}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_unwrap(V)
}
