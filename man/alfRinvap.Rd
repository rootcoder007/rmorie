% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfRinvap}
\alias{alfRinvap}
\title{T^-1 o x, without materialising the inverse frame}
\usage{
alfRinvap(Tf, x)
}
\arguments{
\item{Tf}{A list; the body reads \code{$R}, \code{$t} from it.}
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the alf_core implementation. Called by \code{Alffape}, \code{Alfipa}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
