% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfRcomp}
\alias{alfRcomp}
\title{Frame composition A o B, i.e. (A o B) o x = A o (B o x)}
\usage{
alfRcomp(A, B)
}
\arguments{
\item{A}{A list; the body reads \code{$R} from it.}
\item{B}{A list; the body reads \code{$R}, \code{$t} from it.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfbkb}, \code{Alfschn}. See the file header for the source the module follows.
}
\value{
A list with \code{R}, \code{t}.
}
\keyword{internal}
