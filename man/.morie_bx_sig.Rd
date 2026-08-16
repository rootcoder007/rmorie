% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_sig}
\alias{.morie_bx_sig}
\title{Logistic node function, eq (10.81), saturated rather than overflowing}
\usage{
.morie_bx_sig(b)
}
\arguments{
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{Infomax}, \code{Lstm}, \code{MlpBp}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
