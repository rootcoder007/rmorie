% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.htpinvspd}
\alias{.htpinvspd}
\title{.htpinvspd}
\usage{
.htpinvspd(A, ridge)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the htpfn implementation. Called by \code{Htpfn}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.htpinvspd(A = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), ridge = 5L)
}
