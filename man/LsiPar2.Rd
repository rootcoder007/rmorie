% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiPar2}
\alias{LsiPar2}
\title{Eq (3.47): identical in form to eq (3.46) -- both branches of a}
\usage{
LsiPar2(x, h2)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h2}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
parallel structure see the same input.
}
\value{
A list with \code{s2}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
LsiPar2(V, V)
}
