% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rgemgf_r2}
\alias{.rgemgf_r2}
\title{Equation (5.28), p. 292, written exactly as the book prints it}
\usage{
.rgemgf_r2(x, y)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{y}{Numeric; passed to \code{sum}.}
}
\description{
A step of the rgemgf implementation. Called by \code{rgemgf}, \code{rgemgfd}, \code{rgisint}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rgemgf_r2(V, V)
}
