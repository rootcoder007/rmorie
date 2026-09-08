% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_chol_lower}
\alias{morie_chol_lower}
\title{morie_chol_lower}
\usage{
morie_chol_lower(G) 
}
\description{
morie_chol_lower
}
\arguments{
\item{G}{Argument \code{G}; see Usage.}
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
\examples{
n <- 12
A <- matrix(rnorm(n * 6), nrow = n)
G <- rmorie:::morie_grm(A) + diag(0.3, n)
rmorie:::morie_chol_lower(G)
}
