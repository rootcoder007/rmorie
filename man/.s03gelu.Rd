% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03gelu}
\alias{.s03gelu}
\title{Exact GELU, x * Phi(x) (Hendrycks and Gimpel 2016)}
\usage{
.s03gelu(z)
}
\arguments{
\item{z}{Numeric; combined arithmetically in the body.}
}
\description{
erf(z/sqrt(2)) = 2 pnorm(z) - 1, so 0.5 z (1 + erf(z/sqrt 2)) = z Phi(z).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03gelu(V)
}
