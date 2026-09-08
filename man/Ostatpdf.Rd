% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ostatpdf}
\alias{Ostatpdf}
\title{PDF of the r-th order statistic -- Theorem 2.4.2, eq. (2.4.2), p. 37}
\usage{
Ostatpdf(x, r, n, cdf, pdf) 
}
\description{
PDF of the r-th order statistic -- Theorem 2.4.2, eq. (2.4.2), p. 37
}
\arguments{
\item{x}{Argument \code{x}; see Usage.}
\item{r}{Argument \code{r}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
\item{cdf}{Argument \code{cdf}; see Usage.}
\item{pdf}{Argument \code{pdf}; see Usage.}
}
\value{
A list with \code{pdf}, \code{coef}, \code{fx}, \code{dx}, \code{r}, \code{n}.
}
\keyword{internal}
\examples{
rmorie:::Ostatpdf(x = c(1, 2, 3, 4, 5, 6, 7, 8), r = 5L, n = 5L,
  cdf = c(1, 2, 3, 4, 5, 6, 7, 8), pdf = c(1, 2, 3, 4, 5, 6, 7, 8))
}
