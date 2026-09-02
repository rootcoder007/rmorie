% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdfKurt}
\alias{PdfKurt}
\title{Eq (3.5): K = (1/sigma^4) integral (eta - mu)^4 p(eta) d eta.  The}
\usage{
PdfKurt(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) 
}
\arguments{
\item{pdf}{Passed to \code{.morie_rg_pdfint}.}
\item{x}{Passed to \code{.morie_rg_pdfint}.}
\item{lower}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{-Inf}.}
\item{upper}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{Inf}.}
}
\description{
book gives 3 for a Gaussian and defines the excess K\' = K - 3.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
xg <- seq(-6, 6, by = 0.01)
PdfKurt(x = xg, p = dnorm(xg))
}
