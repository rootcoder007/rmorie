% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdfMS}
\alias{PdfMS}
\title{Eq (3.2): E[eta^2].  Equal to the variance only when mu = 0, so both}
\usage{
PdfMS(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) 
}
\arguments{
\item{pdf}{Passed to \code{.morie_rg_pdfint}.}
\item{x}{Passed to \code{.morie_rg_pdfint}.}
\item{lower}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{-Inf}.}
\item{upper}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{Inf}.}
}
\description{
are returned rather than one being assumed for the other.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
PdfMS(x = seq(-4, 4, by = 0.01), p = dnorm(seq(-4, 4, by = 0.01)))
}
