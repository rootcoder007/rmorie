% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdfVar}
\alias{PdfVar}
\title{Eq (3.3): sigma^2 = integral (eta - mu)^2 p(eta) d eta.  CV =}
\usage{
PdfVar(pdf = NULL, x = NULL, lower = -Inf, upper = Inf)
}
\arguments{
\item{pdf}{Passed to \code{.morie_rg_pdfint}.}
\item{x}{Passed to \code{.morie_rg_pdfint}.}
\item{lower}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{-Inf}.}
\item{upper}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{Inf}.}
}
\description{
sigma/mu is left NULL once mu is negligible against sigma; the book warns it diverges as mu -> 0.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
unif01 <- function(v) if (v >= 0 && v <= 1) 1 else 0
UGRID <- seq(0, 1, length.out = 4001)
PdfVar(unif01, x = UGRID)
}
