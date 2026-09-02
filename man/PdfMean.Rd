% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdfMean}
\alias{PdfMean}
\title{Eq (3.1): mu = E[eta] = integral eta p(eta) d eta}
\usage{
PdfMean(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) 
}
\arguments{
\item{pdf}{Passed to \code{.morie_rg_pdfint}.}
\item{x}{Passed to \code{.morie_rg_pdfint}.}
\item{lower}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{-Inf}.}
\item{upper}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{Inf}.}
}
\description{
A step of the rangayyan_ch3 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
PdfMean(x = seq(-4, 4, by = 0.01), p = dnorm(seq(-4, 4, by = 0.01)))
}
