% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PdfSkew}
\alias{PdfSkew}
\title{Eq (3.4): S = (1/sigma^3) integral (eta - mu)^3 p(eta) d eta}
\usage{
PdfSkew(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) 
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
xg <- seq(0, 20, by = 0.01)
PdfSkew(x = xg, p = dgamma(xg, shape = 2))
}
