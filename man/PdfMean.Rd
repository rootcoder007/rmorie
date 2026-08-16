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
\item{pdf}{Defaults to \code{NULL}.}
\item{x}{Defaults to \code{NULL}.}
\item{lower}{Defaults to \code{-Inf}.}
\item{upper}{Defaults to \code{Inf}.}
}
\description{
Part of the rangayyan_ch3 implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
