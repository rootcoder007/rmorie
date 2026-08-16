% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_pdfint}
\alias{.morie_rg_pdfint}
\title{.morie_rg_pdfint}
\usage{
.morie_rg_pdfint(f, pdf = NULL, x = NULL, lower = -Inf, upper = Inf)
}
\arguments{
\item{f}{Accepted by the signature and not used anywhere in the body.}
\item{pdf}{A function; the body checks with \code{is.function}.}
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{lower}{Defaults to \code{-Inf}.}
\item{upper}{Defaults to \code{Inf}.}
}
\description{
A step of the rangayyan_ch3 implementation. Called by \code{DiffEnt}, \code{PdfKurt}, \code{PdfMean} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_rg_quad}.
}
\keyword{internal}
