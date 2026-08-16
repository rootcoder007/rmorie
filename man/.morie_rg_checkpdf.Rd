% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_checkpdf}
\alias{.morie_rg_checkpdf}
\title{.morie_rg_checkpdf}
\usage{
.morie_rg_checkpdf(mass, tol = 1e-06)
}
\arguments{
\item{mass}{Numeric; combined arithmetically in the body.}
\item{tol}{Defaults to \code{1e-06}.}
}
\description{
A step of the rangayyan_ch3 implementation. Called by \code{DiffEnt}, \code{PdfKurt}, \code{PdfMean} and 3 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{pdf_mass}, \code{pdf_mass_ok}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_checkpdf(V)
}
