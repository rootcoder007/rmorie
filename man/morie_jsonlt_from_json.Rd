% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_from_json}
\alias{morie_jsonlt_from_json}
\title{Parse JSON into R objects}
\usage{
morie_jsonlt_from_json(txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE, flatten = FALSE, simplify = NULL, ...)
}
\arguments{
\item{txt}{JSON text, or a path to a file holding it.}
\item{simplifyVector}{collapse a flat array to an atomic vector.}
\item{simplifyDataFrame}{collapse an array of objects to a data.frame.}
\item{simplifyMatrix}{collapse an array of equal-length arrays to a matrix.}
\item{flatten}{expand nested data.frame columns to outer.inner columns.}
\item{simplify}{FALSE turns all three simplifications off.}
\item{...}{ignored, for call compatibility.}
}
\description{
The jsonlite fromJSON mapping, with the three simplification rules as separate switches.
}
\value{
An R object.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
\examples{
morie_jsonlt_from_json(txt = 5L)
}
