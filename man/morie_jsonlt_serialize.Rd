% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_serialize}
\alias{morie_jsonlt_serialize}
\title{Serialise an R object losslessly}
\usage{
morie_jsonlt_serialize(x, pretty = FALSE)
}
\arguments{
\item{x}{object to serialise.}
\item{pretty}{indent the output.}
}
\description{
Type and attributes travel with the value, so the round trip returns the same object rather than something that merely prints the same.
}
\value{
A length-one character vector of JSON.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
