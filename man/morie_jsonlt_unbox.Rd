% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_unbox}
\alias{morie_jsonlt_unbox}
\title{Mark a value as a JSON scalar}
\usage{
morie_jsonlt_unbox(x)
}
\arguments{
\item{x}{a length-one value.}
}
\description{
R has no scalars, so a length-one vector is ambiguous. This resolves it for one value without turning auto_unbox on for the whole document.
}
\value{
x, tagged so it is never written as an array.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
