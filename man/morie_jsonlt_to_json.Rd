% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_to_json}
\alias{morie_jsonlt_to_json}
\title{Serialise an R object to JSON}
\usage{
morie_jsonlt_to_json(x, pretty = FALSE, ...)
}
\arguments{
\item{x}{object to serialise.}
\item{pretty}{FALSE, TRUE, or an indent width.}
\item{...}{toJSON options: dataframe (rows, columns, values), matrix (rowmajor, columnmajor), Date (ISO8601, epoch), POSIXt (string, ISO8601, epoch, mongo), factor (string, integer), complex (string, list), raw (base64, hex, int, mongo), null (list, null), na (NULL, null, string), auto_unbox, digits, force.}
}
\description{
The jsonlite toJSON mapping, natively. Every documented option, same defaults.
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
