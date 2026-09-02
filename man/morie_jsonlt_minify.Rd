% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_minify}
\alias{morie_jsonlt_minify}
\title{Strip whitespace from JSON text}
\usage{
morie_jsonlt_minify(txt) 
}
\arguments{
\item{txt}{JSON text.}
}
\description{
Removes every byte of whitespace that sits outside a string.
}
\value{
The same JSON with no whitespace outside strings.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
\examples{
morie_jsonlt_minify('{ "a" : [ 1 , 2 ] }')
}
