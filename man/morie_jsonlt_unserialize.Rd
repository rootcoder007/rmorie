% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_unserialize}
\alias{morie_jsonlt_unserialize}
\title{Restore an object written by morie_jsonlt_serialize}
\usage{
morie_jsonlt_unserialize(txt) 
}
\arguments{
\item{txt}{JSON produced by morie_jsonlt_serialize.}
}
\description{
The inverse of morie_jsonlt_serialize.
}
\value{
The original R object.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
