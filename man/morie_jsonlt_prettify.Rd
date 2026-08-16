% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_prettify}
\alias{morie_jsonlt_prettify}
\title{Indent JSON text}
\usage{
morie_jsonlt_prettify(txt, indent = 4L)
}
\arguments{
\item{txt}{JSON text.}
\item{indent}{spaces per level.}
}
\description{
Structure only: a comma inside a string stays a comma, which is why this walks characters rather than running a regular expression over the text.
}
\value{
The same JSON, indented.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
