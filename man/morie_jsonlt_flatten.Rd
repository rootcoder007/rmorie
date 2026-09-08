% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_flatten}
\alias{morie_jsonlt_flatten}
\title{Expand nested data.frame columns}
\usage{
morie_jsonlt_flatten(x, recursive = TRUE) 
}
\arguments{

\item{recursive}{expand nested frames all the way down.}

\item{x}{Argument \code{x}; see Usage.}
}
\description{
A data.frame column that is itself a data.frame becomes columns named outer.inner.
}
\value{
A data.frame with no data.frame columns.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
\examples{
df <- morie_jsonlt_from_json('[{"a":{"b":1,"c":{"d":2}}},{"a":{"b":3,"c":{"d":4}}}]')
morie_jsonlt_flatten(df)
}
