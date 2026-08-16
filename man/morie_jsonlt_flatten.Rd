% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt_flatten}
\alias{morie_jsonlt_flatten}
\title{Expand nested data.frame columns}
\usage{
morie_jsonlt_flatten(df, recursive = TRUE)
}
\arguments{
\item{df}{a data.frame.}
\item{recursive}{expand nested frames all the way down.}
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
