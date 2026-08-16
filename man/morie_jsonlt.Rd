% Generated from the module source by ledger/wave3/gen_rd_jsonlt.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_jsonlt}
\alias{morie_jsonlt}
\title{The jsonlite JSON mapping, natively}
\usage{
morie_jsonlt(x = NULL, route = "to_json", ...)
}
\arguments{
\item{x}{the value or JSON text the route acts on.}
\item{route}{one of to_json, from_json, prettify, minify, flatten, serialize, unserialize.}
\item{...}{options for the chosen route.}
}
\description{
One entry point over every route, so the module can be driven the same way from either language arm.
}
\value{
A list with components route, result and method.
}
\references{
Ooms, J. (2014) "The jsonlite Package: A Practical and Consistent
  Mapping Between JSON Data and R Objects." arXiv:1403.2805.

Bray, T. (ed.) (2017) "The JavaScript Object Notation (JSON) Data
  Interchange Format." RFC 8259.
}
\keyword{internal}
