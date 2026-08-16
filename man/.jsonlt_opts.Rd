% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_opts}
\alias{.jsonlt_opts}
\title{.jsonlt_opts}
\usage{
.jsonlt_opts(dataframe = "rows", matrix = "rowmajor", Date = "ISO8601", POSIXt = "string", factor = "string", complex = "string", raw = "base64", null = "list", na = NULL, auto_unbox = FALSE, digits = 4, force = FALSE)
}
\arguments{
\item{dataframe}{Defaults to \code{"rows"}.}
\item{matrix}{Defaults to \code{"rowmajor"}.}
\item{Date}{Defaults to \code{"ISO8601"}.}
\item{POSIXt}{Defaults to \code{"string"}.}
\item{factor}{Defaults to \code{"string"}.}
\item{complex}{Defaults to \code{"string"}.}
\item{raw}{Defaults to \code{"base64"}.}
\item{null}{Defaults to \code{"list"}.}
\item{na}{Defaults to \code{NULL}.}
\item{auto_unbox}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{digits}{Defaults to \code{4}.}
\item{force}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_ser_opts}, \code{morie_jsonlt_to_json}. See the file header for the source the module follows.
}
\value{
The value of \code{o}, as built in the body.
}
\keyword{internal}
