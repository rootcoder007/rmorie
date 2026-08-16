% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_simplify}
\alias{.jsonlt_simplify}
\title{.jsonlt_simplify}
\usage{
.jsonlt_simplify(x, sv, sdf, sm, flat)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{sv}{A flag; the body branches on it.}
\item{sdf}{A flag; the body branches on it.}
\item{sm}{A flag; the body branches on it.}
\item{flat}{A flag; the body branches on it.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{morie_jsonlt_from_json}. See the file header for the source the module follows.
}
\value{
The value of \code{kids}, as built in the body.
}
\keyword{internal}
