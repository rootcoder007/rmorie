% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_unser}
\alias{.jsonlt_unser}
\title{.jsonlt_unser}
\usage{
.jsonlt_unser(node)
}
\arguments{
\item{node}{A list; the body reads \code{$attributes}, \code{$type}, \code{$value} from it.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{morie_jsonlt_unserialize}. See the file header for the source the module follows.
}
\value{
The value of \code{vec}, as built in the body.
}
\keyword{internal}
