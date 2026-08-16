% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_cell}
\alias{.jsonlt_cell}
\title{One data.frame cell. NULL means "leave this field out", which is what}
\usage{
.jsonlt_cell(col, i, o)
}
\arguments{
\item{col}{A matrix; indexed by row and column.}
\item{i}{See Usage.}
\item{o}{A list; the body reads \code{$factor}, \code{$na} from it.}
}
\description{
jsonlite does for NA inside rows when na is unset.
}
\value{
The value of \code{.jsonlt_scalar}.
}
\keyword{internal}
