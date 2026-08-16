% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{split_sample}
\alias{split_sample}
\title{split_sample}
\usage{
split_sample(n, V = 10L, seed = 0L)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{V}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{10L}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
}
\description{
A step of the tldapar_native implementation. Called by \code{data_adaptive_parameter}, \code{morie_tldapar}, \code{variable_importance}. See the file header for the source the module follows.
}
\value{
A list with \code{estimation}, \code{training}, \code{V}.
}
\keyword{internal}
\examples{
str(split_sample(30, V = 5), max.level = 1)
}
