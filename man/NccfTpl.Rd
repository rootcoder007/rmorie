% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{NccfTpl}
\alias{NccfTpl}
\title{Normalized per shift, so a LOUD stretch cannot outscore a matching}
\usage{
NccfTpl(x, template) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{template}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
one -- that is the whole reason to normalize rather than take the raw CCF.
}
\value{
A list with \code{gamma}, \code{shifts}, \code{peak}, \code{peak_shift}, \code{normalized_per_shift}, \code{bounded_in_unit_interval}, \code{loud_beats_matching_without_normalization}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
NccfTpl(V, V)
}
