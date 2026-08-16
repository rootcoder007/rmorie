% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{quantize}
\alias{quantize}
\title{quantize}
\usage{
quantize(x, bins)
}
\arguments{
\item{x}{Passed to \code{.chronos_vec}.}
\item{bins}{A list; the body reads \code{$centers}, \code{$edges} from it.}
}
\description{
A step of the chronos_native implementation. Called by \code{tokenize}. See the file header for the source the module follows.
}
\value{
A list with \code{tokens}, \code{n_clipped}, \code{clipped_fraction}, \code{in_range}, \code{note}.
}
\keyword{internal}
