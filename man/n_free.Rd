% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{n_free}
\alias{n_free}
\title{n_free}
\usage{
n_free(spec)
}
\arguments{
\item{spec}{A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}. See the file header for the source the module follows.
}
\value{
The value of \code{n}, as built in the body.
}
\keyword{internal}
\examples{
spec <- BatsSpec(periods = c(4), use_trend = TRUE)
n_free(spec)
}
