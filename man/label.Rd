% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{label}
\alias{label}
\title{label}
\usage{
label(spec)
}
\arguments{
\item{spec}{A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox} from it.}
}
\description{
A step of the bats_native implementation. Called by \code{morie_bats}, \code{morie_rmrl}, \code{morie_rmrl_qlearn_flat}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
spec <- BatsSpec(periods = c(4), use_trend = TRUE)
label(spec)
}
