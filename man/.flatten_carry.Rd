% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flatten_carry}
\alias{.flatten_carry}
\title{.flatten_carry}
\usage{
.flatten_carry(spec, carry)
}
\arguments{
\item{spec}{A list; the body reads \code{$harmonics}, \code{$periods}, \code{$use_trend} from it.}
\item{carry}{A list; the body reads \code{$buf}, \code{$dlag}, \code{$elag}, \code{$level}, \code{$s}, \code{$sstar}, \code{$trend} from it.}
}
\description{
A step of the bats_native implementation. Called by \code{state_matrices}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
