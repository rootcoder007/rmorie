% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_boot_freq}
\alias{.morie_boot_freq}
\title{Boot:::freq.array -- R x n resample frequency counts}
\usage{
.morie_boot_freq(boot_obj)
}
\arguments{
\item{boot_obj}{A list; the body reads \code{$index} from it.}
}
\description{
A step of the boot_native implementation. Called by \code{.morie_empinf_reg}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
