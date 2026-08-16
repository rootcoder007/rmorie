% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ci_bca}
\alias{.morie_ci_bca}
\title{.morie_ci_bca}
\usage{
.morie_ci_bca(boot_obj, index, conf)
}
\arguments{
\item{boot_obj}{A list; the body reads \code{$t}, \code{$t0} from it.}
\item{index}{See Usage.}
\item{conf}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the boot_native implementation. Called by \code{morie_boot_ci}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_norm_inter}.
}
\keyword{internal}
