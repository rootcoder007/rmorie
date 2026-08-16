% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lcg_normals}
\alias{.morie_gr_lcg_normals}
\title{Box-Muller normals off the reference LCG, cos then sin per pair}
\usage{
.morie_gr_lcg_normals(count, seed, clamp = FALSE)
}
\arguments{
\item{count}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{clamp}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_ddpm_forward_process}, \code{morie_geron_ddpm_reverse_step}, \code{morie_geron_gaussian_random_projection} and 2 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
