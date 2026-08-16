% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_lcg_vec}
\alias{.morie_w4d_lcg_vec}
\title{.morie_w4d_lcg_vec}
\usage{
.morie_w4d_lcg_vec(n, seed, scale = 0.5)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_senet}, \code{morie_geron_swin}, \code{morie_geron_transformer} and 5 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
