% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w4a_lcg_u}
\alias{.w4a_lcg_u}
\title{.w4a_lcg_u}
\usage{
.w4a_lcg_u(n, seed)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{See Usage.}
}
\description{
A step of the geron_w4a_native implementation. Called by \code{morie_geron_dataloader}, \code{morie_geron_dropout_alt}, \code{morie_lcg_normal}. See the file header for the source the module follows.
}
\value{
The value of \code{u}, as built in the body.
}
\keyword{internal}
