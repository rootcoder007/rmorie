% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lcg_u}
\alias{.morie_gr_lcg_u}
\title{Integer LCG shared by every reproducible morie.fn module:}
\usage{
.morie_gr_lcg_u(count, seed)
}
\arguments{
\item{count}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{See Usage.}
}
\description{
s <- (1664525 s + 1013904223) mod 2^32 `.morie_gr_lcg_u` returns the (s + 0.5)/2^32 uniform stream, `.morie_gr_lcg_w` the symmetric weight stream 2u - 1 scaled.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
