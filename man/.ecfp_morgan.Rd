% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ecfp_morgan}
\alias{.ecfp_morgan}
\title{.ecfp_morgan}
\usage{
.ecfp_morgan(B, invariants, radius, nbits, use_bond_order = TRUE)
}
\arguments{
\item{B}{A list; the body reads \code{$a}, \code{$i}, \code{$j}, \code{$o} from it.}
\item{invariants}{See Usage.}
\item{radius}{A count; the body uses it as \code{seq_len(...)}.}
\item{nbits}{A count; the body uses it as \code{integer(...)}.}
\item{use_bond_order}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the ecfp4 implementation. Called by \code{Ecfp4}, \code{Ecfp6}, \code{Fcfp4}. See the file header for the source the module follows.
}
\value{
A list with \code{bits}, \code{count}, \code{ident}.
}
\keyword{internal}
