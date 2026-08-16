% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_fp_morgan}
\alias{.mor_fp_morgan}
\title{.mor_fp_morgan}
\usage{
.mor_fp_morgan(a, bd, invariants, radius, nbits, use_bond_order = TRUE)
}
\arguments{
\item{a}{A count; the body uses it as \code{seq_len(...)}.}
\item{bd}{A list; the body reads \code{$i}, \code{$j}, \code{$o} from it.}
\item{invariants}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{radius}{Coerced to integer by the body, with \code{as.integer}.}
\item{nbits}{A count; the body uses it as \code{integer(...)}.}
\item{use_bond_order}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the ecfp4_native implementation. Called by \code{morie_ecfp4}, \code{morie_ecfp6}, \code{morie_fcfp4}. See the file header for the source the module follows.
}
\value{
A list with \code{bits}, \code{count}, \code{ident}.
}
\keyword{internal}
