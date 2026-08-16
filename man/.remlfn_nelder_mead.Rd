% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.remlfn_nelder_mead}
\alias{.remlfn_nelder_mead}
\title{.remlfn_nelder_mead}
\usage{
.remlfn_nelder_mead(fn, x0, xatol = 1e-10, fatol = 1e-10, maxiter = 5000)
}
\arguments{
\item{fn}{See Usage.}
\item{x0}{A vector; its length is taken and its elements indexed.}
\item{xatol}{Defaults to \code{1e-10}.}
\item{fatol}{Defaults to \code{1e-10}.}
\item{maxiter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{5000}.}
}
\description{
A step of the remlfn_native implementation. Called by \code{morie_remlfn}. See the file header for the source the module follows.
}
\value{
A list with \code{x}, \code{fun}, \code{nit}, \code{success}.
}
\keyword{internal}
