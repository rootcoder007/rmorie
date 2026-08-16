% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.btv_boot_reps}
\alias{.btv_boot_reps}
\title{.btv_boot_reps}
\usage{
.btv_boot_reps(x, stat, B, seed)
}
\arguments{
\item{x}{A matrix; indexed by row and column.}
\item{stat}{Accepted by the signature and not used anywhere in the body.}
\item{B}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{set.seed}.}
}
\description{
A step of the btvol_native implementation. Called by \code{morie_bt_iid}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
