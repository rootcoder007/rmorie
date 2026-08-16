% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wsm_boot_reps}
\alias{.wsm_boot_reps}
\title{.wsm_boot_reps}
\usage{
.wsm_boot_reps(data, statistic, B, seed)
}
\arguments{
\item{data}{A matrix; the body checks with \code{is.matrix}.}
\item{statistic}{See Usage.}
\item{B}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{See Usage.}
}
\description{
A step of the wsm_native implementation. Called by \code{morie_wsm_bootstrap}, \code{morie_wsm_plug_in}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
