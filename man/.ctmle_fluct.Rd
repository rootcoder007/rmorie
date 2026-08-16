% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ctmle_fluct}
\alias{.ctmle_fluct}
\title{.ctmle_fluct}
\usage{
.ctmle_fluct(Q, H, Y, iters = 60L)
}
\arguments{
\item{Q}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{H}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
}
\description{
A step of the tlctmle_native implementation. Called by \code{ctmle}. See the file header for the source the module follows.
}
\value{
A list with \code{epsilon}, \code{q}.
}
\keyword{internal}
