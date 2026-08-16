% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.remlfn_ranova}
\alias{.remlfn_ranova}
\title{.remlfn_ranova}
\usage{
.remlfn_ranova(y, group)
}
\arguments{
\item{y}{Numeric; passed to \code{mean}.}
\item{group}{Passed to \code{.remlfn_groups}.}
}
\description{
A step of the remlfn_native implementation. Called by \code{morie_remlfn}. See the file header for the source the module follows.
}
\value{
A list with \code{msa}, \code{mse}, \code{sigma2_a_raw}, \code{sigma2_a}, \code{balanced}, \code{dfa}, \code{dfe}, \code{ssa}, \code{sse}.
}
\keyword{internal}
