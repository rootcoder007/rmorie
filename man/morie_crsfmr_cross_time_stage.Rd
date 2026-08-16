% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_cross_time_stage}
\alias{morie_crsfmr_cross_time_stage}
\title{Cross-time stage (equation 3)}
\usage{
morie_crsfmr_cross_time_stage(Z)
}
\arguments{
\item{Z}{A length-L list of length-D lists of d_model vectors.}
}
\description{
Multi-head self-attention within each dimension, weights shared.
}
\value{
A 3D array indexed as \code{out[[i]][[d]]}.
}
\keyword{internal}
\examples{
morie_crsfmr_cross_time_stage(Z = 5L)
}
