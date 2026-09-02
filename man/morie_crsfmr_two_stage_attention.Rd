% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_two_stage_attention}
\alias{morie_crsfmr_two_stage_attention}
\title{Full Two-Stage Attention layer}
\usage{
morie_crsfmr_two_stage_attention(Z, n_router = NULL, router = NULL) 
}
\arguments{
\item{Z}{A length-L list of length-D lists of d_model vectors.}
\item{n_router}{Integer, number of router vectors.}
\item{router}{Optional list of router vectors.}
}
\description{
Cross-time then cross-dimension.
}
\value{
A list with \code{output}, \code{cross_time} and the   operation counts.
}
\keyword{internal}
\examples{
morie_crsfmr_two_stage_attention(Z = 5L)
}
