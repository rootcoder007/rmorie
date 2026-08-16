% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_al_softmax_rows}
\alias{.morie_al_softmax_rows}
\title{.morie_al_softmax_rows}
\usage{
.morie_al_softmax_rows(z)
}
\arguments{
\item{z}{Numeric; passed to \code{exp}.}
}
\description{
A step of the alammar_llm_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_encoder_block}, \code{.morie_gr_softmax_cols} and 13 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_al_softmax_rows(M)
}
