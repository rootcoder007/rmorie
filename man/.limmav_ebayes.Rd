% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_ebayes}
\alias{.limmav_ebayes}
\title{.limmav_ebayes}
\usage{
.limmav_ebayes(sigma2, df, robust_floor = 1e-12)
}
\arguments{
\item{sigma2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{df}{A vector; its length is taken.}
\item{robust_floor}{Passed to \code{>}. Defaults to \code{1e-12}.}
}
\description{
A step of the limmav_native implementation. Called by \code{morie_limmav}. See the file header for the source the module follows.
}
\value{
A list with \code{d0}, \code{s0_sq}, \code{s2_post}, \code{df_total}, \code{no_gene_variation}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.limmav_ebayes(V, V)
}
