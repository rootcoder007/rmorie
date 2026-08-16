% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LisaClust}
\alias{LisaClust}
\title{Local Moran eq (1.17) with EXACT conditional-randomization moments}
\usage{
LisaClust(x, w, alpha = 0.05)
}
\arguments{
\item{x}{Passed to \code{.morie_spx_chkv}.}
\item{w}{Passed to \code{.morie_spx_chkw}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
}
\description{
(simple random sampling without replacement of the other n-1 deviations into the neighbour slots).  The HH/LL/HL/LH labels are Anselin (1996)\'s Moran scatterplot, NOT in Schabenberger & Gotway; a fixed-string search of the book for "quadrant" and "Moran scatter" finds only an unrelated kriging search neighbourhood.
}
\value{
A list with \code{labels}, \code{local}, \code{z}, \code{p_value}, \code{lagged_mean}, \code{counts}, \code{alpha}, \code{conditional_randomization}, \code{hl_and_lh_are_outliers_not_clusters}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(4)
W <- matrix(0, 8, 8)
W[abs(row(W) - col(W)) == 1] <- 1
str(LisaClust(rnorm(8), W), max.level = 1)
}
