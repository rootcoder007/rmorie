% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_fluct}
\alias{.tmlcic_fluct}
\title{One-parameter logistic fluctuation: solve}
\usage{
.tmlcic_fluct(y, off, H, rows = NULL, obs_weights = NULL)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{off}{A vector; indexed elementwise.}
\item{H}{A vector; indexed elementwise.}
\item{rows}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{obs_weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
sum w H (y - sigmoid(off + eps H)) = 0 for eps on the given rows.
}
\value{
The value of \code{eps}, as built in the body.
}
\references{
----------
Balzer, L. B., van der Laan, M. J. & Petersen, M. L. (2018)
"Data-Adaptive Estimation in Cluster Randomized Trials", Ch. 13 in
Targeted Learning in Data Science, Springer, pp. 195-215,
doi:10.1007/978-3-319-65304-4_13.

Balzer, L. B., van der Laan, M. J. & Petersen, M. L. (2016) "Adaptive
pre-specification in randomized trials with and without
pair-matching", Statistics in Medicine 35(25), 4528-4545,
doi:10.1002/sim.7023.

Balzer, L. B., Zheng, W., van der Laan, M. J. & Petersen, M. L.
(2019) "A new approach to hierarchical data analysis", Statistical
Methods in Medical Research 28(6), 1761-1780,
doi:10.1177/0962280218774936.
}
\keyword{internal}
\examples{
.tmlcic_fluct(y = 5L, off = c(1, 2, 3, 4, 5, 6, 7, 8), H = 0.5)
}
