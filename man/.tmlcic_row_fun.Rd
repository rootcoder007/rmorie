% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_row_fun}
\alias{.tmlcic_row_fun}
\title{.tmlcic_row_fun}
\usage{
.tmlcic_row_fun(W, cand)
}
\arguments{
\item{W}{A matrix; indexed by row and column.}
\item{cand}{A list; the body reads \code{$cols}, \code{$interact} from it.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{.tmlcic_fit_working_model}. See the file header for the source the module follows.
}
\value{
The value of \code{function}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.tmlcic_row_fun(V, D)
}
