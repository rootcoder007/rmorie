% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_fit_working_model}
\alias{.tmlcic_fit_working_model}
\title{Logit[Qbar(A,W)] on the candidate\\'s terms, fitted on rows}
\usage{
.tmlcic_fit_working_model(y, A, W, cand, rows, ridge)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{A}{A vector; indexed elementwise.}
\item{W}{Passed to \code{.tmlcic_row_fun}.}
\item{cand}{Passed to \code{.tmlcic_row_fun}.}
\item{rows}{See Usage.}
\item{ridge}{Numeric; passed to \code{max}.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_candidate_tmle}. See the file header for the source the module follows.
}
\value{
A list with \code{q}, \code{b}.
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
