% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rscstep}
\alias{.rscstep}
\title{One C-step of Rousseeuw and Van Driessen (1999).  Theorem, restated}
\usage{
.rscstep(X, idx, h)
}
\arguments{
\item{X}{See Usage.}
\item{idx}{See Usage.}
\item{h}{See Usage.}
}
\description{
in Hubert, Debruyne and Rousseeuw (2018), arXiv 1709.07045, "COMPUTATION": from H1 of size h with mean mu1 and covariance S1, taking H2 to be the h observations with the smallest distances d(x_i, mu1, S1) gives |S2| <= |S1|, with equality iff mu2 = mu1 and S2 = S1. The determinant therefore never increases, which is what makes the iteration terminate. Returns list(idx, det) or NULL when S1 is singular, in which case the objective is already zero and the subset lies on a hyperplane.
}
\value{
A list with \code{idx}, \code{det}.
}
\keyword{internal}
