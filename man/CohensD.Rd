% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CohensD}
\alias{CohensD}
\title{\\' Cohen\\'s d and Hedges\\' g for two independent groups}
\usage{
CohensD(m1, m2, s1, s2, n1, n2) 
}
\arguments{
\item{m1}{Numeric; combined arithmetically in the body.}
\item{m2}{Numeric; combined arithmetically in the body.}
\item{s1}{Numeric; combined arithmetically in the body.}
\item{s2}{Numeric; combined arithmetically in the body.}
\item{n1}{Numeric; combined arithmetically in the body.}
\item{n2}{Numeric; combined arithmetically in the body.}
}
\description{
\\' \\' d = (m1 - m2) / s_pooled with the usual pooled standard deviation. \\' The bias correction is the exact Hedges (1981) factor \\' J = Gamma(df/2) / (sqrt(df/2) Gamma((df-1)/2)), and the variance is \\' 1/n1 + 1/n2 + estimate^2 / (2 (n1 + n2)) evaluated at the estimate. \\' Both conventions were settled against metafor::escalc(measure="SMD"). \\' \\' @param m1,m2 group means \\'
}
\value{
A list with \code{d}, \code{s_pooled}, \code{var_d}, \code{se_d}, \code{j}, \code{j_approx}, \code{hedges_g}, \code{var_g}, \code{se_g}, \code{df}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
CohensD(m1 = 10, m2 = 8, s1 = 2, s2 = 2.5, n1 = 30, n2 = 28)
}
