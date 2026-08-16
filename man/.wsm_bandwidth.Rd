% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wsm_bandwidth}
\alias{.wsm_bandwidth}
\title{Silverman Sec. 3.4.2. "3.28" is 1.06 sigma n^(-1/5), the pure}
\usage{
.wsm_bandwidth(x, rule = "3.31")
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rule}{The body requires: rule must be '3.28', '3.29' or '3.31'. Defaults to \code{"3.31"}.}
}
\description{
normal reference; "3.29" is 0.79 R n^(-1/5) with R the interquartile range; "3.31" is 0.9 A n^(-1/5) and is the book\\'s actual recommendation, reported to land within 10\% of the optimal mean integrated square error across every t-distribution considered, log-normals with skewness up to about 1.8, and normal mixtures separated by up to 3 standard deviations.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.wsm_bandwidth(V)
}
