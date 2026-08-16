% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcds_fluctuate}
\alias{.tmlcds_fluctuate}
\title{One targeting step: the Example 10.3 submodel, solved by Newton}
\usage{
.tmlcds_fluctuate(qa, q1, q0, y, d, g, clip = 1e-08)
}
\arguments{
\item{qa}{Numeric; combined arithmetically in the body.}
\item{q1}{Numeric; combined arithmetically in the body.}
\item{q0}{Numeric; combined arithmetically in the body.}
\item{y}{A vector; its length is taken.}
\item{d}{Numeric; combined arithmetically in the body.}
\item{g}{Numeric; combined arithmetically in the body.}
\item{clip}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
clever covariate A/G - (1-A)/(1-G); returns updated fits and epsilon
}
\value{
A list with \code{qa}, \code{q1}, \code{q0}, \code{eps}.
}
\references{
van der Laan, M. J. & Rose, S. (eds.) (2018) Targeted Learning in Data
Science: Causal Inference for Complex Longitudinal Studies, Springer
Series in Statistics, doi:10.1007/978-3-319-65304-4, Ch. 10 (Sec. 10.1,
Examples 10.2 and 10.3, Sec. 10.1.1).
van der Laan, M. J. & Gruber, S. (2010) "Collaborative double robust
targeted maximum likelihood estimation", The International Journal of
Biostatistics 6(1), article 17, doi:10.2202/1557-4679.1181.
Gruber, S. & van der Laan, M. J. (2010) "A targeted maximum likelihood
estimator of a causal effect on a bounded continuous outcome", The
International Journal of Biostatistics 6(1), article 26,
doi:10.2202/1557-4679.1260.
}
\keyword{internal}
