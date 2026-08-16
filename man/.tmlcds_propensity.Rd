% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcds_propensity}
\alias{.tmlcds_propensity}
\title{Treatment mechanism fit; cols is a list of column vectors}
\usage{
.tmlcds_propensity(d, cols, n, penalty = 0, trim = 0.005)
}
\arguments{
\item{d}{See Usage.}
\item{cols}{See Usage.}
\item{n}{See Usage.}
\item{penalty}{Defaults to \code{0}.}
\item{trim}{Defaults to \code{0.005}.}
}
\description{
Part of the tmlcds_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{g}, \code{b}.
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
