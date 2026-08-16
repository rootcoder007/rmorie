% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcds_qloss}
\alias{.tmlcds_qloss}
\title{L(Qbar) = -{Y log Qbar + (1-Y) log(1-Qbar)}}
\usage{
.tmlcds_qloss(qa, y)
}
\arguments{
\item{qa}{Numeric; passed to \code{log}.}
\item{y}{A vector; its length is taken.}
}
\description{
A step of the tmlcds_native implementation. Called by \code{ctmle_sequence}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.tmlcds_qloss(V, V)
}
