% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcds_lstsq}
\alias{.tmlcds_lstsq}
\title{Ridge-stabilised least squares -- Python k.lstsq ALWAYS adds the}
\usage{
.tmlcds_lstsq(Z, y)
}
\arguments{
\item{Z}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{crossprod}.}
}
\description{
1e-10 ridge, so the R arm must too or near-singular designs drift
}
\value{
A vector, from \code{as.numeric}.
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
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.tmlcds_lstsq(M, M)
}
