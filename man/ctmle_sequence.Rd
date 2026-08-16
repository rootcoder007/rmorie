% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ctmle_sequence}
\alias{ctmle_sequence}
\title{Build the nested sequence of (G, targeted Q) and score each step}
\usage{
ctmle_sequence(y, D, X, tuning = "discrete", penalties = NULL, trim = 0.005, scale = NULL, q_covariates = NULL)
}
\arguments{
\item{y}{Passed to \code{.tmlcds_vec}.}
\item{D}{Passed to \code{.tmlcds_vec}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.tmlcds_mat}.}
\item{tuning}{Compared against \code{"discrete"}. Defaults to \code{"discrete"}.}
\item{penalties}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{trim}{Passed to \code{.tmlcds_propensity}. Defaults to \code{0.005}.}
\item{scale}{Optional; may be \code{NULL}. A flag; the body branches on it.}
\item{q_covariates}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
q_covariates uses 0-based indices, matching the Python arm exactly.
}
\value{
A list with \code{steps}, \code{info}.
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
