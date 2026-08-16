% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcds_refit_on}
\alias{.tmlcds_refit_on}
\title{Out-of-sample loss of step s (1-based), refitting on the training}
\usage{
.tmlcds_refit_on(info, steps, s, tr, fold, tuning, penalties, trim)
}
\arguments{
\item{info}{A list; the body reads \code{$columns}, \code{$q_covariates}, \code{$treatment}, \code{$y_scaled} from it.}
\item{steps}{A vector; indexed elementwise.}
\item{s}{See Usage.}
\item{tr}{A vector; its length is taken.}
\item{fold}{A vector; its length is taken and its elements indexed.}
\item{tuning}{Accepted by the signature and not used anywhere in the body.}
\item{penalties}{Accepted by the signature and not used anywhere in the body.}
\item{trim}{Numeric; combined arithmetically in the body.}
}
\description{
rows tr and fold hold 1-based row indices.
}
\value{
One of two values, depending on the branch taken.
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
