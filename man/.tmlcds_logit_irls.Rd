% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcds_logit_irls}
\alias{.tmlcds_logit_irls}
\title{Logistic regression by IRLS, mirroring k.logit_irls: beta starts at}
\usage{
.tmlcds_logit_irls(X, y, iters = 60L, ridge = 1e-10, tol = 1e-13, penalty = 0)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{iters}{Defaults to \code{60L}.}
\item{ridge}{Defaults to \code{1e-10}.}
\item{tol}{Defaults to \code{1e-13}.}
\item{penalty}{Defaults to \code{0}.}
}
\description{
0, `penalty` is a ridge on the OBJECTIVE for the slopes only (intercept unpenalized), `ridge` stabilises the Newton solve without changing the fitted coefficients
}
\value{
The value of \code{beta}, as built in the body.
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
