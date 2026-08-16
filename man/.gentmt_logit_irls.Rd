% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gentmt_logit_irls}
\alias{.gentmt_logit_irls}
\title{Internal: logistic fit by IRLS, for the binary treatment density}
\usage{
.gentmt_logit_irls(y, Z, max_iter = 60L, ridge = 1e-08)
}
\arguments{
\item{y}{Binary outcome.}
\item{Z}{Design matrix, intercept included.}
\item{max_iter}{Iteration cap.}
\item{ridge}{Ridge added to the information matrix.}
}
\description{
Newton steps on the binomial log-likelihood with a small ridge, so a separated design still returns rather than diverging.
}
\value{
A list with \code{coef} and \code{fitted}.
}
\references{
  Robins, J. M., Hernan, M. A. & Brumback, B. (2000) "Marginal structural
    models and causal inference in epidemiology", Epidemiology 11(5), 550-560.
  Imai, K. & van Dyk, D. A. (2004) "Causal inference with general treatment
    regimes: generalizing the propensity score", JASA 99(467), 854-866.
  Hirano, K. & Imbens, G. W. (2004) "The propensity score with continuous
    treatments", in Applied Bayesian Modeling and Causal Inference from
    Incomplete-Data Perspectives, Wiley, 73-84.
  Hernan M. A. & Robins J. M. (2020) Causal Inference: What If, Sec. 12.3.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gentmt_logit_irls(V, V)
}
