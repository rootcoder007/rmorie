% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gentmt_ols_core}
\alias{.gentmt_ols_core}
\title{.gentmt_ols_core}
\usage{
.gentmt_ols_core(X, y)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A matrix; passed to \code{crossprod}.}
}
\description{
A step of the gentmt_native implementation. Called by \code{.gentmt_treatment_density}. See the file header for the source the module follows.
}
\value{
A list with \code{coef}, \code{se}, \code{vcov}, \code{fitted}, \code{residuals}, \code{sigma2}.
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
