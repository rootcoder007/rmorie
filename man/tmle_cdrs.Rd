% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tmle_cdrs}
\alias{tmle_cdrs}
\alias{morie_tmlcds}
\title{Collaborative TMLE for the ATE}
\usage{
tmle_cdrs(y, D, X, tuning = "discrete", penalties = NULL, n_folds = 5L, 
    trim = 0.005, scale = NULL, q_covariates = NULL) 

morie_tmlcds(y, D, X, tuning = "discrete", penalties = NULL, n_folds = 5L, 
    trim = 0.005, scale = NULL, q_covariates = NULL) 
}
\description{
Builds a nested sequence of treatment mechanisms and selects by the cross-validated loss of the targeted outcome fit, so an instrument stays out of the propensity model. van der Laan & Rose (2018) Ch. 10 Example 10.3.
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{D}{Argument \code{D}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{tuning}{Argument \code{tuning}; see Usage.}
\item{penalties}{Argument \code{penalties}; see Usage.}
\item{n_folds}{Argument \code{n_folds}; see Usage.}
\item{trim}{Argument \code{trim}; see Usage.}
\item{scale}{Argument \code{scale}; see Usage.}
\item{q_covariates}{Argument \code{q_covariates}; see Usage.}
}
\value{
The value of \code{.RichResult}.
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
set.seed(1)
r <- tmle_cdrs(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10)); TRUE
}
