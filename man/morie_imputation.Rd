% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_imputation}
\alias{morie_imputation}
\title{The two-step imputation estimator of Sec. 4}
\usage{
morie_imputation(Y, unit, period, first_treated, X = NULL) 
}
\description{
Fit the two-way model on **untreated** observations only, impute the untreated potential outcome everywhere, and average the residual within each \eqn{(g, t)}{(g, t)} cell.  Sec. 5 proves this is numerically identical to \code{morie_etwfe}.
}
\arguments{
\item{Y}{Argument \code{Y}; see Usage.}
\item{unit}{Argument \code{unit}; see Usage.}
\item{period}{Argument \code{period}; see Usage.}
\item{first_treated}{Argument \code{first_treated}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
}
\value{
A named list mirroring the RichResult payload: \code{estimate},   \code{att}, \code{n_cells}, \code{n_untreated_used}, \code{coef},   \code{method}, \code{identical_to}.
}
\keyword{internal}
\examples{
set.seed(1)
nu <- 4; T <- 5
unit <- rep(1:nu, each = T)
period <- rep(1:T, nu)
ft_unit <- c(NA, NA, 4, 4)
first_treated <- rep(ft_unit, each = T)
treated_now <- !is.na(first_treated) & period >= first_treated
Y <- rnorm(nu * T) + treated_now * 2
morie_imputation(Y, unit, period, first_treated)
}
