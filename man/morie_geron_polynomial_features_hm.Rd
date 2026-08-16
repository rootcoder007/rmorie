% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_polynomial_features_hm}
\alias{morie_geron_polynomial_features_hm}
\title{Polynomial feature expansion up to a given total degree (Geron Ch 4, hmplf)}
\usage{
morie_geron_polynomial_features_hm(X, degree, include_bias = TRUE, interaction_only = FALSE)
}
\arguments{
\item{X}{Data (m, n) or (m,).}
\item{degree}{Max total degree.}
\item{include_bias,interaction_only}{Expansion flags.}
}
\description{
Polynomial feature expansion up to a given total degree (Geron Ch 4, hmplf)
}
\value{
A list with \code{features}, \code{powers}, \code{names}, \code{n_output_features}, \code{degree}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
