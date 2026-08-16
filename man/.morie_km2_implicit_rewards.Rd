% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_implicit_rewards}
\alias{.morie_km2_implicit_rewards}
\title{.morie_km2_implicit_rewards}
\usage{
.morie_km2_implicit_rewards(pi_star, pi_ref, beta)
}
\arguments{
\item{pi_star}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{pi_ref}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{beta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch5_dpo_loss}, \code{morie_kamath_ch5_dpo_pref_simplified}, \code{morie_kamath_ch5_dpo_pref_substituted}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
