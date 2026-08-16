% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fregint}
\alias{Fregint}
\title{Functional regression with environment interaction (MVSML eq. 14.14)}
\usage{
Fregint(y, X, X_E, X_EF = NULL, env = NULL, lam = 0, P = NULL, reference = TRUE)
}
\description{
y = 1_n mu + X_E beta_E + X beta + X_EF beta_EF + e, which adds to (14.13) the environment-by-reflectance interaction.  Pass env, the environment label of each record, to have the block-diagonal X_EF of p.610 built, or pass X_EF directly.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
