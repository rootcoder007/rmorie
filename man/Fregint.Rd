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
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{X_E}{Argument \code{X_E}; see Usage.}
\item{X_EF}{Argument \code{X_EF}; see Usage.}
\item{env}{Argument \code{env}; see Usage.}
\item{lam}{Argument \code{lam}; see Usage.}
\item{P}{Argument \code{P}; see Usage.}
\item{reference}{Argument \code{reference}; see Usage.}
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
rmorie:::Fregint(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8),
  X_E = c(1, 2, 3, 4, 5, 6, 7, 8))
}
