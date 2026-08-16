% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FetalEcg}
\alias{FetalEcg}
\title{FetalEcg}
\usage{
FetalEcg(abdominal, chest, order = 32, mu = 0.005, method = "lms")
}
\arguments{
\item{abdominal}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{chest}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Carried through into a list the body builds. Defaults to \code{32}.}
\item{mu}{Defaults to \code{0.005}.}
\item{method}{Defaults to \code{"lms"}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{fetal}, \code{maternal_estimate}, \code{order}, \code{input_power}, \code{output_power}, \code{suppression_db}, \code{reference_leakage}, \code{single_reference}, \code{widrow_used_multiple_references}, \code{method}.
}
\keyword{internal}
