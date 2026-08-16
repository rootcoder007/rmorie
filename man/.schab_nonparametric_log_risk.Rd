% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_nonparametric_log_risk}
\alias{.schab_nonparametric_log_risk}
\title{.schab_nonparametric_log_risk}
\usage{
.schab_nonparametric_log_risk(alpha, u, phi, gamma, delta = NULL)
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{u}{Numeric; combined arithmetically in the body.}
\item{phi}{A vector; its length is taken.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delta}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the schab_glmm_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
