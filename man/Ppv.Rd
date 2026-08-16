% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ppv}
\alias{Ppv}
\title{Ppv}
\usage{
Ppv(tp, fp = NULL, prevalence = NULL, sensitivity = NULL, specificity = NULL)
}
\arguments{
\item{tp}{A matrix; passed to \code{as.matrix}.}
\item{fp}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{prevalence}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{sensitivity}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{specificity}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_class implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
Ppv(45, 10)
}
