% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edgrn_exact_test}
\alias{edgrn_exact_test}
\title{edgrn_exact_test}
\usage{
edgrn_exact_test(count_a, count_b, lib_a, lib_b, dispersion) 
}
\arguments{
\item{count_a}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{count_b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lib_a}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lib_b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{dispersion}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the edgrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{p_value}, \code{logFC}, \code{dispersion}, \code{note}.
}
\keyword{internal}
\examples{
edgrn_exact_test(10, 25, 1000, 1000, 0.1)
}
