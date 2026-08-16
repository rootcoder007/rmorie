% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{edgrn_ql_f_test}
\alias{edgrn_ql_f_test}
\title{edgrn_ql_f_test}
\usage{
edgrn_ql_f_test(lrt, q, quasi_dispersion, df_residual, df_prior = NULL)
}
\arguments{
\item{lrt}{See Usage.}
\item{q}{See Usage.}
\item{quasi_dispersion}{See Usage.}
\item{df_residual}{See Usage.}
\item{df_prior}{Defaults to \code{NULL}.}
}
\description{
A step of the edgrn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{F}, \code{df1}, \code{df2}, \code{p_value}, \code{lrt_p_value}, \code{method}, \code{note}.
}
\keyword{internal}
