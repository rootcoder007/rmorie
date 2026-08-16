% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_strmkr_strauss_process}
\alias{morie_strmkr_strauss_process}
\title{morie_strmkr_strauss_process}
\usage{
morie_strmkr_strauss_process(coords, r, gamma = NULL, window = NULL, nx = 12L, ny = 12L, max_iter = 100L, tol = 1e-11)
}
\arguments{
\item{coords}{Passed to \code{.strmkr_rows}.}
\item{r}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{gamma}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{window}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{nx}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{12L}.}
\item{ny}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{12L}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100L}.}
\item{tol}{Defaults to \code{1e-11}.}
}
\description{
A step of the strmkr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
morie_strmkr_strauss_process(coords = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), r = 5L)
}
