% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{StftParam}
\alias{StftParam}
\title{STFT window selection under the time-bandwidth limit, eq (8.10)}
\usage{
StftParam(fs, desired_t_res, desired_f_res)
}
\arguments{
\item{fs}{Numeric; combined arithmetically in the body.}
\item{desired_t_res}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{desired_f_res}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{nperseg_time}, \code{nperseg_freq}, \code{nperseg}, \code{achieved_t_res}, \code{achieved_f_res}, \code{tf_product}, \code{heisenberg_bound}, \code{feasible}, \code{method}.
}
\keyword{internal}
