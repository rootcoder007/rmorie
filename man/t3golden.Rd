% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{t3golden}
\alias{t3golden}
\title{t3golden}
\usage{
t3golden(f, lo, hi, iters = 80L) 
}
\arguments{
\item{f}{Accepted by the signature and not used anywhere in the body.}
\item{lo}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{hi}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{80L}.}
}
\description{
A step of the t3util implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
t3golden(function(x) (x - 2)^2, 0, 5)
}
