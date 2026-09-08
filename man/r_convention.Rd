% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{r_convention}
\alias{r_convention}
\title{r_convention}
\usage{
r_convention(fitted) 
}
\arguments{
\item{fitted}{A list; the body reads \code{$aic}, \code{$loglik}, \code{$phi}, \code{$Phi}, \code{$sigma2}, \code{$theta}, \code{$Theta} from it.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ma}, \code{sma}, \code{ar}, \code{sar}, \code{sigma2}, \code{loglik}, \code{aic}, \code{note}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
r_convention(D)
}
