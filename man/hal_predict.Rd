% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hal_predict}
\alias{hal_predict}
\title{hal_predict}
\usage{
hal_predict(model, X) 
}
\arguments{
\item{model}{A list; the body reads \code{$beta}, \code{$columns}, \code{$intercept} from it.}
\item{X}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the tlhal_native implementation. Called by \code{cv_select_lambda}, \code{morie_tlhal}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
hal_predict(D, V)
}
