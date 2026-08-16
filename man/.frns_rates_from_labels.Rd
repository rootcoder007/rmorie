% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frns_rates_from_labels}
\alias{.frns_rates_from_labels}
\title{.frns_rates_from_labels}
\usage{
.frns_rates_from_labels(y_true, y_pred, group, favorable)
}
\arguments{
\item{y_true}{A vector; indexed elementwise.}
\item{y_pred}{A vector; indexed elementwise.}
\item{group}{See Usage.}
\item{favorable}{See Usage.}
}
\description{
A step of the frns_metrics implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.frns_rates_from_labels(y_true = c(1, 2, 3, 4, 5, 6, 7, 8), y_pred = c(1, 2, 3, 4, 5, 6, 7, 8), group = c("a", "b", "c"), favorable = c(1, 2, 3, 4, 5, 6, 7, 8))
}
