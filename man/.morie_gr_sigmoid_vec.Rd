% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_sigmoid_vec}
\alias{.morie_gr_sigmoid_vec}
\title{Grsig kernel: the two-branch overflow-safe logistic}
\usage{
.morie_gr_sigmoid_vec(z)
}
\arguments{
\item{z}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_gru_cell}, \code{morie_geron_logistic_regression_probability}, \code{morie_geron_lstm_cell} and 6 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_sigmoid_vec(V)
}
