% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{KFoldCv}
\alias{KFoldCv}
\title{Section 10.10.3.  The book\\'s point is that the training and test}
\usage{
KFoldCv(X, y, k = 5, classifier = NULL, stratified = TRUE)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{k}{Defaults to \code{5}.}
\item{classifier}{Defaults to \code{NULL}.}
\item{stratified}{Defaults to \code{TRUE}.}
}
\description{
steps must use SEPARATE data: an error rate measured on the samples that trained the classifier is optimistic, and with enough free parameters it reaches zero while the classifier generalizes not at all.  Folds are stratified by default; unstratified folds on unbalanced data can leave a class absent from a training fold, which measures luck rather than generalization.
}
\value{
A list with \code{error_rate}, \code{accuracy}, \code{errors}, \code{n}, \code{k}, \code{per_fold}, \code{stratified}, \code{train_and_test_must_be_separate}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
KFoldCv(V, V)
}
