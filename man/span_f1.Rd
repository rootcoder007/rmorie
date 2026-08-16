% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{span_f1}
\alias{span_f1}
\title{span_f1}
\usage{
span_f1(pred, gold)
}
\arguments{
\item{pred}{Passed to \code{extract_spans}.}
\item{gold}{Passed to \code{extract_spans}.}
}
\description{
A step of the benRea_native implementation. Called by \code{ner_decode}. See the file header for the source the module follows.
}
\value{
A list with \code{precision}, \code{recall}, \code{f1}, \code{true_positives}, \code{n_pred}, \code{n_gold}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
span_f1(V, V)
}
