% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{teacher_votes}
\alias{teacher_votes}
\title{teacher_votes}
\usage{
teacher_votes(teacher_predicts, rows, n_classes = NULL) 
}
\arguments{
\item{teacher_predicts}{Coerced to list by the body, with \code{as.list}.}
\item{rows}{Passed to \code{predict}.}
\item{n_classes}{Defaults to \code{NULL}.}
}
\description{
A step of the pate_native implementation. Called by \code{pate}. See the file header for the source the module follows.
}
\value{
The value of \code{split}.
}
\keyword{internal}
