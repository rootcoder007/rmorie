% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_sorted_counts}
\alias{.morie_gr_sorted_counts}
\title{.morie_gr_sorted_counts}
\usage{
.morie_gr_sorted_counts(y_true, y_scores)
}
\arguments{
\item{y_true}{Coerced to vector by the body, with \code{as.vector}.}
\item{y_scores}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_precision_recall_curve}, \code{morie_geron_roc_curve}. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{s}, \code{P}, \code{N}.
}
\keyword{internal}
