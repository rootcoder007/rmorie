% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.build_folds}
\alias{.build_folds}
\title{Internal helper: Build Folds}
\usage{
.build_folds(n, n_folds, stratify, groups)
}
\description{
Internal helper: Build Folds
}
\value{
The value of \code{split}.
}
\keyword{internal}
\examples{
.build_folds(n = 5L, n_folds = c(1, 2, 3, 4, 5, 6, 7, 8), stratify = c(1, 2, 3, 4, 5, 6, 7, 8), groups = c("a", "b", "c"))
}
