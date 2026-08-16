% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.val_cv_indices}
\alias{.val_cv_indices}
\title{Internal helper: Val Cv Indices}
\usage{
.val_cv_indices(n, n_folds, method, y = NULL, groups = NULL, n_repeats = 10L, random_state = 42L)
}
\description{
Internal helper: Val Cv Indices
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
.val_cv_indices(n = 5L, n_folds = c(1, 2, 3, 4, 5, 6, 7, 8), method = 5L)
}
