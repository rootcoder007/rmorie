% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dml_plr_native}
\alias{.morie_dml_plr_native}
\title{Internal helper: native PLR DML engine (median-aggregated reps)}
\usage{
.morie_dml_plr_native(X, y, d, n_folds = 5L, n_rep = 1L, random_state = 42L)
}
\description{
Internal helper: native PLR DML engine (median-aggregated reps)
}
\value{
A list with \code{theta}, \code{se}.
}
\keyword{internal}
\examples{
.morie_dml_plr_native(X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = c(1, 2, 3, 4, 5, 6, 7, 8), d = c(1, 2, 3, 4, 5, 6, 7, 8))
}
