% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rf_fit}
\alias{.morie_rf_fit}
\title{Internal helper: native random forest}
\usage{
.morie_rf_fit(X, y, task = c("regression", "classification"), n_estimators = 100L, mtry = NULL, max_depth = 30L, min_node = NULL)
}
\description{
Returns fitted values, out-of-bag predictions (ESL p. 592 -- each observation is predicted only by the trees that did not see it), and normalised feature importances.
}
\value{
A list with \code{fitted}, \code{oob}, \code{importance}, \code{levels}, \code{forest}, \code{mtry}, \code{n_estimators}, \code{task}.
}
\keyword{internal}
