% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_attgt_native}
\alias{.morie_attgt_native}
\title{Internal helper: native Callaway-Sant\'Anna ATT(g,t)}
\usage{
.morie_attgt_native(data, outcome, unit, time, gname, covariates = NULL, est_method = "dr", control_group = "nevertreated", biters = 999L, seed = NULL, alpha = 0.05, se_convention = "reference")
}
\description{
Computes group-time average treatment effects on a (possibly unbalanced) panel using the Sant\'Anna-Zhao panel estimators on each two-period comparison. Base period is "varying" (t-1 for pre-treatment periods, g-1 for post-treatment ones), matching `did::att_gt`\'s default.
}
\value{
A list with \code{results}, \code{IF}, \code{n}.
}
\keyword{internal}
