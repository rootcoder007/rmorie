% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_reg_did_panel_native}
\alias{.morie_reg_did_panel_native}
\title{Internal helper: outcome-regression DiD, panel version}
\usage{
.morie_reg_did_panel_native(dy, D, X)
}
\description{
The `est_method = "reg"` estimand of `did::att_gt` / `DRDID::reg_did_panel`.
}
\value{
A list with \code{att}, \code{IF}, \code{se}.
}
\keyword{internal}
