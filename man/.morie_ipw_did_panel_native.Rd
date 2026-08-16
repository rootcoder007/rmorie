% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ipw_did_panel_native}
\alias{.morie_ipw_did_panel_native}
\title{Internal helper: standardized IPW DiD, panel version}
\usage{
.morie_ipw_did_panel_native(dy, D, X)
}
\description{
The `est_method = "ipw"` estimand of `did::att_gt` (Abadie 2005 with standardized weights; `DRDID::std_ipw_did_panel`).
}
\value{
A list with \code{att}, \code{IF}, \code{se}.
}
\keyword{internal}
