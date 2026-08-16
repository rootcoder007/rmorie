% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_tps_hub_resolve}
\alias{.morie_dataset_tps_hub_resolve}
\title{Resolve a TPS Hub item id to its underlying ArcGIS FeatureServer URL.}
\usage{
.morie_dataset_tps_hub_resolve(hub_id, offline = TRUE)
}
\description{
Offline mode looks up the cached catalog; live mode hits the ArcGIS Online items API (`/sharing/rest/content/items/<id>?f=json`).
}
\value{
The value of \code{$}.
}
\keyword{internal}
