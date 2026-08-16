% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_odata_fetch}
\alias{.morie_dataset_odata_fetch}
\title{Generic Socrata OData v4 fetch}
\usage{
.morie_dataset_odata_fetch(view_id, filter = NULL, select = NULL, orderby = NULL, top = NULL, skip = NULL, app_token = NULL, paginate = FALSE, max_pages = 200L, max_features = NULL, base_url = "https://data.cityofchicago.org")
}
\arguments{
\item{view_id}{Socrata view id (UUID `xxxx-xxxx` or publisher alias).}
\item{filter}{Optional OData `$filter` string (caller-supplied   verbatim). See limitation above.}
\item{select}{Optional OData `$select` (comma-separated column   list); defaults to `NULL` = all columns.}
\item{orderby}{Optional OData `$orderby`.}
\item{top}{Optional integer `$top` (per-request row count when   `paginate = FALSE`; per-page row count when `paginate = TRUE`).   `NULL` accepts server default.}
\item{skip}{Optional integer `$skip` (start offset, mostly useful   when `paginate = FALSE`).}
\item{app_token}{Optional Socrata app token (sent as `X-App-Token`).}
\item{paginate}{Logical; if `TRUE`, follow `@odata.nextLink`.}
\item{max_pages}{Safety net on paginated walks.}
\item{max_features}{Optional total row cap.}
\item{base_url}{Portal base URL. Default Chicago.}
}
\description{
Socrata exposes every dataset (base + filtered + map) under a third API mode in addition to SODA2 + SODA3 -- **OData v4** at `/api/odata/v4/<view_id>`. OData is the protocol Tableau, Power BI, Excel, and other BI tools speak natively; reaching it from R is mostly useful when you want morie to ingest the same way those tools do, or to use OData-specific features like server-driven `@odata.nextLink` pagination. Coverage parity vs the other two API modes:   * **Base datasets** (e.g. `ijzp-q8t2`): all three modes work.   * **Derived / map / filtered views** (e.g. `ahwe-kpsy`): OData     returns `value: \[{}\]` (empty objects) -- same failure mode as     SODA2. Use SODA3 (\code{.morie_dataset_soda3_query()}) for these. **Known Socrata limitation -- `$filter`**. As of 2026-05 Socrata\'s OData parser frequently rejects equality filters with `"The types \'Edm.Boolean\' and \'Edm.String\' (or \'Edm.Decimal\') are not compatible."` -- the filter expression tree is mis-typed inside Socrata\'s engine. `$top`, `$skip`, `$select`, and `$orderby` all work reliably. If you need filtering, prefer SODA3 (`morie_datasets_chicago_crime_soql`) or SODA2\'s `$where` (`morie_datasets_chicago_crime`). Pagination:   * `paginate = TRUE` follows the server-provided     `@odata.nextLink` until it disappears (Socrata returns a     nextLink whenever there are more rows; absence == exhausted).   * `max_features` caps the total row count across pages.   * `max_pages` is the safety net. Each response also carries `@odata.id` / `@odata.context` / `@odata.metadata` metadata fields per row; these are stripped from the returned data.frame so columns match the dataset schema.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
