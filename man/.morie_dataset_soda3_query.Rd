% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_soda3_query}
\alias{.morie_dataset_soda3_query}
\title{Generic Socrata SODA3 SoQL-query fetch}
\usage{
.morie_dataset_soda3_query(view_id, soql = "SELECT *", app_token = NULL, paginate = FALSE, page_size = 1000L, max_pages = 200L, max_features = NULL, base_url = "https://data.cityofchicago.org")
}
\arguments{
\item{view_id}{Socrata view id (UUID `xxxx-xxxx` or publisher   alias e.g. `crimes`).}
\item{soql}{Raw SoQL string. Default `"SELECT *"`.}
\item{app_token}{Optional Socrata app token (sent as `X-App-Token`   header, not as `$$app_token` query param -- SODA3 prefers the   header form).}
\item{paginate}{Logical; if `TRUE`, walk `LIMIT page_size OFFSET m`   pages until exhausted.}
\item{page_size}{Per-page row count when paginating.}
\item{max_pages}{Safety net on paginated walks.}
\item{max_features}{Optional total cap across all paged requests.}
\item{base_url}{Portal base URL.  Default   `"https://data.cityofchicago.org"`.}
}
\description{
SODA2 (`/resource/<id>.<ext>`) exposes datasets via key=value query parameters (`$where`, `$limit`, `$offset`). SODA3 (`/api/v3/views/<id>/query.<ext>`) takes a single full SoQL string via `?query=SELECT ... WHERE ... ORDER BY ... LIMIT ... OFFSET ...`. This helper is the SODA3 sibling of \code{.morie_dataset_socrata_fetch()}. Use SODA3 when:   * The dataset is a *filtered view* or *map view* (e.g.     `ahwe-kpsy` "Crimes - Map" derived from `ijzp-q8t2`). SODA2     against these returns `\[{}\]` -- empty rows -- because column     resolution doesn\'t fire on derived views.   * You want to send a full SoQL `SELECT ... WHERE ...` with     aggregations / joins / arbitrary expressions that SODA2\'s     URL-param grammar can\'t express. Pagination here uses `LIMIT n OFFSET m` clauses baked into the SoQL string (SODA3 does NOT accept `$offset` as a separate query parameter). If the caller\'s `soql` already contains its own `LIMIT` / `OFFSET`, the loader leaves it alone and treats the result as a single page; pagination mode strips any caller-supplied `LIMIT`/`OFFSET` before walking.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
