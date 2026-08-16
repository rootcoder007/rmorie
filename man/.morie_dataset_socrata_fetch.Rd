% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_socrata_fetch}
\alias{.morie_dataset_socrata_fetch}
\title{Generic Socrata JSON fetch (handles `$where`, `$limit`, `$$app_token`).}
\usage{
.morie_dataset_socrata_fetch(url, where = NULL, max_features = NULL, app_token = NULL, paginate = FALSE, page_size = 1000L, max_pages = 200L, select = NULL)
}
\description{
Two modes:   * `paginate = FALSE` (default): single SODA2 request. Honours     `$where`, `$limit = max_features` and `$$app_token`. Without an     explicit `$limit` the server enforces its default cap (1,000     rows on the open NYC + Chicago portals).   * `paginate = TRUE`: walk `$offset` in `page_size` chunks until     the server returns a short page (exhausted) or `max_features`     is reached. `max_pages` is a safety net against runaway pulls.     Without an app_token most portals cap `$limit` at 1,000 per     request, so the natural `page_size` is 1000; with a token the     hard server-side cap is 50,000.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
