% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_mgwr_backfit}
\alias{.schab_mgwr_backfit}
\title{A caution the SOC makes necessary: both scores measure how much the}
\usage{
.schab_mgwr_backfit(y, X, coords, kernel = "gaussian", criterion = "aicc", adaptive = FALSE, tol = 1e-05, max_iter = 200L, rss_score = FALSE, bws_same_times = 5L, init_bandwidth = NULL, standardize = TRUE)
}
\arguments{
\item{y}{A matrix; passed to \code{nrow}.}
\item{X}{A matrix; indexed by row and column.}
\item{coords}{Passed to \code{.schab_pairwise_distances}.}
\item{kernel}{Passed to \code{.schab_gwr_fit}. Defaults to \code{"gaussian"}.}
\item{criterion}{Passed to \code{.schab_select_bandwidth}. Defaults to \code{"aicc"}.}
\item{adaptive}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-05}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.}
\item{rss_score}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{bws_same_times}{Passed to \code{>=}. Defaults to \code{5L}.}
\item{init_bandwidth}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{standardize}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
fit MOVED, not how good it is. When the initial single-bandwidth GWR already sits at the wide end of the interval, the first sweep can leave every covariate there, the score is tiny, and the loop stops after two or three sweeps having found no scale separation at all. Measured over eight seeds of a fixture with two genuinely different scales this happened twice. It is a property of the criterion, not of this port -- the reference uses the same score and the same default tolerance. `at_search_boundary` flags it.
}
\value{
A list with \code{bandwidths}, \code{at_search_boundary}, \code{standardized}, \code{y_centre}, \code{y_scale}, \code{x_centre}, \code{x_scale}, \code{params}, \code{fitted}, \code{resid}, \code{bandwidth_gwr}, \code{bandwidth_history}, \code{score_history}, \code{n_iter}, \code{converged}, \code{criterion}, \code{kernel}.
}
\keyword{internal}
\examples{
.schab_mgwr_backfit(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), coords = c(1, 2, 3, 4, 5, 6, 7, 8))
}
