% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_encode}
\alias{.jsonlt_encode}
\title{.jsonlt_encode}
\usage{
.jsonlt_encode(x, o, unbox_ok)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
\item{o}{A list; the body reads \code{$dataframe}, \code{$factor}, \code{$force}, \code{$matrix}, \code{$null} from it.}
\item{unbox_ok}{Passed to \code{.jsonlt_atomic}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_cell}, \code{.jsonlt_df_columns}, \code{morie_jsonlt_to_json}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
.jsonlt_encode(x = c(1, 2, 3, 4, 5, 6, 7, 8), o = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), unbox_ok = c(1, 2, 3, 4, 5, 6, 7, 8))
}
