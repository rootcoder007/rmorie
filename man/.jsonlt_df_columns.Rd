% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_df_columns}
\alias{.jsonlt_df_columns}
\title{.jsonlt_df_columns}
\usage{
.jsonlt_df_columns(df, o)
}
\arguments{
\item{df}{A vector; indexed elementwise.}
\item{o}{Passed to \code{.jsonlt_encode}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_encode}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.jsonlt_df_columns(V, V)
}
