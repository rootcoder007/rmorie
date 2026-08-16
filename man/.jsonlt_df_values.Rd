% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_df_values}
\alias{.jsonlt_df_values}
\title{.jsonlt_df_values}
\usage{
.jsonlt_df_values(df, o)
}
\arguments{
\item{df}{A matrix; passed to \code{nrow}.}
\item{o}{Passed to \code{.jsonlt_cell}.}
}
\description{
A step of the jsonlt_native implementation. Called by \code{.jsonlt_encode}. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.jsonlt_df_values(M, D)
}
