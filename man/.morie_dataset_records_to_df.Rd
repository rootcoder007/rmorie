% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_records_to_df}
\alias{.morie_dataset_records_to_df}
\title{Convert a list-of-records / data.frame response into a clean data.frame.}
\usage{
.morie_dataset_records_to_df(records)
}
\description{
Convert a list-of-records / data.frame response into a clean data.frame.
}
\value{
The value of \code{do.call}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_dataset_records_to_df(D)
}
