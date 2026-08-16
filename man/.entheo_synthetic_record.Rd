% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.entheo_synthetic_record}
\alias{.entheo_synthetic_record}
\title{.entheo_synthetic_record}
\usage{
.entheo_synthetic_record(subject_id, n_tp = 480L, n_chan = 32L, n_parcels = 100L)
}
\description{
.entheo_synthetic_record
}
\value{
A list with \code{subject_id}, \code{condition_order}, \code{eeg}, \code{fmri}, \code{behavioural}, \code{.synthetic}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.entheo_synthetic_record(V)
}
