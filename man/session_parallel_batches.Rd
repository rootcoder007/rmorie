% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{session_parallel_batches}
\alias{session_parallel_batches}
\alias{gru4rec}
\alias{gruforrecommendation}
\title{Build session-parallel mini-batches}
\usage{
session_parallel_batches(sessions, batch_size) 

gru4rec(sessions, batch_size) 

gruforrecommendation(sessions, batch_size) 
}
\arguments{
\item{sessions}{List of integer sequences of length >= 2.}
\item{batch_size}{Number of parallel slots.}
}
\description{
Build session-parallel mini-batches
}
\value{
List with steps, n_steps, batch_size, n_sessions, note.
}
\keyword{internal}
\examples{
session_parallel_batches(list(c(1, 2, 3), c(4, 5), c(6, 7, 8, 9)), 2)
}
