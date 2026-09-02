% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_rotation_cost}
\alias{morie_secrtt_rotation_cost}
\title{Compare the bytes re-encrypted under each strategy}
\usage{
morie_secrtt_rotation_cost(n_records, mean_record_bytes, dek_bytes = 32) 
}
\arguments{
\item{n_records}{Integer number of records.}
\item{mean_record_bytes}{Mean record size in bytes.}
\item{dek_bytes}{Size of a DEK in bytes.}
}
\description{
Compare the bytes re-encrypted under each strategy
}
\value{
List with \code{single_key_bytes},   \code{envelope_kek_bytes}, \code{ratio},   \code{records_touched_single}, \code{records_touched_envelope},   \code{note}.
}
\keyword{internal}
\examples{
morie_secrtt_rotation_cost(n_records = 5L, mean_record_bytes = 5L)
}
