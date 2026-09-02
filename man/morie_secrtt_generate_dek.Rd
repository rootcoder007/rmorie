% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_generate_dek}
\alias{morie_secrtt_generate_dek}
\title{Derive a per-record DEK}
\usage{
morie_secrtt_generate_dek(master_seed, record_id, salt = NULL) 
}
\arguments{
\item{master_seed}{Raw bytes.}
\item{record_id}{Raw bytes (or coercible).}
\item{salt}{Optional raw salt.}
}
\description{
Deriving from a master seed and the record id means the key table is the seed plus an identifier, not one row per record.
}
\value{
List with \code{dek}, \code{dek_hex}, \code{record_id},   \code{note}.
}
\keyword{internal}
\examples{
seed <- as.raw(1:32)
r <- morie_secrtt_generate_dek(seed, record_id = 7L)
r$dek_hex
}
