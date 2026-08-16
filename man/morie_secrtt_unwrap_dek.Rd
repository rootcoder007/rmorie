% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_unwrap_dek}
\alias{morie_secrtt_unwrap_dek}
\title{Unwrap a DEK and record the call}
\usage{
morie_secrtt_unwrap_dek(wrapped, kek, audit_log = NULL)
}
\arguments{
\item{wrapped}{List with \code{wrapped}, \code{nonce}, \code{tag},   \code{kek_id}, optional \code{aad}.}
\item{kek}{Raw KEK.}
\item{audit_log}{Optional list to append to.}
}
\description{
The envelope bounds offline compromise; an attacker who can call the KEK is bounded only by what the audit trail catches, so the call is logged rather than left silent.
}
\value{
List with \code{dek}, \code{kek_id}, \code{audited}.
}
\keyword{internal}
\examples{
seed <- as.raw(1:32)
dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
kek <- as.raw(rev(1:32))
w <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(1:12))
u <- morie_secrtt_unwrap_dek(w, kek)
identical(u$dek, dek)
}
