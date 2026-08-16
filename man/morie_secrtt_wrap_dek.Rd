% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_wrap_dek}
\alias{morie_secrtt_wrap_dek}
\title{Wrap a DEK under a KEK}
\usage{
morie_secrtt_wrap_dek(dek, kek, nonce, kek_id = "kek-1", aad = raw())
}
\arguments{
\item{dek}{Raw 32-byte DEK.}
\item{kek}{Raw KEK.}
\item{nonce}{Raw 12-byte nonce.}
\item{kek_id}{KEK identifier.}
\item{aad}{Raw additional authenticated data.}
}
\description{
The KEK id is bound into the AAD, so a wrapped DEK cannot be replayed under a different KEK.
}
\value{
List with \code{wrapped}, \code{tag}, \code{nonce},   \code{kek_id}, \code{wrapped_hex}, \code{note}.
}
\keyword{internal}
\examples{
seed <- as.raw(1:32)
dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
kek <- as.raw(rev(1:32))
w <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(1:12))
w$kek_id
}
