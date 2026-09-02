% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_rotate_kek}
\alias{morie_secrtt_rotate_kek}
\title{Rotate the KEK: re-wrap every DEK, no record ciphertext touched}
\usage{
morie_secrtt_rotate_kek(wrapped_deks, old_kek, new_kek, new_nonces, new_kek_id = "kek-2", 
    audit_log = NULL) 
}
\arguments{
\item{wrapped_deks}{List of wrapped DEKs.}
\item{old_kek}{Raw KEK.}
\item{new_kek}{Raw KEK.}
\item{new_nonces}{One fresh nonce per wrapped DEK.}
\item{new_kek_id}{New KEK identifier.}
\item{audit_log}{Optional audit list.}
}
\description{
The cheap rotation, and the reason the envelope exists.
}
\value{
List with \code{wrapped} (==\code{estimate}), \code{n},   \code{records_reencrypted}, \code{kek_id}, \code{method},   \code{note}.
}
\keyword{internal}
\examples{
seed <- as.raw(1:32)
dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
kek1 <- as.raw(rev(1:32))
kek2 <- as.raw(rep(7L, 32))
w <- morie_secrtt_wrap_dek(dek, kek1, nonce = as.raw(1:12))
r <- morie_secrtt_rotate_kek(list(w), kek1, kek2,
                             new_nonces = list(as.raw(2:13)))
r$kek_id
}
