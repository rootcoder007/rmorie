% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_tokenization_pipeline}
\alias{morie_alammar_tokenization_pipeline}
\title{WordPiece tokenisation pipeline (Alammar Ch 2)}
\usage{
morie_alammar_tokenization_pipeline(text, vocab, unk_token = "[UNK]", lowercase = TRUE, specials = c("[CLS]", "[SEP]"))
}
\arguments{
\item{text}{Input.}
\item{vocab}{Vocabulary incl. UNK and specials.}
\item{unk_token,lowercase,specials}{Pipeline settings.}
}
\description{
WordPiece tokenisation pipeline (Alammar Ch 2)
}
\value{
A list with \code{tokens}, \code{n_unk}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
