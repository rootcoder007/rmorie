% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sacrb_signature}
\alias{.sacrb_signature}
\title{.sacrb_signature}
\usage{
.sacrb_signature(tokenizer = "13a", lowercase = FALSE, max_n = 4L, n_refs = 1L, version = "morie-sacrb-1")
}
\arguments{
\item{tokenizer}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"13a"}.}
\item{lowercase}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{max_n}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4L}.}
\item{n_refs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1L}.}
\item{version}{Defaults to \code{"morie-sacrb-1"}.}
}
\description{
A step of the sacrb_native implementation. Called by \code{morie_sacrb_bleu}. See the file header for the source the module follows.
}
\value{
A character value.
}
\references{
----------
Papineni, K., Roukos, S., Ward, T. & Zhu, W.-J. (2002) "BLEU: a
Method for Automatic Evaluation of Machine Translation", *Proceedings
of the 40th Annual Meeting of the Association for Computational
Linguistics (ACL 2002)*, 311-318, doi:10.3115/1073083.1073135.
Sec. 2.1 (modified n-gram precision with clipping), Sec. 2.2.2 (the
sentence brevity penalty, the "best match length" as the closest
reference length, and the argument for computing the penalty over the
corpus rather than per sentence), and Sec. 2.3 (the geometric mean
and the BP formula reproduced above).

Post, M. (2018) "A Call for Clarity in Reporting BLEU Scores",
*Proceedings of the Third Conference on Machine Translation (WMT18)*,
186-191, doi:10.18653/v1/W18-6319, arXiv:1804.08771. That BLEU depends on tokenisation and
preprocessing, that these are usually unreported, and that scores are
therefore not comparable across papers; the proposal to score
detokenised output with a single internal tokenisation and to publish
a version signature.
"""
}
\keyword{internal}
