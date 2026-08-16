% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sacrb_bleu}
\alias{morie_sacrb_bleu}
\title{morie_sacrb_bleu}
\usage{
morie_sacrb_bleu(candidates, references, max_n = 4L, weights = NULL, tokenizer = "13a", lowercase = FALSE)
}
\arguments{
\item{candidates}{See Usage.}
\item{references}{See Usage.}
\item{max_n}{Defaults to \code{4L}.}
\item{weights}{Defaults to \code{NULL}.}
\item{tokenizer}{Defaults to \code{"13a"}.}
\item{lowercase}{Defaults to \code{FALSE}.}
}
\description{
Part of the sacrb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{bleu}, \code{score}, \code{precisions}, \code{bp}, \code{candidate_length}, \code{reference_length}, \code{ratio}, \code{tokenizer}, \code{lowercase}, \code{max_n}, \code{signature}, \code{method}.
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
