% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_valid_git_ref}
\alias{.morie_valid_git_ref}
\title{Validate a git ref (branch/tag) before passing it to `git clone`}
\usage{
.morie_valid_git_ref(ref)
}
\arguments{
\item{ref}{See Usage.}
}
\description{
Rejects option-injection (leading \\\'-\\\', e.g. "--upload-pack=...") and any whitespace/metacharacters, while allowing normal ref names.
}
\value{
A logical value.
}
\keyword{internal}
