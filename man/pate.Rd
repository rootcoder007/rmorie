% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{pate}
\alias{pate}
\title{pate}
\usage{
pate(teacher_predicts, queries, gamma = 0.05, delta = 1e-05, n_classes = NULL, student_train_fn = NULL, student_features = NULL, seed = 0, lambdas = NULL)
}
\arguments{
\item{teacher_predicts}{See Usage.}
\item{queries}{See Usage.}
\item{gamma}{Defaults to \code{0.05}.}
\item{delta}{Defaults to \code{1e-05}.}
\item{n_classes}{Defaults to \code{NULL}.}
\item{student_train_fn}{Defaults to \code{NULL}.}
\item{student_features}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
\item{lambdas}{Defaults to \code{NULL}.}
}
\description{
Part of the pate_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{labels}, \code{clean_labels}, \code{votes}, \code{agreement}, \code{epsilon}, \code{epsilon_accountant}, \code{epsilon_data_independent}, \code{accountant}, \code{delta}, \code{gamma}, \code{n_teachers}, \code{n_queries}, \code{student}, \code{note}, \code{method}.
}
\keyword{internal}
