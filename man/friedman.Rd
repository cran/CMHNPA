% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_friedman.R
\name{friedman}
\alias{friedman}
\title{Friedman Test}
\usage{
friedman(
  y,
  groups,
  blocks,
  n_components = 0,
  n_permutations = 0,
  group_scores = NULL,
  sig_digits = 4,
  verbose = FALSE
)
}
\arguments{
\item{y}{a numeric vector for the response variable.}

\item{groups}{a vector giving the group for the corresponding elements of
\code{y}.}

\item{blocks}{a vector giving the block for the corresponding elements of
\code{y}.}

\item{n_components}{the number of polynomial components you wish to test. The maximum number of components is the number of groups less one. If the number of components requested is less than \code{t-2}, a remainder component is created.}

\item{n_permutations}{the number of permutations you wish to run.}

\item{group_scores}{the scores to be applied to the groups. If not declared these will be set automatically and should be checked.}

\item{sig_digits}{the number of significant digits the output should show.}

\item{verbose}{flag for turning on the status bar for permutation tests.}
}
\value{
The Friedman test results statistic adjusted for ties together with the
associated p-value.
}
\description{
\code{friedman} returns the test statistic and p-value for the Friedman test.
}
\examples{
attach(jam)
friedman(y = sweetness_ranks, groups = type, blocks = judge)
}
\references{
Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
