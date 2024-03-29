% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_PARL.R
\name{PARL}
\alias{PARL}
\title{PARL Test}
\usage{
PARL(y, treatment, block1, block2, N_perms = 1000, components = FALSE)
}
\arguments{
\item{y}{a numericc vector for the response variable.}

\item{treatment}{a vector giving the treatment type for the corresponding
elements of \code{y}.}

\item{block1}{a vector giving the first blocking variable for the
corresponding elements of \code{y}.}

\item{block2}{a vector giving the second blocking variable for the
corresponding elements of \code{y}.}

\item{N_perms}{The number of permutations to perform.}

\item{components}{a TRUE or FALSE flag to indicate whether component p-values
should be calculated.}
}
\value{
The PARL test statistic together with the associated p-value. Component
p-values may also be calculated and shown.
}
\description{
\code{PARL} returns the test statistic and p-value for the aligned RL test
performed as a permutation test.
}
\details{
This test is applicable to Latin square designs and is recommended over the
RL and ARL test. The CARL test is much faster to run.
}
\examples{
attach(peanuts)
PARL(y = yield, treatment = treatment, block1 = row, block2 = col,
components = TRUE)
}
\references{
Rayner, J.C.W and Livingston Jr, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
\seealso{
\code{\link[=ARL]{ARL()}} \code{\link[=CARL]{CARL()}}
}
