% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_CARL.R
\name{CARL}
\alias{CARL}
\title{CARL Test}
\usage{
CARL(y, treatment, block1, block2)
}
\arguments{
\item{y}{a numeric vector for the response variable.}

\item{treatment}{a vector giving the treatment type for the corresponding
elements of \code{y}.}

\item{block1}{a vector giving the first blocking variable for the
corresponding elements of \code{y}.}

\item{block2}{a vector giving the second blocking variable for the
corresponding elements of \code{y}.}
}
\value{
The \code{CARL} test statistic adjusted for ties together with the
associated p-value using a chi-squared distribution with t+1 degrees of
freedom.
}
\description{
\code{CARL} returns the test statistic and p-value for the aligned RL test
with empirically fitted degrees of freedom.
}
\details{
This test is applicable to Latin square designs and is recommended over the
RL and ARL test. The test uses t+1 as the degrees of freedom of the
chi-squared null distribution and results in appropriate test sizes as well
as good power.
}
\examples{
attach(peanuts)
CARL(y = yield, treatment = treatment, block1 = row, block2 = col)

}
\references{
Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
\seealso{
\code{\link[=ARL]{ARL()}} \code{\link[=PARL]{PARL()}}
}
