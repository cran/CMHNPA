% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_durbin.R
\name{durbin}
\alias{durbin}
\title{Durbin Test}
\usage{
durbin(y, groups, blocks, components = FALSE)
}
\arguments{
\item{y}{a numeric vector for the response variable.}

\item{groups}{a vector giving the group for the corresponding elements of
\code{y}.}

\item{blocks}{a vector giving the block for the corresponding elements of
\code{y}.}

\item{components}{a TRUE or FALSE flag to indicate if chi-squared or F
orthogonal component p-values are wanted.}
}
\value{
The Durbin test adjusted for ties results.
}
\description{
\code{durbin} returns the results of Durbin's Rank Sum test.
}
\details{
The test is a generalisation of Friedman's test that can be applied to BIBD.
}
\examples{
attach(icecream)
durbin(y = rank, groups = variety, blocks = judge, components = TRUE)
}
\references{
Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
