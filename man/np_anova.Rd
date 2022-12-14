% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_np_anova.R
\name{np_anova}
\alias{np_anova}
\title{np_anova function}
\usage{
np_anova(ordered_vars, predictor_vars, uvw)
}
\arguments{
\item{ordered_vars}{a data frame for the ordered variables being assessed.}

\item{predictor_vars}{a data frame for the un-ordered variables being
assessed.}

\item{uvw}{the degree being assessed. This should be a vector with length
equal the number of elements in the \code{ordered_vars} data frame.}
}
\value{
Where there is only one ordered variable, the function returns a type III
ANOVA table to test for differences of order \code{uvw} across the levels of
the \code{predictor_vars}.

Where there is more than one ordered variable, the function returns a type
III ANOVA table to test for differences in generalised correlations of order
\code{uvw} across the levels of the \code{predictor_vars}.
}
\description{
\code{np_anova} performs a nonparametric ANOVA.
}
\details{
Nonparametric ANOVA is a methodology that is applicable where one as an
ordered response variable as well as both ordered response and predictor
variables.
}
\examples{
attach(jam)
np_anova(ordered_vars = sweetness, predictor_vars = data.frame(type,judge), uvw = 1)
}
\references{
Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
}
