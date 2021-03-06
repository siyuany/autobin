% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iv.R
\name{iv}
\alias{iv}
\title{Information Value}
\usage{
iv(target, groups)
}
\arguments{
\item{target}{the objective variable, which should be binary, i.e., have only two values.}

\item{groups}{the discrete predictor.}
}
\value{
\code{iv} a non-negative numeric.
}
\description{
This function calculates the information value of a discrete predictor on an
objective variable.
}
\examples{
library(ISLR)
data(Default)
with(Default, iv(default, student))

}
\author{
Siyuan Yao
}
