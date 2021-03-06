% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin.R
\name{bin}
\alias{bin}
\alias{bin.default}
\alias{bin.data.frame}
\title{Binning Continous Varaibles Based on Information value}
\usage{
bin(...)

\method{bin}{default}(target, predictor, nbin = 5,
  early_stop_threshold = NA, min.node.pct = 0.05, p = 0.05,
  single.values = NULL, ...)

\method{bin}{data.frame}(df, y, x, ...)
}
\arguments{
\item{target}{the response variable}

\item{predictor}{the continous variable to bin}

\item{nbin}{numbers of binning}

\item{early_stop_threshold}{iv increasing value less than the threshold will
cause the binning process stopped. If not NA, \code{nbin} will be ignored.}

\item{min.node.pct}{the smallest sample proportion of the bins}

\item{p}{p value used for fisher test rejection}

\item{single.values}{some values will be split as a level}

\item{df}{a data frame}

\item{y}{name of target variable}

\item{x}{name of predictor}
}
\value{
a \code{bin} object

\code{cuts} the cut points
\code{IV} the information value
\code{WOE} the weight of evidence tagble
}
\description{
\code{bin} bins a continous variable by maximizing the infomation value.
}
\section{Methods (by class)}{
\itemize{
\item \code{default}: bin.default

\item \code{data.frame}: bin.data.frame
}}

