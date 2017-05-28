\name{col_assoc}
\alias{col_assoc}
\title{Compute column associations}
\description{Computes a matrix of associations (like R-squared or mutual
    information) of a collection of columns.}
\usage{
col_assoc(pm, target = NULL, given = NULL,
          statistic = c("mutual information", "R squared", "classic dep prob"))
}
\arguments{
  \item{pm}{an edp_population_model object, from \code{popmod}}
  \item{target}{an optional list of column names, defaults to all columns}
  \item{given}{a list of values to condition on; \code{list(a = 3)}
      means to compute the association conditional on \code{a} being 3.}
  \item{statistic}{the name of the statistic to compute}
}
\value{
a square matrix of column associations, where the edge length is
\code{length(target)}
}
