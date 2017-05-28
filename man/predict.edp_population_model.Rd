\name{predict.edp_population_model}
\alias{predict.edp_population_model}
\title{Predict values from a population model}
\description{Predict values for the columns in \code{target} for \code{rowids}
given the other values in that row.}
\usage{
\method{predict}{edp_population_model}(object, ...,
        target = NULL, rowids = NULL,
        infer_present = FALSE, seed = NULL)
}
\arguments{
  \item{object}{an EDP population model, from \code{popmod(...)}}
  \item{...}{ignored, accepted for compatibility with \code{predict}}
  \item{target}{a character vector of column names, defaults to
      \code{names(pm)} minus \code{names(where)}}
  \item{rowids}{a integer vector of rowids, defaults to all}
  \item{infer_present}{If TRUE, act as though each column in \code{targets} is
      missing across all rows and so infer a value for every row. Otherwise
      only infer values which were missing in the original data.}
  \item{seed}{if set, an integer to pass to EDP as a random seed for this call}
}
\value{
  a data frame with the columns specified in \code{target}.
}
\examples{
  \dontrun{
    predict(pm, target = c("INCOME"), rowids = c(3, 7))
  }
}
