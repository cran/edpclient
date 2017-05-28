\name{select}
\alias{select}
\title{Fetch columns from a population model}
\description{Fetch specified columns from a population model. This is the data
the population model is trained on.}
\usage{
select(pm, target = NULL, where = NULL, rowids = NULL)
}
\arguments{
  \item{pm}{an EDP population model, from \code{popmod(...)}}
  \item{target}{a character vector of column names, defaults to
      \code{names(pm)} minus \code{names(where)}}
  \item{where}{a list of conditions to select on; \code{list(a = "x", c = 3)}
      means only to return rows where \code{a} is "x" and \code{c} is 3.}
  \item{rowids}{a list of integer row ids to return. These row ids match the
      row names returned by \code{select}. \code{rowids} is exclusive
      with \code{where}.}
}
\value{
  a data frame with the columns specified in \code{target}.
}
\examples{
  \dontrun{
    # similar to SQL "SELECT a, b FROM pm WHERE c = 4;"
    select(pm, c("a", "b"), where = list(c = 4))

    select(pm, c("a", "b"), rowids = c(7, 22))
  }
}
