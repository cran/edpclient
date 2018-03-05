\name{schema}
\alias{schema}
\title{Return the schema of a population or population model}
\description{Returns the schema of an EDP population or population model as a data.frame.}
\usage{
schema(x)
}
\arguments{
  \item{x}{an EDP population from \code{population(...)} or an EDP population
      model from \code{popmod(...)}}
}
\value{
  A data.frame with columns \code{name}, \code{display_name}, \code{stat_type},
  and \code{nvalues}. \code{nvalues} is NA unless \code{stat_type} is a
  categorical type.
}
