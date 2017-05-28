# This file contains functions for manipulating information edpclient stores on
# properties of data frames used for creating populations and returned from
# select and simulate.

ALL_STAT_TYPES <- c("void", "realAdditive", "realMultiplicative", "proportion",
                    "categorical", "unboundedCount", "sequence")

identifying <- function(col) {
  isTRUE(attr(col, "identifying"))
}

`identifying<-` <- function(col, value) {
  stopifnot(is.logical(value) && length(value) == 1)
  attr(col, "identifying") <- value
  return(col)
}

identifying_columns <- function(data) {
  mask <- vapply(names(data), function(x) identifying(data[[x]]), NA)
  names(data)[mask]
}

`identifying_columns<-` <- function(data, value) {
  stopifnot(all(value %in% names(data)))
  for (n in names(data)) {
    identifying(data[[n]]) <- (n %in% value)
  }
  return(data)
}

display_names <- function(data) {
  stopifnot(is.data.frame(data))
  vapply(names(data),
         function(x) ifelse(is.null(display_name(data[[x]])), NA_character_,
                            display_name(data[[x]])),
         NA_character_)
}

`display_names<-` <- function(data, value) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(value))
  stopifnot(ncol(data) == length(value))
  for (i in 1:ncol(data)) {
    display_name(data[[i]]) <- value[i]
  }
  return(data)
}

display_name <- function(col) {
  dn <- attr(col, "display_name")
  ifelse(is.null(dn), NA_character_, dn)
}

`display_name<-` <- function(col, value) {
  stopifnot(is.character(value) && length(value) == 1)
  attr(col, "display_name") <- value
  return(col)
}

stat_type <- function(col) {
  st <- attr(col, "stat_type")
  ifelse(is.null(st), NA_character_, st)
}

`stat_type<-` <- function(col, value) {
  stopifnot(length(value) == 1 && value %in% ALL_STAT_TYPES)
  attr(col, "stat_types") <- value
  return(col)
}
