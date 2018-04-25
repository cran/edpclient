# Map an EDP resource URL to its id. See test_id_from_url for examples.
id_from_url <- function(url, expected_prefix) {
  parts <- strsplit(url, "/")[[1]]
  if (!grepl("https?:", parts[[1]])) {
    stop("'", url, "' does not look like an URL")
  }
  id <- parts[length(parts)]
  if (!startsWith(id, expected_prefix)) {
    stop("'", id, "' does not start with '", expected_prefix, "'")
  }
  return(id)
}

# Given a numeric vector, guess either realAdditive or realMultiplicative.
guess_numeric_stat_type <- function(x) {
  # If someone put an explicit stat type on `x`, use it.
  if (!is.na(stat_type(x))) {
    return(stat_type(x))
  }
  # If `x` has any non-positive values or fewer than four values, always use
  # realAdditive.
  x <- sort(as.vector(na.exclude(x)))
  if (any(x <= 0) || length(x) < 4) {
    return("realAdditive");
  }
  # Compute the correlation of `x` with normal and lognormal quantiles. If it
  # matches lognormal better than normal, and both aren't greater than 0.95,
  # use realMultiplicative. We have the cutoff of 0.95 as heuristic for when
  # both are good.
  q <- qnorm(seq_along(x) / (length(x) + 1))
  q_cor <- cor(q, x)
  lq_cor <- cor(q, log(x))
  debug_message("normal Q-Q cor:", q_cor, "log-normal Q-Q cor:", lq_cor)
  return(ifelse(lq_cor > q_cor && q_cor < 0.95,
                "realMultiplicative", "realAdditive"))
}

# Given a data frame, `data`, generate a list with `data` and `schema` elements
# to be posted to /rpc/population to create a population with.
data_frame_to_json <- function(data) {
  j <- list(num_rows = nrow(data), columns = list())  # in-progress data JSON
  s <- list(columns = list())                         # in-progress schema JSON
  if (length(identifying_columns(data)) > 0) {
    s[["identifying_columns"]] <- as.list(identifying_columns(data))
  }
  # Iterate over columns in `data` and add them to `j` and `s`.
  for (name in names(data)) {
    schema_column <- list(name = name)  # to add to `s`
    if (identifying(data[[name]]) || all(is.na(data[[name]])) ||
        isTRUE(stat_type(data[[name]]) == "void")) {
      schema_column[["stat_type"]] <- "void"
      values <- as.list(as.character(data[[name]]))
    } else if (methods::is(data[[name]], "POSIXt") ||
               methods::is(data[[name]], "Date")) {
      lt <- as.POSIXlt(data[[name]])
      if (any(lt$sec != 0 | lt$min != 0 | lt$hour != 0)) {
        # Values specified to greater than day precision.
        schema_column[["stat_type"]] <- "time"
        secs <- as.POSIXct(data[[name]], origin = "1970-01-01", tz = "UTC")
        values <- as.list(as.integer(secs))
      } else {
        schema_column[["stat_type"]] <- "date"
        days <- as.Date(data[[name]], origin = "1970-01-01", tz = "UTC")
        values <- as.list(as.integer(days))
      }
    } else if (is.numeric(data[[name]])) {
      schema_column[["stat_type"]] <- guess_numeric_stat_type(data[[name]])
      if (!is.null(precision(data[[name]]))) {
        schema_column[["precision"]] <- precision(data[[name]])
      } else {
        # Guessing precision is limited to setting the precision to 1/1 if all
        # values are integers with absolute value smaller than 1000.
        v <- as.vector(na.omit(data[[name]]))
        if (all(round(v) == v) && all(abs(v) < 1000)) {
          schema_column[["precision"]] <- c(1, 1)
        }
      }
      # https://github.com/jeroenooms/jsonlite/issues/169
      # If it weren't for that, we could just write `values <- data[[name]]`.
      # Note that class(NA) == "logical". This doesn't work for real NA_real_.
      values <- lapply(data[[name]], function(x) ifelse(is.na(x), NA, x))
    } else if (is.logical(data[[name]])) {
      schema_column[["stat_type"]] <- "categorical"
      schema_column[["values"]] <- list(list(value = "FALSE"),
                                        list(value = "TRUE"))
      values <- as.list(as.character(data[[name]]))
    } else if (is.factor(data[[name]])) {
      nvalues <- length(levels(data[[name]]))
      if (nvalues > 100) {
        stop("column ", name, " has ", nvalues, " values.")
      }
      schema_column[["stat_type"]] <-
          ifelse(is.ordered(data[[name]]), "orderedCategorical", "categorical")
      schema_column[["values"]] <- lapply(levels(data[[name]]),
                                          function(x) list(value = x))
      values <- as.list(as.character(data[[name]]))
    } else if (isTRUE(stat_type(data[[name]]) == "void")) {
      # If the user asked not to model the column, even if we don't know what
      # it is, try to stringify it and move on.
      schema_column[["stat_type"]] <- "void"
      values <- as.list(as.character(data[[name]]))
    } else {
      stop(sprintf("unknown type for %s: %s", name, class(data[[name]])))
    }
    dn <- display_name(data[[name]])
    if (!is.na(dn)) {
      schema_column[["display_name"]] <- dn
    }
    desc <- attr(data[[name]], "description")
    if (!is.null(desc)) {
      schema_column[["description"]] <- desc
    }
    s[["columns"]] <- append(s[["columns"]], list(schema_column))
    j[["columns"]][[name]] <- values
    stopifnot(length(values) == j[["num_rows"]])
  }
  list(schema = s, data = j)
}

create_population <- function(sess, data, name) {
  stopifnot(is.edp_session(sess))
  stopifnot(is.data.frame(data))
  stopifnot(is.character(name) && !is.na(name[1]) && length(name) == 1)
  post_data <- data_frame_to_json(data)
  req <- list(name = name, data = post_data[["data"]],
              schema = post_data[["schema"]])
  if (!is.null(getOption(
      "edp_this_is_a_lot_of_data_but_i_know_what_im_doing"))) {
    req[["this_is_a_lot_of_data_but_i_know_what_im_doing"]] <- TRUE
  }
  resp <- edp_post(sess, "rpc/population", req)
  pid <- id_from_url(httr::headers(resp)$location, "p-")
  return(population(sess, pid))
}
