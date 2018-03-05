# popmod.R contains functions for creating and operating on
# edp_population_model objects.

print.edp_population_model <- function(x, ...) {
  cat(sprintf("popmod(id=%s, name=%s, #cols=%d, parent_id=%s, %s)\n", x$id,
              x$name, length(x$schema$columns), x$parent_id, x$build_status))
}

# Exported. Returns a list of the columns of edp_population_model `x`.
names.edp_population_model <- function(x, ...) {
  vapply(x$schema$columns, function(c) c$name, "")
}

as.character.edp_population_model <- function(x, ...) {
  x$id
}

# Returns a character vector with the names of all modeled columns of `pm`.
modeled_names <- function(pm) {
  s <- Filter(function(e) e$stat_type != "void", pm$schema$columns)
  return(vapply(s, function(e) e$name, ""))
}

# Returns the schema JSON for population `pid`.
schema_json <- function(sess, pid) {
  stopifnot(is.edp_session(sess))
  stopifnot(is.character(pid) && startsWith(pid, "p-"))
  op <- paste("rpc/population", utils::URLencode(pid), "schema",
              sep = "/")
  schema <- try(httr::content(edp_get(sess, op)), silent = TRUE)
  if (class(schema) == "try-error") {
    stop(sprintf("Could not load schema for population '%s'.", pid))
  }
  schema
}

# Exported; see ?edpclient::popmod.
popmod <- function(sess, pmid) {
  # `pm_resp` is a #/definitions/population_model in edp.schema.json.
  op <- paste("rpc/population_model", utils::URLencode(pmid), sep = "/")
  pm_resp <- try(httr::content(edp_get(sess, op)), silent = TRUE)
  if (class(pm_resp) == "try-error") {
    stop(sprintf("Could not load population model for '%s'.", pmid))
  }
  pm <- list(sess = sess, id = pmid, name = pm_resp$name,
             parent_id = pm_resp$parent_id,
             schema = schema_json(sess, pm_resp$parent_id),
             build_status = pm_resp$build_progress$status)
  class(pm) <- "edp_population_model"
  return(pm)
}

is.popmod <- function(x) {
  "edp_population_model" %in% class(x)
}

schema_data_frame_from_json <- function(j) {
  extract_schema_row <- function(e) {
    c(name = e$name,
      display_name = ifelse(is.null(e$display_name), as.character(NA),
                            e$display_name),
      stat_type = e$stat_type,
      nvalues = length(e$values))
  }
  s <- plyr::ldply(j$columns, extract_schema_row)
  s$nvalues <- as.integer(s$nvalues)
  is.na(s$nvalues) <- (s$nvalues == 0)
  return(s)
}

schema <- function(x) {
  UseMethod("schema", x)
}

schema.edp_population <- function(x) {
  # This makes a network call every time it's called, but the population model
  # one doesn't. This is a little weird.
  schema_data_frame_from_json(schema_json(x$sess, x$pid))
}

schema.edp_population_model <- function(x) {
  schema_data_frame_from_json(x$schema)
}

# Converts a list, which may contain NULLs, to an atomic vector with the same
# elements, replacing NULLs with NAs.  `mode` should be "numeric",
# "character", or "logical". `v` is a list. This function is useful for
# converting from JSON arrays parsed by jsonlite.
unlist_with_na <- function(v, mode) {
  this_NA <- as.vector(NA, mode = mode)
  v_is_null <- vapply(v, is.null, NA)
  v_with_na <- ifelse(v_is_null, this_NA, v)
  return(as.vector(v_with_na, mode = mode))
}

# Given a list structured as result_set.schema.json, returns a data frame with
# the columns of the result set. `target` is the column names for the data
# frame in the correct order (which can be mangled by JSON iteration order).
# `schema` should be the schema for the population model `rs` came from,
# defined in population_schema.schema.json.
result_set_to_data_frame <- function(rs, target, schema) {
  stopifnot(isTRUE(all.equal(sort(names(rs$columns)), sort(target))))
  # If there are rowids (from a select), use them. Otherwise, use the usual
  # 1-based row names. We specify the default row names explicitly to force the
  # initially-empty data.frame to have the right number of rows.
  rowids <- if (is.null(rs$rowids)) seq_along(rs$columns[[1]]) else rs$rowids
  table <- data.frame(stringsAsFactors = FALSE, row.names = rowids)
  # Iterate through columns of the result set, adding them to `table`.
  for (col_name in target) {
    # Find the schema entry for the column named `col_name`.
    col_schema <- Filter(function(e) e$name == col_name, schema$columns)[[1]]
    st <- col_schema$stat_type
    stopifnot(length(st) == 1)
    # Convert the list in the result set to a suitable atomic vector.
    if (st %in% c("realAdditive", "realMultiplicative")) {
      table[[col_name]] <- unlist_with_na(rs$columns[[col_name]], "numeric")
    } else if (st %in% c("categorical", "orderedCategorical")) {
      v <- unlist_with_na(rs$columns[[col_name]], "character")
      levels <- vapply(col_schema$values, function(e) e$value, "")
      table[[col_name]] <- factor(v, levels = levels,
                                  ordered = (st == "orderedCategorical"))
    } else if (st == "date") {
      days <- unlist_with_na(rs$columns[[col_name]], "integer")
      # Round because we can't get exactly the right difftime due to DST.
      table[[col_name]] <- round.POSIXt(
          as.POSIXlt("1970-01-01 UTC") + as.difftime(days, units = "days"),
          units = "days")
    } else if (st == "void") {
      table[[col_name]] <- unlist_with_na(rs$columns[[col_name]], "character")
    } else {
      stop(sprintf("unsupported stat type '%s'", st))
    }
    # Pull identifying-column-ness and display name from schema.
    if (col_name %in% schema$identifying_columns) {
      identifying(table[[col_name]]) <- TRUE
    }
    if (!is.null(col_schema$display_name)) {
      display_name(table[[col_name]]) <- col_schema$display_name
    }
    stat_type(table[[col_name]]) <- col_schema$stat_type
  }
  return(table)
}

select.edp_population_model <- function(x, target = NULL, where = NULL,
                                        rowids = NULL) {
  select(population(x$sess, x$parent_id),
         target = target, where = where, rowids = rowids)
}

make_simulate_req <- function(pm, nsim, seed, target, given) {
  stopifnot(is.null(seed) || is.numeric(seed))
  if (is.null(target)) {
    target <- modeled_names(pm)
  }
  for (g in names(given)) {
    if (!(g %in% modeled_names(pm))) {
      stop(sprintf("Given column '%s' is not modeled.", g))
    }
  }
  if (!is.null(given)) {
    if (!is.data.frame(given)) {
      stop("`given` must be NULL or a data frame.")
    }
    if (nrow(given) != 1) {
      stop("`given`, if set, must have one row.")
    }
  }
  req <- list(target = as.list(target), n = nsim)
  if (!is.null(seed)) {
    req$random_seed <- as.integer(seed)  # seed could be a non-integer number
  }
  if (!is.null(given)) {
    req$given <- given
  }
  return(req)
}

make_simulate_by_model_result <- function(schema, target, resp) {
  # simulate-by-model returns one result set per model. Turn them into data
  # frames with a `model` attribute and stack them.
  model_data_frames <- list()
  for (i in 1:length(resp)) {
    stopifnot(length(resp[[i]]) == 1)  # zero or one given row
    d <- result_set_to_data_frame(resp[[i]][[1]], target, schema)
    d$model <- i
    model_data_frames[[i]] <- d
  }
  all_data <- do.call(rbind, model_data_frames)
  all_data$model <- as.factor(all_data$model)  # not ordered
  return(all_data)
}

# Exported; see ?edpclient::simulate.edp_population_model.
simulate.edp_population_model <- function(object, nsim = 1, seed = NULL, ...,
                                          target = NULL, columns = NULL,
                                          given = NULL, by_model = FALSE) {
  pm <- object  # Give the standard argument a better name.
  # `columns` is the R-idiomatic name. `target` is the EDP-idiomatic name.
  if (is.null(target) && !is.null(columns)) {
    target <- columns
  }
  req <- make_simulate_req(pm, nsim, seed, target, given)
  target <- as.character(req$target)  # i.e. not a `list`
  op <- paste("rpc/population_model", utils::URLencode(pm$id),
              ifelse(by_model, "simulate_by_model", "simulate"), sep = "/")
  resp <- httr::content(edp_post(pm$sess, op, req))
  if (by_model) {
    return(make_simulate_by_model_result(pm$schema, target, resp))
  } else {
    # simulate not-by-model returns one result set per given, and we passed at
    # most one given.
    stopifnot(length(resp) == 1)
    return(result_set_to_data_frame(resp[[1]], target, pm$schema))
  }
}

# Exported; see ?edpclient::predict.edp_population_model.
predict.edp_population_model <- function(object, ...,
                                         target = NULL,
                                         rowids = NULL, infer_present = FALSE,
                                         seed = NULL) {
  stopifnot(is.null(seed) || is.numeric(seed))
  stopifnot(is.null(rowids) || is.numeric(rowids))
  pm <- object  # Give the standard argument a better name.
  if (is.null(target)) {
    target <- modeled_names(pm)
  }
  # If we don't specify rowids, use the magic "all" token
  if (is.null(rowids)) {
    rowids <- "all"
  }
  op <- paste("rpc/population_model", utils::URLencode(pm$id), "infer_observed",
              sep = "/")
  req <- list(target = as.list(target), rowids = rowids,
              infer_present = infer_present)
  if (!is.null(seed)) {
    req$seed <- as.integer(seed)  # seed could be a non-integer number
  }
  resp <- httr::content(edp_post(pm$sess, op, req))
  # TODO(asilvers): This is ignoring the inferred confidence.
  return(result_set_to_data_frame(resp, target, pm$schema))
}

joint_probability <- function(pm, target, log = FALSE) {
  stopifnot(is.popmod(pm))
  stopifnot(is.data.frame(target))
  op <- paste("rpc/population_model", utils::URLencode(pm$id), "logpdf_rows",
              sep = "/")
  resp <- edp_post(pm$sess, op, list(targets = target))
  lp <- as.numeric(httr::content(resp))
  if (log) {
    return(lp)
  } else {
    return(exp(lp))
  }
}

# Returns element (i, j) of the lower-triangular matrix defined by the
# row-major vector `elems`. This layout is specified by
# association_matrix.schema.json. `i` and `j` may be vectors of compatible
# length; in that case, a vector of elements at those index-pairs is returned.
get_lower_tri_elem <- function(elems, i, j) {
  # Suppose the matrix is 4x4. `elems` fills out the Xs of this triangle:
  #
  #   X...
  #   XX..
  #   XXX.
  #   XXXX
  #
  # Then, suppose we want the index of element (i, j) = (3, 2), marked C:
  #
  #   A...
  #   AA..
  #   .C..
  #   ....
  #
  # i * (i - 1) / 2 = 3 * (3 - 1) / 2 = 3 is the number of elements in the
  # triangle above row 3. So, the index into `elems` where you find element
  # (3, 2) is i * (i - 1) / 2 + j.
  return(elems[i * (i - 1) / 2 + j])
}

# Exported; see ?edpclient::col_assoc.
col_assoc <- function(
    pm, target = NULL, given = NULL,
    statistic = c("mutual information", "R squared", "classic dep prob"),
    seed = NULL) {
  statistic <- match.arg(statistic)
  stopifnot(is.null(seed) || is.numeric(seed))
  op <- paste("rpc/population_model", utils::URLencode(pm$id),
              "relation", sep = "/")
  if (is.null(target)) {
    target <- modeled_names(pm)
    if (!is.null(given)) {
      target <- target[!(target %in% names(given))]
    }
  }
  if (!all(names(given) %in% names(pm))) {
    stop("Not all given columns are in population model.")
  }
  if (any(names(given) %in% target)) {
    stop("Can't compute association of givens.")
  }
  req <- list(response_columns = as.list(target),
              predictor_columns = as.list(target),
              given = given, statistic = statistic)
  if (!is.null(seed)) {
    req$random_seed <- as.integer(seed)
  }
  resp <- edp_post(pm$sess, op, req)
  # `elems` is a row-major lower-triangular matrix, as specified in
  # association_matrix.schema.json. Use it to fill in the regular association
  # matrix `a`.
  elems <- unlist_with_na(httr::content(resp)$elements, "numeric")
  a <- matrix(nrow = length(target), ncol = length(target),
              dimnames = list(target, target))
  for (i in seq_along(target)) {
    j <- seq(1, i)
    v <- get_lower_tri_elem(elems, i, j)  # row `i` of `elems`.
    a[i, j] <- v
    a[j, i] <- v
  }
  return(a)
}

build_popmod <- function(pop, name, models = 16, builder = "lazy",
                         iterations = 10) {
  stopifnot(is.population(pop))
  stopifnot(is.character(name) && !is.na(name[1]) && length(name) == 1)
  url <- paste("rpc/population", utils::URLencode(pop$pid), "build", sep = "/")
  req <- list(name = name,
              build_def = list(num_models = models, builder = builder,
                               duration = list(iterations = iterations)))
  resp <- edp_post(pop$sess, url, req)
  pmid <- id_from_url(httr::headers(resp)$location, "pm-")
  popmod(pop$sess, pmid)
}

# Returns the build progress for the population model `pm`. See
# the /definitions/build_progress object in edp.schema.json.
build_progress <- function(pm) {
  op <- paste("rpc/population_model", utils::URLencode(pm$id), sep = "/")
  return(httr::content(edp_get(pm$sess, op))$build_progress)
}

# Waits until `pm` is built (or failed to build) using the server-side wait
# endpoint, timing out after `seconds` seconds.
wait_quietly <- function(pm, seconds) {
  start_secs <- unclass(Sys.time())
  remaining <- seconds
  while (remaining > 0) {
    op <- paste("rpc/population_model", utils::URLencode(pm$id), "wait",
                sep = "/")
    req <- list(seconds = min(600, remaining))
    prog <- httr::content(edp_post(pm$sess, op, req))
    if (!(prog$status %in% c("unbuilt", "in_progress"))) {
      break
    }
    remaining <- start_secs + seconds - unclass(Sys.time())
  }
  pm$build_status <- prog$status
  return(pm)
}

# Waits until `pm` is built (or failed to build) while updating a progress bar.
wait_noisily <- function(pm) {
  poll_secs <- 2
  not_started_message_printed <- FALSE
  # This outer loop either exits immediately or prints a message saying that
  # we're waiting for the build to start, then waits until it starts.
  while (TRUE) {
    prog <- build_progress(pm)
    if (prog$status %in%
        c("inconsistent", "built", "canceled", "cancel_requested", "failed")) {
      pm$build_status <- prog$status
      message(paste(pm$sess$edp_url, "explorer", "explain",
              utils::URLencode(pm$id), sep = "/"))
      return(pm)
    } else if (prog$status == "unbuilt") {
      if (!not_started_message_printed) {
        cat("waiting for build to start...")
        not_started_message_printed <- TRUE
      }
      Sys.sleep(poll_secs)
    } else if (prog$status == "in_progress") {
      # This inner loop creates a progress bar and updates it every few
      # seconds, exiting when the build is no longer in progress for any
      # reason.
      if (not_started_message_printed) {
        cat("build started\n")
      }
      stopifnot(0 <= prog$fraction_done && prog$fraction_done <= 1.0)
      pb <- utils::txtProgressBar(min = 0, max = 1,
                                  initial = prog$fraction_done, style = 3)
      tryCatch({
        while (prog$status == "in_progress") {
          utils::setTxtProgressBar(pb, prog$fraction_done)
          Sys.sleep(poll_secs)
          prog <- build_progress(pm)
        }
      }
      , finally = close(pb))
      # The build is no longer in progress. Drop out to the outer loop, which
      # will exit the next time through.
    } else {
      stop("unknown build status: ", prog$status)
    }
  }
}

wait_for <- function(pm, quiet = FALSE, seconds = Inf) {
  stopifnot(is.popmod(pm))
  if (quiet) {
    pm <- wait_quietly(pm, seconds)
  } else {
    if (seconds != Inf) {
      stop("`seconds` is not compatible with `quiet = FALSE`.")
    }
    pm <- wait_noisily(pm)
  }
  return(pm)
}
