# Print message if someone has run `options(edp_debug = TRUE)`.
debug_message <- function(...) {
  if (isTRUE(getOption("edp_debug"))) {
    message(paste(...))
  }
}

# If `httr_response` indicates a failure, stop with a useful message.
stop_if_unsuccessful <- function(httr_response, http_method, url) {
  code <- httr::status_code(httr_response)
  if (!(200 <= code && code < 300)) {
    stop(sprintf("HTTP status %d from %s %s [%s]", code, http_method, url,
                 httr::content(httr_response, as = "text")),
         call. = FALSE)
  }
}

# GETs <edp_base_url>/<op> with a JWT and returns the httr response.
edp_get <- function(sess, op) {
  url <- paste(sess$edp_url, op, sep = "/")
  debug_message("GET", url)
  bearer <- paste("Bearer", sess$bearer_token, sep = " ")
  resp <- httr::GET(url, httr::add_headers("Authorization" = bearer))
  debug_message("GET response", httr::status_code(resp),
                httr::content(resp, "text", encoding = "UTF-8"))
  stop_if_unsuccessful(resp, "GET", url)
  return(resp)
}

# POSTs `body`, which should be convertible to JSON, to <edp_base_url>/<op>
# with a JWT and returns the httr response.
edp_post <- function(sess, op, body) {
  url <- paste(sess$edp_url, op, sep = "/")
  debug_message("POST", url, jsonlite::toJSON(body, auto_unbox = TRUE))
  bearer <- paste("Bearer", sess$bearer_token, sep = " ")
  resp <- httr::POST(url, body = body, encode = "json",
                     httr::add_headers("Authorization" = bearer))
  debug_message("POST response", httr::status_code(resp),
                httr::content(resp, "text", encoding = "UTF-8"))
  stop_if_unsuccessful(resp, "POST", url)
  return(resp)
}

format_utc_time <- function(seconds) {
  v <- as.POSIXct(seconds, origin = "1970-01-01", tz = "GMT")
  is.na(v) <- (seconds == 0)
  return(v)
}

# Constructor for edp_session objects. See ?edpclient::edp_session.
edp_session <- function(profile = "default") {
  edp_auth <- file.path(Sys.getenv("HOME"), ".edp_auth")
  config <- ini::read.ini(edp_auth)[[profile]]
  if (is.null(config$edp_url) || is.null(config$bearer_token)) {
    stop(sprintf("profile %s needs 'edp_url' and 'bearer_token'", profile))
  }
  class(config) <- "edp_session"
  # Make sure our JWT works.
  edp_get(config, "auth/username")
  return(config)
}

print.edp_session <- function(x, ...) {
  cat(sprintf("edp_session(url=%s)\n", x$edp_url))
}

is.edp_session <- function(x) {
  return(class(x) == "edp_session")
}

popmods <- function(x) UseMethod("popmods", x)

# Exported; see ?edpclient::popmods.
popmods.edp_session <- function(x) {
  resp <- httr::content(edp_get(x, "rpc/population_model"))
  # `resp` is a list of #/definitions/population_model in edp.schema.json.
  plyr::ldply(resp, function(pm) data.frame(
      stringsAsFactors = FALSE,
      name = pm$name,
      id = pm$id,
      parent_id = ifelse(is.null(pm$parent_id), as.character(NA),
                         pm$parent_id),
      creation_time = format_utc_time(pm$creation_time),
      build_status = pm$build_progress$status))
}

populations <- function(sess) {
  stopifnot(is.edp_session(sess))
  resp <- httr::content(edp_get(sess, "rpc/population"))
  plyr::ldply(resp, function(x) data.frame(
      stringsAsFactors = FALSE, name = x$name, id = x$id,
      nmodels = length(x$models),
      creation_time = format_utc_time(x$creation_time)))
}