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
    stop("HTTP status ", code, " from ", http_method, " ", url, " [",
         httr::content(httr_response, as = "text"), "]", call. = FALSE)
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

# POSTs (or uses `verb` if specified) to the EDP-relative URL `op` with a JWT.
# `body` is sent as the POST body; it should be convertible to JSON. Returns
# the httr response.
edp_post <- function(sess, op, body, verb = "POST") {
  url <- paste(sess$edp_url, op, sep = "/")
  encoded_body <- jsonlite::toJSON(body, auto_unbox = TRUE, na = "null",
                                   dataframe = "column")
  debug_message(verb, url, encoded_body)
  bearer <- paste("Bearer", sess$bearer_token, sep = " ")
  resp <- httr::VERB(verb, url, body = encoded_body, encode = "raw",
                     httr::add_headers("Content-Type" = "application/json",
                                       "Authorization" = bearer))
  debug_message(verb, "response", httr::status_code(resp),
                httr::content(resp, "text", encoding = "UTF-8"))
  stop_if_unsuccessful(resp, verb, url)
  return(resp)
}

# DELETEs <edp_base_url>/<op>, authenticating with a JWT.
edp_delete <- function(sess, op) {
  url <- paste(sess$edp_url, op, sep = "/")
  debug_message("DELETE", url)
  bearer <- paste("Bearer", sess$bearer_token, sep = " ")
  resp <- httr::DELETE(url, httr::add_headers("Authorization" = bearer))
  debug_message("DELETE response", httr::status_code(resp),
                httr::content(resp, "text", encoding = "UTF-8"))
  stop_if_unsuccessful(resp, "DELETE", url)
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
  config$username <- httr::content(edp_get(config, "auth/username"))
  return(config)
}

print.edp_session <- function(x, ...) {
  cat(sprintf("edp_session(url=%s)\n", x$edp_url))
}

is.edp_session <- function(x) {
  return(class(x) == "edp_session")
}

populations <- function(sess) {
  stopifnot(is.edp_session(sess))
  resp <- httr::content(edp_get(sess, "rpc/population"))
  plyr::ldply(resp, function(x) data.frame(
      stringsAsFactors = FALSE, name = x$name, id = x$id,
      nmodels = length(x$models),
      creation_time = format_utc_time(x$creation_time)))
}
