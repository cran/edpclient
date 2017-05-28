library("edpclient")

test_identifying_columns <- function() {
  d <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))

  # No identifying columns yet.
  stopifnot(!identifying(d$a))
  stopifnot(length(identifying_columns(d)) == 0)

  # Mark "a" identifying and check that that is reflected.
  identifying(d$a) <- TRUE
  stopifnot(identifying(d$a))
  stopifnot(isTRUE(all.equal(identifying_columns(d), "a")))

  # Mark the set c("b") identifying and check that that is reflected.
  identifying_columns(d) <- c("b")
  stopifnot(!identifying(d$a))
  stopifnot(identifying(d$b))
  stopifnot(isTRUE(all.equal(identifying_columns(d), "b")))
}

test_display_names <- function() {
  d <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))

  # No display names.
  stopifnot(is.na(display_name(d$a)))
  stopifnot(isTRUE(all.equal(display_names(d),
                             c(a = NA_character_, b = NA_character_,
                               c = NA_character_))))

  # Set a display name on "a".
  display_name(d$a) <- "AA"
  stopifnot(display_name(d$a) == "AA")
  stopifnot(isTRUE(all.equal(display_names(d), c(a = "AA", b = NA, c = NA))))

  # Set display names as a vector.
  display_names(d) <- c("A2", NA, "C2")
  stopifnot(display_name(d$a) == "A2")
  stopifnot(isTRUE(all.equal(display_names(d), c(a = "A2", b = NA, c = "C2"))))
}

test_stat_types <- function() {
  d <- data.frame(a = c(1, 2))
  stat_type(d$a) <- "realAdditive"
  stopifnot(stat_type(d$a) == "realAdditive")
}

test_identifying_columns()
test_display_names()
test_stat_types()
