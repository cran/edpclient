# Check `data_frame_to_json`, the main function used by `create_population`,
# without actually calling `create_population`.

library("edpclient")

test_that("ids can be extracted from URLs",  {
  url <- paste("https://betaplatform.empirical.com/rpc/population_model/",
               "pm-b4my56cirthgwaze", sep = "")
  expect_equal(edpclient:::id_from_url(url, "pm-"), "pm-b4my56cirthgwaze")
  expect_error(edpclient:::id_from_url(url, "p-"), "does not start with 'p-'")
  expect_error(edpclient:::id_from_url("arglebargle", "pm-"),
               "does not look like a URL")
})

test_that("numeric stat types are guessed", {
  # Draw from N(2,1) and clip off the non-positive values to generate a vector
  # that is really obviously additive, but not an error to model as
  # multiplicative.
  set.seed(17)
  x <- rnorm(1000, 2, 1)
  x <- x[x > 0]
  # Check that x is additive, but exp(x) is multiplicative.
  expect_equal(edpclient:::guess_numeric_stat_type(x), "realAdditive")
  expect_equal(edpclient:::guess_numeric_stat_type(exp(x)),
               "realMultiplicative")
})

test_that("numeric columns are in POST data and schema", {
  d <- data.frame(x = c(1, 2, NA))
  expected_data <- list(num_rows = 3, columns = list(x = list(1, 2, NA)))
  expected_schema <- list(columns = list(list(name = "x",
                                              stat_type = "realAdditive")))
  post_data <- edpclient:::data_frame_to_json(d)
  expect_equal(expected_data, post_data[["data"]])
  expect_equal(expected_schema, post_data[["schema"]])
})

test_that("categorical columns are in POST data and schema", {
  d <- data.frame(x = c("a", "b", "a", NA))
  expected_data <- list(num_rows = 4,
                        columns = list(x = list("a", "b", "a", NA_character_)))
  expected_schema <- list(columns = list(
      list(name = "x", stat_type = "categorical",
           values = list(list(value = "a"), list(value = "b")))))
  post_data <- edpclient:::data_frame_to_json(d)
  expect_equal(expected_data, post_data[["data"]])
  expect_equal(expected_schema, post_data[["schema"]])
})

test_that("logical columns are in POST data and schema", {
  d <- data.frame(x = c(TRUE, FALSE, NA))
  expected_data <- list(num_rows = 3,
                        columns = list(x = list("TRUE", "FALSE",
                                                NA_character_)))
  expected_schema <- list(columns = list(
      list(name = "x", stat_type = "categorical",
           values = list(list(value = "FALSE"), list(value = "TRUE")))))
  post_data <- edpclient:::data_frame_to_json(d)
  expect_equal(expected_data, post_data[["data"]])
  expect_equal(expected_schema, post_data[["schema"]])
})

test_that("unmodeled columns are in POST data and schema", {
  # We don't know what to do with Date objects, but they can be stringified, so
  # you should be able to create a population with them if you mark them
  # "void".
  d <- data.frame(x = c(as.Date("2017-05-18"), NA))
  stat_type(d$x) <- "void"
  expected_data <- list(num_rows = 2,
                        columns = list(x = list("2017-05-18", NA_character_)))
  expected_schema <- list(columns = list(list(name = "x", stat_type = "void")))
  post_data <- edpclient:::data_frame_to_json(d)
  message(expected_data)
  message(post_data[["data"]])
  message(all.equal(expected_data, post_data[["data"]]))
  expect_equal(expected_data, post_data[["data"]])
  expect_equal(expected_schema, post_data[["schema"]])
})

test_that("display names are in POST schema", {
  d <- data.frame(x = c(1, 2))
  display_name(d$x) <- "XX"
  expected_schema <- list(columns = list(list(name = "x",
                                              stat_type = "realAdditive",
                                              display_name = "XX")))
  post_data <- edpclient:::data_frame_to_json(d)
  expect_equal(expected_schema, post_data[["schema"]])
})
