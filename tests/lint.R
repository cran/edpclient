library("lintr")

# Passing a path is a little gross. This is the path to the source when running
# `R CMD check`.
#
# We have to exclude "object_usage_linter" because of
# https://github.com/jimhester/lintr/issues/27.
linter_mask <- names(lintr::default_linters) != "object_usage_linter"
errors <- lint_package("../00_pkg_src/edpclient",
                       linters = lintr::default_linters[linter_mask])
print(errors)
stopifnot(length(errors) == 0)
