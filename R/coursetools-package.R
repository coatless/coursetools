#' @importFrom magrittr %>%
"_PACKAGE"

# Hack courtesy of
# https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# This quiets the concerns of R CMD check
# e.g. "no visible binding for global variable '.'"
# re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

