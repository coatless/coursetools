#' Read Multiple Flat Files Into _R_ Simultaneously
#'
#' Searches for flat files of a specific type and reads them into _R_ to form
#' one large `tibble`
#'
#' @param dir     Location of the files to be read into R.
#' @param pattern The file extension or pattern to match. Default `NULL` returns
#'                all files in a directory.
#' @param delim   The delimiter between successive observations. Default `,`.
#'
#' @return A `tibble` containing all files read into _R_.
#'
#' @examples
#' \dontrun{
#' # Relative path
#' hw03_grades = read_directory("grades/hw/hw03-grades")
#'
#' # Only CSVs
#' hw01_grades = read_directory("grades/hw/hw01-grades", pattern = ".csv", delim = ",")
#' }
read_directory = function(dir, pattern = NULL, delim = ",") {
  files = list.files(dir, pattern = pattern, full.names = TRUE)

  map_dfr(files, ~read_delim(.x, delim = delim))

}

#' Calculate Homework Grades in the style of Compass2g (Blackboard)
#'
#' Summarize multiple exercise columns into one and calculate output
#'
#' @param hw_data  Either a `data.frame` or `tibble` containing multiple variables
#'                 with the prefix `ex` that are numeric and `netid`.
#' @param hw_name  The name of assessment. Default is `"hw01"`.
#'
#' @return A `tibble` with `Username` and `hw_name` variables.
#'
#' @examples
#' \dontrun{
#' calculate_hw_grade(hw)
#'
#' calculate_hw_grade(hw, hw_name = "hw03")
#' }
calculate_hw_grade = function(hw_data, hw_name = "hw01") {
  hw_data[!duplicated(hw_data$netid),] %>%
    mutate(ex_total = select_if(., is.numeric) %>%
             select(contains("ex")) %>%
             rowSums()) %>%
    select(netid, ex_total) %>%
    rename(Username = netid, !!rlang::sym(hw_name) := ex_total)
}

#' Write Homework Grades in the style of Compass2g (Blackboard)
#'
#' Generate a Comma-Separated-Values (CSV) file containing the grades for one
#' homework assignment.
#'
#' @param grades  Location of the files to be read into R.
#' @param pattern The file extension or pattern to match. Default `NULL` returns
#'                all files in a directory.
#' @param delim   The delimiter between successive observations. Default `,`.
#'
#' @return A `tibble` containing grades and a CSV on file system.
#'
#' @examples
#' \dontrun{
#' write_blackboard_hw_grades(grades)
#' write_blackboard_hw_grades(grades, hw_name = "hw03")
#' write_blackboard_hw_grades(grades, hw_name = "hw02", output_dir = "hw/hw02")
#' }
write_blackboard_hw_grades = function(grades, hw_name = "hw01", output_dir = ".") {
  write_csv(grades, file.path(output_dir, paste0(prefix, "-grades.csv")))
}
