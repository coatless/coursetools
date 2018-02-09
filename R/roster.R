#' Extract Student IDs from Mailing List
#'
#' Takes a mailing list form and extracts the student's ID from it.
#'
#' @param emails    A list of e-mails with format:
#'                  `netid1@domain.edu;netid2@domain.edu;...`
#' @param domain    The domain of the e-mail address, e.g. `domain.edu`
#'                  or `illinois.edu`.
#' @param delim     Value that is separating student e-mail address, e.g. `;` `,` `\\n`.
#'
#' @return A `character vector` containing student IDs.
#' @examples
#' registered_students = "netid1@domain.edu;netid2@domain.edu"
#'
#' extract_student_ids(registered_students, domain = "domain.edu")
#'
#' registered_students_comma = "netid1@domain.edu,netid2@domain.edu"
#' extract_student_ids(registered_students_comma, domain = "domain.edu", delim = ",")
extract_student_ids = function(emails, domain = "illinois.edu", delim = ";") {
  strsplit(emails, delim) %>%
    unlist() %>%
    gsub(paste0("@", domain), "", .)
}
