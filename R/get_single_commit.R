#' Get single commit from repo
#'
#' @param git_repo path to repo
#' @param commit_id numeric commit id
#'
#' @return data frame
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' NULL
get_single_commit <- function(git_repo, commit_id = 10) {

  this_commit <- git2r::commits(git_repo)[[commit_id]]
  previous_commit <- git2r::commits(git_repo)[[commit_id - 1]]

  tree_1 <- git2r::tree(this_commit)
  tree_2 <- git2r::tree(previous_commit)

  code_df <- dplyr::data_frame(code=git2r::diff(tree_1, tree_2, as_char = TRUE) %>%
                          base::strsplit('\n')) %>%
    tidyr::unnest() %>%
    dplyr::filter(grepl('^\\+', code)) %>%
    dplyr::filter(!grepl('^\\+\\+|#', code))

  all_code <- paste(code_df$code, collapse = '\n')

  dplyr::data_frame(sha = this_commit$sha,
             author_name = this_commit$author$name,
             author_email = this_commit$author$email,
             commit_time = lubridate::as_datetime(as.character(this_commit$author$when)),
             code = all_code) %>%
    dplyr::mutate(code_length = base::nchar(code)) %>%
    dplyr::filter(code_length > 2)

}
