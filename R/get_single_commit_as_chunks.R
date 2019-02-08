#' Get single commit as chunks from repo
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
get_single_commit_as_chunks <- function(git_repo, commit_id = 10) {

  this_commit <- git2r::commits(git_repo)[[commit_id]]
  previous_commit <- git2r::commits(git_repo)[[commit_id + 1]]

  tree_1 <- git2r::tree(this_commit)
  tree_2 <- git2r::tree(previous_commit)

  code_df <- dplyr::data_frame(code=git2r::diff(tree_2, tree_1, as_char = TRUE) %>%
                                 base::strsplit('\n')) %>%
    tidyr::unnest() %>%
    dplyr::mutate(line_no = row_number(),
                  new_chunk = grepl('^@@', code),
                  is_fn = grepl('^\\+\\+\\+', code),
                  is_code_add = grepl('^\\+', code),
                  fn = ifelse(is_fn, gsub('^\\+\\+\\+ b', '', code), NA),
                  cumul_chunk = cumsum(new_chunk),
                  cumul_fn = cumsum(is_fn)) %>%
    dplyr::filter(cumul_fn > 0)  %>%
    dplyr::group_by(cumul_fn) %>%
    dplyr::mutate(filename = as.character(max(fn, na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is_code_add, !is_fn) %>%
    dplyr::transmute(line_no, chunk=cumul_chunk, code, filename)

  chunks_df <- code_df %>%
    dplyr::group_by(filename, chunk) %>%
    dplyr::summarise(code = paste(code, collapse='\n')) %>%
    dplyr::ungroup()

  chunks_df %>%
    dplyr::transmute(sha = this_commit$sha,
                      author_name = this_commit$author$name,
                      author_email = this_commit$author$email,
                      commit_summary = this_commit$summary,
                      commit_time = lubridate::as_datetime(as.character(this_commit$author$when)),
                      filename, chunk, code) %>%
    dplyr::mutate(code_length = base::nchar(code)) %>%
    dplyr::filter(code_length > 2)

}
