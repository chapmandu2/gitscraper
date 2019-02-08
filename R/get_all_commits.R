#' Get all commits
#'
#' @param git_repo path to git repo
#'
#' @return data frame
#' @export
#'
#' @examples
#' NULL
get_all_commits <- function(git_repo) {

  purrr::map(1:(length(git2r::commits(git_repo))-1), ~get_single_commit(git_repo, .)) %>%
    dplyr::bind_rows()

}
