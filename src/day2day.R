git_path <- function() {
  system('git rev-parse --show-toplevel', intern = TRUE)
}