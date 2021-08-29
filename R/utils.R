with_sink <- function(file, code) {
  sink(file)
  on.exit(sink())
  force(code)
}

with_null_pdf <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(grDevices::dev.cur()))
  force(code)
}

is_valid_treatment <- function(x) {
  if (is.logical(x) || is.factor(x)) return(TRUE)
  if (!is.numeric(x)) return(FALSE)
  all(x %in% 0:1)
}
