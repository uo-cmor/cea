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

