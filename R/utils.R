with_sink <- function(file, code) {
  sink(file)
  on.exit(sink())
  force(code)
}
