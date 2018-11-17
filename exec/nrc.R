devtools::load_all(quiet = TRUE)

ret <- commandArgs(trailingOnly = TRUE)[1]
cat(compile(ret), sep = "\n")
