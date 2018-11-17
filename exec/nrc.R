devtools::load_all(quiet = TRUE)

ret <- commandArgs(trailingOnly = TRUE)[1]
cat(nrc(ret), sep = "\n")
