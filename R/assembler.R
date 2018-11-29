#' Assemble
#'
#' @param asm assembly
#' @param file file to write to
#' @return file
#' @export
assemble <- function(asm, file = tempfile()) {
    asm_file <- paste0(file, ".s")
    cat(asm, file = asm_file, sep = "\n")
    ret <- system(paste("gcc -o", file, asm_file))
    if (ret == 0) {
        file
    } else {
        stop("gcc failed")
    }
}

#' Execute executable file
#'
#' @param file executable file
#' @return exit status
#' @export
execute <- function(file) (system(file))
