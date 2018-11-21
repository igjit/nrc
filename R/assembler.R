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

execute <- function(file) (system(file))
