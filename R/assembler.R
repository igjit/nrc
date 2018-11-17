assemble <- function(asm) {
    cat(asm, file = "tmp.s", sep = "\n")
    ret <- system("gcc -o tmp tmp.s")
    if (ret == 0) {
        TRUE
    } else {
        stop("gcc failed")
    }
}

execute <- function() (system("./tmp"))
