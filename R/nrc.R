#' @import stringr
#' @import purrr

TK_NUM <- "NUM"

nrc <- function(s) {
    s %>%
        tokenize %>%
        translate %>%
        c("") %>%
        paste(collapse = "\n")
}

to_type <- function(token) {
    num <- suppressWarnings(as.numeric(token))
    if (is.na(num)) {
        list(ty = token)
    } else {
        list(ty = TK_NUM, val = num)
    }
}

tokenize <- function(s) {
    s %>%
        str_replace("\\+", " + ") %>%
        str_replace("\\-", " - ") %>%
        str_trim %>%
        str_split("\\s+") %>%
        .[[1]] %>%
        map(to_type)
}

translate <- function(tokens) {
    c(".intel_syntax noprefix",
      ".global main",
      "main:",
      translate_first(tokens[[1]]),
      translate_rest(tokens[-1]))
}

translate_first <- function(token) {
    if (token$ty != TK_NUM) stop()
    paste0("  mov rax, ", token$val)
}

translate_rest <- function(tokens) {
    if (length(tokens) == 0) {
        "  ret"
    } else {
        t1 <- tokens[[1]]
        t2 <- tokens[[2]]
        if (t2$ty != TK_NUM) stop()
        if (t1$ty == "+") {
            c(paste0("  add rax, ", t2$val),
              translate_rest(tokens[-c(1, 2)]))
        } else if (t1$ty == "-") {
            c(paste0("  sub rax, ", t2$val),
              translate_rest(tokens[-c(1, 2)]))
        }
    }
}
