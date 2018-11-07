#' @import stringr
#' @import purrr

TK_NUM <- "TK_NUM"
ND_NUM <- "ND_NUM"

nrc <- function(s) {
    s %>%
        tokenize %>%
        translate %>%
        c("") %>%
        paste(collapse = "\n")
}

token <- function(s) {
    num <- suppressWarnings(as.numeric(s))
    if (is.na(num)) {
        structure(list(ty = s), class = "token")
    } else {
        structure(list(ty = TK_NUM, val = num), class = "token")
    }
}

node <- function(op, lhs, rhs) {
    structure(list(op = op, lhs = lhs, rhs = rhs), class = "node")
}

node_num <- function(val) {
    structure(list(op = ND_NUM, val = val), class = "node")
}

ty <- function(x) UseMethod("ty")
ty.token <- function(x) x$ty

val <- function(x) UseMethod("val")
val.token <-function(x) x$val

is_num <- function(x) UseMethod("is_num")
is_num.token <- function(x) x$ty == TK_NUM
is_num.node <- function(x) x$op == ND_NUM

tokenize <- function(s) {
    s %>%
        str_replace_all("([()+\\-*/])", " \\1 ") %>%
        str_trim %>%
        str_split("\\s+", simplify = TRUE) %>%
        map(token)
}

translate <- function(tokens) {
    c(".intel_syntax noprefix",
      ".global main",
      "main:",
      translate_first(tokens[[1]]),
      translate_rest(tokens[-1]))
}

translate_first <- function(token) {
    if (!is_num(token)) stop()
    paste0("  mov rax, ", val(token))
}

translate_rest <- function(tokens) {
    if (length(tokens) == 0) {
        "  ret"
    } else {
        t1 <- tokens[[1]]
        t2 <- tokens[[2]]
        if (!is_num(t2)) stop()
        if (ty(t1) == "+") {
            c(paste0("  add rax, ", t2$val),
              translate_rest(tail(tokens, -2)))
        } else if (ty(t1) == "-") {
            c(paste0("  sub rax, ", t2$val),
              translate_rest(tail(tokens, -2)))
        }
    }
}
