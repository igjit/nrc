#' @import stringr
#' @import purrr

TK_NUM <- "TK_NUM"
ND_NUM <- "ND_NUM"

nrc <- function(s) {
    s %>%
        tokenize %>%
        parse %>%
        generate %>%
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

as.character.node <- function(node) {
    if (is_num(node)) {
        as.character(val(node))
    } else {
        paste0("(",
               paste(lapply(node, as.character), collapse = " "),
               ")")
    }
}

ty <- function(x) UseMethod("ty")
ty.token <- function(x) x$ty

val <- function(x) UseMethod("val")
val.token <-function(x) x$val
val.node <-function(x) x$val

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
