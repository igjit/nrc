#' @import stringr
#' @import purrr

TK_NUM <- "TK_NUM"
TK_IDENT <- "TK_IDENT"
TK_ASSIGN <- "="
ND_NUM <- "ND_NUM"
ND_IDENT <- "ND_IDENT"

#' Compile R code
#'
#' @param s source
#' @return assembly
#' @export
compile <- function(s) {
  s %>%
    tokenize %>%
    parse %>%
    generate
}

token <- function(s) {
  num <- suppressWarnings(as.numeric(s))
  if (is.na(num)) {
    if (str_detect(s, "^(\\.|\\.?[A-Za-z][A-Za-z0-9_.]*)$")) {
      structure(list(ty = TK_IDENT, val = s), class = "token")
    } else if (s %in% c("=", "<-")) {
      structure(list(ty = TK_ASSIGN, val = s), class = "token")
    } else {
      structure(list(ty = s, val = s), class = "token")
    }
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

node_ident <- function(val) {
  structure(list(op = ND_IDENT, val = val), class = "node")
}

#' @export
as.character.node <- function(x, ...) {
  if (is_num(x) || is_ident(x)) {
    as.character(val(x))
  } else {
    paste0("(",
           paste(lapply(x, as.character), collapse = " "),
           ")")
  }
}

ty <- function(x) UseMethod("ty")
ty.token <- function(x) x$ty

val <- function(x) UseMethod("val")
val.token <- function(x) x$val
val.node <- function(x) x$val

is_num <- function(x) UseMethod("is_num")
is_num.token <- function(x) x$ty == TK_NUM
is_num.node <- function(x) x$op == ND_NUM

is_ident <- function(x) UseMethod("is_ident")
is_ident.token <- function(x) x$ty == TK_IDENT
is_ident.node <- function(x) x$op == ND_IDENT

tokenize <- function(s) {
  s %>%
    str_replace_all("(<-|==|!=|[()+\\-*/=;,])", " \\1 ") %>%
    str_trim %>%
    str_split("\\s+", simplify = TRUE) %>%
    map(token)
}
