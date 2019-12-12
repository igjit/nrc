#' @importFrom zeallot %<-%

parse <- function(tokens) {
  program(tokens, 1)
}

program <- function(tokens, pos) {
  if (pos > length(tokens)) {
    NULL
  } else {
    c(n_first, pos) %<-% assign(tokens, pos)
    if (pos > length(tokens)) {
      list(n_first)
    } else {
      token <- tokens[[pos]]
      if (ty(token) == ";") {
        c(list(n_first), program(tokens, pos + 1))
      } else {
        stop("unexpected token: ", val(token))
      }
    }
  }
}

assign <- function(tokens, pos) {
  c(lhs, pos) %<-% equality(tokens, pos)
  if (pos > length(tokens)) {
    list(lhs, pos)
  } else {
    token <- tokens[[pos]]
    op <- ty(token)
    if (op == TK_ASSIGN) {
      c(rhs, pos) %<-% assign(tokens, pos + 1)
      list(node(op, lhs, rhs), pos)
    } else if (op == ";") {
      list(lhs, pos)
    } else {
      stop("unexpected token: ", val(token))
    }
  }
}

equality <- function(tokens, pos) {
  c(lhs, pos) %<-% expr(tokens, pos)
  if (pos > length(tokens)) {
    list(lhs, pos)
  } else {
    token <- tokens[[pos]]
    op <- ty(token)
    if (op %in% c("==", "!=")) {
      c(rhs, pos) %<-% expr(tokens, pos + 1)
      list(node(op, lhs, rhs), pos)
    } else {
      list(lhs, pos)
    }
  }
}

expr <- function(tokens, pos) {
  c(lhs, pos) %<-% mul(tokens, pos)
  if (pos > length(tokens)) {
    list(lhs, pos)
  } else {
    token <- tokens[[pos]]
    op <- ty(token)
    if (op %in% c("+", "-")) {
      c(rhs, pos) %<-% expr(tokens, pos + 1)
      list(node(op, lhs, rhs), pos)
    } else {
      list(lhs, pos)
    }
  }
}

mul <- function(tokens, pos) {
  c(lhs, pos) %<-% term(tokens, pos)
  if (pos > length(tokens)) {
    list(lhs, pos)
  } else {
    token <- tokens[[pos]]
    op <- ty(token)
    if (op %in% c("*", "/")) {
      c(rhs, pos) %<-% mul(tokens, pos + 1)
      list(node(op, lhs, rhs), pos)
    } else {
      list(lhs, pos)
    }
  }
}

term <- function(tokens, pos) {
  token <- tokens[[pos]]
  if (is_num(token)) {
    list(node_num(val(token)), pos + 1)
  } else if (is_ident(token)) {
    list(node_ident(val(token)), pos + 1)
  } else if (ty(token) == "(") {
    c(node, pos) %<-% expr(tokens, pos + 1)
    next_token <- tokens[[pos]]
    if (ty(next_token) != ")") {
      stop("missing ): ", ty(next_token))
    }
    list(node, pos + 1)
  } else {
    stop("unexpected token: ", val(token))
  }
}
