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
  c(lhs, pos) %<-% primary(tokens, pos)
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

primary <- function(tokens, pos) {
  token <- tokens[[pos]]
  if (is_num(token)) {
    list(node_num(val(token)), pos + 1)
  } else if (is_ident(token)) {
    if (val(token) == "function") {
      func(tokens, pos)
    } else if (pos < length(tokens) && ty(tokens[[pos + 1]]) == "(") {
      c(args, pos) %<-% call_args(tokens, pos + 1)
      list(node_call(val(token), args), pos)
    } else {
      list(node_ident(val(token)), pos + 1)
    }
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

func <- function(tokens, pos) {
  token <- tokens[[pos]]
  if (pos < length(tokens) && ty(tokens[[pos + 1]]) != "(") {
    stop("unexpected token: ", val(tokens[[pos + 1]]))
  }
  pos <- pos + 2
  args <- list()
  while (ty(tokens[[pos]]) != ")") {
    ident <- node_ident(val(tokens[[pos]]))
    args <- c(args, list(ident))
    pos <- pos + 1
    if (ty(tokens[[pos]]) == ",") pos <- pos + 1
  }
  c(expr, pos) %<-% expr(tokens, pos + 1)
  list(node_function(args, expr), pos)
}

call_args <- function(tokens, pos) {
  pos <- pos + 1
  args <- list()
  while (ty(tokens[[pos]]) != ")") {
    c(node, pos) %<-% expr(tokens, pos)
    args <- c(args, list(node))
    if (ty(tokens[[pos]]) == ",") pos <- pos + 1
  }
  list(args, pos + 1)
}
