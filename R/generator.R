ARG_REGS <- c("rdi", "rsi", "rdx", "rcx", "r8", "r9")

#' Generate assembly code
#'
#' @param nodes list of node
#' @return assembly
#' @export
generate <- function(nodes) {
  functions <- nodes$functions
  nodes <- nodes$nodes
  c(body, n_var) %<-% generate_body(nodes)
  l <- c(".intel_syntax noprefix",
         paste0(".global ", paste0(c(names(functions), "main"),  collapse = ", ")),
         names(functions) %>% map(~ generate_function(., functions[[.]])) %>% flatten_chr,
         "main:",
         indent("push rbp",
                "mov rbp, rsp",
                paste0("sub rsp, ", n_var * 8)),
         indent(body),
         indent("mov rsp, rbp",
                "pop rbp",
                "ret"))
  structure(l, class = "assembly")
}

#' @export
print.assembly <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}

generate_function <- function(name, func) {
  c(paste0(name, ":"),
    indent(generate_function_body(func$expr, func$args),
           "pop rax",
           "ret"))
}

generate_function_body <- function(node, args) {
  if (is_num(node)) {
    paste0("push ", val(node))
  } else if (is_ident(node)) {
    arg_names <- map_chr(args, ~ val(.))
    index <- which(arg_names == val(node))
    paste0("push ", ARG_REGS[index])
  } else if (node$op == "=") {
    stop("TODO")
  } else if (is(node, "node_call")) {
    stop("TODO")
  } else {
    c(generate_function_body(node$lhs, args),
      generate_function_body(node$rhs, args),
      "pop rdi",
      "pop rax",
      generate_binop(node),
      "push rax")
  }
}

generate_body <- function(nodes) {
  vars <- var_map()
  body <- nodes %>%
    map(generate_node, vars) %>%
    map2("pop rax", c) %>%
    flatten_chr
  list(body, length(vars))
}

generate_node <- function(node, vars) {
  if (is_num(node)) {
    paste0("push ", val(node))
  } else if (is_ident(node)) {
    c(generate_lvalue(node, vars),
      "pop rax",
      "mov rax, [rax]",
      "push rax")
  } else if (node$op == "=") {
    c(generate_lvalue(node$lhs, vars),
      generate_node(node$rhs, vars),
      "pop rdi",
      "pop rax",
      "mov [rax], rdi",
      "push rdi")
  } else if (is(node, "node_call")) {
    push_values <- node$args %>%
      map(~ generate_node(.)) %>%
      flatten_chr
    pop_args <- if (length(node$args) > 0) {
                  head(ARG_REGS, length(node$args)) %>%
                    rev %>%
                    paste0("pop ", .)
                }
    c(push_values,
      pop_args,
      paste0("call ", val(node$func)),
      "push rax")
  } else {
    c(generate_node(node$lhs, vars),
      generate_node(node$rhs, vars),
      "pop rdi",
      "pop rax",
      generate_binop(node),
      "push rax")
  }
}

generate_lvalue <- function(node, vars) {
  if (is_ident(node)) {
    adr <- index_of(val(node), vars) * 8
    c("mov rax, rbp",
      paste0("sub rax, ", adr),
      "push rax")
  } else {
    stop("invalid lvalue")
  }
}

generate_binop <- function(node) {
  switch(node$op,
         "+" = "add rax, rdi",
         "-" = "sub rax, rdi",
         "*" = "mul rdi",
         "/" = c("mov rdx, 0",
                 "div rdi"),
         "==" = c("cmp rdi, rax",
                  "sete al",
                  "movzb rax, al"),
         "!=" = c("cmp rdi, rax",
                  "setne al",
                  "movzb rax, al"))
}

indent <- function(...) paste0("  ", c(...))

var_map <- function() new.env(parent = emptyenv())

index_of <- function(var, var_map) {
  if (is.null(var_map[[var]])) {
    index <- length(var_map) + 1L
    var_map[[var]] <- index
  }
  var_map[[var]]
}
