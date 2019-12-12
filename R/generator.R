#' Generate assembly code
#'
#' @param nodes list of node
#' @return assembly
#' @export
generate <- function(nodes) {
  c(body, n_var) %<-% generate_body(nodes)
  l <- c(".intel_syntax noprefix",
         ".global main",
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
print.assembly <- function (x, ...) {
  cat(x, sep = "\n")
  invisible(x)
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
  } else {
    c(generate_node(node$lhs, vars),
      generate_node(node$rhs, vars),
      "pop rdi",
      "pop rax",
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
                      "movzb rax, al")),
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

indent <- function(...) paste0("  ", c(...))

var_map <- function() new.env(parent = emptyenv())

index_of <- function(var, var_map) {
  if (is.null(var_map[[var]])) {
    index <- length(var_map) + 1L
    var_map[[var]] <- index
  }
  var_map[[var]]
}
