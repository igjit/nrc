#' Generate assembly code
#'
#' @param nodes list of node
#' @return assembly
#' @export
generate <- function(nodes) {
    l <- c(".intel_syntax noprefix",
           ".global main",
           "main:",
           indent("push rbp",
                  "mov rbp, rsp",
                  "sub rsp, 208"),
           indent(generate_body(nodes)),
           indent("mov rsp, rbp",
                  "pop rbp",
                  "ret"))
    class(l) <- "assembly"
    l
}

#' @export
print.assembly <- function (x, ...) {
    cat(x, sep = "\n")
    invisible(x)
}

generate_body <- function(nodes) {
    nodes %>%
        map(generate_node) %>%
        map2("pop rax", c) %>%
        flatten_chr
}

generate_node <- function(node) {
    if (is_num(node)) {
        paste0("push ", val(node))
    } else if (is_ident(node)) {
        c(generate_lvalue(node),
          "pop rax",
          "mov rax, [rax]",
          "push rax")
    } else if (node$op == "=") {
        c(generate_lvalue(node$lhs),
          generate_node(node$rhs),
          "pop rdi",
          "pop rax",
          "mov [rax], rdi",
          "push rdi")
    } else {
        c(generate_node(node$lhs),
          generate_node(node$rhs),
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

generate_lvalue <- function(node) {
    if (is_ident(node)) {
        adr <- (utf8ToInt("z") - utf8ToInt(val(node)) + 1) * 8
        c("mov rax, rbp",
          paste0("sub rax, ", adr),
          "push rax")
    } else {
        stop("invalid lvalue")
    }
}

indent <- function(...) paste0("  ", c(...))
