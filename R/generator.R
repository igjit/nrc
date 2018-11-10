generate <- function(node) {
    c(".intel_syntax noprefix",
      ".global main",
      "main:",
      indent(generate_body(node),
             "pop rax",
             "ret"))
}

generate_body <- function(node) {
    if (is_num(node)) {
        paste0("push ", val(node))
    } else {
        c(generate_body(node$lhs),
          generate_body(node$rhs),
          "pop rdi",
          "pop rax",
          switch(node$op,
                 "+" = "add rax, rdi",
                 "-" = "sub rax, rdi",
                 "*" = "mul rdi",
                 "/" = c("mov rdx, 0",
                         "div rdi")),
          "push rax")
    }
}

indent <- function(...) paste0("  ", c(...))
