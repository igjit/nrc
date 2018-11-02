nrc <- function(ret) {
    paste(".intel_syntax noprefix",
          ".global main",
          "main:",
          paste0("  mov rax, ", ret),
          "  ret",
          "",
          sep = "\n")
}
