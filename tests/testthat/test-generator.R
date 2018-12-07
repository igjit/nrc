context("generator.R")

test_that("print.assembly", {
    asm <- c(".intel_syntax noprefix", ".global main")
    class(asm) <- "assembly"
    expect_equal(capture_output(print(asm)), paste(asm, collapse = "\n"))
})
