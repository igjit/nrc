context("generator.R")

test_that("print.assembly", {
  asm <- c(".intel_syntax noprefix", ".global main")
  class(asm) <- "assembly"
  expect_output(print(asm), paste(asm, collapse = "\n"))
})

test_that("index_of", {
  vars <- var_map()
  indices <- map_int(c("a", "b", "c", "b"), ~ index_of(., vars))
  expect_equal(indices, c(1, 2, 3, 2))
})
