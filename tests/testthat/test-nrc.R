context("nrc.R")

test_that("tokenize", {
    expect_equal(tokenize("1"), list(token(1)))
    expect_equal(tokenize("1+2"), list(token(1), token("+"), token(2)))
})
