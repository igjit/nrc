context("parser.R")

test_that("parse", {
    expect_equal(parse(tokenize("1")), node_num(1))
    expect_equal(parse(tokenize("1+2")), node("+", node_num(1), node_num(2)))
    expect_equal(parse(tokenize("1+2*3")), node("+", node_num(1), node("*", node_num(2), node_num(3))))
    expect_equal(parse(tokenize("1*2+3")), node("+", node("*", node_num(1), node_num(2)), node_num(3)))
    expect_equal(parse(tokenize("1*(2+3)")), node("*", node_num(1), node("+", node_num(2), node_num(3))))
    expect_equal(parse(tokenize("(1+2)*3)")), node("*", node("+", node_num(1), node_num(2)), node_num(3)))
})
