context("Testing Operators")

coin <- make.RV(0:1, c(.5, .5))
die <- make.RV(1:6)

test_that("Testing P", {
    expect_equal(P(coin == 0), .5)
    expect_equal(P(die > 3), .5)

    # Should get error on bogus input
    expect_error(P("blarg"))
    expect_error(P(TRUE))
    expect_error(P(2))
})

test_that("Testing E", {
    expect_equal(E(coin), 0.5)
    expect_equal(E(coin + 1), 1.5)
    expect_equal(E((coin + 1)^2), 2.5)
    expect_equal(E(die), 3.5)
    
    # Input needs to be a random variable
    expect_error(E(c(1:3)))
    expect_error(E("blarg"))
})

test_that("Testing Var", {
    expect_equal(Var(coin), .25)
    expect_equal(Var(die), (6*7*13/6/6 - 3.5^2))
    
    expect_error(Var(1:3))
    expect_error(Var("blarg"))
    expect_error(Var(blarg))
})

test_that("Testin others", {
    expect_equal(Sd(coin), .5)
    expect_equal(Skew(coin), 0)
    expect_equal(Kurt(coin), 1)
})


