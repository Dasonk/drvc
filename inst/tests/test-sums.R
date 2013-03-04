context("Testing Sums")

coin <- make.RV(0:1, c(.5, .5))
die <- make.RV(1:6)

test_that("Testing addRV", {
    expect_equal(addRV(coin, coin), make.RV(0:2, c(.25, .5, .25)))
    expect_equal(coin + 2, make.RV(2:3, c(.5, .5)))
    
    expect_error(addRV(coin, 2))
})

test_that("Testing SofIID",{
    tencoins <- SofIID(coin, 10)
    expect_equal(values(tencoins), 0:10)
    expect_equal(probs(tencoins), dbinom(0:10, 10, .5))
    
    twohundredcoins <- SofIID(coin, 200)
    expect_equal(probs(twohundredcoins), dbinom(0:200, 200, .5))
    
    expect_identical(SofIID(coin, 2), SofI(coin, coin))
})

test_that("Testing SofI", {
    coindie <- SofI(coin, die)
    
    expect_equal(values(coindie), 1:7)
    expect_equal(probs(coindie), c(1/12, rep(1/6, 5), 1/12))
    
    X <- make.RV(0:1, .5)
    Y <- make.RV(0:1, c(.2, .8))
    expect_identical(X + Y, SofI(X, Y))
    sXYX <- SofI(X, Y, X)
    expect_equal(P(sXYX == 0), .05)
})