context("Testing comparisons")

X <- make.RV(0:1, c(.5, .5))
Y <- make.RV(0:1, c(.2, .8))
Z <- make.RV(0:1, c(1, 0))
test_that("Comparing with a scalar works", {
    expect_equal(P(Y > .5), .8)
    expect_equal(P(Y < .5), .2)
    expect_equal(P(Y == 0), .2)
    expect_equal(P(Y != 0), .8)
    expect_equal(P(Y >= 0), 1)
    expect_equal(P(Y <= -2), 0)
})

test_that("Comparisons with other RVs works",{
    
    expect_equal(P(X + Y == 1), .5)
    expect_equal(P(X + Y != 1), .5)
    expect_equal(P((X+Y)^2 < 4), .6)
    
    
    expect_equal(P(X == Y), .5)
    expect_equal(P(Y == Z), .2)
})
