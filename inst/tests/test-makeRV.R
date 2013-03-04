context("Testing make.RV")

test_that("Creating RVs with full input works", {
    coin <- make.RV(0:1, c(.5, .5))
    expect_true(is.RV(coin))
    expect_equal(sum(probs(coin)), 1)
    expect_equal(values(coin), 0:1)
})

test_that("Creating RVs with only part input works", {
    # No probabilies assumes uniform
    coin <- make.RV(0:1)
    expect_true(is.RV(coin))
    expect_equal(sum(probs(coin)), 1)
    expect_equal(values(coin), 0:1)
    
    die <- make.RV(1:6)
    expect_true(is.RV(die))
    expect_equal(sum(probs(die)), 1)
    expect_equal(values(die), 1:6)
    
    # A single probability gets repeated
    die <- make.RV(1:6, 1/6)
    expect_true(is.RV(die))
    expect_equal(sum(probs(die)), 1)
    expect_equal(values(die), 1:6)
    
    # A probability vector with length that
    # can be expanded gets repeated
    onlyOddDie <- make.RV(1:6, c(1/3, 0))
    expect_true(is.RV(onlyOddDie))
    expect_equal(sum(probs(onlyOddDie)), 1)
    expect_equal(values(onlyOddDie), 1:6)
    expect_equal(probs(onlyOddDie), rep(c(1/3, 0), 3))

})

test_that("Get warnings with shady input", {
    expect_warning(make.RV(1:7, c(.2, .3)))
    expect_warning(make.RV(0:1, rep(1/3, 3)))
})

test_that("Get errors with insane input", {
    expect_error(make.RV(letters[1:3], 1/3)) 
    expect_error(make.RV(0:1, c(0, 0)))
})