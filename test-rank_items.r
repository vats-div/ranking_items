library(testthat)
source("rank_items.r")

context("Test ranking of items")

test_that("Rasch model works", {

    cc <- list(c(1, 0.2, 0, 1), c(1, 0.3, 0, 1),
               c(1, -0.25, 0, 1))

    expect_equal(rank_items_rasch(cc, 0), 
                 c(1, 3, 2))
})

test_that("2PL model works", {

    cc <- list(c(0.3, 0.2, 0, 1), c(1, 0.3, 0, 1),
               c(0.5, -0.25, 0, 1), c(0.6, 1.2, 0, 1))

    expect_equal(rank_items_2PL(cc, 0, 1),
                 c(1, 3, 2, 4))
})

test_that("Oracle works", {

    cc <- list(c(0.3, 0.2, 0, 1), c(1, 0.3, 0, 1),
               c(0.5, -0.25, 0, 1), c(0.1, 1.2, 0, 1))

    expect_equal(rank_items_ability(cc, 0.2),
                 c(3, 1, 2, 4))

})
