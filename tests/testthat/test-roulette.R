context("roulette")

#
# Tests
#

test_that("spin first", {
  wheel <- gpw.rouletteWheel(c(1, 3, 6))
  value <- gpw.spin(wheel, randomNumberGenerator = function() 0.01)
  expect_identical(value, 1L)
})

test_that("spin first border", {
  wheel <- gpw.rouletteWheel(c(1, 3, 6))
  value <- gpw.spin(wheel, randomNumberGenerator = function() 0.1)
  expect_identical(value, 1L)
})

test_that("spin second", {
  wheel <- gpw.rouletteWheel(c(1, 3, 6))
  value <- gpw.spin(wheel, randomNumberGenerator = function() 0.2)
  expect_identical(value, 2L)
})

test_that("spin second border", {
  wheel <- gpw.rouletteWheel(c(1, 3, 6))
  value <- gpw.spin(wheel, randomNumberGenerator = function() 0.4)
  expect_identical(value, 2L)
})

test_that("spin third", {
  wheel <- gpw.rouletteWheel(c(1, 3, 6))
  value <- gpw.spin(wheel, randomNumberGenerator = function() 0.5)
  expect_identical(value, 3L)
})

test_that("spin last", {
  wheel <- gpw.rouletteWheel(c(1, 3, 6))
  value <- gpw.spin(wheel, randomNumberGenerator = function() 1)
  expect_identical(value, 3L)
})

test_that("gpw.randomInteger bottom", {
  runifMock <- function(max) {
    expect_identical(max, 10L)
    0.1
  }

  value <- gpw.randomInteger(10L, randomNumberGenerator = runifMock)
  expect_identical(value, 1L)
})

test_that("gpw.randomInteger top", {
  runifMock <- function(max) {
    expect_identical(max, 10L)
    9.1
  }

  value <- gpw.randomInteger(10L, randomNumberGenerator = runifMock)
  expect_identical(value, 10L)
})
