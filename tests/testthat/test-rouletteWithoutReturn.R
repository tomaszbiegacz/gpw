context("gpw.rouletteWithoutReturn")

test_that("spin first", {
  wheel <- gpw.rouletteWheelWithoutReturn(list(
    one = 1,
    three = 3,
    six = 6))
  expect_identical(length(wheel$ids), 3L)

  value1 <- gpw.spin(wheel, randomNumberGenerator = function() 0.01)
  expect_identical(value1, "one")
  expect_identical(length(wheel$ids), 3L)

  value2 <- gpw.spin(wheel, randomNumberGenerator = function() 0.01)
  expect_identical(value2, "three")
  expect_identical(length(wheel$ids), 2L)

  value3 <- gpw.spin(wheel, randomNumberGenerator = function() 0.01)
  expect_identical(value3, "six")
  expect_identical(length(wheel$ids), 1L)
})
