test_that("makeUnique makes it unique", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a)
  expect_false(any(duplicated(u)))
})

test_that("makeUnique makes it unique when binding", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a)
  u <- makeUnique(c(a, u))
  expect_false(any(duplicated(u)))
})

test_that("makeUnique reset unique flags when binding", {
  a <- c("a", "a", "b", "a", "b", "c")
  u1 <- makeUnique(c(a, a))
  u <- makeUnique(a)
  u2 <- makeUnique(c(a, u))
  expect_equal(u1, u2)
})

test_that("makeUnique start at 1", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a)
  expect_equal(u[1], "a")
  expect_equal(u[2], "a#1")
})

test_that("makeUnique reverse", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a, reverse = TRUE)
  expect_equal(u[1], "a#2")
  expect_equal(u[2], "a#1")
})

test_that("makeUnique separator", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a, sep = "-")
  expect_equal(u[1], "a")
  expect_equal(u[2], "a-1")
})

test_that("makeUnique separator", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a, fromFirst = TRUE)
  expect_equal(u[1], "a#1")
  expect_equal(u[2], "a#2")
})

test_that("makeUnique separator", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a, fromFirst = TRUE, first = 0)
  expect_equal(u[1], "a#0")
  expect_equal(u[2], "a#1")
})

