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

test_that("makeUnique start at 0", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a)
  expect_equal(u[1], "a#0")
})

test_that("makeUnique separator", {
  a <- c("a", "a", "b", "a", "b", "c")
  u <- makeUnique(a, sep = "-")
  expect_equal(u[1], "a-0")
})

