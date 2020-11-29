test_that("multiplication works", {
  id <- c("a", "b", "c", "c")
  expect_equal(makeUnique(id), c("a", "b", "c", "c_2"))
})
