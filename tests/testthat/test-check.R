x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
da = new("dataElement", x,
         varName = seq(1, 10),
         type = "NMR",
         obsDescr = param)

test_that("check dataElement", {
  expect_equal(check(da), TRUE)
})

param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
da = new("dataElement",
         varName = seq(1, 10),
         type = "ANN",
         obsDescr = param)

test_that("check annotations", {
  expect_equal(check(da), TRUE)
})
