x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 1
filteredDa <- filterWith(da, fi)

test_that("ordering is working", {
  expect_equal(length(getID(filteredDa)), 9)
  expect_equal(getID(filteredDa), seq(2, 10))
  expect_equal(filteredDa[1,], rep(2, 10))
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 1
filteredDa <- filterWith(da, fi)

test_that("ordering is working with single column data.frame", {
  expect_equal(length(getID(filteredDa)), 9)
  expect_equal(getID(filteredDa), seq(2, 10))
  expect_equal(filteredDa[1,], rep(2, 10))
})
