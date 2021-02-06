test_that("creation of dataElement", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  expect_equal(length(da@obsDescr[[1]]$sampleID), 10)
})

test_that("validity check presence of sampleID", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleid = c(seq(1, 9), 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])

  expect_error(new("dataElement", x,
                        varName = as.character(seq(1, 10)),
                        type = "NMR",
                        method = "1D",
                        obsDescr = list(param)), "*sampleID*")
})

test_that("validity check for uniqueness of sampleID", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = c(seq(1, 9), 9),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])

  expect_error(new("dataElement", x,
                   varName = as.character(seq(1, 10)),
                   type = "NMR",
                   method = "1D",
                   obsDescr = list(param)), "*unique*")
})

test_that("creation of annotations", {
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement",
           type = "ANN",
           obsDescr = list(param))
  expect_equal(length(da@obsDescr[[1]]$sampleID), 10)
})

test_that("creation of annotations throw error", {
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  expect_error(new("dataElement",
                   type = "ANN",
                   method = "1D",
                   obsDescr = list(param)), "*method*")
})

test_that("creation of annotations throw error", {
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  expect_error(new("dataElement",
                   type = "ANN",
                   varName = rep("sample", 10),
                   obsDescr = list(param)), "*method*")
})

test_that("creation of annotations throw error", {
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  expect_error(new("dataElement",
                   type = "ANN",
                   .Data = rep("sample", 10),
                   obsDescr = list(param)), "*method*")
})

test_that("creation of annotations throw error", {
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  expect_error(new("dataElement",
                   type = "ANN",
                   varName = rep("sample", 10),
                   .Data = rep("sample", 10),
                   obsDescr = list(param)), "*method*")
})


