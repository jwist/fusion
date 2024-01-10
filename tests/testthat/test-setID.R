test_that("setID works", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10],
                     dataPath = paste0("/test/", seq(1, 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  newID <- paste0("ID", seq(1, 10))
  da <- setID(da, newID)
  expect_equal(getID(da), newID)
})

test_that("setID checks for uniqueness", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10],
                     dataPath = paste0("/test/", seq(1, 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  newID <- paste0("ID", c(seq(1, 9), 9))
  expect_error(setID(da, newID), "*unique")
})
