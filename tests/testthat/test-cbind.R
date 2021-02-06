test_that("cbind can combine two daE", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(1, 10))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(1, 10))))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(11, 20))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(11, 20))))
  db = new("dataElement", x + 10,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  newDa = cbind(da,db)
  expect_equal(dim(newDa)[1], 10)
  expect_equal(dim(newDa)[2], 20)
  expect_equal(dim(newDa@obsDescr[[1]])[1], 10)
  expect_equal(dim(newDa@obsDescr[[1]])[2], 3)
  expect_equal(length(newDa@obsDescr), 4)
  expect_equal(newDa[1,1:10], da[1,])
  expect_equal(newDa[1,11:20], db[1,])
  expect_equal(newDa@obsDescr[[1]], da@obsDescr[[1]])
  expect_equal(newDa@obsDescr[[2]], da@obsDescr[[2]])
  expect_equal(newDa@obsDescr[[3]], db@obsDescr[[1]])
  expect_equal(newDa@obsDescr[[4]], db@obsDescr[[2]])
  expect_equal(sum(is.na(newDa)), 0)
  expect_equal(sum(is.na(newDa@obsDescr[[1]])), 0)
  expect_equal(sum(is.na(newDa@obsDescr[[2]])), 0)
})

test_that("cbind throw error signature", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(1, 10))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(1, 10))))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(11, 20))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(11, 20))))
  db = new("dataElement", x + 10,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  expect_error(cbind(da, data.frame(da@.Data)), "*dataElements*")
})

test_that("cbind throw error type", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  db = new("dataElement", x + 10,
           varName = as.character(seq(1, 10)),
           type = "MS",
           method = "1D",
           obsDescr = param)

  expect_error(cbind(da, db), "*type*")
})

test_that("cbind throw error method", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  db = new("dataElement", x + 10,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "2D",
           obsDescr = param)

  expect_error(cbind(da, db), "*method*")
})

test_that("cbind throw error ID", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(2, 11)),
                          sampleType = rep("sample", 10)))
  db = new("dataElement", x + 10,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  expect_error(cbind(da, db), "*IDs*")
})

test_that("cbind can combine two daE ANN", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(1, 10))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(1, 10))))
  da = new("dataElement",
           type = "ANN",
           obsDescr = param)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(11, 20))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(11, 20))))
  db = new("dataElement",
           type = "ANN",
           obsDescr = param)

  newDa = cbind(da,db)
  expect_equal(dim(newDa@obsDescr[[1]])[1], 10)
  expect_equal(dim(newDa@obsDescr[[1]])[2], 3)
  expect_equal(length(newDa@obsDescr), 4)
  expect_equal(sum(is.na(newDa@obsDescr[[1]])), 0)
  expect_equal(sum(is.na(newDa@obsDescr[[2]])), 0)
})
