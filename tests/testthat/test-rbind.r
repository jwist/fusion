test_that("rbind can combine two daE", {
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
  db = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  newDa = rbind(da,db)
  expect_equal(dim(newDa)[1], 20)
  expect_equal(dim(newDa)[2], 10)
  expect_equal(dim(newDa@obsDescr[[1]])[1], 20)
  expect_equal(dim(newDa@obsDescr[[1]])[2], 3)
  expect_equal(length(newDa@obsDescr), 2)
})

test_that("rbind can combine two daE with list of length 1", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(1, 10))))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(11, 20))))
  db = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  newDa = rbind(da,db)
  expect_equal(dim(newDa)[1], 20)
  expect_equal(dim(newDa)[2], 10)
  expect_equal(dim(newDa@obsDescr[[1]])[1], 20)
  expect_equal(dim(newDa@obsDescr[[1]])[2], 3)
  expect_equal(length(newDa@obsDescr), 1)
})

test_that("rbind can combine two ANN daE", {
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
  param = list(data.frame(sampleID = paste0("sampleID_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(11, 20))),
               data.frame(sampleID2 = paste0("sampleID2_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other2 = paste0("other2_", seq(11, 20))))
  db = new("dataElement",
           type = "ANN",
           obsDescr = param)

  newDa = rbind(da,db)
  expect_equal(dim(newDa@obsDescr[[1]])[1], 20)
  expect_equal(dim(newDa@obsDescr[[1]])[2], 3)
  expect_equal(length(newDa@obsDescr), 2)
})

test_that("rbind can combine two ANN daE of length 1", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(1, 10)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(1, 10))))
  da = new("dataElement",
           type = "ANN",
           obsDescr = param)
  param = list(data.frame(sampleID = paste0("sampleID_", seq(11, 20)),
                          sampleType = rep("sample", 10),
                          other = paste0("other_", seq(11, 20))))
  db = new("dataElement",
           type = "ANN",
           obsDescr = param)

  newDa = rbind(da,db)
  expect_equal(dim(newDa@obsDescr[[1]])[1], 20)
  expect_equal(dim(newDa@obsDescr[[1]])[2], 3)
  expect_equal(length(newDa@obsDescr), 1)
})

test_that("rbind throw errors class", {
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
  db = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)

  expect_error(rbind(da,db, data.frame(c(1))), "*not*")
})

test_that("rbind throw errors type", {
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
  db = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "MS",
           method = "1D",
           obsDescr = param)

  expect_error(rbind(da,db), "*type*")
})

test_that("rbind throw errors method", {
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
  db = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "2D",
           obsDescr = param)

  expect_error(rbind(da,db), "*method*")
})

