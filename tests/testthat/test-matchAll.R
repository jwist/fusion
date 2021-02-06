test_that("matching two, same dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)
  fi <- seq(1, 10) < 7 & seq(1, 10) > 3
  fDc <- filterWith(da, fi)

  mDa <- matchAll(list(fDa, fDb, fDc))
  expect_equal(mDa[[1]], mDa[[2]])
  expect_equal(mDa[[2]], mDa[[3]])
  expect_equal(mDa[[1]], mDa[[3]])
})

test_that("matching two, same dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  fDa@type = "NMR"
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)
  fDb@type = "MS"

  da = new("dataElement",
           type = "ANN",
           obsDescr = list(param))

  fi <- seq(1, 10) < 7 & seq(1, 10) > 3
  fDc <- filterWith(da, fi)
  fDc@type = "ANN"

  mDa <- matchAll(list(fDc, fDa, fDb))
  expect_equal(mDa[[1]]@type, "ANN")
  expect_equal(mDa[[2]]@type, "NMR")
  expect_equal(mDa[[3]]@type, "MS")
})


