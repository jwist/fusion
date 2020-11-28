test_that("matching two, same dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           variableName = seq(1, 10),
           type = "NMR",
           experimentalParameter = param)
  sampleID <- getID(da)

  idx <- sort(sampleID, decreasing = TRUE,
              index.return = TRUE)$ix
  oDa <- orderWith(da, idx)
  mDa <- matchWith(oDa, da)

  expect_equal(mDa, da)
})

test_that("matching two, different dimension, smaller first", {
  fi <- seq(1, 10) > 1
  fDa <- filterWith(da, fi)
  mDa <- matchWith(fDa, da)

  expect_equal(mDa, fDa)
})

test_that("matching two, different dimension, bigger first", {
  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  mDa <- matchWith(da, fDa)

  expect_equal(mDa, fDa)
})

test_that("matching two, different dimension", {
  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)

  mDa <- matchWith(fDb, fDa)

  expect_equal(getID(mDa), c(4:6))
})


