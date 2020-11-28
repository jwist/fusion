x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10),
                   testAnn = LETTERS[1:10])
da = new("dataElement", x,
          variableName = seq(1, 10),
          type = "NMR",
          experimentalParameter = param)
sampleID <- getID(daA)
idx <- sort(sampleID, decreasing = TRUE,
            index.return = TRUE)$ix
oDa <- orderWith(da, idx)
mDa <- matchWith(oDa, da)

test_that("matching two, same dimension", {
  expect_equal(mDa, da)
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 1
fDa <- filterWith(da, fi)
mDa <- matchWith(fDa, da)

test_that("matching two, different dimension, smaller first", {
  expect_equal(mDa, fDa)
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 3
fDa <- filterWith(da, fi)
mDa <- matchWith(da, fDa)

test_that("matching two, different dimension, bigger first", {
  expect_equal(mDa, fDa)
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 3
fDa <- filterWith(da, fi)
fi <- seq(1, 10) < 7
fDb <- filterWith(da, fi)

mDa <- matchWith(fDb, fDa)

test_that("matching two, different dimension", {
  expect_equal(getID(mDa), c(4:6))
})


