x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10),
                   testAnn = LETTERS[1:10])
daA = new("dataElement", x,
          variableName = seq(1, 10),
          type = "NMR",
          experimentalParameter = param)
sampleID <- getID(daA)
idx <- sort(sampleID, decreasing = TRUE,
            index.return = TRUE)$ix
orderedDa <- orderWith(daA, idx)
matchedDa <- matchPairwise(orderedDa, daA)

test_that("matching two, same dimension", {
  expect_equal(getID(daA), getID(matchedDa))
  expect_equal(getDataPart(daA), getDataPart(matchedDa))
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 1
filteredDa <- filterWith(da, fi)
matchedDa <- matchPairwise(filteredDa, da)

test_that("matching two, different dimension", {
  expect_equal(getID(filteredDa), getID(matchedDa))
  expect_equal(getDataPart(filteredDa), getDataPart(matchedDa))
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         variableName = seq(1, 10),
         type = "NMR",
         experimentalParameter = param)
fi <- seq(1, 10) > 1
filteredDa <- filterWith(da, fi)
matchedDa <- matchPairwise(da, filteredDa)

test_that("matching two, different dimension, bigger first", {
  expect_equal(getID(filteredDa), getID(matchedDa))
  expect_equal(getDataPart(filteredDa), getDataPart(matchedDa))
})
