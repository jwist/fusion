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
orderedDa <- order(da, idx)
matchedDa <- matchWith(orderedDa, da)

test_that("ordering ID", {
  expect_equal(getID(da), getID(matchedDa))
})

test_that("ordering data matrix", {
  expect_equal(getDataPart(matchedDa), getDataPart(da))
})

