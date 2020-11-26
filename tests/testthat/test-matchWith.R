test_that("ordering ID", {
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
  matchedDa <- matchWith(orderedDa, daA)
  expect_equal(getID(daA), getID(matchedDa))
})


