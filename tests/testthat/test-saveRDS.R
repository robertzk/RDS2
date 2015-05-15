context("saveRDS")

describe("Writing a vanilla object", {
  test_that("it has exactly the same behavior as the base saveRDS function", {
    file <- tempfile()
    native_obj <- list(x = 1)
    saveRDS(native_obj, file)
    expect_identical(native_obj, readRDS(file))
  })

})
