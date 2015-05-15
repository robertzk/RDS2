context("readRDS")

describe("Reading a vanilla object", {
  test_that("it has exactly the same behavior as the base readRDS function", {
    file <- tempfile()
    native_obj <- list(x = 1)
    base::saveRDS(native_obj, file)
    expect_identical(native_obj, readRDS(file))
  })
})

describe("Writing a non-vanilla object", {
  test_that("the base readRDS reads an object with the read method applied", {
    file <- tempfile()
    nonnative_obj <- list(x = "pointer")
    attr(nonnative_obj, "RDS2.serialize") <- list(
      read  = function(obj) { obj$x <- rawToChar(obj$x); obj },
      write = function(obj) { obj$x <- charToRaw(obj$x); obj }
    )
    saveRDS(nonnative_obj, file)
    expect_equal(nonnative_obj, readRDS(file))
  })
})

