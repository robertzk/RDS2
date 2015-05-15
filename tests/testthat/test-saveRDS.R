context("saveRDS")
library(testthatsomemore)

describe("Writing a vanilla object", {
  test_that("it has exactly the same behavior as the base saveRDS function", {
    file <- tempfile()
    native_obj <- list(x = 1)
    saveRDS(native_obj, file)
    expect_identical(native_obj, base::readRDS(file))
  })
})

describe("Writing a non-vanilla object", {
  test_that("the base saveRDS writes an object with the write method applied", {
    file <- tempfile()
    nonnative_obj <- list(x = "pointer")
    attr(nonnative_obj, "RDS2.serialize") <- list(
      read  = function(obj) { obj$x <- rawToChar(obj$x); obj },
      write = function(obj) { obj$x <- charToRaw(obj$x); obj }
    )
    saveRDS(nonnative_obj, file)
    expect_identical(list(x = charToRaw("pointer")),
                     without_attributes(base::readRDS(file)))
  })

  test_that("it undoes side effects to reference class objects", {
    file <- tempfile()
    nonnative_obj <- list2env(list(x = "pointer"), parent = globalenv())
    attr(nonnative_obj, "RDS2.serialize") <- list(
      read  = function(obj) { obj$x <- rawToChar(obj$x); obj },
      write = function(obj) { obj$x <- charToRaw(obj$x); obj }
    )
    saveRDS(nonnative_obj, file)
    expect_identical(as.list(nonnative_obj), list(x = "pointer"))
    expect_identical(list(x = charToRaw("pointer")),
                     as.list(base::readRDS(file)))
  })
  
  testthatsomemore::package_stub("RDS2", "deserialize", function(...) { }, {
    test_that("the side effect is not undone if deserialize is a no-op (negative test)", {
      file <- tempfile()
      nonnative_obj <- list2env(list(x = "pointer"), parent = globalenv())
      attr(nonnative_obj, "RDS2.serialize") <- list(
        read  = function(obj) { obj$x <- rawToChar(obj$x); obj },
        write = function(obj) { obj$x <- charToRaw(obj$x); obj }
      )
      saveRDS(nonnative_obj, file)
      expect_false(identical(as.list(nonnative_obj), list(x = "pointer")))
    })
  })
})

describe("Writing reference class objects", {
})

