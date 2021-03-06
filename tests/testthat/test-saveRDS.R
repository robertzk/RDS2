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

  test_that("a read method is not necessary", {
    file <- tempfile()
    nonnative_obj <- list(x = "pointer")
    attr(nonnative_obj, "RDS2.serialize") <- list(
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
  test_that("using saveRDS does not have side effects on the reference class object", {
    file <- tempfile()
    nonnative_obj <- ref_class_object()
    nonnative_obj$set_env("foo", "bar")
    attr(nonnative_obj, "RDS2.serialize") <- list(
      read  = function(obj) { obj$set_env("foo", "bar"); obj },
      write = function(obj) { obj$set_env("foo", "baz"); obj }
    )
    saveRDS(nonnative_obj, file)
    expect_identical(nonnative_obj$get_env("foo"), "bar")
    expect_equal(base::readRDS(file)$env$foo, "baz")
  })
})

describe("Writing size-zero objects", {
  test_that("using saveRDS on a non-size-zero object does not give a warning", {
    file <- tempfile()
    native_obj <- list(x = 1)
    tryCatch(saveRDS(native_obj, file), warning = function(w) stop("Warning issued"))
  })

  test_that("using saveRDS on a size-zero object gives a warning", {
    expect_warning(saveRDS(NULL, tempfile()), "Size-0")
  })
})

