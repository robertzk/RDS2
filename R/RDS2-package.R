#' An improvement to serialization in R.
#'
#' RDS2 offers a slightly improved serialization format over R's built-in
#' [RDS](http://cran.r-project.org/web/packages/RDS/index.html) that solves
#' the problem of serializing external pointers (e.g., to non-native C
#' structures).
#'
#' The package overwrites R's built-in \code{\link{readRDS}} and 
#' \code{\link{saveRDS}} functions. If the object being saved or read
#' has an attributes called "RDS2.serialize", it will use this attribute
#' to serialize any normally unserializable content associated with the
#' object.
#'
#' The RDS2.serialize attribute should be a list consisting of a read
#' and write function that transforms the non-native R object into a
#' native R object. For example, it could take C structures attached
#' to the object, serialize them as \code{\link{raw}} objects expressing
#' the binary contents and attach them to the R object during writing,
#' and reverse this process during reading.
#'
#' @name RDS2
#' @docType package
NULL
