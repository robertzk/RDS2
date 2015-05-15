#' Serialize an R object to a connection.
#'
#' This version of readRDS and saveRDS provides capabilities to define
#' serializers for non-native R objects (such as external pointers to C structures).
#'  
#' The function behaves exactly the same as \code{\link[base]{saveRDS}}
#' for native R objects. However, if the object has an attribute 
#' called "RDS2.serialize", this will be used to serialize the object
#' instead. Specifically, the attribute must be a list with keys
#' \code{"read"} and \code{"write"} which must be functions that
#' transform the object into a vanilla R object.
#'
#' For example, suppose we have an object \code{a <- list(x = 1, y = z)},
#' where \code{z} is an external pointer to a C structure.
#' We could set the "RDS2.serialize" attribute as follows.
#'
#' \code{
#'   attr(a, "RDS2.serialize") <- list(
#'     read  = function(obj) { obj$y <- raw_to_ptr(obj$y); obj },
#'     write = function(obj) { obj$y <- ptr_to_raw(obj$y); obj }
#'  )
#' }
#'
#' Here, \code{raw_to_ptr} and \code{ptr_to_raw} are helper functions
#' that serialize and deserialize the C structure to an R object,
#' such as a \code{\link{raw}} vector.
#'
#' @name saveRDS
#' @note The attribute "RDS2.serialize" will be serialized along with the
#'   object, so you must be careful that the parent environment chain of
#'   helper functions used in the read and write methods do not contain
#'   large objects. In general, it is better to use no helper functions
#'   (i.e., the \code{read} and \code{write} functions should be pure
#'   functions rather than closures, and you should set their
#'   \code{environment(read) <- globalenv()} explicitly.).
#'
#'   The mechanism provided by RDS2 is slightly different than the
#'   \code{refhook} argument to the base \code{\link[base]{readRDS}} and
#'   \code{\link[base]{saveRDS}}, since it encloses the serialization procedure
#'   within the serialized object. This allows for greater portability, since
#'   (if these functions are pure) the consumer of an RDS2-serialized object
#'   need only have the RDS2 package attached, rather than the function or
#'   library the \code{refhook} may be from.
#' @param object ANY. The R object to serialize to a file. This object should
#'   have an attribute with the name "RDS2.serialize" if the RDS2 package
#'   capabilities for serializing and deserializing non-native R objects
#'   wish to be used. It should consist of a list with one or both of the
#'   keys \code{read} and \code{write} that take the object as input
#'   and convert a vanilla-to-non-vanilla and non-vanilla-to-vanilla R
#'   object, respectively. (Here, non-vanilla means it may reference non-native
#'   R objects, such as external pointers to C structures).
#'
#'   If the "RDS2.serialize" attribute has the list element
#'   \code{side_effects = FALSE}, an additional deserialization step will
#'   not be executed during \code{saveRDS}. This can be used to slightly speed
#'   up that function. For example, if \code{saveRDS} is serializing a reference
#'   class object or environment, where the \code{write} function can have 
#'   side effects on the object, we must be careful to undo these effects.
#'   Setting \code{attr(object, "RDS2.serialize")$side_effects = FALSE},
#'   we skip this reversal, if we are confident the serialization procedure
#'   does not affect the object or any of its components.
#' @param ... arguments to pass to \code{\link[base]{saveRDS}} or
#'    \code{\link[base]{saveRDS}}. If the first argument of \code{saveRDS},
#'    that is, the \code{object} parameter, has an attribute called
#'   "RDS2.serialize", special serialization and deserialization will occur
#'   prior to writing to the file.
#' @return For \code{readRDS}, an R object. For \code{saveRDS}, \code{NULL},
#'   invisibly.
#' @export
#' @examples
#' file <- tempfile()
#' native_obj <- list(x = 1)
#' saveRDS(native_obj, file)
#' stopifnot(identical(native_obj, readRDS(file)))
#' 
#' # We do not have any C structures to play with, but we will pretend
#' # by converting the string "pointer" to a raw vector.
#' nonnative_obj <- list(x = 1, y = "pointer")
#' attr(nonnative_obj, "RDS2.serialize") <- list(
#'   read  = function(obj) { obj$y <- rawToChar(obj$y); obj },
#'   write = function(obj) { obj$y <- charToRaw(obj$y); obj }
#' )
#' saveRDS(nonnative_obj, file)
#' without_attributes <- function(obj) { attr(obj, "RDS2.serialize") <- NULL; obj }
#' stopifnot(identical(list(x = 1, y = charToRaw("pointer")),
#'   without_attributes(base::readRDS(file))))
#' stopifnot(all.equal(nonnative_obj, readRDS(file)))
#' # Without RDS2, the vanilla object that was passed through the "write" method
#' # is stored in the file. We cannot load the object correctly unless RDS2
#' # is in the search path, so consumers of this RDS file should be careful.
#'
#' # With RDS, the object is deserialized correctly.
saveRDS <- function(object, ...) {
  serialized_object <- serialize(object)
  return_value <- base::saveRDS(serialized_object, ...)

  ## Some objects, such as reference class objects, will experience side-effects
  ## (mutation) during serialization. At the expense of computational slowness,
  ## we undo the serialization to revert these side effects.
  if (has_side_effects(object)) {
    deserialize(serialized_object)
  }

  return_value
}

#' @rdname saveRDS
#' @export
readRDS <- function(file, ...) {
  raw_object <- base::readRDS(file, ...)
  deserialize(raw_object)
}

#' Serialize or deserialize an R object according to its RDS2 serialization.
#'
#' @name serialize
#' @seealso \code{\link{saveRDS}}
#' @param object ANY. The R object to serialize.
#' @return For serialize, the serialized R object. For deserialize, the
#'   deserialized R object.
#'
#'   The function \code{attr(object, "RDS2.serialize")$write} will be
#'   used to perform the serialization and the 
#'   \code{attr(object, "RDS2.serialize")$read} function will be used
#'   to perform the deserialization.
serialize <- function(object) {
  if (object.size(object) == 0) {
    warning("Size-0 object is being serialized.", call. = TRUE)
    NULL
  } else {
    write_method(object)(object)
  }
}

#' @rdname serialize
deserialize <- function(object) { 
  if (object.size(object) == 0) {
    warning("Size-0 object is being serialized.", call. = TRUE)
    NULL
  } else {
    read_method(object)(object)
  }
}

write_method <- function(object) {
  attr(object, "RDS2.serialize")$write %||% identity
}

read_method <- function(object) {
  attr(object, "RDS2.serialize")$read %||% identity
}

has_side_effects <- function(object) {
  ## If the user specifies that this object's serialization does
  ## not have side effects on the R object, we can skip the deserialization
  ## step in `saveRDS`.
  !identical(attr(object, "RDS2.serialize")$side_effects, FALSE)
}

