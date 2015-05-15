without_attributes <- function(object) {
  attributes(object)[setdiff(names(attributes(object)), c("class", "names", "row.names"))] <- NULL
  object
}
