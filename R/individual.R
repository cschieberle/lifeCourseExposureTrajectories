
# Class for an individual person.
# 
#' @export
#
Individual <- setClass(
  "Individual",
  
  slots = c(
    id = "character",
    age = "numeric",
    sex = "numeric",
    edulevel = "numeric"
  ),
  
  prototype = list(
    id = NULL, 
    age = 0, 
    sex = 0, 
    edulevel = 0
  ),
  
  validity = function(object) {
    return(TRUE)
  }
)