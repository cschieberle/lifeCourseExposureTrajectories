
# Class for an individual person.
# 
#' @export
#
Individual <- setClass(
  "Individual",
  
  slots = c(
    id = "character",
    age = "numeric",
    sex = "character",
    edulevel = "numeric"
  ),
  
  prototype = list(
    id = NULL, 
    age = 0, 
    sex = "F", 
    edulevel = 0
  ),
  
  validity = function(object) {
    return(object@sex %in% c("F", "M") & object@age >= 0)
  }
)