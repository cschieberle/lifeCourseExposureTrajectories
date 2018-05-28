Individual <- setClass(
  "Individual",
  
  slots = c(
    id = "character",
    age = "numeric",
    sex = "numeric",
    edulevel = "numeric"
  ),
  
  prototype = list(
    NULL, 
    0, 
    0, 
    0
  )
)