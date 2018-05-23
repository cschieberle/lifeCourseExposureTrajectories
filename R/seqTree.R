library(TraMineR)

#' Determines whether a node in the sequence dissimilarity tree is a leaf node.
#' Only sequences within leaf nodes will be considered as they are not split further.
#'
#' @param node Node in sequence dissimilarity tree
#
isLeafNode <- function(node) {
  return(is.null(node$kids))
}

#' Visits a node in the sequence dissimilarity tree
#'
#' @param node Node in sequence dissimilarity tree
#' @param min.age min.age
#' @param max.age max.age
#' @param min.sex min.sex
#' @param max.sex max.sex
#
visitTreeNode <- function(node, min.age, max.age, min.sex, max.sex) {
  message(paste(node$id, min.age, max.age, min.sex, max.sex), appendLF=T)
  return(data.frame(node$id, min.age, max.age, min.sex, max.sex))
}


#' Traverses the sequence dissimilarity tree.
#'
#' @param node Node in sequence dissimilarity tree
#' @param min.age min.age
#' @param max.age max.age
#' @param min.sex min.sex
#' @param max.sex max.sex
#' @export
#
traversePreOrder <- function(node, df = NULL, min.age = 16, max.age = 100, min.sex = 1, max.sex = 2) {
  df.copy <- df
  if (!is.null(node)) {
    # visit node if it is a leaf node
    if (isLeafNode(node)) {
      df.copy <- rbind(df.copy, visitTreeNode(node, min.age, max.age, min.sex, max.sex))
    } else {
      # in case the node has child nodes:
      # 1. determine the split-variable and break-value
      st.varnames <- c("SEX", "AGE.AT.FRST.INTRVW")
      if (st.varnames[ node$split$varindex ] == "AGE.AT.FRST.INTRVW") {
        split.age <- node$split$breaks
        df.1 <- traversePreOrder(node$kids[[1]], df, min.age, split.age, min.sex, max.sex)
        df.2 <- traversePreOrder(node$kids[[2]], df, split.age+1, max.age, min.sex, max.sex)
        df.copy <- rbind(df.copy, df.1, df.2)
      } else if (st.varnames[ node$split$varindex ] == "SEX")  {
        split.sex <- node$split$breaks
        df.1 <- traversePreOrder(node$kids[[1]], df, min.age, max.age, min.sex, split.sex)
        df.2 <- traversePreOrder(node$kids[[2]], df, min.age, max.age, split.sex+1, max.sex)
        df.copy <- rbind(df.copy, df.1, df.2)
      }
    }
  }
  names(df.copy) <- c("NODEID", "AGE.MIN", "AGE.MAX", "SEX.MIN", "SEX.MAX")
  return(df.copy)
}

#' Finds a specific node in the sequence dissimilarity tree.
#'
#' @param node Node in sequence dissimilarity tree
#' @param node.id node.id
#' @export
#
findNodeById <- function(node, node.id) {
  if (!is.null(node)) {
    if (node$id == node.id) {
      return(node)
    } else {
      node.left <- findNodeById(node$kids[[1]], node.id)
      if (!is.null(node.left)) {
        return(node.left)
      }
      node.right <- findNodeById(node$kids[[2]], node.id)
      if (!is.null(node.right)) {
        return(node.right)
      }
    }
  }
  return(NULL)
}

#' Determines corresponding leaf node in the sequence dissimilarity tree for a
#' given set of characteristics (age, sex, etc.).
#'
#' @param age age
#' @param sex sex
#' @export
#
getNodeId <- function(df, age, sex) {
  nodes <- subset(df, AGE.MIN <= age & age <= AGE.MAX & SEX.MIN <= sex & sex <= SEX.MAX)
  stopifnot(nrow(nodes) == 1)
  return(nodes$NODEID)
}
