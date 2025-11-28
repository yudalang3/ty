#' Do the hypothesis testing for two correlation coefficient groups.
#'
#' Reference: https://blog.csdn.net/CoolCoolCarrot/article/details/106001211
#'
#' @param x the pcc of group1
#' @param y the pcc of group2
#'
#' @return the result of the t test
#' @export
#'
#' @examples
#' two_groups_correlationTest()
two_groups_correlationTest <- function(x = c(0.1,0.2,0.3), y = c(0.4,0.5,0.6)) {
  z1 <- 0.5 * log( (1 + x) / (1 - x));
  z2 <- 0.5 * log( (1 + y) / (1 - y));
  t.test(z1,z2)
}


#' Function to calculate the Jaccard coefficient
#' Input: Two character vectors
#' Output: Jaccard coefficient
#'
#' @param vec1 char vector1
#' @param vec2 char vector2
#'
#' @return the jcc
#' @export
#'
#' @examples
#' jaccard_coefficient(letters[1:10], letters[5:12])
#' vec1 <- c("apple", "banana", "cherry")
#' vec2 <- c("banana", "cherry", "date", "apple")
#' jaccard_index <- jaccard_coefficient(vec1, vec2)
#' print(jaccard_index)
jaccard_coefficient <- function(set1, set2) {
  # Convert input vectors to sets to remove duplicate elements
  # Current assume the input is not the error vector
  # set1 <- unique(vec1)
  # set2 <- unique(vec2)

  # Calculate the size of the intersection
  intersection_size <- length(intersect(set1, set2))

  # Calculate the size of the union
  union_size <- length(union(set1, set2))

  # Calculate the Jaccard coefficient
  jaccard_index <- intersection_size / union_size

  return(jaccard_index)
}


