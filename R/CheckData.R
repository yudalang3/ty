#' Check any NA values in the data.frame
#'
#' @param df support input automic vector, matrix and data.frame
#' @description
#' 该函数执行的功能为：如果输入的df参数是一个一维向量，那么返回是否有任何一个元素为NA；
#' 如果输入的df参数为一个matrix也判定是否有任一元素为NA；
#' 如果输入的df参数为一个 data.frame 也判定是否有任一行中存在NA
#'
#'
#' @return T for has
#' @export
#'
#' @examples
#' check_anyNA(df)
check_anyNA <- function(df) {
  if (is.vector(df, mode = "any")) {
    return(any(is.na(df)))
  } else if (is.matrix(df)) {

    return(any(is.na(df)))
  } else if (is.data.frame(df)) {

    cat("any NA is: ", anyNA(df) )
    cat("\n       Col  names are: ", colnames(df) )
    cat("\ncolSums(is.na(df)) is: ", colSums(is.na(df)) )
    cat("\n")
    ret <- df[!complete.cases(df), ]
    invisible(ret)
  } else {
    stop("Unsupported data type. Input must be a vector, matrix, or data.frame.")
  }
}

#' Check any Inf values in the data.frame
#'
#' @param df
#'
#' @return T for has
#' @export
#'
#' @examples
#' check_anyInf(df)
check_anyInf <- function(df) {
  if (is.vector(df, mode = "any")) {
    return(any(is.infinite(df)))
  } else if (is.matrix(df)) {
    return(any(is.infinite(df)))
  } else if (is.data.frame(df)) {
    return(any(apply(df, 1, function(row)
      any(is.infinite(
        row
      )))))
  } else {
    stop("Unsupported data type. Input must be a vector, matrix, or data.frame.")
  }
}

#' Check any NaN values in the data.frame
#'
#' @param df
#'
#' @return T for has
#' @export
#'
#' @examples
#' check_anyNaN(df)
check_anyNaN <- function(df) {
  if (is.vector(df, mode = "any")) {
    return(any(is.nan(df)))
  } else if (is.matrix(df)) {

    return(any(is.nan(df)))
  } else if (is.data.frame(df)) {
      purrr::map(df, ~ is.nan(.x)) %>% any()
  } else {
    stop("Unsupported data type. Input must be a vector, matrix, or data.frame.")
  }
}

#' Check any Null values in the data.frame
#'
#' @param df
#'
#' @return T for has
#' @export
#'
#' @examples
#' check_anyNull(df)
check_anyNull <- function(df) {
  if (is.vector(df, mode = "any")) {
    return(any(is.null(df)))
  } else if (is.matrix(df)) {

    return(any(is.null(df)))
  } else if (is.data.frame(df)) {
    return(any(is.null(df)))
  } else {
    stop("Unsupported data type. Input must be a vector, matrix, or data.frame.")
  }
}


#' Check any zero string ("") in the data.frame
#'
#' @description
#' Currently only implements the characters vector.
#'
#'
#' @param df
#'
#' @return T for has
#' @export
#'
#' @examples
#' check_anyZeroStr(df)
check_anyZeroStr <- function(df) {
  if (is.vector(df, mode = "character")) {
    return(any(is.na(df)))
  } else if (is.matrix(df)) {
    stop("Unsupported data type. Input must be a character vector")
    # return(any(is.na(df)))
  } else if (is.data.frame(df)) {
    stop("Unsupported data type. Input must be a character vector")
    # return(any(apply(df, 1, function(row)
    #   any(is.na(
    #     row
    #   )))))
  } else {
    stop("Unsupported data type. Input must be a character vector")
  }
}
