#' Quick plot the box plot
#'
#' @param x : A list with names and values
#' @param colo : colors
#'
#' @return no values
#' @export
#'
#' @examples
#' x <- list(A = 1:10, B = 2:11, C = 0:9)
#' colors <- RColorBrewer::brewer.pal(n = 3, "Paired")
#' yyBoxplot(x,colors)
#'
yyBoxplot <- function(x, colo = NULL) {
  if (is.null(x)) {
    stop("The x should be assigned.")
  }

  lenOfX <- length(x)
  if (is.null(colo)) {
    colo <- terrain.colors(lenOfX)
  }else {
    if (lenOfX != length(colo)) {
      stop("Sorry, the input color should have the same length with x.")
    }
  }

  generateDF <- function(var, index, name) {
    if (is.null(name)) {
      names <- paste0('V', index)
    }
    values <- as.double(var)
    df <- data.frame(names = name, values = values)
    return(df)
    # cat(var,"\t",index, "\t" , name ,"\n")
  }
  ll <- mapply(generateDF, x, 1:lenOfX, names(x) , SIMPLIFY = F)
  data <- do.call(rbind, ll)

  # Basic boxplot
  boxplot(data$values ~ data$names , col = colo, )
  legend(x = "topright", names(x),
         fill = colo)
}
