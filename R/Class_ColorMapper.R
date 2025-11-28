
#' Generate continuous value to color mapper
#'
#' @description
#'
#' If you input a matrix, the range could be the max and min value of the matrix.
#'
#' You can also input a numeric vector.
#'
#' The return colors have the same dim with the input.
#'
#' For the colors, you may need the `colorRampPalette` function to generate more colors.
#'
#' For example: colorRampPalette(c("blue", "red"))( 50 ) , it will produce 50 colors that gradient from blue to red.
#'
#'
#' @param colo_of_NA color of NA
#' @param colors colors see description for details.
#' @param range The range of the colors to map, see description for details.
#'
#' @return the instance of class
#' @export
#'
#' @examples
#' values <- rnorm(10)
#' continuousMapper(colors = c("navy", "white", "firebrick3"), range = range(values))
#' continuousMapper(colors = colorRampPalette(c("blue", "red"))( 50 ), range = range(values))
continuousMapper <- function(colors = NA,
                             range = NA,
                             colo_of_NA = 'grey') {
  cMapper <-
    ContinuousValue2ColorMapper(colo_of_NA = colo_of_NA,
                                range = range,
                                colors = colors)
  return(cMapper)

}

#' Generate discrete value to color mapper
#'
#' @param colo_of_NA color of NA
#' @param colors colors
#' @param break_points break_points for the intervals
#'
#' @return colors have the same dim with the input
#' @export
#'
#' @examples
#' a <- discreteMapper(colors = c('red', 'blue'), break_points = c(1,2,3))
discreteMapper <- function(colors = NA,
                           break_points = NA,
                           colo_of_NA = 'grey') {
  dMapper <-
    DiscreteValue2ColorMapper(colo_of_NA = colo_of_NA,
                              colors = colors,
                              break_points = break_points)
  return(dMapper)
}

#' Generate colors for the input x
#'
#' @description
#' In fact, I need to admit that: the discrete value to colors mapper, is not practical.
#' you can directly use the named vector to get the colors corresponding to the value.
#'
#' targetColors <- RColorBrewer::brewer.pal(9, name = 'Set1')
#' colorMap <- set_names(targetColors, nm = letters[seq2_along(1,targetColors)])
#  you can get the colors by
#' colorMap[data] |> as.character()
#'
#' @param x matrix or numeric vector
#' @param values2colorMapper the instance: discrete or continues
#'
#' @return colors
#' @export
#'
#' @examples
#' ## This is a low-level situation
#' a <- discreteMapper(colors = c('red', 'blue'), break_points = c(1,2,3))
#' generate_colors(1:10,a)
#'
#' ## A real application situation
#' targetColors <- RColorBrewer::brewer.pal(9, name = 'Set1')
#' range <- c(1,10);
#' break_points <- seq.int(from = range[1], to = tail(range,1), length.out = 50)
#' a <- discreteMapper(colors = colorRampPalette(targetColors)(49) , break_points = break_points)
#'
#' generate_colors(1:10,a)
#'
generate_colors <- function(x, values2colorMapper) {
  if (!inherits(values2colorMapper, 'Value2ColorMapper')) {
    stop("The Value2ColorMapper should not be non.")
  }

  colors = values2colorMapper@colors
  intervals = values2colorMapper@break_points
  colo_of_NA = values2colorMapper@colo_of_NA

  # check the data
  rangeRet <- range(x, na.rm = T)
  if(rangeRet[1] < intervals[1]){
    rlang::warn('The minimum value of the data is less than lowest break point.');
    cat('Range of data is: ', rangeRet, '\n');
    cat('interval is: ', intervals[1], ' ... ' ,tail(intervals,1) , '\n');
  }
  if(rangeRet[2] > tail(intervals,1)){
    rlang::warn('The maximum value of the data is great than highest break point.');
    cat('Range of data is: ', rangeRet, '\n');
    cat('interval is: ', intervals[1], ' ... ' ,tail(intervals,1) ,  '\n');
  }

  row_value2color_mapper <- function(x) {
    index_intervals <- findInterval(x, vec = intervals, all.inside = T)

    ret <- colors[index_intervals]
    ret[is.na(index_intervals)] <- colo_of_NA

    return(ret)
  }

  if (inherits(x, 'matrix')) {
    ret <- apply(x, 1, row_value2color_mapper)
    # do not forget to transpose
    t(ret)
  } else {
    row_value2color_mapper(x)

  }
}



#' Generate equal dispersed intervals from x numeric values according to the number of intervals and center parameter.
#'
#' @param x : numeric values, e.g. (1,1.2,3,0.5)
#' @param numOfBreakPoints : number of intervals (number of break points - 1)
#' @param center : should the break points, equally distribute on the two side of the 0.
#'
#' @return break_points: a vector consists of the break points, e.g. c(1,3,5,7)
#'
#' @examples
#' x = c(1,2,3,4,5,6,7), numOfIntervals = 3, center =F
#' generate_equal_spersed_intervals(x, numOfIntervals,center) will
#' return c(1,4,7) where 1,4,7 is the break points.
generate_equal_spersed_intervals = function(x, numOfBreakPoints, center = F) {
  if (center) {
    m = max(abs(range(x, na.rm = T)))
    res = seq(-m, m, length.out = numOfBreakPoints)
  }
  else{
    res = seq(from = min(x, na.rm = T), to = max(x, na.rm = T), length.out = numOfBreakPoints)
  }
  return(res)
}

# value to color mapper
setClass(
  Class = 'Value2ColorMapper',
  slots = c(
    colo_of_NA = 'character',
    colors = 'character',
    break_points = 'numeric'
  )
)

# # Section 1

# Define class ------------------------------------------------------------

DiscreteValue2ColorMapper <-
  setClass(Class = 'DiscreteValue2ColorMapper',
           contains = 'Value2ColorMapper')

# # Section 2
ContinuousValue2ColorMapper <-
  setClass(Class = 'ContinuousValue2ColorMapper',
           contains = 'Value2ColorMapper')

init_discrete_mapper <- function(.Object,
                                 colo_of_NA = 'grey',
                                 colors = NULL,
                                 break_points = NULL) {
  if (is.null(colors) || is.null(break_points)) {
    stop('You need to input both ranger and colors')
  }

  .Object@colo_of_NA <- colo_of_NA
  .Object@colors <- colors

  if (length(break_points) < 2) {
    stop("For discrete corlor mapper, you need to input intervals")
  }
  if (length(break_points) - 1 == length(colors)) {
    .Object@break_points <- break_points
  } else {
    stop(
      "The numOfIntervals should equal to numOfColors (numOfBreakPoints - 1 = numOfColors)"
    )
  }
  return(.Object)
}

setMethod("initialize",
          "DiscreteValue2ColorMapper",
          init_discrete_mapper)

init_continuous_mapper <- function(.Object,
                                   colo_of_NA = 'grey',
                                   colors = NULL,
                                   range = NULL) {
  if (is.null(colors) || is.null(range)) {
    stop('You need to input both ranger and colors')
  }

  .Object@colo_of_NA <- colo_of_NA
  .Object@colors <- colors
  # 注意这里的细节，区间数量为颜色数量 + 1
  numOfIntervals <- length(colors) + 1

  .Object@break_points <-
    generate_equal_spersed_intervals(range, numOfBreakPoints = numOfIntervals)

  return(.Object)
}

setMethod("initialize",
          "ContinuousValue2ColorMapper",
          init_continuous_mapper)


setMethod("show",
          "ContinuousValue2ColorMapper",
          function(object) {
            cat("This is the ContinuousValue2ColorMapper")
            cat("\nColor of NA value:\t", object@colo_of_NA)
            cat("\nColors:\t", object@colors)
            cat("\nIntervals: \t", object@break_points)
          })
setMethod("show",
          "DiscreteValue2ColorMapper",
          function(object) {
            cat("This is the DiscreteValue2ColorMapper")
            cat("\nColor of NA value:\t", object@colo_of_NA)
            cat("\nColors:\t", object@colors)
            cat("\nIntervals: \t", object@break_points)
          })

