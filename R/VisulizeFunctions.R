#' Draw a venn graph of three sets with good outlook, the dark theme.
#'
#' @param x a list contains tree sets
#'
#' @return the intersections and diff results.
#' @export
#'
#' @examples
#' plotThreeSetsVennGraph_deep()
venn_plot <- function(x, ...) {
  if ( missing(x) || is.null(x)) {
    exampleDat = list(
      R = c(1:70, 71:110, 111:120, 121:140),
      B = c(141:200, 71:110, 111:120, 201:230),
      G = c(231:280, 111:120, 121:140, 201:230)
    )
  }else {
    exampleDat <- x
  }

  # 判定exampleDat是否有names，如果没有就报错退出
  if (is.null(names(exampleDat))) {
    rlang::abort("The input list should have names for each set.")
  }

  exampleDat = list(
    R = c(1:70, 71:110, 111:120, 121:140),
    B = c(141:200, 71:110, 111:120, 201:230),
    G = c(231:280, 111:120, 121:140, 201:230)
  )

  # https://github.com/yanlinlin82/ggvenn
  ggvenn::ggvenn(
    data = exampleDat,         # 数据列表
    columns = names(exampleDat),
    show_elements = F,        # 当为TRUE时，显示具体的交集情况，而不是交集个数
    label_sep = "\n",         # 当show_elements = T时生效，分隔符 \n 表示的是回车的意思
    show_percentage = T,      # 显示每一组的百分比
    digits = 1,               # 百分比的小数点位数
    fill_color = c("#E41A1C", "#1E90FF", "#FF8C00", "#80FF00"), # 填充颜色
    fill_alpha = 0.5,         # 填充透明度
    stroke_color = "white",   # 边缘颜色
    stroke_alpha = 0.5,       # 边缘透明度
    stroke_size = 0.5,        # 边缘粗细
    stroke_linetype = "solid", # 边缘线条 # 实线：solid  虚线：twodash longdash 点：dotdash dotted dashed  无：blank
    set_name_color = "black", # 组名颜色
    set_name_size = 6,        # 组名大小
    text_color = "black",     # 交集个数颜色
    text_size = 4             # 交集个数文字大小
  )
}



#' Plot points sequential.
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param default.units default is inch
#'
#' @export
#'
#' @examples
#'
#' plot_points_sequential(x = 1:3, y = 1:3)
plot_points_sequential <- function(x, y, default.units = 'in') {

  grid.newpage()
  grid.circle(
    x = x,
    y = y ,
    r = 0.02 ,
    default.units = default.units ,
    gp = gpar(fill = 'red', alpha = seq(from = 0.1, to = 0.7, length.out = length(x)))
  )

  grid.text(
    x = x,
    y = y ,
    r = 0.02 ,
    default.units = default.units ,
    label = 1:length(x),
    vjust = 0,
    hjust = 0
  )

}

# directly use the plot utils in cellChat
# plot_pieChat <- function(variables) {
#
# }


#' Quickly plot a piechart of a plot.
#'
#' https://ggplot2-book.org/programming#sec-functions
#'
#' @param data the long format data frame.
#' @param mapping the mapping object of ggplot2
#'
#' @return the ggplot object
#' @export
#'
#' @examples
#' piechart(mpg, aes(factor(1), fill = drv)) + theme_bw()
piechart <- function(data, mapping) {
  ggplot(data, mapping) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    xlab(NULL) +
    ylab(NULL)
}


#' Transform the data.frame to be the scaled values. Only apply to the numeric values.
#'
#' @param df data.frame class instance
#'
#' @return scaled data.frame with long format. The numeric columns are substitute to variable and value columns.
#' @export
#'
#' @examples
#' pce_data(mpg)
transNumericDat2longFormat <- function(df) {
  is_numeric <- vapply(df, is.numeric, logical(1))

  # Rescale numeric columns
  rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }
  df[is_numeric] <- lapply(df[is_numeric], rescale01)

  # Add row identifier
  df$.row <- rownames(df)

  # Treat numerics as value (aka measure) variables
  # gather_ is the standard-evaluation version of gather, and
  # is usually easier to program with.
  df |> tidyr::pivot_longer(names(df)[is_numeric], names_to = "variable", values_to = "value")
}


#' Quickly draw the parallel coordinates plots.
#'
#' @description
#' Referenced from the https://ggplot2-book.org/programming#sec-functions.
#'
#' @param df the data frame like the demo data mpg in `ggplot2` package.
#' @param ... the parameters import to the `geom_line()`
#'
#' @return the ggplot2 object
#' @export
#'
#' @examples
#' draw_pcpPlot(mpg) + theme_bw()
#' draw_pcpPlot(mpg, aes(colour = drv)) + theme_bw()
draw_pcpPlot <- function(df, ...) {
  df <- transNumericDat2longFormat(df)
  ggplot(df, aes(variable, value, group = .row)) + geom_line(...)
}


#' Quick plot the ecdf.
#'
#' @param data df with wider format
#' @param ... parameters to the stat_ecdf
#'
#' @return the ggplot object
#' @export
#'
#' @examples
#' data <- as.data.frame(matrix(rnorm(100 * 5), ncol = 5))
#' draw_ecdfPlot(data)
draw_ecdfPlot <- function(data, ...){
  data_long <-  tidyr::pivot_longer(data,cols = colnames(data), names_to = 'variable' , values_to = 'value')
  # 绘制多个列的 ECDF 合并在一张图中
  ggplot(data_long, aes(x = value, group = variable, color = variable)) +
    stat_ecdf(geom = "step", ... ) +
    labs(title = "Combined ECDF for Multiple Columns", x = "Value", y = "Cumulative Probability") +
    theme_minimal()

}

