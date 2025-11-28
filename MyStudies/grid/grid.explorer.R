library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

p <- qplot(1,1)
p2 <- xyplot(1~1)
r <- rectGrob(gp=gpar(fill="grey90"))
t <- textGrob("text")
grid.arrange(t, p, p2, r, ncol=2)


gs <- lapply(1:9, function(ii)
  grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
grid.arrange(grobs=gs, ncol=4,
             top="top label", bottom="bottom\nlabel",
             left="left label", right="right label")
grid.rect(gp=gpar(fill=NA))




lay <- rbind(c(1,1,1,2,3),
             c(1,1,1,4,5),
             c(6,7,8,9,9))
grid.arrange(grobs = gs, layout_matrix = lay,top="top label", bottom="bottom\nlabel",
             left="left label", right="right label")

grid.arrange(grobs = gs)




set.seed(123)
pl <- lapply(1:11, function(.x)
  qplot(1:10, rnorm(10), main=paste("plot", .x)))
ml <- marrangeGrob(pl, nrow=2, ncol=2)


## draw zongzi
library(grid)
#产生一块画布
grid.newpage()
#先来绘制四个角，用来定位坐标系
#产生四个填充了颜色的圆对象
cirs <- circleGrob( x = c(0, 0, 1, 1),
             y = c(0, 1, 1, 0),
             r = 0.1,
             gp = gpar(fill = c("red","black","blue","green")))
#绘制四个圆形
grid.draw(cirs)
#可以看到这个坐标系就是笛卡尔直角坐标系的第一象限
#默认的坐标是0-1的百分比

#绘制粽子的整体轮廓
grid.polygon(x = c(0.3,0.7,0.5), y = c(0.4,0.4,0.8),)
#标记上轮廓的四个顶点
grid.text(
  label = c("(0.3,0.4)", "(0.7,0.4)", "(0.5,0.8)"),
  x = c(0.3, 0.7, 0.5),
  y = c(0.4, 0.4, 0.8),
  hjust = c(1,0,0),
  vjust = c(0,0,0)
)
# 最后，绘制身体
grid.lines(x = c(0.6,0.4) , y = c(0.4,0.6))
grid.lines(x = c(0.5,0.6) , y = c(0.5,0.6))
grid.lines(x = c(0.6,0.65) , y = c(0.4,0.45))
#其实就是画了几条线而已

