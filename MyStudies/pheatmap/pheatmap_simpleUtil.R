# 这个函数我估计是作者根据自己的需求定制的。默认的is.na估计返回的不是他要的效果。
# 这是一个作者自定义的“增强版”缺失值判断函数，用来覆盖 base R 的 is.na() 在某些情况下的行为。
is.na2 = function(x) {
  if (is.list(x) | length(x) > 1) {
    return(FALSE)
  }
  if (length(x) == 0) {
    return(TRUE)
  }

  return(is.na(x))
}

#不明白这是个什么东西
# 这是一个恒等函数（identity function）：“输入是什么，就返回什么”。
# 与 base R 已经提供的 identity() 功能完全相同，只是作者加了 ...，使它可以忽略额外参数。
identity2 = function(x, ...) {
  return(x)
}


#
