library(dplyr)
library(purrr)
library(tibble)

# 假设您的 temp_ret 已经生成（这是一个模拟数据）
n_bootstrap <- 100
set.seed(42) # 设置种子以确保结果可重现

temp_ret <- purrr::map(1:n_bootstrap, function(i) {
  # 模拟 c(score, score_up, score_down) 的结果
  score <- rnorm(1, mean = 0.5, sd = 0.1)
  score_up <- score + runif(1, 0, 0.05)
  score_down <- score - runif(1, 0, 0.05)
  return(c(score, score_up, score_down))
})

temp_df <- purrr::map_dfr(
  temp_ret,
  ~ tibble(
    score      = .x[1],
    score_up   = .x[2],
    score_down = .x[3]
  )
)
