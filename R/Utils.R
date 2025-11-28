#' Set the max_size of the memory
#' @description
#' The full name of this function is: configurate the memory for R that ensuring computation.
#'
#' @return NULL
#' @export
#'
#' @examples
#' config_ensureMemory()
config_ensureMemory <- function() {
  Sys.setenv('R_MAX_VSIZE'=32000000000);
  cat("Now the  is: _", Sys.getenv('R_MAX_VSIZE'), "_\n");
}


#' Get the element counts of a vector, it use the base::table() function.
#' Please refer the table function for the suitable situations.
#'
#' @param vec a vector, e.g.: atomic vector
#'
#' @return a tibble with two column
#' @export
#'
#' @examples
#' get_vec_element_counts(letters)
get_vec_element_counts <- function(vec) {
  aTable <- base::table(vec)
  tibble::tibble(name = names(aTable), count = aTable %>% as.integer()) %>%
    dplyr::arrange(dplyr::desc(count))
}

#' Put string vector elements to string.
#'
#' @description
#' The full name of this function is: paste the vector element to string that for assignment.
#'
#' @param v a vecter, better to be char vector
#'
#' @return the string of assign a vector
#' @export
#'
#' @examples
#' # 创建一个字符向量 v
#' v <- c("a", "b", "c")
#' 使用 paste 函数将字符向量打印成字符串格式
#' output <- paste("c(", paste0('"', v, '"', collapse = ","), ")")
#' # 输出结果
#' cat(output)
#' 另一个有趣的例子：
#' tryR::pas_vectorElements2str(1:10) |> cat()
pas_vectorElements2str <- function(v) {
  output <- paste("c(", paste0('"', v, '"', collapse = ","), ")")
  return(output)
}

#' Input two char vectors or vector-like variable, describe the content of the two vector.
#'
#' @description
#' The full name of this function is describe two char sets.
#'
#'
#' @param x any elements will be convert to the char vector
#' @param y any elements will be convert to the char vector
#' @param onlyNum show the numbers only
#'
#' @return a list of three elements
#' @export
#'
#' @examples
#' des_two_charSets( 1:10, 2:12)
des_two_charSets <- function(x, y, onlyNum = T) {
  charX <- as.character(x)
  charY <- as.character(y)

  the_input_names <- c(deparse1(substitute(x)),"intersection",deparse1(substitute(y)))

  # 检查 x 是否有重复
  if (anyDuplicated(charX)) {
    warning(the_input_names[1], " contains duplicate values.")
  }

  # 检查 y 是否有重复
  if (anyDuplicated(charY)) {
    warning(the_input_names[3], " contains duplicate values.")
  }


  onlyFirst <- setdiff(charX, charY)
  onlySecond <- setdiff(charY, charX)
  intersection <- intersect(charX, charY)

  ret <- list(onlyFirst,
       intersection ,
       onlySecond)
  # cat(names(ret),"\n")
  # cat(deparse1(substitute(x)),"\n")
  # cat(deparse1(substitute(y)),"\n")

  names(ret) <- the_input_names
  # ret <- list(onlyFirst = onlyFirst,
  #      intersection = intersection ,
  #      onlySecond = onlySecond)

  if (onlyNum) {
    num <- purrr::map_int(ret, ~ length(.x))
    item <- names(ret)
    tibble::tibble( item, num)
  }else {
    ret;
  }
}

#' In R the data structure is too Weird and strange, so we need to take a comprehensive of a variable.
#'
#' @param x
#'
#' @return
#' No return, just the prints to watch.
#' @export
#'
#' @examples
#' des_object_str(x = 1:10)
des_object_str <- function(x, complexStr = F) {
  cat("str(x) is:\n    ")

  cat(str(x))

  cat("class(x) is:\n    ")

  cat(class(x), "\n")
  cat("typeof(x) is:\n    ")

  cat(typeof(x) , "\n")


  cat("sloop::s3_class(x) is:\n    ")

  cat(sloop::s3_class(x), "\n")
  cat("sloop::otype(x) is:\n    ")

  cat(sloop::otype(x) , "\n")


  if (complexStr) {
    cat("lobstr::tree(x) is:\n    ")

    print(lobstr::tree(x), "\n")
    cat("lobstr::ref(x) is:\n    ")

    print(lobstr::ref(x) , "\n")
    cat("lobstr::sxp(x) is:\n    ")

    print(lobstr::sxp(x) , "\n")
  }

}


#' Prepare the path for windows. If no argument input, it will read the content from the clipboard.
#' It not only return the value, but change the clip board.
#'
#' @description
#' This is a paste function, very convenient for Windows user.
#' The full name is: paste file path which is prepared for MS Windows.
#'
#' @param path: if no input, it will read content from the clipboard.
#'
#' @return: path string represents the path in ms windows.
#' @export
#'
#' @examples
#' pathPrep_MSWindows()
pas_pathPrep_MSWindows <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  # gsub(pattern = "\\\\", replacement = "/", x = strs )
  # 除了这个gsub之外，竟然还有这个方法
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

#' Get the string vector form the clipboard.
#'
#' @returns atomic string vector
#' @export
#'
#' @examples
#' get_strVector_fromClipboard()
get_strVector_fromClipboard <- function() {
  strs <- readClipboard()
  return(strs)
}


#' Obtain the days from earlier to latter
#'
#' @description
#' The full name is : calculate elapsed day from `earlier` to `latter`.
#'
#'
#' @param earlier the early date
#' @param latter the late date
#'
#' @return days in integer
#' @export
#'
#' @examples
#' elapsed_days('2021-10-01', '2022-01-01')
cal_elapsed_days <- function(earlier, latter) {
  # 定义起始日期和结束日期
  start_date <- as.Date(earlier)
  end_date <- as.Date(latter)

  # 计算两个日期之间的天数差
  days_between <- as.numeric(difftime(end_date, start_date, units = "days"))

  # 打印结果
  cat("从", start_date, "到", end_date, "经过了", days_between, "天。\n")

  invisible(days_between)
}


#' Orgnize the string list to the table(tibble)
#'
#' @param l most be a 'list' and must has name attributes
#'
#' @return a table with row number is max element length, padding with ""
#' @export
#'
#' @examples
#' orgnize_strList2table(list(a = letters[1:5], b = LETTERS[1:3], c = c('gg')))
orgnize_strList2table <- function(l) {
  if (length(l) != length(names(l))) {
    rlang::abort("Please input list with names' length equal to values.")
  }
  # 找到最长的向量长度
  max_length <- purrr::reduce(l, ~ max(.x, length(.y)) , .init = 0)

  purrr::map2(l, names(l) ,function(vec, name){
    length(vec) <- max_length
    vec[is.na(vec)] <- "" # 将NA替换为空字符串
    ret <- tibble::tibble(vec)
    colnames(ret) <- name
    return(ret)
  }) %>% dplyr::bind_cols()

}


#' Conveniently export the list to temporary file.
#'
#' @param l : a list with char as the elements.
#'
#' @return nothing
#' @export
#'
#' @examples
#' export_strList2tmpFile_asTable(list(a = letters, b = 1:10))
export_strList2tmpFile_asTable <- function(l, out_path = NULL){
  if (out_path == NULL) {
    path <- getGlobalVars()[['tmpFile']]

    if (rlang::is_empty(path)) {
      rlang::abort("setGlobalVars( list( 'tmpFile' = '/your/path/file.txt') );\n Please set first.")
    }
  } else {
    path <- out_path
  }

  # data.frame is a list , so check the data.frame first
  if (is.data.frame(l)) {
    table <- l;
  } else if (is.list(l)) {
    table <- orgnize_strList2table(l)
  } else {
    stop("input l is not list nor data.frame.")
  }

  readr::write_tsv(x = table, file = path)
}



#' Force create file even without dirs
#'
#' @param file_path the file path like a/b/c
#'
#' @return nothing
#' @export
#'
#' @examples
#' force_create_file_evenWithout_dirs(a/b/v)
force_create_file_evenWithout_dirs <- function(file_path) {
  # 提取目录路径
  dir_path <- dirname(file_path)

  # 检查目录是否存在，如果不存在则创建
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
    # 如果没有提供内容，创建一个空文件
    file.create(file_path)
}


#' 定义一个自定义函数来进行排序
#'
#' @param data a tibble
#' @param target_names the vector for filter rows
#' @param column_name the column name to filter
#'
#' @return a filtered tibble
#' @export
#'
#' @examples
#' reorder_rows(tibble, c('a','b','c'), col1)
reorder_rows <- function(data, target_names, column_name) {
  ret <- data %>% dplyr::slice(match(target_names, !!rlang::sym(column_name)))


  allIn <- all(target_names %in% (dplyr::pull(data , !!rlang::sym(column_name))))
  if (!allIn) {
    rlang::warn('The input target_names not all contains in the column_name')
  }

  ret;
}


#' Clear the console of the Rstudio
#'
#' @export
#'
#' @examples
#' clear_console()
clear_console <- function() {
  cat("\014")
}

#' Clear all variables in the Global env.
#'
#' Equal to `rm(list = objects())` in the console
#'
#' @export
#'
#' @examples
#' clear_all_vars()
clear_all_vars <- function() {
  rm(list = objects(envir = globalenv()), envir = globalenv())
}

#' Clear Temporary variables
#'
#' @param remove_pattern The default pattern remove the header string tmp
#'
#' @export
#'
#' @examples
#' rm_temp_vars()
rm_temp_vars <- function(remove_pattern = "^tmp_") {
  rm(list =ls(pattern = remove_pattern,envir = globalenv()),
     envir = globalenv())
}


