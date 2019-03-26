#' Convert Chinese strings to four corner code.
#' @encoding UTF-8
#' @param Chin.strs The string need to be converted
#' @param sep Character used to seperate different characters. With current programming, should only be '_' or ''
#' @param ... Unused
#' @return four corner code of \code{Chin.str}.
#' @examples
#' \dontrun{ChStr2fc("海上生明月")}

ChStr2fc <- function(Chin.strs,sep = "_",...){
  maxchar = max(nchar(Chin.strs))

  OS = Sys.info()['sysname']
  switch(
    OS,
    Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
    Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
    Windows = Sys.setlocale(locale = 'chs')
  )
  resmat = vector('list', length = maxchar)

    for (i in 1:maxchar) {
    chars = substr(Chin.strs, i, i)
    chars[chars == ''] <- '_'
    resmat[[i]] = unlist(mget(chars, FClib, ifnotfound = chars))
  }
  res = do.call(paste, c(resmat[1:length(resmat)], sep = sep))
  gsub('_+$', '', res)
}
