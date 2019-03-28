#' Convert Chinese strings to four corner code.
#' @encoding UTF-8
#' @param Chin.strs The string need to be converted
#' @param sep Character used to seperate different characters. With current programming, should only be '_' or ''
#' @param ... Unused
#' @return four corner code of \code{Chin.str}.


ChStr2fc <- function(Chin.strs,sep = "_",...){
  maxchar = max(nchar(Chin.strs))

  enc = Encoding(Chin.strs)
  if(!all(enc == 'UTF-8')) Chin.strs = enc2utf8(Chin.strs)
  # OS = Sys.info()['sysname']
  # switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Windows = Sys.setlocale(locale = 'chs'))

  resmat = vector('list', length = maxchar)

    for (i in 1:maxchar) {
    chars = substr(Chin.strs, i, i)
    chars[chars == ''] <- '_'
    chars_ = substr(stringi::stri_escape_unicode(chars),2,999)
    chars_[nchar(chars_) == 0] = '_'
    resmat[[i]] = unlist(mget(chars_, FClib, ifnotfound = chars))
  }
  res = do.call(paste, c(resmat[1:length(resmat)], sep = sep))
  gsub('_+$', '', res)
}

