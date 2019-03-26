#' Convert Chinese strings to wubi code (based on radicals).
#'
#' @param Chin.str The string need to be converted
#' @param sep Character used to seperate different characters, must be '_' or '' for proper functioning
#' @param ... unused
#' @return wubi code of \code{Chin.str}.
#' @examples
#' ChStr2wb(c("\u6d77\u4e0a\u751f\u660e\u67081","\u5929\u6daf\u5171\u6b64\u65f6"))

ChStr2wb <- function(Chin.strs, sep = "_", ...){

  maxchar = max(nchar(Chin.strs))

  OS = Sys.info()['sysname']
  switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
         Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
         Windows = Sys.setlocale(locale = 'chs'))

  resmat = vector('list',length=maxchar)
  for(i in 1:maxchar){
    chars = substr(Chin.strs,i,i)
    chars[chars == ''] <- '_'
    resmat[[i]] = unlist(mget(chars,WBlib,ifnotfound = chars))
  }
  res = do.call(paste,c(resmat[1:length(resmat)],sep=sep))
  gsub('_+$','',res)

}

