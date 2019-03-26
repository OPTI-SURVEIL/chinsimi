#' Convert Chinese strings to pinyin.
#'
#' @param Chin.str The string need to be converted
#' @param tones Whether the output should be toned (T) or toneless.
#' @param multi Whether the output should list multiple pinyins for characters with multiple pronunciations
#' @param sep Character used to seperate different characters. Must be '' or '_' for proper operation
#' @param ... Unused
#' @return pinyin of \code{Chin.str}.
#' @examples
#' ChStr2py(c("\u6d77\u4e0a\u751f\u660e\u6708","\u5929\u6daf\u5171\u6b64\u65f6"))

ChStr2py <- function(Chin.strs, tones = TRUE, multi = TRUE, sep = "_", ...){

    maxchar = max(nchar(Chin.strs))
  OS = Sys.info()['sysname']
  switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
         Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
         Windows = Sys.setlocale(locale = 'chs'))

  resmat = vector('list',length=maxchar)

  for(i in 1:maxchar){
    chars = substr(Chin.strs,i,i)
    chars[chars == ''] <- '_'
    resmat[[i]] = unlist(mget(chars,pylib,ifnotfound = chars))
    if(!multi){
      resmat[[i]] = gsub('[|,*$','',resmat[[i]])
    }
  }

  res = do.call(paste,c(resmat[1:length(resmat)],sep=sep))

  res = gsub('_+$','',res)

  if(!tones) res = gsub('[1-5]','',res)

  res
}







