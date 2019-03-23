#'Extract radical decomposition of Chinese Character strings.
#'@param Chin.strs A vector of Chinese character strings
#'@param sep Separating character to be placed between results for individual
#'  Hanzi. Must be '_' or '' for proper functionality
#'@param structure whether to append ideographic structure characters at the end of the string
#'@param ... unused
#'@return A vector of radical decomposition strings from the original input
#'
#'@examples
#'ChStr2rad('凨冪',structure=F)
#'ChStr2rad('凨冪',structure=T)

ChStr2rad <- function(Chin.strs, sep = "", structure=FALSE,....)
{

  maxchar = max(nchar(Chin.strs))

  OS = Sys.info()['sysname']
  switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
         Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
         Windows = Sys.setlocale(locale = 'chs'))

  resmat1 = vector('list',length=maxchar)
  if(structure) resmat2 = vector('list',length=maxchar)

  for(i in 1:maxchar){
    chars = substr(Chin.strs,i,i)
    chars[chars == ''] <- '_'
    resmat1[[i]] = unlist(mget(chars,rad100lib,ifnotfound = chars))
    if(structure){

      resmat2[[i]] = unlist(mget(chars,str1lib,ifnotfound = '*'))
      resmat2[[i]][chars=='_'] = ''
    }
  }

  res = do.call(paste,c(resmat1[1:length(resmat1)],sep=sep))
  res = gsub('_+$','',res)

  if(structure){
    res2 = do.call(paste,c(resmat2[1:length(resmat2)],sep=sep))
    res2 = gsub('_+$','',res2)
    return(paste(res,res2,sep=sep))
  }
  res
}
