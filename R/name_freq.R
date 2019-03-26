#'Functions to calculate empirical relative frequency of observed name components. Used to infer the rarity of observed name patterns for matching.
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@param refdata Dataset from which to calculate name component frequencies; can be a precalculated lookup table or a raw vector (will add overhead to repeated computations)
#'@param start initial index of name substring on which to get frequencies
#'@param end final index of name substring on which to get frequencies
#'@param log logical: whether to return log relative frequency
#'@return An integer vector of the same length as n
#'@details for each name, returns the relative frequency (or log relative frequency) of the specified substring in the reference data

name_freq = function(n,refdata,start = 1,end = 9999, log = T){

  OS = Sys.info()['sysname']
  switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
         Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
         Windows = Sys.setlocale(locale = 'chs'))

  if(!any(class(refdata) == 'environment')){
    ns = substr(refdata,start,end)
    ns[nchar(ns)==0] = NA
    refdata = table(ns)
    refdata = refdata/sum(refdata)
    temp = as.list(refdata)
    names(temp) = names(refdata)
    refdata = list2env(temp)
  }

  ns = substr(n,start,end)
  ns[nchar(ns)==0] = NA

  res = 1:length(ns)
  res[is.na(ns)] = NA
  res[!is.na(ns)] = unlist(mget(ns[!is.na(ns)], refdata, ifnotfound = 0))

  res

}



