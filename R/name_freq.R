#'Functions to calculate empirical relative frequency of observed name components. Used to infer the rarity of observed name patterns for matching.
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@param refdata Dataset from which to calculate name component frequencies; can be a precalculated lookup table or a raw vector of names (will add overhead to repeated computations)
#'@param start initial index of name substring on which to get frequencies
#'@param end final index of name substring on which to get frequencies
#'@param log logical: whether to return log relative frequency
#'@return An integer vector of the same length as n
#'@details for each name, returns the relative frequency (or log relative frequency) of the specified substring in the reference data
#'name_freq() returns relative frequencies of a list of names, given substrings
#'name_freq_table() returns a lookup table for each name substring of the specified start and stop digits from reference data
#'name_freq_compare() compares two lists of names given substrings and a lookup table

name_freq_table = function(refdata,start = 1, end= 9999){
  # OS = Sys.info()['sysname']
  # switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Windows = Sys.setlocale(locale = 'chs'))

  # if(any(class(refdata) == 'environment')){
  #   enc = Encoding(names(refdata))
  #   if(!all(enc == 'UTF-8')){
  #     nms = enc2utf8(names(refdata))
  #     refdata= as.list(refdata)
  #     names(refdata) = nms
  #     refdata = list2env(refdata)
  #   }
  # }

  if(!any(class(refdata) == 'environment')){
    enc = Encoding(refdata)
    if(!all(enc == 'UTF-8')) refdata = enc2utf8(refdata)
    ns = substr(refdata,start,end)
    ns[nchar(ns)==0] = NA
    refdata = data.table::data.table(nm = ns)
    ndef = sum(!is.na(ns))
    refdata = refdata[!is.na(nm)]
    refdata[,freq:= .N/ndef, by = nm]
    temp = as.list(refdata[,freq])
    names(temp) = refdata[,nm]
    refdata = list2env(temp)
  }
  refdata
}

name_freq = function(n,refdata,start = 1,end = 9999, log = T){

  # OS = Sys.info()['sysname']
  # switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Windows = Sys.setlocale(locale = 'chs'))

  # if(any(class(refdata) == 'environment')){
  #   enc = Encoding(names(refdata))
  #   if(!all(enc == 'UTF-8')){
  #     nms = enc2utf8(names(refdata))
  #     refdata= as.list(refdata)
  #     names(refdata) = nms
  #     refdata = list2env(refdata)
  #   }
  # }

  if(!any(class(refdata) == 'environment')) refdata = name_freq_table(refdata, start, end)

  ns = substr(n,start,end)
  ns[nchar(ns)==0] = NA

  res = unlist(mget(ns, refdata, ifnotfound = 0))
  res[is.na(names(res))] = NA

  if(log) log(res) else res
}

name_freq_compare = function(n1,n2,refdata,start = 1,end = 9999, log = T){

  # OS = Sys.info()['sysname']
  # switch(OS, Linux = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Darwin = Sys.setlocale(locale = 'zh_CN.GBK'),
  #        Windows = Sys.setlocale(locale = 'chs'))

  # if(any(class(refdata) == 'environment')){
  #   enc = Encoding(names(refdata))
  #   if(!all(enc == 'UTF-8')){
  #     nms = enc2utf8(names(refdata))
  #     refdata= as.list(refdata)
  #     names(refdata) = nms
  #     refdata = list2env(refdata)
  #   }
  # }

  if(!any(class(refdata) == 'environment')) refdata = name_freq_table(refdata, start, end)

  ns1 = substr(n1,start,end)
  ns1[nchar(ns1)==0] = NA

  ns2 = substr(n2,start,end)
  ns2[nchar(ns2)==0] = NA

  doinds = which(ns1==ns2)

  res = rep(NA,length(ns1))

  res[doinds] = unlist(mget(ns1[doinds], refdata, ifnotfound = 0))

  if(log) log(res) else res
}


