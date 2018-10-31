#'Functions to indicate the presence of characters signalling uncertainty
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@return A logical vector of the same length as n
#'@details aka_check checks for strings such as 又名 which may indicate that two
#'  names are listed in the field;

aka_check = function(n){
  n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  inds = grep('(又名)|(法名)|(真名)|(用名)|(别名)|(笔名)',n)
  orinds = grep('又',n)
  orinds = orinds[sapply(strsplit(n[orinds],'又'),function(l) min(nchar(l))>=2 & length(l) > 1)]
  inds = unique(c(inds,orinds))
  1:length(n) %in% inds
}

