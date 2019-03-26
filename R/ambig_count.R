#'Functions to indicate the presence of characters signalling uncertainty
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@return An integer vector of the same length as n
#'@details ambig_count counts for open-closed parentheses pairs,
#'question marks, and strings such as "又名" which may indicate that
#'ambiguous or multiple name information is included in the field;


ambig_count = function(n){
  #n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  qmark_count = stringr::str_count(n,'[？?]')
  paren_count = stringr::str_count(n,'\\(.*?\\)')
  aka_count = stringr::str_count(n,'(又名)|(法名)|(真名)|(用名)|(别名)|(笔名)')

  or_count = grepl('又',n) * (nchar(gsub('又.*','',n[1])) > 1) * (nchar(gsub('.*又','',n[1])) > 1)

  qmark_count + paren_count + aka_count + or_count
}

