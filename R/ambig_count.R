#'Functions to indicate the presence of characters signalling uncertainty
#'@encoding UTF-8
#'@param n A string or vector of strings. Should be Chinese names.
#'@return An integer vector of the same length as n
#'@details ambig_count counts for open-closed parentheses pairs,
#'question marks, and strings such as "\u53c8\u540d" which may indicate that
#'ambiguous or multiple name information is included in the field;

ambig_count = function(n) {
  #n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  qmark_count = stringr::str_count(n, '[\\uff1f?]')
  paren_count = stringr::str_count(n, '\\(.*?\\)')
  aka_count = stringr::str_count(
    n,
    '(\\u53c8\\u540d)|(\\u6cd5\\u540d)|(\\u771f\\u540d)|(\\u7528\\u540d)|(\\u522b\\u540d)|(\\u7b14\\u540d)'
  )
  or_count = grepl('\\u53c8', n) * (nchar(gsub('\\u53c8.*', '', n[1])) > 1) * (nchar(gsub('.*\\u53c8', '', n[1])) > 1)
  qmark_count + paren_count + aka_count + or_count
}

