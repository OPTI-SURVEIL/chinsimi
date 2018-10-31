#'Functions to indicate the presence of characters signalling uncertainty
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@return A logical vector of the same length as n
#'@details qmark_check detects question marks, which are used to signal the
#'  place of an unknown character

qmark_check = function(n){
  n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  inds = grep('[？?]',n)
  1:length(n) %in% inds
}

