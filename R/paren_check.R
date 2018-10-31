#'Functions to indicate the presence of characters signalling uncertainty
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@return A logical vector of the same length as n
#'@details paren_check detects parentheses, which are used to show possible
#'  character substitutions, alternate names, or comments;

paren_check = function(n){
  n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  inds = grep('[\\(.*?\\)]',n)
  inds = unique(c(inds,grep('[（）]',n)))
  1:length(n) %in% inds
}
