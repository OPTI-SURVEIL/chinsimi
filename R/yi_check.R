#'Functions to indicate the presence of characters signalling uncertainty
#'
#'@param n A string or vector of strings. Should be Chinese names.
#'@return A logical vector of the same length as n
#'@details yi_check checks for commonly substituted Yi names

yi_check = function(n){
  #n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  inds = grep('赤黑',n)
  1:length(n) %in% inds
}
