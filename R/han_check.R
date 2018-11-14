#' Rough detection of Han Chinese names
#'
#' @param n A string or vector of strings. Should be Chinese names.
#' @return A logical vector of the same length as n
#'
#' @details Han classification is based on two characteristics. The first is
#'   whether each name starts with one of the 400 most popular family names
#'   listed for 2013 by Baidu. The second requirement is that the name is 2-4
#'   characters long.


hancheck = function(n){ #returns true for names that are highly likely to be Han (i.e. start with one of 400 popular family names and have 2-4 characters total)
  #n[grep('\\{.*?\\}',n)] = unlist(revpy(n))
  n = gsub('\\(.*?\\)','',n)
  nc = nchar(n)
  hn = substr(n,1,1) %in% hannames$hannames | substr(n,1,2) %in% hannames$hannames

  hc = nc %in% c(2:4) & hn
  hc
}
