#' Function to parse format used for storing character matches when original
#' names are written partially in pinyin
#'
#' @param n A string or vector of strings. Should be Chinese names.
#' @param all A logical indicator. If set to TRUE, all possible names are
#'   returned. If false, only the first character match is returned.
#' @return A list of parsed names
#'
#' @examples
#' #original name: chen1溜
#' revpy('{chen1,[嗔,諃,琛,賝,謓,抻,郴,瞋,捵,瘨,縝,綝,棽]}溜', all=F)
#' revpy('{chen1,[嗔,諃,琛,賝,謓,抻,郴,瞋,捵,瘨,縝,綝,棽]}溜', all=F)

revpy = function(n,all=F){
  todo = grep('\\{.*?\\}',n)
  lapply(n[todo],function(n_){
    matches = regmatches(n_,gregexpr('\\{.*?\\}',n_))[[1]]
    combns = as.list(strsplit(n_,'\\{.*?\\}')[[1]])
    repl = which(sapply(combns,nchar)==0)
    pymatches = regmatches(matches,gregexpr('\\[.*?\\]',matches))
    clist = lapply(pymatches, function(str) {
      gsub('\\[|\\]','',strsplit(str,',')[[1]])
    })
    combns[repl] = clist
    if(all){
      apply(expand.grid(combns),1,paste0,collapse='')
    }else{
      paste0(sapply(combns,'[[',1),collapse='')
    }
  })
}

