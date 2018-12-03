#' Function to parse format used for storing character matches when original
#' names are written partially in pinyin
#'
#' @param n A string or vector of strings. Should be Chinese names.
#' @param all A logical indicator. If set to TRUE, all possible names are
#'   returned. If false, only the first character match is returned.
#' @param py A logical indicator. If set to TRUE, the original pinyin element is
#'   returned instead of possible characters
#' @return A list of parsed names
#'
#' @examples
#' #original name: chen1溜
#' revpy('{chen1,[嗔,諃,琛,賝,謓,抻,郴,瞋,捵,瘨,縝,綝,棽]}溜', all=F)
#' revpy('{chen1,[嗔,諃,琛,賝,謓,抻,郴,瞋,捵,瘨,縝,綝,棽]}溜', all=T)
#' revpy('{chen1,[嗔,諃,琛,賝,謓,抻,郴,瞋,捵,瘨,縝,綝,棽]}溜', py=T)

revpy = function(n,all=F,py=F){
  todo = grep('\\{.*?\\}',n)
  if(py){
    n[todo] = sapply(n[todo],function(n_){
      matches = regmatches(n_,gregexpr('\\{.*?\\}',n_))[[1]]
      temp = gsub('\\{.*?\\}',' ',n_)
      combns = as.list(strsplit(temp,'')[[1]])
      repl = which(combns==' ')
      matches = gsub('[[:punct:]]','',gsub('\\[.*?\\]','',matches))
      combns[sapply(combns,nchar)==0] <- matches
      paste0(combns,collapse='')
    })
  return(n)
  }else{
    n_ = as.list(n)
    n_[todo] = lapply(n[todo],function(n_){
      matches = regmatches(n_,gregexpr('\\{.*?\\}',n_))[[1]]
      temp = gsub('\\{.*?\\}',' ',n_)
      combns = as.list(strsplit(temp,'')[[1]])
      repl = which(combns==' ')
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
    if(all) return(n_)
    else return(unlist(n_))
  }
}

