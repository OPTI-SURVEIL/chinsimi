#' Compute levenstein edit similarity between strings, accounting for ambiguous
#' characters and potentially aggregating similarity over multiple
#' transformations (e.g. pinyin for phonetic similarity and four corner code for
#' visual similarity)
#'
#' @param s_1 A vector or matrix of characters representing one set of original
#'   strings
#' @param s_2 A vector or matrix of characters representing a second set of
#'   original strings (must be the same length as s_1)
#' @param aggr A character function name or function object indicating how
#'   similarities calculated over multiple transformations should be aggregated
#' @param method A character input identifying the string similarity method to
#'   be used by stringsim from the 'stringdist' package
#' @param ... Other options to be passed to stringsim
#' @return A vector of similarities corresponding to the pairwise
#'   comparisons of elements of s_1 and s_2

sim_func <- function(s_1, s_2,aggr='mean',method='lv',...){ #inputs may be string vectors

    if(any(c('data.frame','matrix') %in% class(s_1))){
      temp = lapply(1:ncol(s_1), function(c){
        sim_func(s_1[,c],s_2[,c])
      })
      temp = do.call(cbind,temp)
      return(apply(temp,1,match.fun(aggr)))
    }
  res = rep(0,length(s_1))
  blankinds = which(s_1 == '' | s_2 == '')

    hominds = which(pmax(grepl('\\[\\w+,\\w+\\]',s_1),grepl('\\[\\w+,\\w+\\]',s_2))==1)
    if(length(hominds)==0){
      res = stringsim(s_1,s_2,method=method,...)
      res[blankinds] = NA
      return(res)
    }
    res = rep(0,length(s_1))
    todoinds = 1:length(s_1)[-c(blankinds,hominds)]
    res[todoinds] = stringsim(s_1[todoinds],s_2[todoinds],method=method,...)

    torun = do.call(rbind,lapply(hominds,function(i){

      v1 = s_1[i]
      v2 = s_2[i]
      s1 = if(grepl('\\[\\w+,\\w+\\]',v1)){
        homonym(v1)
      }else v1
      s2 = if(grepl('\\[\\w+,\\w+\\]',v2)){
        homonym(v2)
      }else v2

      matrix(c(rep(s1,each=length(s2)),rep(s2,length(s1)),rep(i,length(s1)*length(s2))),ncol=3)
    }))

    simtemp = stringsim(torun[,1],torun[,2],method=method,...)

    res[hominds] = tapply(simtemp,torun[,3],max)
    res[blankinds] = NA
    return(res)
  }
