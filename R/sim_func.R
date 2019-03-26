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

sim_func <- function(s_1, s_2,aggr='mean',method='lv',q = 1,...){ #inputs may be string vectors

  blankinds = which((nchar(s_1) + nchar(s_2)) == 0)

  one_blankinds = which((nchar(s_1) + nchar(s_2)) != 0 & (nchar(s_1) * nchar(s_2)) == 0)

  doinds = which((nchar(s_1) * nchar(s_2)) > 0)

  if(method == 'jaccard'){s_1 = paste0('_',s_1,'_'); s_2 = paste0('_',s_2,'_')}

    if(any(c('data.frame','matrix') %in% class(s_1))){
      temp = lapply(1:ncol(s_1), function(c){
        sim_func(s_1[,c],s_2[,c])
      })

      temp = do.call(cbind,temp)
      return(apply(temp,1,match.fun(aggr)))
    }

  res = rep(0,length(s_1))

  hominds = unique(grep('\\[\\w*,\\w*\\]',paste(s_1,s_2)))
    if(length(hominds)==0){
      res = stringsim(s_1[doinds],s_2[doinds],method=method,q = q, ...)
      res[blankinds] = NA
      res[one_blankinds] = 0
      return(res)
    }
    doinds = setdiff(doinds,hominds)
    res[doinds] = stringsim(s_1[doinds],s_2[doinds],method=method,q = q, ...)

    torun = do.call(rbind,lapply(hominds,function(i){

      v1 = s_1[i]
      v2 = s_2[i]

      s1 = if(grepl('\\[\\w*,\\w*\\]',v1)){
        homonym(v1)
      }else v1
      s2 = if(grepl('\\[\\w*,\\w*\\]',v2)){
        homonym(v2)
      }else v2
      matrix(c(rep(s1,each=length(s2)),rep(s2,length(s1)),rep(i,length(s1)*length(s2))),ncol=3)
    }))
    simtemp = stringsim(torun[,1],torun[,2],method=method,q=q,...)

    res[hominds] = tapply(simtemp,as.integer(torun[,3]),max)
    res[blankinds] = NA
    res[one_blankinds] = 0

    return(res)

  }

