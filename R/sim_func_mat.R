#' Compute a matrix of similarity measures between strings, accounting for ambiguous
#' characters. In situations where all combinations or permutations of a set
#' of strings are to be compared, this implementation is more efficient than calling sim_func
#' However, it may be less memory efficient for large inputs
#'
#' @param s_1 A vector or matrix of characters representing one set of original
#'   strings
#' @param s_2 A vector or matrix of characters representing a second set of
#'   original strings for comparison. If missing, a lower
#'   triangular matrix comparing s_1 to itself will be returned.
#' @param method A character input identifying the string similarity method to
#'   be used by stringsim from the 'stringdist' package
#' @param ... Other options to be passed to stringsim
#' @return A matrix of similarities corresponding to the pairwise comparisons of
#'   elements of s_1 and s_2. Upper triangular when s_2 is missing.

sim_func_mat <- function(s_1, s_2,method='lv',q = 1,...){ #inputs may be string vectors

  if(method %in% c('cos','jaccard')) s_1 = paste0('_',s_1,'_')

  ncs1 = nchar(s_1)
  blankmargins1 = which(ncs1 == 0)
  hommargins1 = grepl('\\[\\w*,\\w*\\]',s_1)
  ns1 = length(s_1)
  doinds1 = (1:ns1)[!hommargins1 & ncs1>0]

  if(missing(s_2)){
    mat = matrix(0,nrow = ns1,ncol = ns1)

    dist1 = stringdistmatrix(s_1[doinds1],method = method, q = q,...)

    nhcombs = RcppAlgos::comboGeneral(doinds1,2)

    if(method == 'lv'){
      denom = pmax(ncs1[nhcombs[,1]],ncs1[nhcombs[,2]])
      dist1 = dist1/denom
    }

    if(method == 'lcs'){#fraction of ordered characters in shorter string present in longer string
      tchar = ncs1[nhcombs[,1]] + ncs1[nhcombs[,2]]
      minchar = pmin(ncs1[nhcombs[,1]],ncs1[nhcombs[,2]])
      dist1 = 1 - (tchar - dist1) /(2 * minchar)
    }

    mat[cbind(nhcombs[,2],nhcombs[,1])] = 1 - dist1

   if(sum(hommargins1)>0){
     hominds = which(hommargins1)
     nhinds = which(!hommargins1)
     homonyms = lapply(s_1[hominds],homonym)
      #first, for each homonym string, get minimum distance to all non-homonym strings
      dist_h_vs_nonh = lapply(homonyms,function(v){
        temp = stringdistmatrix(v,b=s_1[doinds1],method = method, q = q, ...)
        if(method == 'lv'){
          ncinds = expand.grid.jc(1:length(v),doinds1)
          denom = pmax(nchar(v)[ncinds[,1]],ncs1[ncinds[,2]])
          temp = temp / denom
        }
        if(method == 'lcs'){
          ncinds = expand.grid.jc(1:length(v),doinds1)
          tchar = nchar(v)[ncinds[,1]] + ncs1[ncinds[,2]]
          minchar = pmin(nchar(v)[ncinds[,1]],ncs1[ncinds[,2]])
          temp = 1 - (tchar - temp) /(2 * minchar)
        }
        apply(temp,2,min)
      } )

      if(length(homonyms)>1){
        hdo = RcppAlgos::comboGeneral(1:length(homonyms),2)

        dist_h_vs_h = lapply(1:nrow(hdo),function(i){
          v1 = homonyms[[hdo[i,1]]]; v2 = homonyms[[hdo[i,2]]]
          temp = stringdistmatrix(v1,v2,method = method, q = q,...)
          if(method == 'lv'){
            ncinds = expand.grid.jc(1:length(v1),1:length(v2))
            denom = pmax(nchar(v1)[ncinds[,1]],nchar(v2)[ncinds[,2]])
            temp = temp / denom
          }
          if(method == 'lcs'){
            ncinds = expand.grid.jc(1:length(v1),1:length(v2))
            tchar = nchar(v1)[ncinds[,1]] + nchar(v2)[ncinds[,2]]
            minchar = pmin(nchar(v1)[ncinds[,1]],nchar(v2)[ncinds[,2]])
            temp = 1 - (tchar - temp) /(2 * minchar)
          }
          min(temp)
        })
      }


      #fold homonym scores back into result


      h_nhcombs = expand.grid.jc(doinds1,hominds)
      h_nhcombs = t(apply(h_nhcombs,1,sort, decreasing = T))
      mat[h_nhcombs] = 1 - unlist(dist_h_vs_nonh)


      if(length(homonyms)>1){
        h_hcombs = cbind(hominds[hdo[,1]],hominds[hdo[,2]])
        h_hcombs = t(apply(h_hcombs,1,sort,decreasing = T))

        mat[h_hcombs]  = 1-unlist(dist_h_vs_h)
      }

   }
    if(length(blankmargins1)>1){
      b_bcombs = RcppAlgos::comboGeneral(blankmargins1,2)
      b_bcombs = t(apply(b_bcombs,1,sort,decreasing = T))
      mat[b_bcombs] = NA
    }

    return(mat)
  }
  if(method %in% c('cos','jaccard')) s_2 = paste0('_',s_2,'_')
  ncs2 = nchar(s_2)
  blankmargins2 = which(ncs2 == 0)
  hommargins2 = grepl('\\[\\w*,\\w*\\]',s_2)
  ns2 = length(s_2)
  doinds2 = (1:ns2)[!hommargins2 & ncs2>0]

  dist1 = stringdistmatrix(s_1[doinds1],s_2[doinds2],method = method, q = q,...)

  nhcombs = expand.grid.jc(doinds1,doinds2)

  if(method == 'lv'){
    denom = pmax(ncs1[nhcombs[,1]],ncs2[nhcombs[,2]])
    dist1 = dist1/denom
  }

  if(method == 'lcs'){#fraction of ordered characters in shorter string present in longer string
    tchar = ncs1[nhcombs[,1]] + ncs2[nhcombs[,2]]
    minchar = pmin(ncs1[nhcombs[,1]],ncs2[nhcombs[,2]])
    dist1 = 1 - (tchar - dist1) /(2 * minchar)
  }
  mat = matrix(0,nrow = ns1,ncol = ns2)

  mat[nhcombs] = 1-dist1

  if((sum(hommargins1) + sum(hommargins2)) >0){
    hominds1 = which(hommargins1)
    hominds2 = which(hommargins2)
    nhinds1 = which(!hommargins1)
    nhinds2 = which(!hommargins2)

    homonyms1 = lapply(s_1[hominds1],homonym)
    homonyms2 = lapply(s_2[hominds2],homonym)

    #first, for each homonym string, get minimum distance to all non-homonym strings
    dist_h_vs_nonh1 = lapply(homonyms1,function(v){
      temp = stringdistmatrix(v,b=s_2[doinds2],method = method, q = q, ...)
      if(method == 'lv'){
        ncinds = expand.grid.jc(1:length(v),doinds2)
        denom = pmax(nchar(v)[ncinds[,1]],ncs2[ncinds[,2]])
        temp = temp / denom
      }
      if(method == 'lcs'){
        ncinds = expand.grid.jc(1:length(v),doinds2)
        tchar = nchar(v)[ncinds[,1]] + ncs2[ncinds[,2]]
        minchar = pmin(nchar(v)[ncinds[,1]],ncs2[ncinds[,2]])
        temp = 1 - (tchar - temp) /(2 * minchar)
      }
      apply(temp,2,min)
    })

    dist_h_vs_nonh2 = lapply(homonyms2,function(v){
      temp = stringdistmatrix(v,b=s_1[doinds1],method = method, q = q, ...)
      if(method == 'lv'){
        ncinds = expand.grid.jc(1:length(v),doinds1)
        denom = pmax(nchar(v)[ncinds[,1]],ncs1[ncinds[,2]])
        temp = temp / denom
      }
      if(method == 'lcs'){
        ncinds = expand.grid.jc(1:length(v),doinds1)
        tchar = nchar(v)[ncinds[,1]] + ncs1[ncinds[,2]]
        minchar = pmin(nchar(v)[ncinds[,1]],ncs1[ncinds[,2]])
        temp = 1 - (tchar - temp) /(2 * minchar)
      }
      apply(temp,2,min)
    })

    hdo = expand.grid.jc(seq_along(homonyms1),seq_along(homonyms2))
    if(nrow(hdo)>0){
      dist_h_vs_h = lapply(1:nrow(hdo),function(i){
        v1 = homonyms1[[hdo[i,1]]]; v2 = homonyms2[[hdo[i,2]]]
        temp = stringdistmatrix(v1,v2,method = method, q = q,...)
        if(method == 'lv'){
          ncinds = expand.grid.jc(1:length(v1),1:length(v2))
          denom = pmax(nchar(v1)[ncinds[,1]],nchar(v2)[ncinds[,2]])
          temp = temp / denom
        }
        if(method == 'lcs'){
          ncinds = expand.grid.jc(1:length(v1),1:length(v2))
          tchar = nchar(v1)[ncinds[,1]] + nchar(v2)[ncinds[,2]]
          minchar = pmin(nchar(v1)[ncinds[,1]],nchar(v2)[ncinds[,2]])
          temp = 1 - (tchar - temp) /(2 * minchar)
        }
        min(temp)
      })
    }



    #fold homonym scores back into result


    h_nhcombs1 = expand.grid.jc(doinds2,hominds1)
    mat[cbind(h_nhcombs1[,2],h_nhcombs1[,1])] = 1 - unlist(dist_h_vs_nonh1)

    h_nhcombs2 = expand.grid.jc(doinds1,hominds2)
    mat[h_nhcombs2] = 1 - unlist(dist_h_vs_nonh2)

    if(nrow(hdo)>0){
      h_hcombs = cbind(hominds1[hdo[,1]],hominds2[hdo[,2]])
      mat[h_hcombs] = 1 - unlist(dist_h_vs_h)
    }

  }
  if((length(blankmargins1) + length(blankmargins2))>1){
    b_bcombs = expand.grid.jc(blankmargins1,blankmargins2)
    mat[b_bcombs] = NA
  }

  return(mat)
}

