#'Extract radical decomposition of Chinese Characters strings
#'@param n A vector of Chinese character strings
#'@param times Maximum number of serial decompositions to use
#'@return A vector of radical decompositions of the original strings processed
#'  through at most \code{times} rounds of decomposition
#'@examples
#'Chstr2rad('凨冪',1)
#'Chstr2rad('凨冪',2)

Chstr2rad = function(n, times = 1) {
  temp = strsplit(n, '')
  t = 0
  inds = 1:length(temp)
  while(t < times){
    if(t==0){
      rads = lapply(temp,function(v){
        sapply(v, function(c)
          ifelse(c %in% names(idslib), substr(idslib[[c]], 2, 100), c))
      })
    }else{
      newrads = lapply(rads[inds],function(v){
        unlist(lapply(lapply(lapply(strsplit(v,''), sapply,function(c)
          ifelse(c %in% names(idslib), substr(idslib[[c]], 2, 100), c)),paste0),paste0,collapse=''))
      })
      rads[inds] = newrads

      inds = inds[which(sapply(inds,function(i)!all(newrads[[which(inds==i)]] == rads[[i]])))]
      if(length(inds)==0) break
      #rads = lapply(1:length(rads),function(i) paste(rads[[i]],rads2[[i]],sep=paste0(rep('|',t),collapse='')))
    }

    t = t+1
  }
  sapply(rads,paste0,collapse='_')
}

