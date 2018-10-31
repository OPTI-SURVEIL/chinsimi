#'Extract structure information from Chinese Characters strings
#'@param n A vector of Chinese character strings
#'@param times Maximum number of serial decompositions to use
#'@return A vector of unicode ideograph structure strings representing the
#'  structure of the input characters, processed through at most \code{times}
#'  rounds of decomposition
#'@examples
#'Chstr2struct('凨冪',1)
#'Chstr2struct('凨冪',2)



Chstr2struct = function(n, times = 1) {
  temp = strsplit(n, '')
  t = 0
  inds = 1:length(temp)
  while(t < times){
    if(t==0){
      struct = lapply(temp,function(v){
        sapply(v, function(c)
          ifelse(c %in% names(idslib), substr(idslib[[c]], 1, 1), '*'))
      })
      rads = lapply(temp,function(v){
        sapply(v, function(c)
          ifelse(c %in% names(idslib), substr(idslib[[c]], 2, 100), c))
      })
    }else{
      struct2 = lapply(rads[inds],function(v){
        unlist(lapply(lapply(lapply(strsplit(v,''), sapply,function(c)
          ifelse(c %in% names(idslib), substr(idslib[[c]], 1, 1), '')),paste0),paste0,collapse=''))
      })

      struct[inds] = lapply(inds,function(i) paste(struct[inds][[i]],struct2[[i]],sep=paste0(rep('|',t),collapse='')))

      rads = lapply(rads,function(v){
        unlist(lapply(lapply(lapply(strsplit(v,''), sapply,function(c)
          ifelse(c %in% names(idslib), substr(idslib[[c]], 2, 100), '')),paste0),paste0,collapse=''))
      })
      inds = inds[which(sapply(inds,function(i) max(nchar(rads[[i]])) > 0))]
      if(length(inds)==0) break
    }

    t = t+1
  }
  if(times>1) struct = lapply(struct,function(v) gsub('[|]+$','',v))
  sapply(struct,paste0,collapse='_')
}

