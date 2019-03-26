#'Extract structure or radical information from Chinese Characters strings.  This function was
#'rather slow, so was used initially to generate single time and total
#'decomposition structure libraries. For use on new data, see ChsStr2struct and ChsStr2rad.
#'@param n A vector of Chinese character strings
#'@param times Maximum number of serial decompositions to use
#'@return A vector of unicode ideograph structure strings representing the
#'  structure of the input characters, processed through at most \code{times}
#'  rounds of decomposition
#'@examples
#'ids2struct('\u51e8\u51aa',1)
#'ids2rad('\u51e8\u51aa',2)

# strchar = c('\u2ff0','\u2ff1','\u2ff2','\u2ff3','\u2ff4','\u2ff5','\u2ff6','\u2ff7','\u2ff8','\u2ff9','\u2ffa','\u2ffb')
#
# structget = function(string){
#   matches = gregexpr('[\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb].*[\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb]|[\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb]',string)
#   res = regmatches(string,matches)
#   res = gsub('[^\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb]','+',res)
#   res
# }
#
# radget = function(string){
#   matches = gregexpr('[^\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb].*[^\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb]|[^\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb]',string)
#   res = regmatches(string,matches)
#   res = gsub('[\u2ff0\u2ff1\u2ff2\u2ff3\u2ff4\u2ff5\u2ff6\u2ff7\u2ff8\u2ff9\u2ffa\u2ffb]','+',res)
#   res
# }
#
#
# ids2struct = function(n, times = 1) {
#   #temp = strsplit(n, '')
#   temp = n
#   t = 0
#   inds = 1:length(temp)
#   while(t < times){
#     if(t==0){
#       struct = lapply(temp,function(v){
#         sapply(v, function(c)
#           ifelse(c %in% names(idslib), structget(idslib[[c]]), '*'))
#       })
#       rads = lapply(temp,function(v){
#         sapply(v, function(c)
#           ifelse(c %in% names(idslib), radget(idslib[[c]]), c))
#       })
#     }else{
#       struct2 = lapply(rads[inds],function(v){
#         unlist(lapply(lapply(lapply(strsplit(v,''), sapply,function(c)
#           ifelse(c %in% names(idslib), structget(idslib[[c]]), '')),paste0),paste0,collapse=''))
#       })
#
#       struct[inds] = lapply(inds,function(i) paste(struct[inds][[i]],struct2[[i]],sep=paste0(rep('|',t),collapse='')))
#
#       rads = lapply(rads,function(v){
#         unlist(lapply(lapply(lapply(strsplit(v,''), sapply,function(c)
#           ifelse(c %in% names(idslib), radget(idslib[[c]]), '')),paste0),paste0,collapse=''))
#       })
#       inds = inds[which(sapply(inds,function(i) max(nchar(rads[[i]])) > 0))]
#       if(length(inds)==0) break
#     }
#
#     t = t+1
#   }
#   if(times>1) struct = lapply(struct,function(v) gsub('[|]+$','',v))
#   sapply(struct,paste0,collapse='_')
# }
#
# ids2rad = function(n, times = 1) {
#   #temp = strsplit(n, '')
#   temp = n
#   t = 0
#   inds = 1:length(temp)
#   while(t < times){
#     if(t==0){
#       rads = lapply(temp,function(v){
#         sapply(v, function(c)
#           ifelse(c %in% names(idslib), radget(idslib[[c]]), c))
#       })
#     }else{
#       newrads = lapply(rads[inds],function(v){
#         unlist(lapply(lapply(lapply(strsplit(v,''), sapply,function(c)
#           ifelse(c %in% names(idslib), radget(idslib[[c]]), c)),paste0),paste0,collapse=''))
#       })
#       rads[inds] = newrads
#
#       inds = inds[which(sapply(inds,function(i)!all(newrads[[which(inds==i)]] == rads[[i]])))]
#       if(length(inds)==0) break
#       #rads = lapply(1:length(rads),function(i) paste(rads[[i]],rads2[[i]],sep=paste0(rep('|',t),collapse='')))
#     }
#
#     t = t+1
#   }
#   sapply(rads,paste0,collapse='_')
# }
#
# str1lib = lapply(chars,ids2struct)
# rad1lib = lapply(chars,ids2rad)
# names(rad1lib) = chars
# names(str1lib) = chars
#
# rad1lib = list2env(rad1lib)
# str1lib = list2env(str1lib)
#
# save(rad1lib,file='data/rad1lib.rda')
# save(str1lib,file='data/str1lib.rda')
#
#
# #run on cluster
# library(parallel)
# library(foreach)
# library(doParallel)
#
# nc = detectCores()-1
# cl = makeCluster(nc,outfile='')
# registerDoParallel(cl)
#
# str100lib = foreach(c = chars) %dopar% {
#   print(which(chars==c)/length(chars))
#   ids2struct(c,100)
# }
#
# rad100lib = foreach(c = 1:length(chars)) %dopar% {
#   library(ChinSimi)
#   print(c/length(chars))
#   ids2rad(chars[c],100)
# }

