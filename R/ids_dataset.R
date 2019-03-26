#'Character decomposition dataset
#'
#'@source https://github.com/cjkvi/cjkvi-ids/blob/master/ids.txt
#'
#'@description processed with the following code:
#'ids = read_csv('C:/Users/phil_collender/Documents/bleh/RLpaper/RLmanuscript/ids.csv')
#'ids = subset(ids, Char %in% names(pylib))
#'ids$Decomp6 <- ids$Decomp7 <- NULL
#'
#'todo = lapply(ids,function(v) grep('\\[',v))
#'idscleaner = function(n){
#'  matches = unlist(regmatches(n,gregexpr('\\[.*?\\]',n)))
#'  keep = grepl('G',matches)
#'  n[keep] = gsub('\\[.*?\\]','',n[keep])
#'  n[!keep] = NA
#'  n
#'}
#'for(i in 1:length(todo)){
#'  ids[[i]][todo[[i]]] = idscleaner(ids[[i]][todo[[i]]])
#'}
#'for(i in 1:length(todo)){
#'  ids[[i]][is.na(ids[[i]])] <- ids[[i+1]][is.na(ids[[i]])]
#'  ids[[i+1]][is.na(ids[[i]])] <- NA
#'}
#'
#'structchar = c('\u2ff0','\u2ff1','\u2ff2','\u2ff3','\u2ff4','\u2ff5','\u2ff6','\u2ff7','\u2ff8','\u2ff9','\u2ffa','\u2ffb')
#'ids = subset(ids, substr(Decomp,1,1) %in% structchar)
#'nms = ids$Char
#'ids = lapply(1:length(nms),function(i){
#'  ids[i,2]
#'})

#'names(ids) = nms
#'idslib = list2env(ids)
#'save(idslib,file = 'data/idslib.rda')

"idslib"

