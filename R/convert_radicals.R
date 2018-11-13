#'Extract radical decomposition of Chinese Character strings.
#'@param Chin.strs A vector of Chinese character strings
#'@param sep Separating character to be placed between results for individual
#'  Hanzi
#'@param parallel option to run with parallel calculations
#'@param full Boolean option to return either a single round of decomposition
#'  (if F), or full character decomposition (if T)
#'@return A vector of radical decomposition strings from the original input
#'
#'@examples
#'ChStr2rad('凨冪',full=F)
#'ChStr2rad('凨冪',full=T)

ChStr2rad <- function(Chin.strs = "", sep = "_", parallel = FALSE, full=FALSE)
{
  # Convert one string to four corner code
  if(full){
    radlib = rad100lib
  }else{
    radlib = rad1lib
  }

  ChStr2rad <- function(Chin.str, radlib){
    Sys.setlocale(category = 'LC_ALL', locale = 'chs')
    if(is.na(Chin.str)) return(NA)
    Chin.char <- unlist(strsplit(Chin.str, split = "")) # divide the string to characters

    # convert a single character to pinyin
    ChChar2rad <- function(Chin.char){
      ChCharrad <- radlib[[Chin.char]]

      if(length(ChCharrad) == 0) ChCharrad = Chin.char

      return(ChCharrad)
    }

    paste(sapply(Chin.char, ChChar2rad), collapse = sep)
  }

  # Use parallel computing to convert strings if parallel is TRUE
  if(parallel)
  {
    no_cores <- parallel::detectCores() - 1  # Get the number of available string
    cl <- parallel::makeCluster(no_cores)   # Initiate cluster
    rad <- parallel::parSapply(cl, X = Chin.strs, FUN = ChStr2rad, radlib)
    parallel::stopCluster(cl)
    return(rad)
  } else {
    sapply(Chin.strs, ChStr2rad, radlib)
  }
}
