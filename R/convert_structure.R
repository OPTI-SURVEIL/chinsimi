#'Extract structure information from Chinese Character strings.
#'@param Chin.strs A vector of Chinese character strings
#'@param sep Separating character to be placed between results for individual Hanzi
#'@param parallel option to run with parallel calculations
#'@param full Boolean option to return either major character structure only, or layered structure information based on full radical decomposition
#'@return A vector of unicode ideograph structure strings representing the
#'  structure of the input characters
#'@examples
#'ChStr2struct('凨冪',full=F)
#'ChStr2struct('凨冪',full=T)

ChStr2struct <- function(Chin.strs = "", sep = "_", parallel = FALSE, full=FALSE)
{
  # Convert one string to four corner code
  if(full){
    strlib = str100lib
  }else{
    strlib = str1lib
  }

  ChStr2struct <- function(Chin.str, strlib){
    Sys.setlocale(category = 'LC_ALL', locale = 'chs')
    if(is.na(Chin.str)) return(NA)
    Chin.char <- unlist(strsplit(Chin.str, split = "")) # divide the string to characters

    # convert a single character to pinyin
    ChChar2struct <- function(Chin.char){
      ChCharstruct <- strlib[[Chin.char]]

      if(length(ChCharstruct) == 0) ChCharstruct = '*'

      return(ChCharstruct)
    }

    paste(sapply(Chin.char, ChChar2struct), collapse = sep)
  }

  # Use parallel computing to convert strings if parallel is TRUE
  if(parallel)
  {
    no_cores <- parallel::detectCores() - 1  # Get the number of available string
    cl <- parallel::makeCluster(no_cores)   # Initiate cluster
    struct <- parallel::parSapply(cl, X = Chin.strs, FUN = ChStr2struct, strlib)
    parallel::stopCluster(cl)
    return(struct)
  } else {
    sapply(Chin.strs, ChStr2struct, strlib)
  }
}
