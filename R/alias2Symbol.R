#' alias2Symbol_Mm
#'
#' @param alias of a gene
#'
#' @return corresponding symbol according to MGI or HGNC nomenclature
#' @export
#'
#' @examples
alias2Symbol_Cvekl <- function(alias,species="Mm"){
  symbol <- unlist(limma::alias2Symbol(alias, species=species), use.names = F)
  if (identical(symbol, character(0))) {
    return(alias)
  }else if(identical(symbol, character(0))==F){
    return(paste(symbol, collapse=";"))
  }
}
