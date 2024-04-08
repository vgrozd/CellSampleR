#' Iterate over idents internal
#'
#' @param ident A character
#' @param keep.proportions A character
#' @param splits A character
#' @param cell.meta A character
#' @param subidents A character
#' @param idents A character
#' @param split.by A character
#' @param sample.sizes A character
#' @param equal_splits A character
#' @param seed A numeric
#'
#' @return Iteration over idents
#' @export
#'
iterate_over_idents <- function(ident, keep.proportions, splits, cell.meta, subidents, idents, split.by, sample.sizes, equal_splits, seed=seed){


  subtype.props <- list()
  if(keep.proportions){
    subtype.props[splits] <- lapply(splits, FUN=function(x){return(x=(prop.table(table(as.character(cell.meta[[subidents]][cell.meta[[idents]]==ident & cell.meta[[split.by]]==x])))))})

  }else{
    subtype.props[splits]<- rep(list(prop.table(table(as.character(cell.meta[[subidents]][cell.meta[[idents]]==ident])))),length(splits))
  }


  # GET EQUAL SPLIT
  if(equal_splits){sample.sizes[1:length(sample.sizes)] <- min(sample.sizes, na.rm=TRUE)}

  # ITERATE OVER SPLITS
  return(
    as.numeric(
      unlist(
        sapply(
          splits,
          subset_split,
          cell.meta=cell.meta,
          subidents=subidents,
          idents=idents,
          ident=ident,
          split.by=split.by,
          sample.sizes=sample.sizes,
          subtype.props=subtype.props,
          seed=seed
        )
      )
    )
  )

}
