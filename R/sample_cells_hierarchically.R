#' sample_cells_hierarhically.R
#'
#' @param cell.meta A character
#' @param idents A character
#' @param subidents A character
#' @param split.by A character
#' @param equal_splits A character
#' @param as.data A character
#' @param sample.size A character
#' @param keep.proportions A character
#' @param seed A character
#' @param plot A character
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' sample_cells_hierarchically()
#'
sample_cells_hierarchically <- function(
    cell.meta,
    idents,
    subidents,
    split.by,
    equal_splits=FALSE,
    as.data=FALSE,
    sample.size=NULL,
    keep.proportions=FALSE,
    seed=42,
    plot=FALSE
){

  # SANITY CHECKS
  if(!is.null(sample.size) & equal_splits) stop("Please either provide a fixed sample size or specify equal_splits, but not both")
  if(is.null(idents) | !(idents %in% colnames(cell.meta))) stop("Please specify an 'idents' name that is a feature of 'cell.meta'")
  if(is.null(subidents) | !(subidents %in% colnames(cell.meta))) stop("Please specify a 'subidents' name that is a feature of 'cell.meta'")
  if(is.null(split.by) | !(split.by %in% colnames(cell.meta))) stop("Please specify a 'split.by' name that is a feature of 'cell.meta'")
  if(!is.numeric(seed)) stop("Please specify a valid 'seed'")

  # Add argument checks

  # GET IDENT GROUPS
  .idents=unique(as.character(cell.meta[[idents]]))

  # GET SPLITS
  splits = as.character(unique(cell.meta[[split.by]]))

  # GET SAMPLE SIZES
  sample.sizes=c()
  if(!is.null(sample.size)){
    if(sample.size>min(table(cell.meta[[idents]], cell.meta[[split.by]]), na.rm=TRUE)) stop(paste0("Sample size is larger than the smallest ident group (", min(table(cell.meta[[idents]], cell.meta[[split.by]]), na.rm=TRUE), "). Please reduce 'sample.size'"))
    sample.sizes[splits]=rep(sample.size, length(splits))
  } else {
    if(equal_splits){
      sample.sizes[splits]=rep(min(table(cell.meta[[idents]], cell.meta[[split.by]]), na.rm=TRUE), length(splits))
    } else{
      sample.sizes=sapply(splits,FUN=function(x){min(table(cell.meta[[idents]][cell.meta[[split.by]]==x]), na.rm=TRUE)})
    }
  }

  # ITERATE OVER IDENTS
  .ind <- as.numeric(
    unlist(
      sapply(
        .idents,
        FUN=iterate_over_idents,
        keep.proportions=keep.proportions,
        splits=splits,
        cell.meta=cell.meta,
        subidents=subidents,
        idents=idents,
        split.by=split.by,
        sample.sizes=sample.sizes,
        equal_splits=equal_splits,
        seed=seed
      )
    )
  )
  # RETURN DATA/INDEX
  return(.ind)
  # return(ifelse(as.data,cell.meta[.ind,],.ind))
}
