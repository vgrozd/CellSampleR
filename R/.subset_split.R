.subset_split <- function(split, cell.meta, subidents, idents,ident, split.by, sample.sizes, subtype.props, seed=seed){



  #GET INDEXES OF SPLIT & IDENT
  ist=sort_by_name(table(
    as.character(
      cell.meta[[subidents]][cell.meta[[idents]]==ident & cell.meta[[split.by]]==split]
    )
  ))

  soll=sort_by_name(
    unlist(sapply(subtype.props[[split]], FUN=function(x){round(x*as.numeric(sample.sizes[split]),0)})) ## TODO CHeck if equal sample.sizes, make sample.sizes same in previous step
  )

  # SUBSET AND RETURN INDICES
  # TODO Add seed option
  # TODO Also add check if subsampling with sampe proportions is possible with the cell numbers

  if(!setequal(names(ist), names(soll))) stop(paste0(split.by, " ", split, " misses ", subidents, " subclasses in class ", ident, "... \n Please use 'keep.proportions=TRUE'"))
  if(!all(names(ist)==names(soll))) stop("Sampling is not synchronized! ")
  subtypes=names(ist)
  return(
    as.numeric(
      unlist(
        sapply(subtypes, FUN=function(subtype){
          ind_of_subtype <- which(cell.meta[[subidents]]==subtype & cell.meta[[split.by]]==split)

          tryCatch({
            set.seed(seed)
          }, error=function(e){
            warning(paste0("Could not set seed to ", seed, ", using default seed ('42'). "))
            set.seed(42)
          })
          samp <- sort(sample(1:ist[subtype],soll[subtype],replace=FALSE)) #TODO remove sort here later for efficiency
          ind_of_subtype[samp]
        })
      )
    )
  )
}
