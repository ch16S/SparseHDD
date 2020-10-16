#
#
#
#


create_assay<- function(abundance,taxonomy=NULL, assay_class="bacteria" ){

  if(attr(abundance,"type")!= "abundance"){
    stop("requires abundance data")
  }

  if(!is.null(taxonomy)){

    if(attr(taxonomy,"type")!= "taxonomy"){
      stop("requires taxonomic data")
    }
    if(! rownames(taxonomy) %in% colnames(abundance) ){
      stop("taxonomy does not match abundance")
    }
  }

   assay<- list(abundance=abundance,taxonomy=taxonomy)
   class(assay)<- c(assay_class,"assay")

   return(assay)
}


create_sparseHDD<- function(sampleInfo,assays){

  if(! "assay" %in%  class(assays[[1]]) ){
    stop("assays should be a list of length N assays")
  }

  for(i in seq_along(assays)){

    if(any((samples(assayList[[i]]) %in% si$sample)==FALSE)){

      remove<- which((samples(assayList[[1]]) %in% si$sample)==FALSE)
      assays[[i]]$abundance<- assays[[i]]$abundance[-remove,]
    }
  }


  no_assays<- length(assays) +1

  assay_names<- sparseHDD<- vector("list",length=no_assays)
  sparseHDD[[1]]<- sampleInfo
  assay_names[1]<- "sampleInfo"
  for(i in seq_along(assays)){
    j<- i+1

    sparseHDD[[j]]<- assays[[i]]
    assay_names[j]<- class(assays[[i]])[1]
  }

  names(sparseHDD)<- assay_names
  class(sparseHDD)<- "sparseHDD"
  return(sparseHDD)

}


features<- function(assay){
  return(colnames(assay$abundance))
}

samples<- function(assay){
  return(rownames(assay$abundance))
}



sparse_SV_table<- function(database=NULL){
  asv_long<- as_tibble(tbl(con,database))

  asvs<- as.integer(as.factor(asv_long$SV))
  samples<-  as.integer(as.factor(asv_long$MetagenNumber))
  unq_asvs<- unique(asv_long$SV)
  unq_samples<- unique(asv_long$MetagenNumber)


  trpl<- slam::simple_triplet_matrix(i=samples,
                                     j=asvs,
                                     v=asv_long$Abundance,
                                     nrow=length(unq_samples),
                                     ncol= length(unq_asvs ),
                                     dimnames=list(unq_samples, unq_asvs))


  class(trpl)<-c("abundance","simple_triplet_matrix")
  return(trpl)
}

to_dgCmatrix<- function(d){
  sm<- sparseMatrix(i = d$i, j = d$j, x = d$v)
  rownames(sm)<- d$dimnames[[1]]
  colnames(sm)<- d$dimnames[[2]]
  attr(sm,"type")<- c("abundance")
  return(sm)
}


sample_info(sampleInfo,sample_names){
  sampleInfo$samples<- sampleInfo[,sample_names]
  attr(sampleInfo,"type")<- "sample_info"
  return(sampleInfo)
}

