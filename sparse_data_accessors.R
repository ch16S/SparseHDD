
#Intial sparse functions.

rowMeans_sparse <- function (dgCMat) {
  RowInd <- dgCMat@i + 1
  sapply(split(dgCMat@x, RowInd), mean)
}

colMeans_sparse <- function (dgCMat) {
  ColInd <- dgCMat@p + 1
  sapply(split(dgCMat@x, ColInd), mean)
}



rowScale_sparse<-
  function(dgCMat,row_means)
  {
    RowInd <- dgCMat@i + 1
    index<- match( RowInd, 1:max(RowInd))
    dgCMat@x <- dgCMat@x - row_means[index]
    return(dgCMat)
  }

colScale_sparse<-
  function(dgCMat,col_means)
  {
    ColInd <- dgCMat@p + 1
    index<- match( RowInd, 1:max(ColInd))
    dgCMat@x <- dgCMat@x - col_means[index]
    return(dgCMat)
  }




calculate<-
  function(...){

    if(type==)

  }

ordinate

plot

agglomerate

ordinate<-


  ## Aggregate by sample information or taxonomy.

  aggregate<- function(sparse,aggregateby){



  }


shannon <-
  function(x){
    x <- x / rowSums(x)
    x <- -x * log(x,exp(1))
    H <- rowSums(x)
    return(H)
}


simpson<- function(x){
  H<- shannon(x)
  H<- 1- H
  return(H)
}


invSimpson<- function(x){
 H<- 1/simpson(x)
  return(H)
}

pielou<-
  function(x){
    H<- shannon(x)
    O<- richness(x)
    P<- shannon/richness
    return(P)
  }

richness<- function(x){
  H<- apply(x,1,function(k)sum(k>0))
  attr(H,"metric")<- "richness"
  class(H)<- "alphaDiversity"
  return(H)
}

calculate<- function(.data, whatFunc){
  func<- get(whatFunc)
  result<- func(.data$abundance)
  index<- ncol(.data$sampleInfo)+1

  .data$sampleInfo[,index]<- result
  colnames(.data$sampleInfo)[index]<- attr(result,"metric")
  attr(.data$sampleInfo[index],"plot_type")<- class(result)


}
filter<- function(assay,margin, flist){

   keep <- apply(assay$abundance,1,flist)
   assay$abundance<- assay$abundance[ ,keep ]
   taxa_keep<- which(featurs(assay$abundance) %in% rownames(assay$taxonomy))
   assay$taxonomy<- assay$taxonomy[taxa_keep, ]

   return(assay)

 }

subset<- function(grouping){

  keep<- dplyr::select(sparseHDD$sampleInfo)

}




data %>%
  aggomerate(margin="features", "Phylum") %>%
  calculate("shannon") %>%
  ggplot()+
  geom_point(aes(Phosphotase,shannon))+
  theme_light()

setGeneric("calculate",
           )


 x <- x * x
  H <- apply(x, MARGIN, sum, na.rm = TRUE)
  else H <- sum(x, na.rm = TRUE)
  if (index == "simpson")
    H <- 1 - H
  else if (index == "invsimpson")
    H <- 1/H
  if (any(NAS <- is.na(total)))
    H[NAS] <- NA
  H

}


calculate


setGeneric("rowSums",
           function(object){
             if(class(object)=="scooner"){
               plot(object)
             }else if(class(object)=="midi"){
               sum(object)
             }else{
               warning("incorrect class")
             }
           }
)



create_env()
``
