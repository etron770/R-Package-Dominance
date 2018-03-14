#' Function FDI
#' 
#' A package to calculate Dominance Indices, print Soical Network Graphs and Music Notation Graphs.
#' @name FDI
#' @param  data_sheet	 \bold{either} a data.frame f.e imported from a data sheet containing\cr  
#' "Name","item.number"\cr
#' "action.from.","action.to","kind.of.action"\cr
#' "name.of.action","action.number","classification","weighting"\cr
#' \cr
#' \bold{or} only "action.from.","action.to","kind.of.action"if exists actions and items\cr
#' \cr
#' actions: with  "name.of.action","action.number","classification","weighting\cr 
#' items:  with "Name","item.number"\cr 
#' @param  bytes	 a string where each enabled action is set to 1 and each disabled action is set to 0\cr
#' Setting a behaviour to 2 means it is count double
#' @param \dots \bold{Additional parameters:}
#'  \describe{
#'   \item{\bold{actions}}{(data.frame) with "name.of.action","action.number","classification","weighting"; Classification 1 if "action.from"" wins; Classification 2 if "action.to" wins}
#'   \item{\bold{Weigting}}{the factor which should be used to calculate the behavior (1 for "action.from"" wins -1 for "action.to" wins")\cr
#' Setting a behaviour to 2 means it is count double}
#'   \item{\bold{vcolors}}{as much colors as items, colors will returned as sorted FDI colors means color 1 = item rank 1, color 2 = item rank 2, and so on}
#'   \item{\bold{workbook}}{the XlConnect Workbook for the Excel file to be changed\cr
#' note: The workbook must be opened before}
#'   \item{\bold{sheet}}{the sheet name ( FDI will be added to be sure not to delete any data}
#'   \item{\bold{savecounts}}{if TRUE: save the counts of actions as sheet (availalbe only with workbook}
#'   \item{\bold{saveFDI}}{if TRUE: save the FDI as sheet (availalbe only with workbook}
#'  }  
#' 
#' @return returns a list with\cr
#'FDI - the Frequency Based Dominance index\cr
#'Colors - the colors supported by vcolors sorted by FDI of the items\cr
#'FDI_count_matrix - the counts from which the FDI was calulated\cr
#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' @references{
#' The Construction of Dominance Order: Comparing Performance of Five Methods Using an Individual-Based Model C. K. Hemelrijk, J. Wantia and L. Gygax, Behaviour Vol. 142, No. 8 (Aug., 2005), pp. 1037-1058
#' \url{http://www.jstor.org/stable/4536286}\cr
#'On using the DomWorld model to evaluate dominance ranking methods , de Vries, Han,  Behaviour, Volume 146, Number 6, 2009 , pp. 843-869(27)
#'\url{http://dx.doi.org/10.1163/156853909X412241}
#' }
#' @section Hints:{
#' \bold{to create a excel sheet for ADI data:}\cr
#' library(XLConnect)\cr 
#' data(data_ADI)\cr
#' setwd('/your data directory/')\cr 
#' wb <- loadWorkbook("sheet_for_using_with_ADI_and_FDI.xlsx",create=TRUE)\cr
#' createSheet(wb, name = 'FDI_DATA')\cr
#' writeWorksheet(wb,data_FDI,sheet='FDI_DATA')\cr
#' saveWorkbook(wb)\cr
#' \cr
#' \bold{to load the excel sheet:}\cr
#' library(XLConnect)\cr
#' setwd('/your data directory/')\cr
#' wb <- loadWorkbook("sheet_for_using_with_ADI_and_FDI.xlsx")\cr
#' data_sheet <- readWorksheet(wb1, sheet = "FDI_DATA")\cr  
#' }
#' @examples { #you can eihter use:
#' data_sheet=data.frame   ("action.from"=c(1,4,2,3,4,3,4,3,4,3,4,3,4,3,4),
#'                          "action.to"=c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
#' "kind.of.action"= c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),stringsAsFactors=FALSE)
#' items= data.frame ("Name"=c("item1","item2","item3","item4","item5","item6") ,
#'                    "item.number"=c(1:6),stringsAsFactors=FALSE)
#' actions=data.frame("name.of.action"= c("leading","following","approach","bite","threat to bite",
#'                                       "kick","threat to kick", "chase","retreat"),
#'                   "action.number"=c(1:9),
#'                   "classification"=c(1,2,1,1,1,1,1,1,2) ,
#'                   "weighting"=c(1,-1,1,1,1,1,1,1,-1),stringsAsFactors=FALSE)
#' #all  encounters  without leading and following
#' bytes= "001111111"  
#' FDI(data_sheet,items=items,actions=actions,bytes)
#' # or you can use a complete f.e Excel sheet
#' # you can save this data as basic excel sheet to work with
#' data(data_ADI)
#' bytes= "001111111"  
#' FDI(data_ADI,bytes)
#'    }
#' @export FDI
#' @importFrom gdata rename.vars  
#' @importFrom XLConnect createSheet writeWorksheet saveWorkbook



FDI <-
  function(data_sheet,bytes,...){
    
    #TODO Check Excel file for errors
    #TODO Check and eleminate NA items
    #TODO Check Excel file einbauen
    #TODO Check and eleminate NA items
    #TODO Vcolor ausgeben

    
    #--------------------- ?bergabe parameter ----------
    args = list(...)
    
    if ("workbook" %in% names(args)){
      wb <- args$workbook
    }
    
    
    if ("sheet" %in% names(args)) {
      sheet_new <- paste(args$sheet, "FDI",sep="-")
      sheet_new_counts <- paste(args$sheet, "counts",sep="-")
    }
    
    if ("savecounts" %in% names(args)){
      savecounts <- args$savecounts
      if ((savecounts != TRUE) && (savecounts != FALSE)){
        warning("Error: savecounts must be TRUE or FALSE, default FALSE")
        return
      }  
    }
    else
      savecounts<- "FALSE"
    if ("saveFdi" %in% names(args)){
      savecounts <- args$saveFdi
      if ((saveFdi != TRUE) && (savecounts != FALSE)){
        warning("Error: saveFdi must be TRUE or FALSE, default FALSE")
        return
      }  
    }
    else
      saveFdi<- "FALSE"
    
    if ("countmatrix" %in% names(args)){
      countmatrix <- args$countmatrix
      if ((countmatrix != TRUE) && (countmatrix != FALSE)){
        warning("Error: countmatrix must be TRUE or FALSE, default FALSE")
        return
      }  
    }
    else
      countmatrix <- "FALSE"
    
    # as we build the package for reading a complete excel sheet we must build one data.frame to compute singel frames
    if (("actions" %in% names(args)) &  ("items" %in% names(args))) 
    { 
      actions <- args$actions
      items <- args$items   
      data_length = length(data_sheet$action.from)
      tempNA= c(1:data_length)
      tempNA[1:data_length] =NA
      tempString_NA= c(1:data_length)
      tempString_NA[1:data_length] = "<NA>"
      
      data_temp=data.frame("action.from"=data_sheet$action.from,"action.to"=data_sheet$action.to,"kind.of.action"=data_sheet$kind.of.action,
                           "Name"=tempString_NA,"item.number"=tempNA,
                           "name.of.action"=tempString_NA,
                           "action.number"=tempNA,
                           "classification"=tempNA,
                           "weighting"=tempNA,stringsAsFactors=FALSE)
      
      data_temp$Name[1:length(items$Name)] = items$Name [1:length(items$Name)]
      data_temp$item.number[1:length(items$item.number)] =items$item.number
      data_temp$name.of.action[1:length(actions$name.of.action)] =actions$name.of.action
      data_temp$action.number[1:length(actions$action.number)] = actions$action.number
      data_temp$classification[1:length(actions$classification)] = actions$classification
      data_temp$weighting[1:length(actions$weighting)] = actions$weighting
      
      data_sheet = data_temp  # compute with the complete frame
      
    }
    
    
    if (countmatrix == FALSE) {
      FDIResult =  ADI(data_sheet,bytes)
      FDI = FDIResult[3]$ADI_count_matrix
      vcolors = FDIResult[2]$Colors
    }  
    else {
      vcolors = ""
      FDI = data_sheet
    }  
    
    items= length(FDI[1,])
    FDI_Rownames =c(1:(items+2))  # add additional FDI Rownames
    FDI_Rownames[1:items] = rownames(FDI)
    FDI_Rownames[items+1] = "Sum_Bi"
    FDI_Rownames[items+2] = "Sum_bij"
    
    
    
    FDI_Colnames =c(1:(items+4)) # add additional FDI Colnames
    FDI_Colnames[1:items] = colnames(FDI)
    FDI_Colnames[items+1] = "Sum_Li"
    FDI_Colnames[items+2] = "Sum_lij"
    FDI_Colnames[items+3] = "FDI" 
    FDI_Colnames[items+4] = "id"
    FDI_Colnames[items+5] = "rank"   
    
    tempdata <- matrix(NA,nrow=items+2,ncol=items+5, dimnames = list(FDI_Rownames,FDI_Colnames)) #create matrix
    
    for (I in (1: items)) {   #fill matrix with FDI count matrix
      tempdata[I,] =  c(FDI [I,] ,0,0,0,0,0)
    }
    
    
    # Fill matrix  
    for (I in (1: items))  #calculate Sum_Bi
    {
      tempdata[ items+1,I] =sum(tempdata[,I], na.rm = TRUE)
    }   
        
    for (I in (1: items)) #calculate Sum_Li
    {
      tempdata[I, items+1] =sum(tempdata[I,], na.rm = TRUE)
    }   
      
    
    for (I in (1: items))  #calculate Sum_Lij
    { Lij_temp =0 
      
      for (J in (1: items)) 
      {
        if ((  tempdata[I,J] > 0) &  ( I != J )) {
          tempdata[I,J]
          Lij_temp =Lij_temp + tempdata[J, items+1]
        }
      }   
      tempdata[I, items+2] =Lij_temp
    }   
    
    for (J in (1: items))  #calculate Sum_Bij
    { bij_temp =0 
      
      for (I in (1: items)) 
      {
        if ((  tempdata[I,J] > 0) &  ( I != J )) {
          bij_temp =bij_temp + tempdata[items+1,I]
        }
      }   
      tempdata[items+2,J] =bij_temp
    }  
    

    for (I in (1: items)) { #calculate FDI 
      tempdata[I,items+3] = ((tempdata[items+1,I] + tempdata[items+2,I]+1)/ (tempdata[I,items+1]+tempdata[I,items+2]+1))
    
    }
    #--------------------------- sort matrix    -----------
    
    #print(tempdata)
    test<-0
    test2<-0 
    id=c(1:items)
    tempdata[1:items,items+4]=id
    
    test <- as.data.frame(tempdata)
    test <- test[order(test$FDI,decreasing = FALSE),]  #TODO Order increasing but without SUM_BI BIJ
    position=c(1:items)
    test2<-test
    vcolors2<- vcolors
    for (X in (1:length(test[,1]-1)))
      for (J in (1:length(test[,1]-1)))
        if  (rownames(test[X,]) == colnames(test[J])){
          test2[,X]<-test[,J]
          colnames(test2[X])<- colnames(test[J])
          if (vcolors[1] != ""){
            vcolors2[X] <- vcolors[J]
          }
        }
    
    
    test2 <- rename.vars(test2, colnames(test2),c(rownames(test),colnames(test2[length(colnames(test2))-2]),colnames(test2[length(colnames(test2))-1]),colnames(test2[length(colnames(test2))])),info=FALSE)
    test2[1:items,items+5]= test2[1:items,items+5]= data.frame("rank"=position)
    
   tempdata <- as.matrix(test2)
    #--------------------------- end sort matrix    -----------
    
    #----------------------------------------------------------------------
    
    if ((exists("wb") )  && (exists("sheet_new"))) {
      if (saveFdi==TRUE){
            createSheet(wb, name = sheet_new)
            writeWorksheet(wb,tempdata,sheet=sheet_new,rownames="Row Names")
      }      
      if(savecounts ==TRUE){
        createSheet(wb, name = sheet_new_counts)
        writeWorksheet(wb,tempdata[,1:items],sheet=sheet_new_counts,rownames="Row Names") 
        
      }
      saveWorkbook(wb)
      #delete warnings until Problem 
      #1: In names(res)[1] <- colname :
      #  number of items to replace is not a multiple of replacement length
      # when adding rowmanes with writeWorksheet is solved
      assign("last.warning", NULL, envir = baseenv())  
      
      
    }
    #else
    #  print('Remarks: No changes to excel sheet: missing wb or sheet')
    
    
    FDI <-  tempdata
    
    rm(tempdata)
    rm(I)
    rm(J)   
    rm(bij_temp)
    rm(Lij_temp)
    rm(FDI_Colnames)
    rm(FDI_Rownames)   
    return (FDI)
    
    
    #  TODO   save Workbook
  }