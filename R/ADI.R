#' Function ADI
#' 
#' A package to calculate Dominance Indices, print Soical Network Graphs and Music Notation Graphs.
#' @name ADI
#' @param  data_sheet	 \bold{either} a data.frame f.e imported from a data sheet containing\cr  
#' "Name","item.number"\cr
#' "action.from.","action.to","kind.of.action"\cr
#' "name.of.action","action.number","classification","weighting"\cr
#' \cr
#' \bold{or} only "action.from.","action.to","kind.of.action"if exists actions and items\cr
#' \cr
#' actions: with  "name.of.action","action.number","classification","weighting\cr 
#' Weigting the factor which should be used to calculate the behavior (1 for "action.from"" wins -1 for "action.to" wins")\cr
#' Setting a behaviour to 2 means it is count double\cr
#' items:  with "Name","item.number"\cr 
#' @param  bytes	 a string where each enabled action is set to 1 and each disabled action is set to 0\cr
#' Setting a behaviour to 2 means it is count double\cr
#' @param \dots \bold{Additional parameters:}
#'  \describe{
#'  
#'   \item{\bold{actions}}{(data.frame) with "name.of.action","action.number","classification","weighting"; Classification 1 if "action.from"" wins; Classification 2 if "action.to" wins}
#'   \item{\bold{Weigting}}{the factor which should be used to calculate the behavior (1 for "action.from"" wins -1 for "action.to" wins")\cr
#' Setting a behaviour to 2 means it is count double}
#'   \item{\bold{vcolors}}{as much colors as items, colors will returned as sorted ADI colors means color 1 = item rank 1, color 2 = item rank 2, and so on}
#'   \item{\bold{workbook}}{the XlConnect Workbook for the Excel file to be changed\cr
#' note: The workbook must be opened before}
#'   \item{\bold{sheet}}{the sheet name ( ADI will be added to be sure not to delete any data}
#'   \item{\bold{savecounts}}{if TRUE: save the counts of actions as sheet (availalbe only with workbook)}
#'   \item{\bold{saveAdi}}{if TRUE: save the FDI as sheet (availalbe only with workbook)}
#'  }
#'
#' 
#' 
#' 
#' @return returns a list with\cr
#'ADI - the Average Dominance index\cr
#'Colors - the colors supported by vcolors sorted by ADI of the items\cr
#'ADI_count_matrix - the counts from which the ADI was calulated\cr
#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' @references {
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
#' wb <- loadWorkbook("sheet_for_using_with ADI_and FDI.xlsx",create=TRUE)\cr
#' createSheet(wb, name = 'ADI_DATA')\cr
#' writeWorksheet(wb,data_ADI,sheet='ADI_DATA')\cr
#' saveWorkbook(wb)\cr
#' \cr
#' \bold{to load the excel sheet:}\cr
#' library(XLConnect)\cr
#' setwd('/your data directory/')\cr
#' excelfile<-"sheet_for_using_with ADI_and FDI.xlsx"\cr
#' wb <- loadWorkbook("sheet_for_using_with ADI_and FDI.xlsx")\cr
#' data_sheet <- readWorksheet(wb1, sheet = "ADI_DATA")\cr  
#' }
#' @examples  #you can eihter use:
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
#' ADI(data_sheet,items=items,actions=actions,bytes)
#' # or you can use a complete f.e Excel sheet
#' # you can save this data as basic excel sheet to work with
#' data(data_ADI)
#' bytes= "001111111"  
#' ADI(data_ADI,bytes)
#' 
#' @export ADI
#' @importFrom gdata rename.vars  
#' @importFrom XLConnect createSheet writeWorksheet saveWorkbook
#' 

ADI <- 
  function(data_sheet,bytes,...){
  #TODO Check Excel file for errors
  #TODO Check and eleminate NA items
  
#--------------------- parameter ----------
args = list(...)

if ("workbook" %in% names(args)){
  wb <- args$workbook
}


if ("sheet" %in% names(args)) {
  sheet_new <- paste(args$sheet, "ADI",sep="-")
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

if ("saveAdi" %in% names(args)){
  savecounts <- args$saveAdi
  if ((saveAdi != TRUE) && (savecounts != FALSE)){
    warning("Error: saveAdi must be TRUE or FALSE, default FALSE")
    return
  }  
}
else
  saveAdi<- "FALSE"

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
if ("vcolors" %in% names(args))
  vcolors <- args$vcolors
else
  vcolors <-""

if  (countmatrix == FALSE){
  
 results <- search.win.lose(data_sheet,bits=bytes)
 win_lose_results <- results$data.win.lose
 items <- results$items    

 if (max(data_sheet$item.number,na.rm=TRUE) < length(vcolors))
           {
                    warning("Error max count of colors does not match")
                    stop('Error: max count of colors does not match count of item.number ')
      }
#----------------------------------------------------------------------------------
#
#
#TODO pruefen ob die Anzal der Tiere mit der laenge des namensvektors uebereinstimmt
#
#
#----------------------------------------------------------------------------------

  # Crsave also the counts of actions as sheet (availalbe only with workbook\create Data matrix
  ADI_Rownames =c(1:items+3)
  ADI_Rownames[1:items] = as.vector(data_sheet$Name[1:items])
  ADI_Rownames[items+1] = "results.ADI"
  ADI_Rownames[items+2] = "id"
  ADI_Rownames[items+3] = "rank" 
  tempdata <- matrix(0,nrow=items,ncol=items+2, dimnames = list(ADI_Rownames[1:items],ADI_Rownames[1:(items+2)]))
  # set diagonal left/up to right/down to NA
  for (I in (1:items))
    tempdata[I,I] <- NA
  # Fill matrix  
  for (I in (1: length(win_lose_results$wins)))
  { 
  tempdata[as.integer(win_lose_results$wins[I]),as.integer(win_lose_results$loses[I])] <-  
  tempdata[as.integer(win_lose_results$wins[I]),as.integer(win_lose_results$loses[I])]+ 1
  }
}  #if  (countmatrix == FALSE)
else
{
  tempdata <- data_sheet
  items =length(tempdata[,1])
  ADI_Rownames =c(1:items+3)
  ADI_Rownames[1:items] = rownames(data_sheet)
  ADI_Rownames[items+1] = "results.ADI"
  ADI_Rownames[items+2] = "id"
  ADI_Rownames[items+3] = "rank" 
  
}
   tempdata 
  result.data <- matrix(0,nrow=items,ncol=items+2,   #Items + ADI + RAnge
        dimnames = list(ADI_Rownames[1:items],ADI_Rownames[1:(items+2)]))

  # matrix[down,right] 
  for (column in (1:items))
    result.data[column,column] <- NA

   for (column in (1:items))
       for (Row in (1:items))
      {
      if (Row != column)
        result.data[column,Row] <- tempdata[column,Row]/ (tempdata[column,Row]+tempdata[Row,column])
      }  
      
   count_points <- 0
      
   for (Row in (1:items))
   {   count.points <- 0
       count.relations <- 0

       for (column in (1:items))
      {                                                                        
        if (!is.na(result.data[column,Row]))
        {
            count.points <- count.points + result.data[Row,column] 
            count.relations <- count.relations +1
         }   
      }  
      
      if (count.relations > 0)
      result.data[Row,column+1] <- count.points/count.relations
    }  
    
#--------------------------- sort matrix    -----------
test<-0
test2<-0 
id=c(1:items)
result.data[,items+2]=id

test <- as.data.frame(result.data)
test <- test[order(test$results.ADI,decreasing = TRUE),]
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


test2 <- rename.vars(test2, colnames(test2), c(rownames(test),colnames(test2[length(colnames(test2))-1]),colnames(test2[length(colnames(test2))])),info=FALSE)
test2[,items+3]= test2[,items+3]= data.frame("rank"=position)

result.data <- as.matrix(test2)
#--------------------------- end sort matrix    -----------

#     result.data <- result.data[order(result.data_sheet$results.ADI) , ] 
#----------------------------------------------------------------------

if ((exists("wb") )  && (exists("sheet_new"))) {
  if (saveAdi==TRUE){
  createSheet(wb, name = sheet_new)
  writeWorksheet(wb,result.data,sheet=sheet_new,rownames="Row Names")
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

   return(list("ADI"=result.data,"Colors"=vcolors2,"ADI_count_matrix"=tempdata[,1:items]))

rm(test2)
rm(test)
rm(tempdata)   
rm(vcolors2)
rm(position)
rm(id)
}
