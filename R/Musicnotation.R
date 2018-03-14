#' Function musicnotation
#' 
#' @name musicnotation
#' @description A function to visualize interaction wit a musicnotation graph.
#' @param data_sheet	 \bold{either} a data.frame f.e imported from a data sheet containing\cr  
#' "Name","item.number"\cr
#' "action.from.","action.to","kind.of.action"\cr
#' "name.of.action","action.number","classification","weighting"\cr
#' \cr
#' \bold{or} only "action.from.","action.to","kind.of.action"if exists actions and items\cr
#' \cr
#' actions: with  "name.of.action","action.number","classification","weighting\cr 
#' items:  with "Name","item.number"\cr 
#' Setting a behaviour to 2 means it is count double\cr
#' 
#' @param \dots \bold{Additional parameters:}
#'  \describe{
#'   \item{\bold{colors}}{a factor of colors as much as actions}
#'   \item{\bold{lwd}}{line width if lwd_arrows is not used also for line width arrows}
#' # TODO check this it not working -> no show_items all items will be shown
#'   \item{\bold{show_items}}{items to be shown}
#'   \item{\bold{angel_arrows}}{The angel aof the arrow head default 20}
#'   \item{\bold{length_arrows}}{the lenght of the arrow default 0.05}
#'   \item{\bold{lwd_arrows}}{the line width of the arrows default 1}
#'   \item{\bold{actions_colors}}{a vector of colors for actions f.e to show one special action}
#'   \item{\bold{starting_time}}{builds the graph with data bewteen starting and ending time}
#'   \item{\bold{ending_time}}{builds the graph with data bewteen starting and ending time}
#'   \item{\bold{user_colors}}{a vector of colors as much as items to show differetn colors for items}
#'   \item{\bold{color_bits}}{a vector of colors as much as items 1 shows the horse colored 0 in black (defined with actions_colors)}
#' }
#' 
#' @return returns a list with\cr
#'ADI - the Average Dominance index\cr
#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' @references {
#' #Chase, I. D. (2006). Music notation: a new method for visualizing social interaction in animals and humans. Front Zool, 3, 18. 
#' \url{http://dx.doi.org/10.1186\%2F1742-9994-3-18}\cr
#' }
#' @section Hints:{
#' \bold{to create a excel sheet for Musicnotation data:}\cr
#' library(XLConnect)\cr 
#' data(data_Musicnotation)\cr
#' setwd('/your data directory/')\cr 
#' wb <- loadWorkbook("sheet_for_using_with_Musicnotation.xlsx",create=TRUE)\cr
#' createSheet(wb, name = 'Musicnotation_DATA')\cr
#' writeWorksheet(wb,data_Musicnotation,sheet='Musicnotation_DATA')\cr
#' saveWorkbook(wb)\cr
#' \cr
#' \bold{to load the excel sheet:}\cr
#' library(XLConnect)\cr
#' setwd('/your data directory/')\cr
#' wb <- loadWorkbook("sheet_for_using_with_Musicnotation.xlsx")\cr
#' data_sheet <- readWorksheet(wb1, sheet = "Musicnotation_DATA")\cr  
#' }
#' @examples { #you can eihter use:
#' data_sheet=data.frame   ("action.from"=c(1,4,2,3,4,3,4,3,4,3,4,3,4,3,4),
#'                          "action.to"=c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
#'                          "kind.of.action"= c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
#'                          "Time"=c('03:15:00','03:17:30','03:20:00','03:20:30','03:21:00',
#'                                   '03:21:30','03:22:00','03:22:30','03:23:00','03:23:30',
#'                                   '03:25:00','03:25:30','03:26:00','03:26:30','03:27:00'),
#'                          stringsAsFactors=FALSE)
#' items= data.frame ("Name"=c("item1","item2","item3","item4","item5","item6") ,
#'                    "item.number"=c(1:6),stringsAsFactors=FALSE)
#' actions=data.frame("name.of.action"= c("leading","following","approach","bite","threat to bite",
#'                                       "kick","threat to kick", "chase","retreat"),
#'                   "action.number"=c(1:9),
#'                   "classification"=c(1,2,1,1,1,1,1,1,2) ,
#'                   "weighting"=c(1,-1,1,1,1,1,1,1,-1),stringsAsFactors=FALSE)
#' # set colors for special encounters
#' color= c("green","green","red","red","red","red","red","red")    
#' 
#' Musicnotation(data_sheet=data_sheet,actions=actions,items=items,sort_dominance=TRUE)
#' #or you can use a complete f.e Excel sheet
#' #you can save this data as basic excel sheet to work with
#' data(data_Musicnotation)
#' Musicnotation(data_sheet=data_Musicnotation,sort_dominance=TRUE) }
#' @export Musicnotation
#' @importFrom XLConnect createSheet writeWorksheet saveWorkbook
#' @importFrom grDevices rainbow
#' @importFrom chron times
#' @importFrom graphics plot par text lines title arrows

Musicnotation <-
function(data_sheet, ...)
{
 args = list(...)
# as we build the package for reading a complete excel sheet we must build one data.frame to compute singel frames
#We can use the same data sheet as for ADI or sociogram but we need addtional the time column
if (("actions" %in% names(args)) &  ("items" %in% names(args))) 
{ 
  actions <- args$actions
  items <- args$items   

  data_length = length(data_sheet$action.from)
  temp_NA= c(1:data_length)
  temp_NA[1:data_length] =NA
  tempString_NA= c(1:data_length)
  tempString_NA[1:data_length] = "<NA>"

  data_temp=data.frame("action.from"=data_sheet$action.from,
                       "action.to"=data_sheet$action.to,
                       "kind.of.action"=data_sheet$kind.of.action,
                       "Time"=data_sheet$Time,
                       "Name"=tempString_NA,
                       "item.number"=temp_NA,
                       "dominance.order"=temp_NA,
                       "name.of.action"=tempString_NA,
                       "action.number"=temp_NA,
                       "classification"=temp_NA,
                       "weighting"=temp_NA,stringsAsFactors=FALSE)

  data_temp$Name[1:length(items$Name)] = items$Name [1:length(items$Name)]
  data_temp$item.number[1:length(items$item.number)] =items$item.number
  data_temp$dominance.order[1:length(items$item.number)] =items$item.number
  data_temp$name.of.action[1:length(actions$name.of.action)] =actions$name.of.action
  data_temp$action.number[1:length(actions$action.number)] = actions$action.number
  data_temp$classification[1:length(actions$classification)] = actions$classification
  data_temp$weighting[1:length(actions$weighting)] = actions$weighting

  data_sheet = data_temp  # compute with the complete frame

}
if (length(grep('-', data_sheet$Time[1]))>0) # time format   comes as "1899-12-31 10:00:00 UTC" when imported by readWorksheet(
{
data_sheet$Time=format(data_sheet$Time, "%H:%M:%S")
}

# ---------------------------------
#      Testvariablen


#----------------------------------
	count <- 1
# ------------------ checking number of used does not exeed max_items.

  max_items<- 0
  min_items<- 0   #TODO for What are they ??
	max_items <- max(data_sheet$action.from,na.rm=TRUE)
	count_max <- length(data_sheet$action.from)
	if (max_items < max(data_sheet$action.to,na.rm=TRUE))
		max_items <- max(data_sheet$action.to,na.rm=TRUE)
      if (max(data_sheet$item.number,na.rm=TRUE) < max_items)
	{
		stop(paste("Error max count of items: ",max(data_sheet$action.from),   " does not match max Items: ", ,max(data_sheet$item.number,na.rm=TRUE)))
	                                              
  }
if ("sort_dominance" %in% names(args))
{ sort_dominance <- args$sort_dominance  
} 
 else 
 {
  sort_dominance=TRUE
 }  
if ("lwd" %in% names(args))
{
 lwd <- args$lwd  
} 
else
{
 lwd=1 
 }

 name.of.action  <- c(1:max(data_sheet$action.number,na.rm=TRUE))
if ("bytes" %in% names(args))
{
 bytes <- args$bytes  
 } 
else
{
  bytes = c(1:max(data_sheet$action.number,na.rm=TRUE))
  bytes[1:length(bytes)]=1
  bytes = toString(bytes)
  bytes= gsub("[,]", "", bytes)
  bytes= gsub("[ ]", "", bytes)

 }



if ("lwd_arrows" %in% names(args))
{
 lwda <- args$lwd_arrows  
 } 
else
{
 lwda=lwd 
 }

if ("angel_arrows" %in% names(args))
{
 angel_a <- args$angel_arrows  
 } 
else
{
 angel_a=20 
 }

if ("length_arrows" %in% names(args))
{
 length_a <- args$length_arrows  
 } 
else
{
 length_a=0.05 
 }


      
  if ("show_items" %in% names(args))
  {
    show_items <- args$show_items
  }
  else
  {
   show_items=c(1:max(data_sheet$item.number,na.rm=TRUE))
   show_items[1:length(show_items)]=1
   show_items = toString(show_items)
   show_items= gsub("[,]", "", show_items)
   show_items= gsub("[ ]", "", show_items)
   }
   
#print("Music Notation: using show_items:")   
#print(show_items)



max_items <-  max(data_sheet$item.number,na.rm=TRUE)       # because some items may not be used in actions
# ----------- end checking number of used does not exeed max_items.
# -----------   defining colors  ----------------------------------
       
if ("action_colors" %in% names(args))
{ 
  		max_actions <- max(data_sheet$action.number,na.rm=TRUE)
  		if (length(args$action_colors) != max_actions)
  		{	

   		stop(paste("Error max count of action.number: ", max(data_sheet$action.number,na.rm=TRUE)," does not max colors: ",length(args$action_colors)))
  		      
  	 	} # if
     action_color <- args$action_colors
     change_action_color = TRUE
}
else {
  change_action_color = FALSE
  }


if ("user_colors" %in% names(args))
{ 
    if (length(args$user_colors) < max_items )
    {
    		stop(paste("Error max count of items: ",max_items, "does not max colors:",length(args$user_colors)))
	                                                      
    }
     cl <- args$user_colors
} else
{	
  cl=c("black","red","blue","green","orange","magenta","yellow","steelblue","gray","tan") 

	if (max_items > 10)	
		cl=c("black","red","blue","green","orange","magenta","yellow","steelblue","gray","tan",rainbow(max_items-10))
}		

# ----------- end  defining colors ---------------------------------
# ------------------ checking whether show_items is in a proper format

if (("paired" %in% names(args)) & !("color_bits" %in% names(args)))
		{
			stop("Error: paired needs additional color_bits ")
		      
		}
if ("paired" %in% names(args))
  if (!(is.data.frame(args$paired)) & (length(args$paired[1,])!= 2))
		{
			stop("Error: paired must be a data.frame with two rows ")
		      
		}

if (("color_bits" %in% names(args)) & !("action_colors" %in% names(args)))
		{
			stop("Error: color_bits needs additional  action_colors ")
		    
		}

if ("color_bits" %in% names(args))
	if (args$color_bits != "0")
	{ 	
		detect_0 <- detect_bits(args$color_bits,FALSE)
		detect_1 <- detect_bits(args$color_bits,TRUE)
		length_bits <- 0
		if (detect_1[1] != -1) # -1: no bit 1 found
		length_bits <- length(detect_1)
		if (detect_0[1] != -1) # -1: no bit 1 found
		length_bits <- length_bits+length(detect_0)
		if (length_bits != max_items)
			# Nr. of its == 1 + Nr. of show_items = 0
		{
			stop(paste("Error: max count of items:" ,max_items," does not length of color_bits:",length_bits))
		      
		}
      }
# -------- end checking whether show_items is in a proper format
# ------------------ checking whether show_items is in a proper format
	if (show_items != "0")
	{ 	
		detect_0 <- detect_bits(show_items,FALSE)
		detect_1 <- detect_bits(show_items,TRUE)
		length_bits <- 0
		if (detect_1[1] != -1) # -1: no bit 1 found
		length_bits <- length(detect_1)
		if (detect_0[1] != -1) # -1: no bit 1 found
		length_bits <- length_bits+length(detect_0)
		if (length_bits != max_items)
			# Nr. of its == 1 + Nr. of show_items = 0
		{
			stop(paste("Error: ax count of items: ",max_items," does not length of show_items: ",length_bits))
		      
		}
      }
# -------- end checking whether show_items is in a proper format




# -------end checking whether actions is in a proper format
# ende -------------------- detect count of items and last itme -------------------------------
#-------------- define data for special section --------------------------
count_min <- 1

if ("starting_time" %in% names(args))
  { count_min <- 1
  for (counter in (1:count_max))
    {
     if (times(args$starting_time) >= times(data_sheet$Time[counter]))
     count_min <- counter
    }
    if (count_min == 1)  
        print("Warning: starting_time not available in data sheet")
  	min_time <- times(args$starting_time)
  }	
else  	
  {
     	count_min <- 1
    	min_time <- min(times(data_sheet$Time),na.rm=TRUE)
	}

if ("ending_time" %in% names(args))
  {count_max_temp <- count_max	
   for (counter in (1:count_max))
    {
     if (times(args$ending_time) <= times(data_sheet$Time[counter]))
     count_max_temp <- counter
    }
    if (count_max_temp == count_max)  
        print("Warning: ending_time not available in data sheet")
    count_max <- count_max_temp
    max_time <- times(args$ending_time)
   } 
else  	
	max_time <- max(times(data_sheet$Time),na.rm=TRUE)

#-------------- end defining data for special section --------------------------
# ----------- sort: highest point count on top ----------------------
#result$item.number <- data_sheet$dominance.order
    data.dominance.order.save <-data_sheet$dominance.order
    ADI_temp = ADI(data_sheet, bytes=bytes)
    items <- max(data_sheet$item.number,na.rm=TRUE)
    points_items <- data.frame(name=c(1:items ),item.number=c(1:items ),dominance.points=c(1:items),item.order=c(1:items))
    temp<-as.data.frame(ADI_temp$ADI ) #only ADI form Results
########
    points_items$dominance.points<-temp$results.ADI
    points_items$name<-names(temp)[1:items]
    points_items$item.number <- temp$id 
    points_items$item.order  <- temp$rank
############    
    name.of.action  <- c(1:max(data_sheet$action.number,na.rm=TRUE))
    action.number   <- c(1:max(data_sheet$action.number,na.rm=TRUE))
    classification  <- c(1:max(data_sheet$action.number,na.rm=TRUE))
    weighting       <- c(1:max(data_sheet$action.number,na.rm=TRUE))
#    points_sorted<- points_items[order(points_items$dominance.points,decreasing = FALSE),]
    result=points_items

#    for (i in (items:1)) 
     	data_sheet$dominance.order[1:max_items] <-result$item.number
       if (sort_dominance ==FALSE)
     		result <- result[order(result$item.number,decreasing = FALSE),] #sort for display 

#    result$item.number <- result$item.number
    
   

       
    
# ----------- end sort : highest point count on top ----------------------

#------------------ define Range --------------------
	par(cex=1.2,lwd=1) 
	xrange<- x <- c(min_time,max_time)
	yrange<- y <- c(min_items+1,max_items)
	plot(x, y,bty = "]", xaxs = "i", yaxs = "i", type="n",adj=0, asp=0, xlab="", ylab="",font.axis=2,yaxt='n')
#	box_left <- min_time +   times("00:00:01")
#  rect(0,0,	box_left,max_items+0.3 ,col="white",border="white");

# ENDE ------------- end define Range --------------------

	if (show_items != "0")
	  	bits_equal_one <- detect_bits(show_items,TRUE)
	else
	     bits_equal_one <- c(1:50) # set all show_items for lines below
 
	if ("color_bits" %in% names(args))
	  	bits_equal_one_color <- detect_bits(args$color_bits,TRUE)
	else
	     bits_equal_one_color <- c(1:50) # set all show_items for lines below
 

	for (counter in (1:max_items))
	{				# / equal or
     text(min_time,result$item.order[counter],pos=2,cex=1,result$name[counter],xpd=TRUE)
 		lines(c(min_time,max_time),c(result$item.order[counter],result$item.order[counter]),lwd=lwd,col=cl[result$item.number[counter]])
	}
	title(ylab="",xlab="time",cex.lab=1.2,font.lab=2 )
# -- im result$item.number wird festgelegt an welcher Stelle das Pferd in der Rangordnung steht

	for (counter in (count_min:count_max))
	{
	  if  ((data_sheet$action.from[counter] > 0 ) & data_sheet$kind.of.action[counter] >0)
		if (match(result$item.number[data_sheet$action.from[counter]],bits_equal_one,nomatch=0)> 0)
		{ # show only items when bit is set to one - all show_items are set to one above if show_items is not set in function call      
		
       if (match(data_sheet$kind.of.action[counter],bits_action_equal_1 <- detect_1,nomatch=0)> 0)
         {
        if ("color_bits" %in% names(args))  # disply only special horses colored
        { if (match(result$item.number[data_sheet$action.from[counter]],bits_equal_one_color,nomatch=0)> 0)
      	     action_cl <- cl[data_sheet$action.from[counter]]
      	     else
      	     action_cl ="black"
      	}     
      	else 
      	{
          if (change_action_color != FALSE)
            action_cl <- action_color[data_sheet$kind.of.action[counter]]
            else      	       
          	action_cl <- cl[data_sheet$kind.of.action[counter]]
         }
      			x1 <- times(data_sheet$Time[counter])	
      			x2 <- times(data_sheet$Time[counter])	

      	 		y1 <- result$item.number[result$item.number[data_sheet$action.to[counter]]]
      			y2 <- result$item.number[result$item.number[data_sheet$action.from[counter]]]

    		  	arrows(x1,y1,x2,y2,length=length_a,angle=angel_a,
      	   		code=1,col=action_cl,lty = par("lty"),lwd=lwda)
	   	   }  #(match(data_sheet$kind.of.action[counter],bits_action_equal_1 <- detect_1,nomatch=0)> 0)
       
		}
	}#for (counter in (1:count_max))
 return(ADI_temp)#from function rank_order

}
