#' Social Network Graphs
#' 
#' computes social network graphs with igraph
#' 
#' 
#' @param data_sheet either a data.frame f.e imported from a data sheet
#' containing
#' 
#' "Name","item.number"
#' 
#' "action.from.","action.to","kind.of.action"
#' 
#' "name.of.action","action.number","classification","weighting"
#' 
#' or only "action.from.","action.to","kind.of.action"if exists actions and
#' items
#' 
#' data_sheet: with "action.from.","action.to","kind.of.action"
#' 
#' items with "Name","item.number"
#' @param bits as sting where each enabled action is set to 1 and each
#' disabled action is set to 0
#' @param ...  Postscript: FALSE (default) or path to PS output file
#' 
#' soziogram_layout:
#' layout.auto,layout.random,layout.circle,layout.sphere,layout.fruchterman.reingold,layout.kamada.kawai,layout.spring,layout.reingold.tilford,layout.fruchterman.reingold.grid,layout.lgl,layout.graphopt,layout.svd,layout.norm
#' 
#' curved: how much the lines between the nodes ar curved 0 ist stright,
#' default is 0.2
#' 
#' scal_value: the multiplicator of the nodes, default: 1/3
#' 
#' linesize_add: value to add to the linesize (helpful for a graph with thin
#' lines. default 0
#' 
#' log: log2 size of linewitdh, default= false
#' 
#' canvas.width: default 1000
#' 
#' canvas.height: default 800
#' 
#' tkplot: interactiv tkplot, default = true
#' 
#' @return
#' 
#' \item{sociogram}{ the igraph object} 
#' \item{counts_circles }{the count of cirles} 
#' \item{count_interactions }{the count of interactions}
#' \item{line_size}{the used linesize} 
#' \item{counts_circles }{vector of min #' and max line size} the last for are helpful to change circle size and #' linewidth

#' @author Knut Krueger, \email{Knut.Krueger@equine-science.de}
#' @keywords ~kwd1 ~kwd2
#' @section Hints:{
#' \bold{to create a excel sheet for Sociogram data:}\cr
#' library(XLConnect)\cr 
#' data(data_Network_1)\cr
#' setwd('/your data directory/')\cr 
#' wb <- loadWorkbook("sheet_for_using_with_Sociogram.xlsx",create=TRUE)\cr
#' createSheet(wb, name = 'Sociogram_DATA')\cr
#' writeWorksheet(wb,data_Network_1,sheet='Sociogram_DATA')\cr
#' saveWorkbook(wb)\cr
#' \cr
#' \bold{to load the excel sheet:}\cr
#' library(XLConnect)\cr
#' setwd('/your data directory/')\cr
#' wb <- loadWorkbook("sheet_for_using_with_Sociogram.xlsx")\cr
#' data_sheet <- readWorksheet(wb1, sheet = "Sociogram_DATA")\cr  
#' }
#' @examples {
#' ## you can eihter use:
#' data_sheet=data.frame   ("action.from"=c(1,4,2,3,4,3,4,3,4,3,4,3,4,3,4),
#'                    "action.to"=c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),
#'                    "kind.of.action"= c(4,1,1,4,3,4,3,4,3,4,3,4,3,4,3),stringsAsFactors=FALSE)
#' items= data.frame ("Name"=c("item1","item2","item3","item4","item5","item6") ,
#'                    "item.number"=c(1:6),stringsAsFactors=FALSE)
#' actions=data.frame("name.of.action"= c("leading","following","approach","bite","threat to bite",
#'                    "kick","threat to kick", "chase","retreat"),
#'                    "action.number"=c(1:9),
#'                    "classification"=c(1,2,1,1,1,1,1,1,2) ,
#'                    "weighting"=c(1,-1,1,1,1,1,1,1,-1),stringsAsFactors=FALSE)
#' ## all  encounters  without leading and following
#' bytes= "001111111"  
#' Sociogram(data_sheet,items=items,actions=actions,bytes)
#' ## mor you can use a complete f.e Excel sheet
#' ## you can save this data as basic excel sheet to work with
#' data(data_Network_1)
#' ## set 1 for action you want to show
#' bytes= "00111111111000000000"    
#' Sociogram(data_Network_1,bytes)
#' }
#' @export Sociogram
#' 
#' @importFrom igraph tkplot layout.kamada.kawai delete.vertices degree plot.igraph count.multiple E E<- %--% V V<- vcount graph graph.empty
#' @importFrom  stats na.omit 
#' @importFrom grDevices rainbow dev.off heat.colors postscript

Sociogram <-
function(data_sheet,bits,...)
{
#library(igraph)
#--------------------- uebergabe parameter ----------
args = list(...)


with_groups=FALSE
if (("actions" %in% names(args)) &  ("items" %in% names(args))) 
{ 
  actions <- args$actions
  items <- args$items   

  data_length = length(data_sheet$action.from)
  temp_NA= c(1:data_length)
  temp_NA[1:data_length] =NA
  tempString_NA= c(1:data_length)
  tempString_NA[1:data_length] = "<NA>"

  data_temp=data.frame("action.from"=data_sheet$action.from,"action.to"=data_sheet$action.to,"kind.of.action"=data_sheet$kind.of.action,
                      "Name"=tempString_NA,"item.number"=temp_NA,
                      "name.of.action"=tempString_NA,
                      "action.number"=temp_NA,
                     "classification"=temp_NA,
                      "weighting"=temp_NA,stringsAsFactors=FALSE)

  data_temp$Name[1:length(items$Name)] = items$Name [1:length(items$Name)]
  data_temp$item.number[1:length(items$item.number)] =items$item.number
  data_temp$name.of.action[1:length(actions$name.of.action)] =actions$name.of.action
  data_temp$action.number[1:length(actions$action.number)] = actions$action.number
  data_temp$classification[1:length(actions$classification)] = actions$classification
  data_temp$weighting[1:length(actions$weighting)] = actions$weighting

  data_sheet = data_temp  # compute with the complete frame

}

data_Items_g2=data_sheet
Group= with_groups
bytes= bits  

  
if ("Postscript" %in% names(args))
{ Postscript = args$Postscript 
}else
{  Postscript = FALSE }
  
if ("soziogram_layout" %in% names(args))
{ soziogram_layout = args$soziogram_layout 
}else
{  soziogram_layout = layout.kamada.kawai }

if ("curved" %in% names(args))
{ curved = args$curved 
}else
{  curved = 0.2 }

if ("scal_value" %in% names(args))
{ scal_value = args$scal_value 
}else
{  scal_value = 1/3 }
  scal_value = 1/scal_value
if ("line_value" %in% names(args))
{ line_value = args$line_value 
}else
{  line_value = 1/3 }

if ("linesize_add" %in% names(args))
{ linesize_add = args$linesize_add 
}else
{  linesize_add = 0 }

if ("log" %in% names(args))
{ log = args$log 
}else
{  log = FALSE }

if (Group == TRUE)
{
  max_items <- max(na.omit(data_Items_g2$item.number))-1
} else  
{
  max_items <-  max(na.omit(data_Items_g2$item.number))
}  

 if ("canvas.width" %in% names(args))
{ canvas.width = args$canvas.width
}else
{  canvas.width=1000 }

if ("canvas.height" %in% names(args))
{ canvas.height = args$canvas.height
}else
{  canvas.height=800 }


if ("tkplot" %in% names(args))
{ tkplot  = args$tkplot
}else
{  tkplot=TRUE }


#max_items = 23  #no group       eliminate group when group was recorded
data_Items_g1<- subset(data_Items_g2,data_Items_g2$action.to <= max_items & data_Items_g2$action.from <= max_items)       


data_Items_temp <- subset(data_Items_g2,data_Items_g2$item.number <= max_items)       
temp_data <- na.omit(data.frame("Name"=data_Items_temp$Name,"item.number"=data_Items_temp$item.number))       #item number not uses chaged to Item order 

label_names <- c(1:max_items)
  
max_items<- length(na.omit(temp_data$Name))  



#---------------excamples ----------------------------
#scal_value = 3 #size of circles
#line_value = 3 #size of  lines
Data_all <- data_Items_g1
names(Data_all)
#Data_all$kind.of.action <- Data_all$test.2.kind.of.action
#igraph_data<-c()
#igraph_size<-c()

#calculate size of nodes and size of  edges
results1 <- search.win.lose(Data_all,bits=bytes,old_style=FALSE)
win_lose_results1 <- results1$data.win.lose

l_c = 0 #c(1:max_items)
for (x in 1:max_items )
{  
   l_c[x] = sum(win_lose_results1$wins==x) + sum(win_lose_results1$loses==x )
}   



scale <- function(v, a, b) {
  v <- v-min(v) ; v <- v/max(v) ; v <- v * (b-a) ; v+a
}



color <- heat.colors(max_items)

#l_c <- c(l1,l2,l3,l4,l5,l6,l9,l10,l11,l12,l14,l15,l16,l17,l18,l19,l20,l21,l22) #size of nodes = sum leadings missing 7,8 13

# build Dataframe to sort edges by width


# TODO  write function to eleminate bits == 0

igraph_data1 = NA
igraph_data2 = NA
detected <- detect_bits (bytes,set=FALSE)
number <- length(detected)
detected <- detect_bits (bytes,set=TRUE)  # we need true for the next loop
number <- number + length(detected)
temp2 =c()
temp1 =c()
for (x in 1:number )
{  
   if (any(detected==x))
   {
     temp <- subset( Data_all$action.to ,Data_all$kind.of.action == x)
     temp2 <- c(temp2,temp)

     temp <- subset( Data_all$action.from ,Data_all$kind.of.action == x)
     temp1 <- c(temp1,temp)

   }  
}
igraph_data2 <- temp2
igraph_data1 <- temp1
#igraph_data2 <-(subset( Data_all$action.to ,Data_all$kind.of.action > 2) ) #count of all witout leadings and following
#igraph_data1 <-(subset( Data_all$action.from ,Data_all$kind.of.action > 2) ) #count of all witout leadings and following



#--------------------------- matrix mit anzahl actions ohne leading and following   zu erstellen ---------------------------------
#
#
#
#
#----------------------------------------------------------------------------------------------------------

graph_data <- data.frame("from"=igraph_data1,"to"=igraph_data2)

z <- c()
for (i in (1:length(graph_data$from)))
{
z <- rbind(z,graph_data$from[i])
z <- rbind(z,graph_data$to[i])
}

 igraph_data <- z

# warum?
#max_items <- max(c(igraph_data1,igraph_data2,na.rm=TRUE))+1

g <- graph.empty(n=max_items, directed=TRUE)
min(igraph_data)
igraph_data = igraph_data
g <- graph( igraph_data,n=max_items, directed=TRUE )
vcount(g)

V(g)$color <-  heat.colors(max_items)

#circles<- scale(l_c, 10 ,30)
circles<-l_c/scal_value

V(g)$size <- circles


V(g)$label <- label_names


#E(g)$color <-   c("red","green","blue")
E(g)$color <-   "black"

#,"black","thistle","orange","yellow","magenta","turquoise3")

E(g)[ V(g)[ color=="lightblue" ] %--% V(g)[ color=="green" ] ]$color <- "red"

 count= count.multiple(g)
  E(g)$width = 1
 E(g)$width = scale(count,1,5)
 if (log==TRUE)
 {
   E(g)$width = count/log2(line_value)  + linesize_add
 } else
 {
    line_value = 1/line_value 
 E(g)$width =count/line_value  + linesize_add
 }
E(g)$curved <- curved
g <- delete.vertices(g, which(degree(g) < 1))  #delete all without connections
#plot.igraph(g, layout=layout.kamada.kawai, vertex.label.font=2)
if (Postscript != FALSE)
{
  ps_file <- args$Postscript
postscript(file= ps_file,
                       onefile = TRUE,pointsize=15, fonts=c("serif", "Helvetica"),
                   horizontal=FALSE,family = "Helvetica",  paper = "special",height=10,width=10)
}             
else      
{
Postscript = FALSE
}


plot.igraph(g,layout=soziogram_layout, vertex.label.font=4, margin =0)

if (Postscript == TRUE) {
  dev.off()
}

if (tkplot == TRUE) {  
    tkp.id <-  tkplot(g, canvas.width= canvas.width, canvas.height= canvas.height, margin =0) 
}    
#  tkplot.fit.to.screen(tkp.id, width = NULL, height = NULL)
 
return (list(sociogram=g,counts_circles=l_c,count_interactions=count.multiple(g),line_size=c(min(E(g)$width),max( E(g)$width))))
#TODO graph Ausgabe fuer tkplot

}
