library(lazyeval)
library(dplyr)
library(igraph)

#Import as data frame the RDS file with packages information. It can be obtained from CRAN,
download.file("http://cran.r-project.org/web/packages/packages.rds", "packages.rds")
con <- gzfile("packages.rds")
rds <- readRDS(con)
close(con)
data <- as.data.frame(rds, stringsAsFactors = FALSE)

# Clean column names
data <- data[,!duplicated(names(data))] #Eliminate duplicated names column
names(data) <- gsub(" ","_", names(data))
names(data) <- gsub("/","_", names(data))
names(data) <- gsub("@","_", names(data))

#And now we convert it to a tbl_df class to use it with dplyr
data <- tbl_df(data)


# Filter all packages authored by Hadley Wickham, and select a subset of variables
hadley <- data %>%
  filter(grepl("Hadley Wickham|Hadley\nWickham", Author)) %>%
  select(Package, Author, Depends, Imports, Suggests, LinkingTo, Enhances)

#Vector of packages
packages <- unique(hadley$Package)
length(packages)

#Create a function to extract names of related packages in the form package1, package2
relations <- function(var){
  temp <- strsplit(var, ",") #Split string of dependences
  package2 <- unlist(temp) #
  #Eliminate some characters...
  package2 <- gsub(" ","", package2) 
  package2 <- gsub("\\(.*\\)","",package2)
  package2 <- gsub("\n","",package2)
  package1 <- rep(hadley$Package,unlist(lapply(temp,length))) #Obtain the corresponding id
  df <- data.frame(package1,package2, stringsAsFactors = FALSE)
  #We want only related packages created by H.W.
  df <- df %>%
    filter(package2%in%packages,
           package2!=package1
    )
  return(df)
}

#Apply the function to each variable and collapse the resulting list to a single data frame
hadley2 <- lapply(hadley, relations)
hadley2 <- do.call("rbind", hadley2)

#Eliminate possible duplicates
edges <- tbl_df(distinct(hadley2))  
edges


#We create the igraph object
g <- graph.data.frame(edges, vertices= packages,  directed = F) # We create the igraph object based on the "edges" data frame

# Edges Properties

E(g)$arrow.width <- 0 # I don't want end of arrows to be displayed but that can change in the future
E(g)$curved <- 0.2 #Make edges curved
E(g)$color  <- "#BFBFBF"
E(g)$width  <- 10

# Vertex Properties
V(g)$label.family <- "sans" #Label font family
V(g)$label.cex <- 3 # Label font size proportional to 12
V(g)$label.color <- "#333333" # Label font color
V(g)$label.font <- 2 #1 plain, 2 bold, 3 italic, 4 bold and italic
V(g)$size <- degree(g, mode = "in", loops = F) #Size proportional to degree
cl <- optimal.community(g) #Find communities in the network

#Color of vertices based on communities
V(g)$color <- unlist(c("#E2D200", "#BFBFBF", "#46ACC8", "#E58601", rep("#BFBFBF",6))[cl$membership])
V(g)$frame.color <- unlist(c("#E2D200", "#BFBFBF", "#46ACC8", "#E58601", rep("#BFBFBF",6))[cl$membership])
set.seed(123)
layout <- layout.kamada.kawai(g)

png(filename="images/hadleyverse.png", width=1.8*1920, height=1.8*1080) #call the png writer
plot(g, margin=-0.1, layout=layout)
dev.off()
