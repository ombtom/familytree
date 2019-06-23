#St Andrews Academic Family Tree Visualiser
#Written by Tom Groves
#With significant help from jessesadler.com/post/network-analysis-with-r/

#To fix:
#Do something with "weight" column (counts number of identical rows)
#Add "year" (select/hide(?) by) and "count" (line weighting for path finder) columns to edge list
#Add filter by year feature https://datastorm-open.github.io/visNetwork/options.html ("select by a column"... might not work with directed graphs (?))
#Use output from pathfinder to define group of edges with bolder styling
#Add features like shortest_path() (igraph function) and time evolution from https://kateto.net/wp-content/uploads/2016/06/Polnet%202016%20R%20Network%20Visualization%20Workshop.pdf
#Increase edge length to make plot clearer

setwd("/Users/Tom/Desktop/R network")
library(tidyverse)

#Read big list of all nodes and edges
letters <- read_csv("data csvs/isthisfamily.csv")

#Create list of all unique parent nodes
sources <- letters %>%
  distinct(source) %>%
  rename(label = source)

#Create list of all unique child nodes
destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

#Join lists of unique parents/children, to create list of all unique nodes
nodes <- full_join(sources, destinations, by = "label")

#Add column of unique node IDs
nodes <- nodes %>% rowid_to_column("id")

#Add column for weight (duplicate row test)... does this block even need to exist
per_route <- letters %>%
  group_by(source, destination) %>% #... wtf does this do?
  summarise(weight = n()) %>% #Orders data alphabetically by parent, then by child, and removes duplicate edges. Also adds column "weight" which counts any duplicated rows.
  ungroup()

#The first of these functions adds column of source IDs; the second adds column of destination IDs; hence, we get df with edge definitions
edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% #Joins tables, matching per_route.source to nodes.label... i.e. adds column with ID of source on right side of per_route, and calls this new df "edges"
  rename(from = id) #Now our ID column is renamed "from"

edges <- edges %>% #At this point, edges df is per_route with ID ("from") column on right side
  left_join(nodes, by = c("destination" = "label")) %>% #Joins tables, matching edges.destination to nodes.label... basically adds column with ID of destination on right side of edges, and sets edges df to this
  rename(to = id) #Now our ID column is renamed "to"

#Our edges df doesn't need node names (e.g. "Ben") and node IDs (e.g. "492"); hence, remove names
edges <- select(edges, from, to, weight)

#.
#.
#.
#Now to make pretty graphics
#First with visNetwork (uses vis.js), later with networkD3 (uses d3 visualisation library)

library(visNetwork)

#Create "width" column in edges df
#edges <- mutate(edges, width = weight/5 + 1)

#Find ID of hub nodes
hubIDs <- NULL
for (i in 1:nrow(nodes)) {
  if (startsWith(toString(nodes[i,2]),"hub,") == TRUE) {
    hubIDs <- append(hubIDs,i)
  }
}

#Find ID of human nodes
humanIDs <- setdiff(1:nrow(nodes),hubIDs)

#Define types of node, human and hub
#Also define font.size etc. vectors since apparently visNetwork can only do font.size etc. this way... surely not
nodeTypes = NULL
fontSizes = NULL
nodeValues = NULL #size of nodes
for (i in 1:nrow(nodes)) {
  if (i %in% hubIDs) {
    nodeTypes[i] <- "hub"
    fontSizes[i] <- 0
    nodeValues[i] <- 0.1
  } else {
    nodeTypes[i] <- "human"
    fontSizes[i] <- 14
    nodeValues[i] <- 1
  }
}

#Use node types to redefine nodes df, so node types can have different styles in final output
nodes <- data.frame(nodes, group = nodeTypes, font.size = fontSizes, value = nodeValues)

#Now similar (previous two blocks) for edges
#... except much longer because they have more customisations and dependencies. NB edges can't have groups, like nodes can
#... need to be careful with order of if(){} statements, since later statement will override earlier customisations
yearsList <- NULL
edgeTypes <- NULL
edgeColors <- NULL
edgeArrows <- NULL
edgeWidths <- NULL
for (i in 1:nrow(edges)) {
  #grab and convert ID numbers from edge df to names from node df
  fromID <- edges[i,1]
  toID <- edges[i,2]
  fromName <- nodes[as.integer(fromID),2]
  toName <- nodes[as.integer(toID),2]
  
  #deal with partners, distinguishing between married and unmarried
  if (startsWith(toString(fromName),"hub,") == FALSE && startsWith(toString(toName),"hub,") == FALSE) {
    fromRows <- which(letters$source == fromName) #find rows in letters df where fromName is in "from" column
    toRows <- which(letters$destination == toName) #find rows in letters df where toName is in "to" column
    matchList <- match(toRows, fromRows) #find when fromRows and toRows refer to the same row in letters df
    toRowID <- which(!is.na(matchList)) #assumes only one index has same value in toRows and fromRows, i.e. no duplicate rows of a pair in letters df
    lettersRowOfPair <- toRows[toRowID[1]] #index 1 prevents duplicate rows of pair in letters df from breaking everything
    
    if (!is.na(letters[lettersRowOfPair,3])) { #requires keeping third column of letters csv as "married"
      edgeTypes[i] <- "marriedPartners"
      edgeColors[i] <- "#ff0000"
      edgeArrows[i] <- FALSE #no arrows for partner edges
      edgeWidths[i] <- 3 #thicker for married than unmarried
    } else {
      edgeTypes[i] <- "unmarriedPartners"
      edgeColors[i] <- "#ff0000"
      edgeArrows[i] <- FALSE #no arrows for partner edges
      edgeWidths[i] <- 1 #thinner for unmarried than married
    }
  } else {
    edgeTypes[i] <- NA #keeps edgeTypes same length as edges df
    edgeColors[i] <- NA #keeps edgeColors same length as edges df. NB default is #848484
    edgeArrows[i] <- "to" #keeps edgeArrows same length as edges df
  }
  
  #now set edge widths, depending on whether parent-to-hub or hub-to-child
  if (startsWith(toName,"hub,") == TRUE) {
    edgeColors[i] <- "#0000e6" #colour for parent-to-hub edges
    edgeWidths[i] <- 3
  }
}

#Use edge types to redefine edges df, so edge types can have different styles in final output
#edges <- data.frame(edges, group = edgeTypes) #... turns out edges can't have groups, rip sort-by-year
edges <- data.frame(edges, width = edgeWidths, color = edgeColors, arrows = edgeArrows)

visNetwork(nodes, edges) %>% #Creates the visualisation
  visGroups(groupname = "hub", color = "black", shape = "dot") %>%
  visGroups(groupname = "human", shape = "box", color = list(
    background = "white",
    border = "black",
    highlight = "lightblue"
    )
  ) %>%
  visEdges(hoverWidth = 3, selectionWidth = 6, physics = FALSE, smooth = FALSE) %>% #changes size of nearby edges when edge or node is hovered, clicked... check how to enable hover in browser. Turn physics and smooth arrows off for faster rendering.
  visOptions(nodesIdSelection = list(values=humanIDs), selectedBy = "group")#%>% #adds drop-down ID selecter... need to alphabetise this
  #visHierarchicalLayout() #Layout algorithm
  #visIgraphLayout() #Computing coordinates with igraph decreases plotting time, but will disable physics and smooth arrows regardless of above
  #add ", selectedBy = "year" once year column in node df


