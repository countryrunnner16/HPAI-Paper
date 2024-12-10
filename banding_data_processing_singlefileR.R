library(data.table)
library(sf)

#### Read in dataset
#Reading in the banding data
banding_data = fread()

#Read in the spatial layer (county vs state etc)

### Replace this with the county-level dataset (gadm36_2)
counties = read_sf(dsn = , layer= "gadm36_2")
#subset the countries where canadian geese appear
counties = subset(counties,is.element(NAME_0,c("Canada","United States","Mexico")))


#####Subsetting the banding data
#Determine the band IDs that appear more than once
bands_to_keep = names(which(table(banding_data$BAND)>1))
#Subset banding data to only include bands that occur more than once
banding_data_sub = subset(banding_data,is.element(BAND, bands_to_keep))
#Get rid of banding data without valid coordinates
banding_data_sub = subset(banding_data_sub, !is.na(banding_data_sub$LON_DD) & !is.na(banding_data_sub$LAT_DD))

#####Associate banding data with locations
#turn banding data into spatial data
banding_data_sf <- st_as_sf(banding_data_sub, coords = c("LON_DD","LAT_DD"))
st_crs(banding_data_sf) = st_crs(counties)

#Pair banding data points with spatial units
require(data.table)
banding_data_sf$ID = 1:nrow(banding_data_sf)
counties_visited =  st_join(banding_data_sf,counties,join = st_intersects)
#Isolate banding data ID and county ID 
#make sure to change the variable "GID_1" to the appropriate ID (for counties, it will be "GID_2")
counties_visited_sub = st_drop_geometry(counties_visited[,c("ID","GID_2")])

setDT(counties_visited_sub); setDT(banding_data_sf) 

#merge county IDs with original dataset
banding_data_with_counties= counties_visited_sub[banding_data_sf, mult = "first", on = "ID", nomatch=0L]


#####Create adjacency matrix
#defined as the number in [i,j] representing the number of birds that traveled from county i to county j

#generate empty adjacency matrix
county_adjacency_matrix = matrix(0, nrow(counties),nrow(counties))

bands_to_iterate = unique(banding_data_with_counties$BAND)
banding_data_with_county_df = st_drop_geometry(banding_data_with_counties) 
banding_data_with_county_df = banding_data_with_county_df[,-c("geometry")]


#For loop that iterates over all bands, and then fills in the adjacency matrix for each band ID
#create unique, numeric ID for counties
counties$newID = 1:nrow(counties)
counties_df = st_drop_geometry(counties[,c("newID","GID_2")])

banding_data_with_county_df = merge(banding_data_with_county_df,counties_df, by.x = "GID_2",by.y="GID_2")

#fill in the adjancy matrix
for (i in 1:length(bands_to_iterate)){
  #Subset rows that have data for the band we're looking at
  banding_data_sf_sub = subset(banding_data_with_county_df, BAND == bands_to_iterate[i])
  
  #Get all combinations of the places visited
  combinations = expand.grid(fr = banding_data_sf_sub$newID, to = banding_data_sf_sub$newID)  
  for (j in 1:nrow(combinations)){
    county_adjacency_matrix[combinations$fr[j], combinations$to[j]] = county_adjacency_matrix[combinations$fr[j], combinations$to[j]] +1
  
  }
  if (i %% 1000 == 0){
    print(i)}
}

write.csv(county_adjacency_matrix,file = ())
county_adjacency_matrix = read.csv()[,-1]
#remove the self connections
county_adjacency_matrix_no_diag = county_adjacency_matrix
diag(county_adjacency_matrix_no_diag) = 0


#Making the data a long dataset for plotting
centroid_df = as.data.frame(st_coordinates(st_centroid(counties)))
centroid_df$ID = 1:nrow(centroid_df)

od_long = expand.grid(orig = counties$newID,dest=counties$newID)
val_test = apply(od_long,MARGIN = 1 , FUN = function(x){
  county_adjacency_matrix[x[1],x[2]]
})

od_long$flow = unlist(val_test)
#Add coordinates to the long dataset for the origin and destination
plotting_data = merge(od_long,centroid_df,by.x="orig",by.y="ID")
names(plotting_data)[which(names(plotting_data)=="X")] = "x_fr"
names(plotting_data)[which(names(plotting_data)=="Y")] = "y_fr"

plotting_data = merge(plotting_data,centroid_df,by.x="dest",by.y="ID")
names(plotting_data)[which(names(plotting_data)=="X")] = "x_to"
names(plotting_data)[which(names(plotting_data)=="Y")] = "y_to"

plotting_data_sub = subset(plotting_data,flow > 10 & flow < 5000)
plotting_data_sub = plotting_data_sub[order(plotting_data_sub$flow,decreasing=F),]
library(ggplot2)
line_plot = ggplot()   + 
  geom_sf(fill="light grey",colour="NA",data=counties,size=.5) +
  geom_segment(aes(x=x_fr, y=y_fr, xend = x_to, yend = y_to,colour=flow), 
               data=plotting_data_sub, linewidth =.5,alpha=1) +
  scale_colour_distiller(palette="Spectral",trans="log10")+
  ggtitle(paste0("Line Plot; total connections: ", sum(county_adjacency_matrix), " Species: ", "Name"))+
  xlim(-170,-63) + ylim(22,72)
ggsave(line_plot, filename=paste0("lineplot","Name",".png"))

library(igraph)
#https://eehh-stanford.github.io/SNA-workshop/intro-igraph.html

county_adjacency_matrix = as.matrix(read.csv()[,-1])

#Create an igraph object
graph <- graph_from_adjacency_matrix(county_adjacency_matrix, mode="directed", weighted=TRUE)
#initial plotting
#lay <- layout_with_fr(graph)
#plot(graph,edge.width=log2(E(graph)$weight)+1, layout=lay, vertex.color="lightblue")



#detect communities
community_graph = walktrap.community(graph)
#add community membership to shapefile
members = community_graph$membership
members[is.element(members,which(table(community_graph$membership)<10))] = NA
counties$membership = members
#plot shapefile
commstructure = ggplot()   + 
  geom_sf(colour="NA",data=counties,size=.5,fill="light grey")+
  geom_sf(colour="NA",data=counties_communities_only,size=.5, mapping = aes(fill = as.factor(membership))) +
  ggtitle(paste0("Community Structure; Total connections: ", sum(county_adjacency_matrix), " Species: ","Name"))+
  xlim(-170,-63) + ylim(22,72)

ggsave(commstructure, filename=paste0("community structure","Name",".png"))

#eigenvector centrality 
counties$evec = eigen_centrality(graph)$vector

evec_plot = ggplot()   +
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = evec)) +
  scale_fill_distiller(palette="YlOrRd",trans="log10")+
  ggtitle(paste0("Eigenvector centrality; total connections: ", sum(county_adjacency_matrix), " Species: ","Name"))+
  xlim(-170,-63) + ylim(22,72)
ggsave(evec_plot, filename=paste0("evecplot",'Name',".png"))

#betweenness centrality 
counties$betweenness = betweenness(graph)

betweenness_plot = ggplot()   +
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = betweenness)) +
  scale_fill_distiller(palette="Spectral",trans="log2")+
  ggtitle(paste0("Betweenness centrality; Total connections: ", sum(county_adjacency_matrix), " Species: ","Name"))+
  xlim(-170,-63) + ylim(22,72)

ggsave(betweenness_plot, filename=paste0("betweennessplot","Name",".png"))


 