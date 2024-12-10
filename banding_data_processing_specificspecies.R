library(data.table)
library(sf)

#### Read in data
#Read in the spatial layer (county vs state etc)

### Replace this with the county-level dataset (gadm36_2)
counties = read_sf(dsn = "gadm36", layer= "gadm36_2")
#subset the countries
counties = subset(counties,is.element(NAME_0,c("Canada","United States","Mexico")))


#Reading in the banding data

all_species = data.frame(ID = c(1400, 1370, 1350, 1420, 1500, 1690, 3520, 3330, 3560, 3370, 3760), 
           file = c("04","06","06","06","10","01","21","21","23","21","22"), 
           species = c("greenwinged_teal","wigeon","gadwall","shoveler","ringnecked_duck","lesser_snow_goose","bald_eagle","coopers_hawk","peregrine_falcon","redtailed_hawk","snowy_owl"),
           bands = 0)

for (i in 1:nrow(all_species)){
  filename = paste0("NABBP_2022_grp_",all_species$file[i],".csv")
  ID_sub = all_species$ID[i]
  species_name = all_species$species[i]
  
  
  
  banding_data = fread(filename)
  banding_data = subset(banding_data, SPECIES_ID == ID_sub)
  
  #####Subsetting the banding data
  #Determine the band IDs that appear more than once
  bands_to_keep = names(which(table(banding_data$BAND)>1))
  all_species$bands[i]= length(bands_to_keep)
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
  for (j in 1:length(bands_to_iterate)){
    #Subset rows that have data for the band we're looking at
    banding_data_sf_sub = subset(banding_data_with_county_df, BAND == bands_to_iterate[j])
    
    #Get all combinations of the places visited
    combinations = expand.grid(fr = banding_data_sf_sub$newID, to = banding_data_sf_sub$newID)  
    for (k in 1:nrow(combinations)){
      county_adjacency_matrix[combinations$fr[k], combinations$to[k]] = county_adjacency_matrix[combinations$fr[k], combinations$to[k]] +1
      
    }
    if (j %% 1000 == 0){
      print((paste(i,j)))}
  }
  
  write.csv(county_adjacency_matrix,filename = paste0("county_adjacency_matrix_",species_name,"_county.csv"))
  county_adjacency_matrix = matrix(0, nrow(counties),nrow(counties))
  
  }

counties$newID = 1:nrow(counties)
filenames = list.files("data",full.names=T)
filenames_name = list.files("data",full.names=F)
filenames =  filenames[grepl('.*\\.csv', filenames)]
filenames_name =  filenames_name[grepl('.*\\.csv', filenames_name)]
for (i in 1:length(filenames)){
county_adjacency_matrix = as.matrix(read.csv(filenames[i])[,-1])
#remove the self connections
diag(county_adjacency_matrix) = 0
subname = substr(filenames_name[i],25,30)


#####Making the data a long dataset for plotting
centroid_df = as.data.frame(st_coordinates(st_centroid(counties)))
centroid_df$ID = 1:nrow(centroid_df)

od_long = expand.grid(orig = counties$newID,dest=counties$newID)
od_long$flow = 0
for (i in 1:nrow(od_long)){
  od_long$flow[i] = county_adjacency_matrix[od_long$orig[i],od_long$dest[i]]
}

#Add coordinates to the long dataset for the origin and destination
plotting_data = merge(od_long,test_centroid,by.x="orig",by.y="ID")
names(plotting_data)[which(names(plotting_data)=="X")] = "x_fr"
names(plotting_data)[which(names(plotting_data)=="Y")] = "y_fr"

plotting_data = merge(plotting_data,test_centroid,by.x="dest",by.y="ID")
names(plotting_data)[which(names(plotting_data)=="X")] = "x_to"
names(plotting_data)[which(names(plotting_data)=="Y")] = "y_to"

plotting_data = subset(plotting_data,flow > 0)
plotting_data = plotting_data[order(plotting_data$flow,decreasing=F),]
library(ggplot2)
line_plot = ggplot()+
  geom_sf(fill="light grey",colour="NA",data=counties,size=.5) +
  geom_segment(aes(x=x_fr, y=y_fr, xend = x_to, yend = y_to,colour=flow), 
               data=plotting_data, linewidth =.5,alpha=1) +
  scale_colour_distiller(palette="Spectral",trans="log10")+
  xlim(-170,-63) + ylim(22,72)


ggsave(line_plot, filename="")
library(igraph)
#https://eehh-stanford.github.io/SNA-workshop/intro-igraph.html

#Create an igraph object
graph <- graph_from_adjacency_matrix(county_adjacency_matrix, mode="directed", weighted=TRUE)


#detect communities
community_graph = walktrap.community(graph)
#add community membership to shapefile
members = community_graph$membership
members[is.element(members,which(table(community_graph$membership)<10))] = NA
counties$membership = members
#plot shapefile
comm_plot = ggplot()   + 
  geom_sf(colour="NA",data=counties,size=.5, mapping = aes(fill = as.factor(membership))) + 
  xlim(-170,-63) + ylim(22,72)
ggsave(community_graph, filename="")
}