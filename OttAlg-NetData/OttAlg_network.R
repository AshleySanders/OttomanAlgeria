# install necessary libraries
install.packages("statnet")
install.packages("igraph")
install.packages("intergraph")


# activate igraph library for initial build
library(igraph)

# Build initial network.
ott_alg_edge <- read.csv(file="/Users/asg/networks/data/OttAlg-Edges.csv", sep=',',header=TRUE,colClasses="character") # import edgelist
ott_Alg_node <- read.csv(file = "/Users/asg/networks/data/OttAlg-Nodes.csv", sep=",", header = TRUE, colClasses = "character") # import nodes list

ott_alg_edge <- data.frame(ott_alg_edge) # isolated nodes are listed as loops in edgelist so they are included in the network object.
ott_alg_node <- data.frame(ott_Alg_node)

ott_alg_net <- graph_from_data_frame(ott_alg_edge, directed = F, vertices = ott_alg_node) # build initial network as igraph object from edges and nodes data frames

detach(package:igraph)
library(statnet)
library(intergraph)

# Convert from igraph to network object in statnet.
class(ott_alg_net)
net <- asNetwork(ott_alg_net)
class(net) # double check the conversion. The class should now be "network."

# Convert the edges to undirected so we can examine the existence of relationships rather than their directionality.
symm <- symmetrize(net, rule = "weak") 
oa_net <- network(symm, matrix.type = "adjacency", directed = FALSE, loops = TRUE)
network.vertex.names(oa_net) <- ott_alg_node$Label 

# Run a summary without the adjacency matrix to get an overview of the network

summary(oa_net, print.adj = FALSE) # The most frequently identified relationship types are sons, followed by wives, and then daughters. 

# Create an initial visualization. 
op <- par(mar = rep(0,4))
gplot(oa_net, gmode="graph", vertex.col = "lightblue", vertex.cex=1.1, edge.col = "lightgrey")
par(op)

# Add vertex attributes for gender, ethnicity, tribal affiliation, reference type, as well as node degree.

set.vertex.attribute(oa_net, "gender", OttAlg.node$Gender)
set.vertex.attribute(oa_net, "ethnicity", OttAlg.node$Ethnicity)
set.vertex.attribute(oa_net, "tribe", OttAlg.node$Tribal.Affiliation)
set.vertex.attribute(oa_net, "reference.type", OttAlg.node$Named)
oa_net %v% "alldeg" <- degree(oa_net)
list.vertex.attributes(oa_net)

# Set edge attribute
set.edge.attribute(oa_net, "relation", ott_alg_edge$Label)
list.edge.attributes(oa_net)

# Calculate degree and betweenness centrality.

# Betweeness centrality is based on the distances between vertices. It is (roughly) the number of geodesic paths that pass through any given node. Vertices with a high betweenness score will often act as bridging nodes between one or more communities.

oa_df <- data.frame(cbind(degree(oa_net, gmode = "graph"), betweenness(oa_net, gmode="graph"), ))
row.names(oa_df) <- oa_net %v% "vertex.names"
colnames(oa_df)[1] <- "degree"
colnames(oa_df)[2] <- "btw"
oa_df.sort <- oa_df[order(-oa_df$degree),] # sort data frame by node degree (aka degree centrality)

detach(package:statnet)
library(igraph)

# Calculate harmonic closeness centrality and add it to the data frame of network stats.

hcc <- harmonic_centrality(oa_i_net, vids=V(oa_i_net))

oa_df <- oa_df %>% 
  mutate(hcc = hcc)

View(oa_df)

#----------------------------------------------------------------------

# Plot social network and histogram of degree distribution

plot(oa_i_net, layout=layout_with_fr(oa_i_net))

hist(degree(oa_i_net)) #create a histogram of the degree distribution
# We can see that nearly all nodes have a degree less than 10 with one node above 20. In fact, most nodes have a degree of 2 or less. We can double check this by looking at the node degree mean.

mean(degree(oa_i_net)) # The outlier with a degree of 22 is pulling our mean up, so let's check the median. 
median(degree(oa_i_net)) # Node degree median = 2. This gives us a better sense of the center of the degree data.

sd(oa_df$degree) # Standard deviation of nodal degrees as indication of variability in the graph. With a median of 2, mean of 2.18, and a standard deviation of 2.18, it appears that most nodes have degrees between 0 and 4, meaning we have identified between 0 and 4 relations for most people included in the network. Those with a degree of 0 are isolates, meaning we have not found evidence of their relationships. 

library(modeest)

mfv(oa_df$degree) # identify the mode (=2)

boxplot(oa_df$degree) # Graph a simple boxplot and histogram to check this intuition.

hist(oa_df$degree)

# Eigenvector Centrality
# The Eigenvector corresponding to the largest Eigenvalue of the adjacency matrix gives a high score to vertices that either have a lot of connections, or are connected to someone with a lot of connections.

ec <- eigen_centrality(oa_i_net)
oa_df <- cbind(oa_df, ec)
View(oa_df)

library(dplyr)
oa_df <- select(oa_df, degree, btw, oa_transitivity, vector, hcc)
colnames(oa_df) <- c("degree", "btw", "transit", "ec", "hcc")

# Those with high eigenvector centrality scores seem to be those in the neighborhoods of nodes with highest degrees, as well as the high degree nodes themselves. These are the people with some of the greatest influence, and therefore worth examining further. 

# Who are they? What are their characteristics (ethnicity, gender, tribal affiliation)? What are the differences between those with high and low eigenvector centrality scores? What does this tell us about the socio-political community based on our current mapping of it? 

# Add betweenness and eigenvector centralities as vertex attributes
oa_net %v% "btw" <- oa_df$btw
oa_net %v% "ec" <- oa_df$ec

# Examining patterns among those with high and low eigenvector centrality scores as a way to test patterns among people who were members of powerful governors' inner circles. Were they different from the general (yet still elite) population of this social-political network?

oa_net_df <- as.data.frame(oa_net, unit="vertices")
oa_net_ec <- filter(oa_net_df, ec > 0.1)
ec_eth_gender <- table(oa_net_ec$ethnicity, oa_net_ec$gender)
fisher.test(ec_eth_gender, simulate.p.value = TRUE)
oa_net_low_ec <- filter(oa_net_df, ec < 0.1)
low_ec_eth_gender <- table(oa_net_low_ec$ethnicity, oa_net_low_ec$gender)
low_ec_eth_gender
fisher.test(low_ec_eth_gender, simulate.p.value = TRUE)
oa_net_eth_gender <- table(oa_net_df$ethnicity, oa_net_df$gender)
oa_net_eth_gender
filter(oa_net_ec, ethnicity=="Ottoman-European")

# I manually calculated observed proportions, expected values and expected proportions, as well as chi-square statistics by comparing observed proportions among those with high eigenvector centrality scores with expected proportions for the total population and using the total population proportion as the expected values. In both cases, the chi-square statistics were significant, meaning that those who rank highly by eigenvector centrality scores are significantly different in ethnicity/gender pairs than the rest of the population.

# I could see which characteristics contributed most to the chi-square statistic: Algerian men and Ottoman-Europeans of both genders. In the first instance, far fewer Algerian men appear among those who rank highly in EC scores, and far more Ottoman-Europeans appear than expected. The latter are all sons and daughters of Salah Bey. Who was his European wife and is this data accurate or incorrectly coded?

# About the same proportion of Algerian women as expected appeared in the ranking, however. 

#-------------------------------------------------------------------------------
# Clustering
#-------------------------------------------------------------------------------

# The relative frequency of closed triangles is often known as clustering, or the transitivity of a network.

transitivity(oa_i_net, type = "global") # 3*(# of triangles)/(# of connected triples)

# We have a relatively high global transitivity, which means if one node is connected to two other nodes, there is a 22% chance that the other two nodes are also connected. Since this is a social network, it is not surprising that the network has a fairly high transitivity score, especially given the way the network data was constructed (including nodes based on inference, which completed many family triads that began as father-son dyads).

# We can also examine local clusters around particular nodes

# The local clustering coefficient of a vertex (node) in a graph quantifies how close its neighbors are to being a clique (complete graph). Duncan J. Watts and Steven Strogatz introduced the measure in 1998 to determine whether a graph is a small-world network (https://en.wikipedia.org/wiki/Small-world_network)

# We define k_{i} as the number of vertices, | N i | |N_{i}|, in the neighborhood, N_{i}, of a vertex. The local clustering coefficient C_{i} for a vertex v_{i} is then given by a proportion of the number of links between the vertices within its neighborhood divided by the number of links that could possibly exist between them. 

oa_transitivity <- transitivity(oa_i_net, type = "local")
hist(oa_transitivity) # View distribution of local transitivity scores. 

# There are a number of family clusters of triangles, so it makes sense that we have so many nodes with local transitivity scores of 1. I'm curious about the nodes with fairly high local transitivity scores of 0.6-0.7, as well as those with moderate to low scores of 0.3-0.4. There are isolated nodes (listed as self-loops in the edges list), and several dyads, which likely account for the lowest local transitivity scores. To examine these questions, I added the transitivity scores to the dataframe of other measures.

oa_df <- cbind(oa_df, oa_transitivity)
View(oa_df) # sorted dataframe by transitivity scores in descending order.

# Those with moderately high local transitivity scores included the Turkish wife of Ahmed El-Kolli, Hossein Bey-Zereg-Ainou, Son of Redjeb. 

# Women among those with moderately low transitivity scores include Daughter of Ahmed-El-Kolli, Deikra bint al-hadj ibn Bou-Zeyyid al Muqrani, Tourkia, Daughter of Bou-Henak, Daughter of Soultan ben Menasser, Daughter of Brahim ben Bou-Aziz, Daughter of Si Sadira, Daughter of Bou-Diaf, Redraja, Deikha, Wife of Bou Diaf, Daughter of Bou Mezrag, and Ashush. 

# Based on the way we constructed the network data, it makes sense that women would generally have higher local transitivity scores, since we often had to resort to adding women by inference and closing triads that began as a father-son dyad. Therefore, more women will be members of closed triads than men. 

# Ahmed El Koli's local transitivity score is only 0.25, and Salah Bey's is 0.009. The larger the family network, the less likely I've been able to find relations of relations, and they're less likely to be closely connected with one another. 

diam <- diameter(ott_alg_net, directed = FALSE, unconnected = TRUE) # Calculate the largest geodesic with diameter.
diam # calculate and return the diameter of the graph, indicating that is unconnected. A diameter of 8 seems fairly short for a social network, but given that this is a historical network from sparse data, a diameter of 8 could be seen as measure of success in sussing out relationships.

# Another view of clustering is to look at subgraph centrality.

# Subgraph centrality (SC) of a node is a weighted sum of the numbers of all closed walks of different lengths in the network starting and ending at the node.

library(igraph)
library(netrankr)
library(magrittr)
subgraph = subgraph_centrality(oa_i_net)
oa_df <- oa_df %>% mutate(subgraph = subgraph)
View(oa_df) #sort by subgraph centrality

# Top individuals by subgraph centrality include: Salah Bey, Ahmed El Kolli, Mohammed Cherif, Turkish wife of Ahmed El Kolli, Ferhat Bey, El Hadj Ahmed Bey, Ahmed El Kolli's daughter (presumably the one who married Salah Bey), two other sons of Ahmed El Kolli, and Ali Pasha el-Tunisi. 

#---------------------------------------------------------------------

## Determining calculable centrality measures based on an analysis of the graph structure using CINNA library and PCA (dimensional reduction) analysis ##

install.packages("CINNA")
library(CINNA)

pr_cent<-proper_centralities(oa_i_net) 

# Output from proper_centralities:
# [1] "subgraph centrality scores"                      
# [2] "Topological Coefficient"                         
# [3] "Average Distance"                                
# [4] "Barycenter Centrality"                           
# [5] "BottleNeck Centrality"                           
# [6] "Centroid value"                                  
# [7] "Closeness Centrality (Freeman)"                  
# [8] "ClusterRank"                                     
# [9] "Decay Centrality"                                
# [10] "Degree Centrality"                               
# [11] "Diffusion Degree"                                
# [12] "DMNC - Density of Maximum Neighborhood Component"
# [13] "Eccentricity Centrality"                         
# [14] "Harary Centrality"                               
# [15] "eigenvector centralities"                        
# [16] "K-core Decomposition"                            
# [17] "Geodesic K-Path Centrality"                      
# [18] "Katz Centrality (Katz Status Index)"             
# [19] "Kleinberg's authority centrality scores"         
# [20] "Kleinberg's hub centrality scores"               
# [21] "clustering coefficient"                          
# [22] "Lin Centrality"                                  
# [23] "Lobby Index (Centrality)"                        
# [24] "Markov Centrality"                               
# [25] "Radiality Centrality"                            
# [26] "Shortest-Paths Betweenness Centrality"           
# [27] "Current-Flow Closeness Centrality"               
# [28] "Closeness centrality (Latora)"                   
# [29] "Communicability Betweenness Centrality"          
# [30] "Community Centrality"                            
# [31] "Cross-Clique Connectivity"                       
# [32] "Entropy Centrality"                              
# [33] "EPC - Edge Percolated Component"                 
# [34] "Laplacian Centrality"                            
# [35] "Leverage Centrality"                             
# [36] "MNC - Maximum Neighborhood Component"            
# [37] "Hubbell Index"                                   
# [38] "Semi Local Centrality"                           
# [39] "Closeness Vitality"                              
# [40] "Residual Closeness Centrality"                   
# [41] "Stress Centrality"                               
# [42] "Load Centrality"                                 
# [43] "Flow Betweenness Centrality"                     
# [44] "Information Centrality"                          
# [45] "Dangalchev Closeness Centrality"                 
# [46] "Group Centrality"                                
# [47] "Harmonic Centrality"                             
# [48] "Local Bridging Centrality"                       
# [49] "Wiener Index Centrality"   

# Testing this approach with the first five suggested centrality measures

calculate_centralities(oa_i_net, include = pr_cent[1:5])%>%
  pca_centralities(scale.unit = TRUE)


#---------------------------------------------------------------------

# Cutpoints and Bridges

detach(package:igraph)
library(statnet)

?cutpoints
cuts <- as.list(cutpoints(oa_net, mode = "graph", return.indicator = TRUE))
names <- as.list(ott_Alg_node$Label)
gender <- as.list(ott_Alg_node$Gender)
ethnicity <- as.list(ott_Alg_node$Ethnicity)

cutpoints <- data.frame(cbind(names, gender, ethnicity, cuts)) # Create a data frame with the node names, gender, ethnicity and cutpoint data

cutpoints <- dplyr::filter(cutpoints, cutpoints$cuts==TRUE) # data frame of just the nodes that are cutpoints

# Explore the cutpoints data
cutpoints$gender <- as.character(cutpoints$gender) 
table(cutpoints$gender) # almost equal number of men and women, with slightly more women than men (20:19) when men outnumber women 2:1 in this graph! 11/20 of these women are unnamed, and all of these unnamed women are significant daughters, likely who married someone else in the graph, thus connecting the two families. 


cutpoints$ethnicity <- as.character(cutpoints$ethnicity)
table(cutpoints$ethnicity) # Of those who are cutpoints, they are nearly twice as likely to be Algerian as opposed to Ottoman or mixed-ethnicity.


#---------------------------------------------------------------------

# Convert ott_alg_net back to "network" class and keep oa_i_net as "igraph"
library(intergraph)
ott_alg_net <- asNetwork(ott_alg_net)
class(ott_alg_net)


## Layouts ##

# Fruchterman-Reingold

op <- par(mar = rep(0,4))
plot(ott_alg_net, mode = "fruchtermanreingold", vertex.cex=1.5)
par(op)

## Adding Attributes ##
# Make sure statnet and NOT igraph is loaded.
my_pal <- brewer.pal(3, "RdYlBu")
genderlab <- get.vertex.attribute(ott_alg_net, "Gender")
gendercat <- as.factor(get.vertex.attribute(ott_alg_net, "Gender"))
plot(ott_alg_net, vertex.cex=1.5, vertex.col=my_pal[gendercat])

# Creating a custom palette:
install.packages("unikn")
library(unikn)

color_scheme <- c("#953553", "#F08562", "#128387", "#347aad", "#152a56")
names_colscheme <- c("camelot", "burnt sienna", "pine green", "astral", "blue zodiac")
pal_colors   <- newpal(color_scheme, names_colscheme)
seecol(pal_colors, title = "Personal color scheme")

my2colors <- c("#F08562","#347aad")
names_2colors <- c("burnt sienna", "astral")
pal_2colors <- newpal(my2colors, names_2colors)
seecol(pal_2colors, title = "Personal Complementary Colors")

plot(ott_alg_net, vertex.cex=1.5, vertex.col=pal_2colors[gendercat])

# Sizing nodes by centrality
btw <- betweenness(ott_alg_net, gmode="graph")
plot(ott_alg_net, mode = "fruchtermanreingold", vertex.cex=log(btw), vertex.col=my2colors[gendercat], usearrows=FALSE)

# Create rescale function for betweenness centrality

#---------------------------------------------------------------------
library(netrankr)

# need an igraph network object

P <- neighborhood_inclusion(oa_i_net) # calculation neighborhood- inclusion pre-order, the most general requirement for any centrality index. 
comparable_pairs(P) # assess how many pairs of individuals are already ordered before applying any index. 

# Only around 3% of pairs of individuals are comparable, leaving 97% of pairs that could be ordered in many different ways. This suggests that a dominance-based approach is more reliable than an index approach (traditional measures of centrality).

## DOMINANCE APPROACH ##
# to determine most central individual(s) and to use centrality as an explanatory variable

library(igraph)
library(netrankr)
library(magrittr)

# Can centrality explain social or political status?


