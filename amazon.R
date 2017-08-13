setwd("C:/Users/Downloads")

library(igraph)

# Load Amazon dataset
amazon.df <- read.csv("Amazon0302.csv", header = T)

###################################################################################################
# Q.1 - Sample of size 1000 [1: 1000]
###################################################################################################
sample.df <- amazon.df[1:1000,]

g <- graph.edgelist(as.matrix(sample.df), directed = TRUE)

# Calculate centrality indexes
inDegree <- degree(g, mode = "in")
outDegree <- degree(g, mode = "out")
betweennessCentrality <- betweenness(g)
inClosenessCentrality <- closeness(g, mode = "in")
outClosenessCentrality <- closeness(g, mode = "out")

###################################################################################################
# Q.2-A - 40 samples of size 1000 [1: 1000], [1001, 2000], ...
###################################################################################################

# Create DF to store the calculated values
results.df <- data.frame()
count <- 0

for (i in seq(1, 1000*40, by = 1000)) {
  count <- count + 1
  sample.df <- amazon.df[i:(1000 * count),]

  cat(paste("Taking sample:", count, "Range:", i, "->", count * 1000, "\n"))

  g <- graph.edgelist(as.matrix(sample.df), directed = TRUE)

  # Calculate centrality indexes
  add.df <- data.frame("in.degree"=mean(degree(g, mode = "in")),
                       "out.degree"=mean(degree(g, mode = "out")),
                       "betweenness"=mean(betweenness(g)),
                       "in.closeness"=mean(closeness(g, mode = "in")),
                       "out.closeness"=mean(closeness(g, mode = "out")))

  results.df <- rbind(results.df, add.df)
}

hist(results.df$in.degree, main="Histogram for In-Degree")
hist(results.df$out.degree, main="Histogram for Out-Degree")
hist(results.df$betweenness, main="Histogram for Betweenness")
hist(results.df$in.closeness, main="Histogram for In-Closeness")
hist(results.df$out.closeness, main="Histogram for Out-Closeness")

###################################################################################################
# Q.3- 40 samples of size 1000 [1: 1000], [1001, 2000], ...
###################################################################################################

# Create DF to store the calculated values
results.df <- data.frame()
count <- 0

for (i in seq(1, 40)) {
  count <- count + 1
  sample.df <- amazon.df[1:(1000 * count),]

  cat(paste("Taking sample:", count, "Range:", "1", "->", count * 1000, "\n"))

  g <- graph.edgelist(as.matrix(sample.df), directed = TRUE)

  # Calculate centrality indexes
  add.df <- data.frame("in.degree"=mean(degree(g, mode = "in")),
                       "out.degree"=mean(degree(g, mode = "out")),
                       "betweenness"=mean(betweenness(g)),
                       "in.closeness"=mean(closeness(g, mode = "in")),
                       "out.closeness"=mean(closeness(g, mode = "out")))

  results.df <- rbind(results.df, add.df)
}

hist(results.df$in.degree, main="Histogram for In-Degree (after a change)")
hist(results.df$out.degree, main="Histogram for Out-Degree (after a change)")
hist(results.df$betweenness, main="Histogram for Betweenness (after a change)")
hist(results.df$in.closeness, main="Histogram for In-Closeness (after a change)")
hist(results.df$out.closeness, main="Histogram for Out-Closeness (after a change)")

