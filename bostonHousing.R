setwd("/home/ze/Downloads")

# Load CSV
boston.df <- read.csv("BostonHousing.csv", header=TRUE)

# Remove space from "CAT. MEDV" column name
colnames(boston.df)[14] <- "CAT.MEDV"

############
# Question 1
############

# Draw scatter plot
plot(
  MEDV ~ LSTAT,
  data=boston.df,
  main = "Q1: Relation between MEDV and LSTAT",
  xlab = "Median value of owner-occupied homes in $1000s",
  ylab = "Percentage of lower status of the population"
)

############
# Question 2
############

# Compute mean MEDV er CHAS (0,1)
plot.df <- aggregate(boston.df$MEDV, by = list(boston.df$CHAS), FUN = mean)
# Change column names
names(plot.df) <- c("CHAS", "MeanMEDV")
# Set variable as factor
plot.df$CHAS<-factor(plot.df$CHAS, labels=c("No", "Yes"))

# Draw bar plot
barplot(
  plot.df$MeanMEDV,
  names.arg = plot.df$CHAS,
  main = "Q2: Relation between CHAS and avg. MEDV",
  xlab = "Does tract bound Charles River?",
  ylab = "Avg. median value of owner-occupied homes in $1000s",
  ylim=c(0, 30),
  col=c("red","green"),
  xlim=c(0, 1),
  width=0.1
)

############
# Question 3
############
library(ggplot2)
library(scales)

# Split two dataframes, df1 is for median house values > 1000$ and
# df2 is for median house values < 1000$
bs.df = boston.df[,c("CRIM", "CAT.MEDV")]
df1 = subset(bs.df, CAT.MEDV == 1)
df2 = subset(bs.df, CAT.MEDV == 0)
df1$CAT.MEDV <- "lower"
df2$CAT.MEDV <- "bigger"

vegLengths <- rbind(df1, df2)

# Draw density plot
ggplot(
  vegLengths,
  aes(CRIM, colour=CAT.MEDV, fill = CAT.MEDV)
) + geom_density(alpha = 0.1) +
    xlim(0,25) + ylim(0, 1.6) +
    xlab("Crime Percentage") +
    ggtitle("Q3: Distribution of CRIM by CAT.MEDV")


############
# Question 4
############

# Draw histogram
hist(
  boston.df$MEDV,
  main="Q4: MEDV distribution graph",
  ylim=c(0, 200),
  xlim=c(0, 50),
  xlab = "Median value of owner-occupied homes in $1000s"
)

