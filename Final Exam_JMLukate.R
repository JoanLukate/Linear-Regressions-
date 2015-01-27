############################################################################################
#Final--General Instructions
############################################################################################

#For this exam, you will be asked to work with the "examdata.csv" file. We would like you do each of the steps below, carefully doing or providing responses to each point. The data is a subset of a larger data collection effort of a roughly nationally representative sample in the United States. We have also provided you with a codebook.csv" file which lists each variable, its description (e.g., what question was originally asked), the possible responses that participants could give to each question.

#Example question: Please show the histogram of variable "blah" in data frame "test"
hist(test$blah)

#In the example above, we have the question appear as a comment (so its GREEN), and then you provide your answer as R code.
#Please send back your completed script to us. To evaluate it, we will run the script, checking that each part works. The only thing that we should need to do is set the working directory. Otherwise, your script needs to run without any modification. 

#Other questions will ask you to interpret output. For these questions, write the interpretation as a comment--so it has "#" before it. 

#Example: Please interpret the output of model1

#Answer: In model 1, the intercept means blah blah....

#IMPORTANT: We will not make modifications to your script. Any part that does not run is automically 0 points for that part. 


############################################################################################
#Final--Questions
############################################################################################

#1) Set the working directory--make sure to paste in your code for setting the working directory
getwd()
setwd("C:/statistic//Linear Regressions")

#load relevant packages
library("psych")
#library("Hmisc") #NOTE that the package is loaded later on, as it masks describe() from the psych package
library("data.table")
library("ppcor")
library("ggplot2")

#2) Load in the data and call the dataframe "exam"

data <- read.csv("examdata.csv", header = TRUE)

#3) Show descriptive statistics for all the variables in the file
summary <- sapply(data, summary, na.rm= TRUE) # option 1
desstat_total <- describe(data[, c(1:56)]) # option 2, provides also skew, kurtosis and SE

#show output
summary
desstat_total

#4) Typically, we want to transform continuous variables that have a skew outside the -1 to 1 range. Take all the continuous variables with a skew that is outside this range and show their histograms. Then show histograms of the variable(s) once you make an appropriate transformation to get rid of the skew. Finally, show the descriptive statistics of those transformed variable(s) to check that the skew is gone.

#following the codebook, variables 21 - 56 including subjective health (variable 1) are treated as continuous variables
#create a vector called "skewdata" with the variable names of all continuous variables and bind the value of the skew to it
skewdata <- data.frame()
skewdata <- as.vector(names(data[c(1,21:56)]))
skewdata <- cbind.data.frame(skewdata, skew(data[c(1,21:56)] , na.rm = TRUE, type = 3), stringsAsFactors = FALSE)
colnames(skewdata) <- c("var_name", "skew") #assign appropriate variable
str(skewdata)

#select continuous variables that have a skew outside the -1 to 1 range
skewed <- skewdata[(skewdata$skew >= 1 | skewdata$skew <= -1),]
skewed

#historgram of morality including line for the mean
hist_morality <- ggplot(data, aes(x=morality)) + geom_histogram(binwidth=.5, colour="black", fill="white") + coord_cartesian(ylim = c(0, 300)) + geom_vline(aes(xintercept=mean(morality, na.rm=T)), color = "blue", linetype = "dashed", size = 1)                                                                                                                                

hist_morality 

#alternatively call hist() using the base plotting system - for some reason that I could not solve the output from ggplot and hist is different with regard to the last column. The underlying algorithm seems to process the data differently.  
hist(data$morality, main = "Histogram", xlab = "morality")

#transformation: reverse score transformation due to the negative skew, followed by log transformation
data$morality_log <- log(8-data$morality)

#visual assessment of transformed data
k <- na.omit(data[, c("morality")])
x<- na.omit(data[, c("morality_log")])
par(mfrow = c(1,2))
with(data, {
  h1 <- hist(data$morality, xlab = "morality", main = "Histogram 1")
  xfit<-seq(min(k),max(k),length=40)
  yfit<-dnorm(xfit,mean=mean(k),sd=sd(k))
  yfit <- yfit*diff(h1$mids[1:2])*length(k)
  lines(xfit, yfit, col="blue", lwd=2)
  h <- hist(data$morality_log, xlab = "log morality ", main = "Histogram 2")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2) 
})

#NOTE: please ensure that the Hmsic package has not yet been enable, as this masks describe() in the psych package.
desstats <- describe(data$morality)
desstats <- rbind(desstats, describe(data$morality_log))
desstats #skew reduced to 0.22

#in case Hmsic package is active
skew(data$morality)
skew(data$morality_log)

#5) Choose two continuous variables. Make one your IV and make the other your DV--choose carefully since we will keep working with these variables. Plot the relationship between the two variables. 
#predictor: activitylevel, DV: SWL

relation <- ggplot(data, aes(activitylevel, swl))  + geom_point(alpha = 1/3) + labs(title = "Relationship between Activity Level and Satisfaction with Life") + labs(x = "activity level") + labs(y = "satisfaction with life") + theme_bw(base_family = "Avenir")

relation

#6) There's a lot of data here and so the plot may not be all that helpful on its own in telling you what the relationship is. So add a line that can help us understand what kind of relationship these variables have. 
relation <- relation + geom_smooth(col = "red", se = FALSE, size = 1) 

relation

#7) Run a correlation between your two variables
library("ppcor")
library("Hmisc")
temp <- subset(data, select = c("swl", "activitylevel"))
cor(temp, use = "complete.obs", method = "pearson")
#disadvantage: cor() only provides the correlation coefficient but no p-values
corr.test(temp)$p # get p values
cor.test(temp$swl, temp$activitylevel) # get df, t value and confidence interval 
      
#8) Write an interpretation of what the correlation coefficient and p-value mean in the above correlation
#ANSWER: The correlation coefficient describes the degree to which two variables can be describes in a linear relationship. A value of +1 indicates a perfect positive relatioship, whereas a value of -1 suggests a perfect negative relationship. Step (7) found a correlation coefficient of 0.2969. The correlation coefficient of approximately 0.3 suggests a positive relationship between activity level and satisfaction with life, and may be interpreted as a medium effect, if we treat the coefficent as a measure of effect size. However, the correlation coefficient gives us no indication of the direction of causlity, i.e. which variable causes the other to change. It only tells us that there is a linear relationship.
#The p value allows us to establish how confident we can be that there is indeed a relationship between the two variables. In step (7), a significance value of less than .001 was found. The p value is the proability of finding the same (or a more extreme) result with the same direction when the null hypothesis were true.Thus, the probablity of getting a correlation coefficent of 0.29 in a sample of ~974 people if the null hypothesis were true is very low, i.e. close to zero. As such the p-value provides us with confidence that there is indeed a relationship between activity level and swl. 

#9) How much total variance does your IV account for in your DV?
R_squared <- cor(temp, use = "complete.obs", method = "pearson")^2 #matrix containing r squared
R_squared*100 #matrix expressing the percentage of total variance explained

#Answer: Activity level (IV) accounts for 8.81% of total variance in satisfaction with life (DV). 

#10) Lets now run a regression that maps onto the above correlation. Keep the X and Y the same in the regression as you did in your correlation. Show the results and confidence intervals of the regression. 

model1 <- lm(swl ~ activitylevel, data = data, na.action = na.omit)

summary(model1)
confint(model1, level=0.95)

#11) Write an interpretation of each estimate and p-value of the above summary output (don't worry about interpreting the SE, df, or T)

#b0 (value in the estimate column for the (intercept)) =2.55719. Hence, when activity level = 0, satisfaction with life = 2.55719. b1 = 0.41406 is the slope of the regression line. It describes the change in satisfaction with life for every one unit increase in activity level. Hence, if activity level increases by 1, the model predicts that satisfactin with life increases by 0.41406 units. 
#The p value is the probability that the observed t value( 14.153) occurs in the population if b = 0, because the t test tells us whether the b-value is different from 0. In this model, the probability of finding these t-values in the population if the values of b were less than 0 is less than .0001. Hence, the bs are different from 0, allowing us to conclude that activity level significantly (p < .001) contributes to predicting satisfaction with life. 

#The summary output also lists the results of an ANOVA, providing the F-ratio and the associated significance value. F = 93.99 is significant at p < .001, which means that the chance of such a large F-ratio if the null hypothesis were true is less than 0.1%. Hence, the regression model predicts satisfaction with life significantly well.

#12) Write an interpretation of the confidence intervals

#The confidence intervals are constructed in a way that their boundaries tell us whether there is a 95% (or more/less) chance that these boundaries contain the true value of b. The model presented here is relatively good, as we have a small confidence intervall CI95(0.33, 0.49), indiciating that in this sample, the value of b is close to the true value of b in the population. Moreover, we can assume that p < .05, as the CI95 does not include 0 in its range, suggesting that the b-values tell us the direction of the relationship between satisfaction with life and activity level. However, the lower limit is below 1. Hence, there is a chance that the direction of the relationship as observed in model 1 is opposite to the direction of the relationship in the population. I would thus conclude that we have to treat the findings with caution. 

#13) Add the regression line to your plot

relation <- relation + geom_smooth(method = "lm", col = "steelblue", size = 1) 

relation

#alternatively, we can plot the relationship using plot() in the base plotting system
dev.off() # call device off to ensure that previous settings do not interfer with plot
with(data, {
  plot(activitylevel, swl, main = "Visualization of Regression model 1", xlab = "activity level", ylab = "satisfaction with life")
  curve(2.55719+.41406*x, add=T)
})

#14) Flip your X and Y in the regression model and rerun it. Show the output of the new model and then describe how this model differs from the one your ran above. Discuss whether the relationship we are capturing is ultimately the same or different. If its the same, why are some numbers different?

model1_reversed <- lm(activitylevel~ swl, data = data, na.action = na.omit)

summary(model1_reversed)
confint(model1_reversed, level=0.95)

#ANSWER: The model differs from model 1 in that it describes the relationship between satisfaction with life (IV) and activity level (DV). In the previous model, I regressed activitylevel onto SWL. Hence, b0 = 2.55719 represented satisfaction with life when activitylevel = 0. In the reversed model, b0 = 3.17857 represents the level of activity assuming SWL = 0. The slope of the original regression model was b1 = 0.41406 (compared to b1 = 0.21293 in the reversed model)and it described the change in SWL for every one unit increase in the level of activity. In the reversed model, however, b1 means the change in activity level for every one unit increase in satisfaction with life. Hence, if satisfaction with life increases by 1, the model predicts that the level of activity increases by 0.21293 units. Hence, the increase is smaller than in model 1, which we can also see if we plot the two regression models next to each other:

par(mfrow = c(1,2))
with(data, {
  plot(activitylevel, swl, main = "Model 1", xlab = "activity level", ylab = "satisfaction with life")
  curve(2.55719+.41406*x, add=T, col = "red")
  plot(swl, activitylevel, main = "Model 1 reversed", xlab = "satisfaction with life", ylab = "activity level")
  curve(2.983773+0.16983*x, add = T, col = "red")
})

#As a result, we find a graphical difference between the two plots. 
#Ultimately, the relationship that we capture is different. The reason is that by trying to find the 'line of best fit', we try to reduce the standard errors. Hence, we try and minimize the vertical distances between the observed data points and the regression line.In doing so, we assume that all the error is in y, not in x. Thus, if we flip x and y around, we do not capture the same relationship, as now all the error is assume in x rather than y. Put differently, the relationship is only the same if we standradize both variables prioir to regressing them. Because otherwise, the slope of the regression b1(y onto x) = Cov(x,y)/Var(x) whereas the slope of the regression b1(x on y) = Cov(y,x)/Var(y). So they would only be the same if Var(x) = Var(y).  
      
#15) Pick 3 more continuous IVs. Show the necessary steps to test (a) whether each additional predictor in the model explains a significant amount of additional variance in the DV, and (b) what is the partial correlation of each IV with the DV? 

##############################################################################
#Descriptives
##############################################################################
#IV2: depression
#IV3: cheerfulness
#IV4: trust

#histograms
par(mfrow = c(1,3))
with(data, {
  hist(data$depression, xlab = "depression", main = "Histogram 1")
  hist(data$cheerfulness, xlab = "cheerfulness", main = "Histogram 2")
  hist(data$trust, xlab = "trust", main = "Histogram 3")
})

#plots
relation2 <- ggplot(data, aes(depression, swl))  + geom_point(alpha = 1/3) + geom_smooth(method = "lm", col = "blue")+ labs(title = "Relationship between Depression and Satisfaction with Life") + labs(y = "satisfaction with life") +  theme_bw(base_family = "Avenir")

relation2

relation3 <- ggplot(data, aes(cheerfulness, swl))  + geom_point(alpha = 1/3) + geom_smooth(method = "lm", col = "blue")+ labs(title = "Relationship between Cheerfulness and Satisfaction with Life") + labs(y = "satisfaction with life") + theme_bw(base_family = "Avenir")

relation3

relation4 <- ggplot(data, aes(trust, swl))  + geom_point(alpha = 1/3) + geom_smooth(method = "lm", col = "blue")+ labs(title = "Relationship between Trust and Satisfaction with Life") + labs(y = "satisfaction with life") +  theme_bw(base_family = "Avenir")

relation4

#correlation matrix
temp <- na.omit(data[,c("swl", "activitylevel", "depression", "cheerfulness", "trust")])
corr.test(temp)

#########################################################################
###ANALYSES
#######################################################################
#check individually how much variance (R squared) each predictor explains in DV
m1 <- lm(swl ~  depression, data = data, na.action = na.omit)
m2 <- lm(swl ~  cheerfulness, data = data, na.action = na.omit)
m3 <- lm(swl ~  trust, data = data, na.action = na.omit)

summary(m1)
summary(m2)
summary(m3)

#create temporary file that omits missing values for multiple regression models
temp <- na.omit(data[,c("swl", "activitylevel", "depression", "cheerfulness", "trust")])

#model with TWO predictors
model2 <- lm(swl ~ activitylevel + depression, data = temp, na.action = na.omit)
# model with THREE predictors
model3 <- lm(swl ~ activitylevel + depression + cheerfulness, data = temp, na.action = na.omit)
#model with FOUR predictors
model4 <- lm(swl ~ activitylevel + depression + cheerfulness + trust, data = temp, na.action = na.omit)

summary(model2)
confint(model2, level=0.95)
summary(model3)
confint(model3, level=0.95)
summary(model4)
confint(model4, level=0.95)
#calling model summary allows us to see, under 'coefficients', that each predictor significantly contributes to the model. We can also see that the R squared increases for each model 2-4, i.e. each model explains more variance in satisfaction with life.
#Specifically,the first model with depression and activitylevel as predictors explained 18.68% of variance in SWL. The second model, which added cheerfulness, explained 30.04% of variance in SWL, suggesting that cheerfulness is an important predictor of SWL. Moreover, all three predictors  were significant, indiciating that changes in the predictor's value are related to changes in the response variable. The p value for the (Intercept) was also significant (p <.001). Finally, adding trust to the model increased the total variance explained to 32.5%. All coefficients are supposedly different from 0, p < .001. 
#As none of the predictors has lost its predictive power, I would argue that each additional predictor in the model explains a signfiicant amount of additional variance in SWL. 

#partial correlation for each of the predictors
library("ppcor")
#create temporary file that omits missing values for multiple regression models
temp <- na.omit(data[,c("swl", "activitylevel", "depression", "cheerfulness", "trust")])
pcor(temp)

#ANSWER: The partial correlations between DV and each IV controlling for the other IVs can be found in the under $estimate in the output: 
#The partial correlation between trust and SWL is 0.1873431, controlling for activity level, depression and cheerfulness 
#The partial correlation between cheerfulness and SWL is 0.2975023, controlling for activity level, depression and trust
#The partial correlation between depression and SWL is -0.1114013, controlling for activity level, cheerfulness and trust
#The partial correlation between activity level and SWL is 0.1442486, controlling for depression, cheerfulness and trust


#16) Write an interpretation of the results of the model with all 4 IVs as predictors. Describe each estimate and p-value (don't worry about interpreting the SE, df, or T)
summary(model4)
confint(model4, level=0.95)

#ANSWER: 
#b0 = 0.82104 - satisfaction with life (SWL) of someone when activity level = 0, depression = 0, cheerfulness 0 = and trust = 0
#b1 = 0.17821, describes how much SWL (on average) increases for every 1 unit increase in activity level, assuming depression, cheerfulness and trust remain the same
#b2 =  -0.11783, describes how much SWL (on average) DEcreases for every 1 unit INcrease in depression, assuming activity level, cheerfulness and trust remain the same
#b3 = 0.42243 , describes how much SWL (on average) increases for 1 unit increase in cheerfulness, assuming depression, activity level and trust remain the same
#b4 = 0.22491, describes how much SWL (on average) increases for every 1 unit increase in trust, assuming depression, cheerfulness and activity level remain the same
#the p value of the t value (2.789) of the (Intercept) is significant at p < .001, suggesting that the b value is different from 0. Hence, we can conclude that activity level significantly (p < .001) contributes to predicting satisfaction with life.

#The p value is the probability that the observed t value( 14.153) occurs in the population if b = 0, because the t test tells us whether the b-value is different from 0. In this model, the probability of finding these t-values in the population if the values of b were less than 0 is less than .001. Hence, the bs are different from 0, allowing us to conclude that activity level significantly (p < .001) contributes to predicting satisfaction with life. For all other bs (b1, b2, b3, b4), the p value of the t value is < .0001, and thus we can conclude that all b-values are different from 0. As a result, each predictor is a meaningful addition to the model as changes in the predictor's value are associated with changes in SWL. 

#The summary output also lists the results of an ANOVA, providing the F-ratio and the associated significance value. F = 116.6 is significant at p < .001, which means that the chance of such a large F-ratio if the null hypothesis were true is less than 0.1%. Hence, the multiple regression model predicts satisfaction with life significantly well.

  
#17) Try finding another continuous variable in the data that moderates the effect of your original IV on your original DV. Center both IVs. Run a model with the interaction and show its results.

#moderation: y = b0 + b1*x1 + b2*Z + b3*x1*Z + E
#original IV: activitylevel, DV: SWL, moderator: vulnerability

#step1: center IVs
data$activitylevel_cen = data$activitylevel - mean(data$activitylevel, na.rm = T)
data$vulnerability_cen = data$vulnerability - mean(data$vulnerability, na.rm = T)

#Step 2: create regression model with interation term
interaction1 <- lm(swl ~ activitylevel_cen*vulnerability_cen, data = data, na.action = na.omit)

summary(interaction1)
confint(interaction1, level=0.95)

#18) Write an interpretation of each estimate and p-value of the above summary output (don't worry about interpreting the SE, df, or T)

#b0 = 4.25543 means that SWL is at 4.25543, when activitylevel and vulnerability are 0
#b1 =  0.38765 is the change in SWL' when activity level increases by 1 unit and vulnerability = 0
#b2 = -0.21556 is the change in SWL' when vulnerability increases by 1 unit and activity level = 0
#b3 = 0.06666 is a) change in slope between activity level and SWL for 1 unit increase in vulnerability, or b) change in slope between vulnerability and SWL for 1 unit increase in activity level
#the p values suggest that b1, b2 and b3 are different from 0 at p <.001. However, we have less confidence that b4 is different from 0, with p at < .05. 


#19) You are interested in visualing the interaction. Take the necessary steps to do this. Do it both ways, with each of the IV serving as the "moderator".

##############################################################################
#Descriptives
##############################################################################

#look at histograms of your data

hist_activitylevel <- ggplot(data, aes(x=activitylevel)) + geom_histogram(binwidth=.5, colour="black", fill="white") + coord_cartesian(ylim = c(0, 250)) + geom_vline(aes(xintercept=mean(morality, na.rm=T)), color = "blue", linetype = "dashed", size = 1) 
hist_vulnerability <- ggplot(data, aes(x=vulnerability)) + geom_histogram(binwidth=.5, colour="black", fill="white") + coord_cartesian(ylim = c(0, 250)) + geom_vline(aes(xintercept=mean(morality, na.rm=T)), color = "blue", linetype = "dashed", size = 1)
hist_swl <- ggplot(data, aes(x=swl)) + geom_histogram(binwidth=.5, colour="black", fill="white") + coord_cartesian(ylim = c(0, 250)) + geom_vline(aes(xintercept=mean(morality, na.rm=T)), color = "blue", linetype = "dashed", size = 1)

hist_activitylevel
hist_vulnerability
hist_swl

##############################################################################
#Analyses
##############################################################################

#step 1: center around high and low values of each variable.
#predictor: activitylevel
data$high_activitylevel <- data$activitylevel-mean(data$activitylevel, na.rm=T) - SD(data$activitylevel, na.rm=T)
data$low_activitylevel <- data$activitylevel-mean(data$activitylevel, na.rm=T) + SD(data$activitylevel, na.rm=T) 
#mediator: vulnerability
data$high_vulnerability <- data$vulnerability-mean(data$vulnerability, na.rm=T) - SD(data$activitylevel, na.rm=T)
data$low_vulnerability <- data$vulnerability-mean(data$vulnerability, na.rm=T) + SD(data$activitylevel, na.rm=T) 

#Run models
model_cen <- lm(swl ~ activitylevel_cen*vulnerability_cen, data = data, na.action = na.omit)
model_high <- lm(swl ~ activitylevel_cen*high_vulnerability, data = data, na.action = na.omit)
model_low <- lm(swl ~ activitylevel_cen*low_vulnerability, data = data, na.action = na.omit)
#run models for exchanged IV and moderator
rmodel_high <- lm(swl ~ high_activitylevel*vulnerability_cen, data = data, na.action = na.omit)
rmodel_low <- lm(swl ~ low_activitylevel*vulnerability_cen, data = data, na.action = na.omit)

summary(model_cen)
summary(model_high)
summary(model_low)
summary(rmodel_high)
summary(rmodel_low)

#define equations
eq1 <- function(x)4.01551 +  0.46184*x #high
eq2 <- function(x)4.49534 + 0.31346*x #low

eq3 <- function(x)4.68688 -0.14137*x  #high
eq4 <- function(x)3.82398 -0.28975*x #low

#plot relationships using ggplot
interaction1 <- ggplot(data, aes(x = activitylevel_cen, y = swl)) +
  geom_point(alpha = 1/3) + geom_smooth(method = "lm", colour = "black", se = FALSE, size = 1,    fullrange = TRUE) +
  stat_function(fun = eq1, colour = "red", size = 1, fullrange = TRUE) +
  stat_function(fun = eq2, colour = "blue", size = 1, fullrange = TRUE)+
  labs(title ="Visualization of interaction")

interaction1

interaction2 <- ggplot(data, aes(x = vulnerability_cen, y = swl)) +
  geom_point(alpha = 1/3) + geom_smooth(method = "lm", colour = "black", se = FALSE, size = 1,    fullrange = TRUE) +
  stat_function(fun = eq3, colour = "green", size = 1, fullrange = TRUE) +
  stat_function(fun = eq4, colour = "yellow", size = 1, fullrange = TRUE)+
  labs(title ="Visualization of interaction 2")

interaction2

#alternatively, quickly plot interactions with base plotting system 
with(data, {
  #plots the two lines if the x-axis is activitylevel and vulnerability is the moderator
  curve(4.01551 +  0.46184*x,add=F, col = "green", from=-5, to=10, xlab="x", ylab="swl", main = "Interaction")
  curve(4.49534 + 0.31346*x,add=T, col = "red",xlab="x", ylab="swl")
  #plots the two lines if the x-axis is vulnerability and activitylevel is the moderator
  curve(4.68688 -0.14137*x,add=T, col = "orange", from=-5, to=10, xlab="x", ylab="swl")
  curve(3.82398 -0.28975*x,add=T, col = "blue", from=-5, to=10, xlab="x", ylab="swl")
  legend("topright", lty = c(1,1,1), col = c("green", "red", "orange", "blue"), legend = c("high vulneravility", "low vulnerability", "high activity level", "low activity level"), box.col = "white")
})

#20) Now choose a categorical variable and use that as a moderator of the original effect (original IV predicting original DV). Take the necessary steps to test this. Show output of your model.

##############################################################################
#Data prep
##############################################################################
# create categorical variable for gender
table(data$gender)

data$male[data$gender == 1 & !is.na(data$gender)] <- 1
data$male[!data$gender == 1 & !is.na(data$gender)] <- 0

data$female[data$gender == 2 & !is.na(data$gender)] <- 1
data$female[!data$gender == 2 & !is.na(data$gender)] <- 0

data$unspecified_gender[data$gender == 3 & !is.na(data$gender)] <- 1
data$unspecified_gender[!data$gender == 3 & !is.na(data$gender)] <- 0

#centers continous variable  around high and low values of each variable.
data$high_activitylevel <- data$activitylevel-mean(data$activitylevel, na.rm=T) - SD(data$activitylevel, na.rm=T)
data$low_activitylevel <- data$activitylevel-mean(data$activitylevel, na.rm=T) + SD(data$activitylevel, na.rm=T) 
#centered variable for activity level exists already 

###############################################################################
###Descriptive Statistics - already explored earlier, thus step is omitted here
###############################################################################

##############################################################################
#Analyses
##############################################################################

#interaction between gender and activitylevel in predicting satisfaction with life (swl)
int1 <- lm(swl~male*activitylevel_cen, data=data, na.action=na.omit)
int2 <- lm(swl~female*activitylevel_cen, data=data, na.action=na.omit)
int3 <- lm(swl~unspecified_gender*activitylevel_cen, data=data, na.action=na.omit)

#statistics 
summary(int1)
summary(int2)
summary(int3)

confint(int1, level=.95)
confint(int2, level=.95)
confint(int3, level=.95)

#visualisation of model
interaction3 <- ggplot(data, aes(x = vulnerability_cen, y = swl, col = factor(gender))) +
  geom_point(alpha = 1/3, color = "black") + geom_smooth(method = "lm", se = FALSE, size = 1,    fullrange = TRUE) + labs(title ="Visualization of interaction 3")  
                                                                                                                                                                        

interaction3

#unfortuantely, no time left to edit the legend :(

#21) Write an interpretation of each estimate and p-value of the above summary output (don't worry about interpreting the SE, df, or T)

#for int 1 - male: 
#b0 = 4.25388 is satisfaction with life of men with an average activity level; b1 = -0.01117 is the increase in SWL for a single unit increase in activitylevel for men; b2 = 0.35031 is the difference in SWL between males and non-males assuming activitylevel is equal to average. b3 = 0.28351 is the difference in the activity level effect between males and non-males. The p value tells us that b1, b3 and b4 are different from 0, however, b2 is not significant. The interaction term is only significant at p < .05. 

#for int2 -female (1), non-female(0):
#b0 = 4.24627 is women's satisfaction with life when their activity level is equal to average; b1 = 0.00449 is the increase in SWL for a single unit increase in activitylevel for women; b2 = 0.35031 is the difference in SWL between males and non-males assuming activitylevel is equal to average. b3 = 0.59297 is the difference in the activity level effect between females and non-females. The p value tells us that b1, b3 and b4 are different from 0, however, b2 is not significant. The interaction term is only significant at p < .01. 
      
#for int3 -unspecified gender (1), non-unspecified gender(0):
#b0 = 4.24086 is the satisfaction with life of people who have not specified their gender when their activity level is equal to average; b1 = 0.19601 is the increase in SWL for a single unit increase in activitylevel for people who have not specified their gender; b2 = 0.42033 is the difference in SWL between people who have not specified their gender and those who did specify their gender(i.e. male/female) assuming activitylevel is equal to average. b3 = -0.69216 is the difference in the activity level effect between people who have not specified their gender and those who have specified it. The p value tells us that b1, b3 and b4 are different from 0, however, b2 is not significant. The interaction term is not significant. Also, the sample sizes (unspecified geder = 10 versus specified gender = 975) are very unequal, which most likely impacts on the robustness of the model. 

#the three models show that males generall have a higher satisfaction with life when their activity level is at average than women and those who did not specify their gender. 

#as the models all use dummy variables, we can calculate the following three equations (note, I did those by hand on a paper, i.e. I wrote down the regression equation for each model including the coefficents and then simplified them to the following model SWL' = Inercept + b1 + activitylevel*(b2 + b3)):
#SWL(male) = 4.24 +0.63*activitylevel
#SWL(female) = 4.2445 + .356*activitylevel
#SWL(unspecified) = 4.43 - 0.27*activitylevel
#Accodring to these three models, those who did not specify their gender have a higher satisfaction with life (4.43) when activity level is average than men (4.24) or women (4.2445). However, the biggest change in satisfaction for life occurs in men, i.e. for every 1 unit increase in the level of activity, men's satisfaction with life increases by 0.63. For women, the increase is 0.356. For those who did not specify their gender, however, every 1 unit increase in activitylevel is associated with a decrease of swl. 

#22) Take your original IV and DV, and see if you can find a mediator of this effect. Run the mediation and show the results. Write an interpretation of what you have learned from doing this analysis.
      
temp <- na.omit(data[,c("swl", "activitylevel", "selfdiscipline")]) #create subset that omits missing values to allow for mediate()
##############################################################################
#Analyses
##############################################################################

#Set up models to explore mediation
mo1 <- lm(swl ~ activitylevel, data = temp, na.action = na.omit) #main effect 
mo2 <- lm(selfdiscipline ~ activitylevel, data = temp, na.action = na.omit) #x --> M
mo3 <- lm(swl ~ selfdiscipline + activitylevel, data = temp, na.action = na.omit) # M + X --> Y

summary(mo1)
summary(mo2)
summary(mo3)

#Create mediation; 50 sims for bootstrapping here, but usually we do 20,000
mediation1 <- mediate(mo2, mo3, sims = 50, boot = TRUE, treat = "activitylevel", mediator = "selfdiscipline")

summary(mediation1)

#INTERPRETATION: The first model found a significant main effect, p < .001. In other words, activtylevel predicts SWL. The second model found that activity level also predicts the moderator, selfdiscipline. In the last model, I modelled activity level and self-discipline onto SWL. Both remained significant predictors (p < .001) of SWL, which suggests that selfdiscipline does not mediate the relationship between activitylevel and SWL. 
#Interpreting the output of mediation1, ACME = 0.0935 is the estimated average increase in SWL that results from selfefficancy (mediator) rather than from level of activity. However, although the confidence interval is very small, they are also below 1. Furthermore, of tHe estimated 0.4141 (total effect) increase in SWL due to the level of activity, an estimated 0.0935 (ACME) result from selfdiscipline and the remaining 0.3206 (ADE) is caused by the predictor, i.e. level of activity. The proportion of the original effect that is mediate = 0.2258. 
#At most, I could argue that the effect of activitylevel on SWL is partially explained by selfdisciline. 


#23) Imagine you want to conduct a study with a central IV and three control IVs, and you want to figure out how many participants to recruit to detect the effect of the central IV when controlling for the other three IVs. You are hoping to achieve 95% chance of rejecting the null hypothesis. You estimate that the three control IVs should have a model r of .2. The partial correlation of your central IV is .25 (in the model with all four IVs predicting the DV). Take the steps necessary to figure out the necessary number of participants you want to recruit.

#step 1: identify analysis and given values
#u (number of predictors) = 4
#sig.level = .05
# f2 = [(0.2 - R^2(central IV))/(1-0.2)]
#r of 3 IVs = .2 --> r squared = .2^2 = 0.04
#power is generally .8 or higher, I go for .9

#step 2: calculate R squared for central IV, when partial correlation = 0.25

#step 3: run analysis
pwr.f2.test(power=.9, u=4, sig.level=.05, f2=((0.2 - R^2(central IV))/(1-0.2))
