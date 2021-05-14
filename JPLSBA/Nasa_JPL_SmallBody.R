######################################################################
#the orignal file downloaded is split into 3 to be uploaded to github
#data <- read_csv("Nasa_JPL_SmallBody.csv")
#data_1 <- data[1:360353,]
#data_2 <- data[360354:720706,]
#data_3 <- data[720707:1081059,]
#write_csv(data_1,'JPL_1.csv')
#write_csv(data_2,'JPL_2.csv')
#write_csv(data_3,'JPL_3.csv')
#######################################################################


rm(list = ls(all.names=TRUE))
gc()

#load the required libraries
library(knitr)
library(markdown)
options(digits = 5)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(wsrf)) install.packages("wsrf", repos = "http://cran.us.r-project.org")

###################################################################################################
# Please NOTE:
# THIS CODE CHUNK WILL DOWNLOAD THE FILES REQUIRED BY THIS RMD, if not present.
# THE FILES WILL BE DOWNLOADED FROM A GITHUB REPOSITORY.
# DOWNLOADED FILES ARE ~ 223MB

#####TO DOWNLOAD MANUALLY PLEASE STOP EXECUTION OF THIS CHUNK #####
#####AND RUN THE "manualDownload" CHUNK, THE NEXT ONE.#####

# NO FILES WILL BE DELETED. ONCE THIS CHUNK HAS COMPLETED 
# THE JPL_1.zip,JPL_2.zip,JPL_3.zip CAN BE REMOVED. 

# ALL OTHER FILES ARE REQUIRED FOR THE REMAINING CODE EXECUTION.
# approximately ~560MB OF DISK SPACE IS REQUIRED.


if(file.exists("Nasa_JPL_SmallBody.csv") != TRUE){ 
  #Download the required files
  
  dl <- tempfile()
  download.file("https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/JPL_1.zip",dl)
  #sha256sum should be F156FD5A7AF7202145D7B21F1B40BA62626F8192D46CAC290A0ABC9CD2A8DBEE
  dat1<- read_csv(dl)
  rm(dl)
  
  dl <- tempfile()
  download.file("https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/JPL_2.zip",dl)
  #sha256sum should be 2603A25A4C44973FB0C7C0A607980C8D992538AF55A247DAE815483C708CBDB8
  dat2<- read_csv(dl)
  rm(dl)
  
  dl <- tempfile()
  download.file("https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/JPL_3.zip",dl)
  #sha256sum should be ECDE408DD9D89396F22F2D9AAB2015F3FC6EA688832B3C8CC86D884EBA1924E8
  dat3<- read_csv(dl)
  rm(dl)
  
  dat <- bind_rows(dat1,dat2,dat3)
  write_csv(dat,'Nasa_JPL_SmallBody.csv')
  rm(dat1,dat2,dat3,dat)
}

if(file.exists("JPLdesc.csv") != TRUE){ 
  download.file("https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/JPLdesc.csv",
                "JPLdesc.csv")}

if(file.exists("one.jpg") != TRUE){ 
  download.file("https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/one.jpg",
                "one.jpg")}

if(file.exists("two.jpg") != TRUE){ 
  download.file("https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/two.jpg",
                "two.jpg")}

###################################################################################################

###################################################################################################
# Please NOTE:
# THIS CODE CHUNK COMBINES THE FILES REQUIRED FOR THIS RMD

GIT_LINK = "https://raw.githubusercontent.com/VeneetBhardwaj/R/main/JPLSBA/"

#PLEASE DOWNLOAD ALL THE FILES FROM THE FOLDER ABOVE TO THE CURRENT FOLDER.
# DOWNLOADED FILES ARE ~ 223MB

# ALL OTHER FILES ARE REQUIRED FOR THE REMAINING CODE EXECUTION.
# approximately ~560MB + 233 MB (downloaded) OF DISK SPACE IS REQUIRED.
# NO FILES WILL BE DELETED. ONCE THIS CHUNK HAS COMPLETED 
# THE JPL_1.zip,JPL_2.zip,JPL_3.zip CAN BE REMOVED. 

if(file.exists("JPL_1.zip") != TRUE) cat("\n Please Download JPL_1.zip \n")
if(file.exists("JPL_2.zip") != TRUE) cat("\n Please Download JPL_2.zip \n")
if(file.exists("JPL_3.zip") != TRUE) cat("\n Please Download JPL_3.zip \n")
if(file.exists("JPLdesc.csv") != TRUE) cat("\n Please Download JPLdesc.csv \n") 
if(file.exists("one.jpg") != TRUE) cat("\n Please Download one.jpg \n") 
if(file.exists("two.jpg") != TRUE) cat("\n Please Download two.jpg \n") 

if(file.exists("Nasa_JPL_SmallBody.csv") != TRUE){ 
  #unzip the required files
  if(file.exists("JPL_1.zip") == TRUE & 
     file.exists("JPL_2.zip") == TRUE &
     file.exists("JPL_3.zip") == TRUE ){ 
    
    #JPL_1.zip
    #sha256sum should be F156FD5A7AF7202145D7B21F1B40BA62626F8192D46CAC290A0ABC9CD2A8DBEE
    dat1<- read_csv("JPL_1.zip")
    
    #JPL_2.zip
    #sha256sum should be 2603A25A4C44973FB0C7C0A607980C8D992538AF55A247DAE815483C708CBDB8
    dat2<- read_csv("JPL_2.zip")
    
    #JPL_3.zip
    #sha256sum should be ECDE408DD9D89396F22F2D9AAB2015F3FC6EA688832B3C8CC86D884EBA1924E8
    dat3<- read_csv("JPL_3.zip")
    
    dat <- bind_rows(dat1,dat2,dat3)
    write_csv(dat,'Nasa_JPL_SmallBody.csv')
    rm(dat1,dat2,dat3,dat,GIT_LINK)
  }
}
###################################################################################################

# Please NOTE:
# Some parts of the following code chunks are configured to run on a 6+ core computer.
# Please reconfigure num.threads and the n.cores parameters of the ranger and gbm models accordingly.

###################################################################################################
data <- data.table::fread("Nasa_JPL_SmallBody.csv", na.strings = c(NA,"NA",""))
dim(data)
names(data) 

#load the descriptions csv.
data$id <- str_trim(data$id)
desc_d <- as.data.frame(data.table::fread("Nasa_JPL_SmallBody_description.csv"))
desc_d[,2:4] 

## Outcome Variable

#check for missing values
cat("pha missing values: ", sum(is.na(data$pha)),
    "\nneo missing values : ", sum(is.na(data$neo)))

#This is the outcome variable
data$pha <- as.character(data$pha)
data$neo <- as.character(data$neo)
#remove the missing values.
data <- data[complete.cases(data$neo),]
data <- data[complete.cases(data$pha),]
#convert character to numeric, 
data$pha <- ifelse(data$pha == "Y",1,0)
data$neo <- ifelse(data$neo == "Y",1,0)
#define the classes
data <- data %>%
  mutate(neo_class = as.factor(ifelse(pha == 1,"PHO",
                                      ifelse(pha == 0 & neo == 1,"NEO", 
                                             ifelse(pha == 0 & pha == 0,"SO","Error")))))
describe(data$neo_class)

## Missing data


#list features with less than 100% data
m_chk <- tibble()
data_f <- data.frame(data)
for(iter in 1:(ncol(data_f))){
  x <- sum(is.na(data_f[,iter]))
  cname <- as.character(colnames(data_f[iter]))
  uni <- as.character(n_distinct(data_f[iter]))
  perc <- round((((dim(data)[1]-x)/(dim(data_f)[1]))*100),1)
  if (x > 0) {
    m_chk <- bind_rows(m_chk,data_frame(Variable=cname,
                                        Distinct=uni,
                                        Missing=as.character(x),
                                        percentage=str_c(perc,"%")))}
  
}
m_chk 


rm(m_chk,x,uni,perc,iter,cname,data_f)

dim(data)

#descriptive and model variables dataset separated
model_variables <- data %>%  select(id,class,neo_class,H,e,t_jup,
                                    a,q,ad,moid,moid_jup,i,om,w,per,n,ma,rms)

descriptive_data_full <- data %>% select(setdiff(names(data),names(model_variables)))
descriptive_data_full <- bind_cols(id=data$id,moid=data$moid,descriptive_data_full)
#data.table::fwrite(descriptive_data,'descriptive_data_full.csv')
descriptive_data <- descriptive_data_full %>%
  select(id,full_name,producer,neo,pha,per_y,moid,first_obs,last_obs)
data.table::fwrite(descriptive_data,'descriptive_data.csv')

#use only the complete cases. drop the remaining.
model_variables <- model_variables[complete.cases(model_variables),]




# description of the features to be used in the model
x <- ifelse(c(desc_d$column_name) %in% c(names(model_variables)),1,0)
m_chk <- tibble()
for(iter in 1:59){  if (x[iter]==1) {
  m_chk <- bind_rows(m_chk,data_frame(Variable=desc_d[iter,2], 
                                      Description=desc_d[iter,3],
                                      Units=desc_d[iter,4]))}}
m_chk 

rm(iter,m_chk,x)
dim(model_variables)



### T-tests
Tdata <- model_variables %>%  
  mutate(i = i*pi/180, om = om*pi/180, w = w*pi/180, ma = ma*pi/180) %>%
  select(neo_class,H,e,t_jup,a,q,ad,moid,moid_jup,i,om,w,per,n,ma,rms)
Tdata <- as.data.frame(Tdata)
describe(Tdata$neo_class)
pha_y <- Tdata[Tdata$neo_class =="PHO", ]
pha_n <- Tdata[Tdata$neo_class =="NEO", ]

ttest_results <- tibble(Parameter="", Mean_PH="", Mean_NEO="", Mean_differnce="", pvalue="", stderr="")
t_test <- for(i in 2:(ncol(pha_y)-1)){
  x <- t.test(pha_y[,i], pha_n[,i],var.equal = FALSE, alternative="two.sided",conf.level = 0.99)
  #if(x$p.value > 0.01){
  ttest_results <- bind_rows(ttest_results,
                             data_frame(Parameter=colnames(pha_y[i]), 
                                        Mean_PH= as.character(round(x$estimate[1],2)), 
                                        Mean_NEO=as.character(round(x$estimate[2],2)),
                                        Mean_differnce=
                                          as.character(round(x$estimate[2] - x$estimate[1], 4)),
                                        pvalue=as.character(round(x$p.value,2)),
                                        stderr=as.character(round(x$stderr,2))))
}

print(ttest_results)


neo_y <- Tdata[Tdata$neo_class =="NEO",]
neo_n <- Tdata[Tdata$neo_class =="SO",]
neo_y <- na.omit(neo_y)
neo_n <- na.omit(neo_n)
ttest_results <- tibble(Parameter="", Mean_NEO="", Mean_SO="", Mean_differnce="", pvalue="", stderr="")
t_test <- for(i in 2:(ncol(neo_y)-1)){
  x <- t.test(neo_y[,i], neo_n[,i],var.equal = FALSE, alternative="two.sided",conf.level = 0.99)
  #if(x$p.value > 0.01){
  ttest_results <- bind_rows(ttest_results,
                             data_frame(Parameter=colnames(neo_y[i]), 
                                        Mean_NEO= as.character(round(x$estimate[1],4)), 
                                        Mean_SO=as.character(round(x$estimate[2],4)),
                                        Mean_differnce=
                                          as.character(round(x$estimate[2] - x$estimate[1],4)),
                                        pvalue=as.character(round(x$p.value,2)),
                                        stderr=as.character(round(x$stderr,2))))
}

print(ttest_results)

rm(descriptive_data,data,descriptive_data_full, pha_y,pha_n, neo_n, neo_y,ttest_results, 
   i, t_test,Tdata)

## Validation and Model datasets

# Validation set will be 10% of model_variables data
set.seed(131825431, sample.kind = "Rounding")

validation_index <- createDataPartition(model_variables$neo_class,
                                        times = 1, p = 0.1, list = FALSE)

JPL_data <- model_variables[-validation_index,]
JPL_validation <- model_variables[validation_index,]


dim(JPL_data)
dim(JPL_validation)

## Variable Summary



### id, class, neo_class

JPL_SmallBody_data <- data.frame(JPL_data)
summary(JPL_SmallBody_data[1:3])

#The table below shows the distribution of the Outcome Variable by each orbital class.
tab <- table(JPL_SmallBody_data[2:3])
tab 

#A visual representation of the differrent class by the neo_class.

tab %>% as.data.frame() %>%
  ggplot(aes(class,Freq)) +
  geom_bar(stat="identity", fill = 9, alpha=0.5) +
  facet_wrap(~neo_class, ncol=3,scales = "free") +
  theme(axis.text.x = element_text(angle=90,size=8, hjust=1),
        axis.text.y = element_text(size=8, lineheight = 1.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")
rm(tab)

### H, e, t_jup, a
k <- 24576
summary(JPL_SmallBody_data[4:7])

par(mfrow=c(1,4))
for(iter in 4:7) {
  hist(JPL_SmallBody_data[,iter], main=names(JPL_SmallBody_data)[iter],xlab='', ylab='')}

par(mfrow=c(1,4))
X <- JPL_SmallBody_data[,3:7]
for(iter in 2:5) {dat <- list(PHO = X[X$neo_class == "PHO",iter],
                              NEO = X[X$neo_class == "NEO",iter],SO = X[X$neo_class == "SO",iter])
boxplot(dat,xlab="neo_class", cex=0, main=str_c(names(X)[iter], ""))}
rm(X,dat,iter)

par(mfrow=c(1,4))
X <- JPL_SmallBody_data[1:k,3:7]
for(iter in 2:5) { dat <- list(PHO = X[X$neo_class == "PHO",iter],
                               NEO = X[X$neo_class == "NEO",iter],SO = X[X$neo_class == "SO",iter])
stripchart(dat,vertical=TRUE,method="jitter", pch=16, col=1, main=str_c(names(X)[iter], ""))}
rm(X,dat,iter)


### q, ad, moid, moid_jup

summary(JPL_SmallBody_data[8:11])

par(mfrow=c(1,4))
for(iter in 8:11) { hist(log(JPL_SmallBody_data[,iter]),
                         main=str_c(names(JPL_SmallBody_data)[iter],"(log)"),xlab='', ylab='')}

par(mfrow=c(1,4))
X <- JPL_SmallBody_data %>% select(neo_class,q,ad,moid,moid_jup)
for(iter in 2:5) {  dat <- list(PHO = X[X$neo_class == "PHO",iter],
                                NEO = X[X$neo_class == "NEO",iter],SO = X[X$neo_class == "SO",iter])
boxplot(dat,xlab="neo_class",log='y',cex=0,  main=str_c(names(X)[iter], " (log)"))}

rm(X,dat,iter)

par(mfrow=c(1,4))
X <- JPL_SmallBody_data %>% select(neo_class,q,ad,moid,moid_jup)
X <- X[1:k,]
for(iter in 2:5) {  dat <- list(PHO = X[X$neo_class == "PHO",iter],
                                NEO = X[X$neo_class == "NEO",iter],SO = X[X$neo_class == "SO",iter])
stripchart(dat,vertical=TRUE,method="jitter", pch=16, col=1, main=str_c(names(X)[iter], ""))}

rm(X,dat,iter)


### i, om, w

summary(JPL_SmallBody_data[12:14]*pi/180)

par(mfrow=c(3,1))
for(iter in 12:14) { 
  hist((JPL_SmallBody_data[,iter]*pi/180),
       main=str_c(names(JPL_SmallBody_data)[iter]," (radians)"), xlab='', ylab='')}

par(mfrow=c(1,3))
qqnorm(log2(JPL_SmallBody_data[1:k,12]*pi/180), 
       main=str_c(names(JPL_SmallBody_data)[12]," (log)"))
qqline(log2(JPL_SmallBody_data[1:k,12]*pi/180))
for(iter in 13:14) {
  qqnorm((JPL_SmallBody_data[1:k,iter]*pi/180), main=names(JPL_SmallBody_data)[iter])
  qqline((JPL_SmallBody_data[1:k,iter]*pi/180))}
rm(iter)


### per, n, ma, rms

summary(JPL_SmallBody_data[15:18])

#Outliers for rms
JPL_SmallBody_data[JPL_SmallBody_data$rms > 100, 18]

#Outliers for per
table((JPL_SmallBody_data[JPL_SmallBody_data$per > (1000 *365), 3]))

X <- (JPL_SmallBody_data[JPL_SmallBody_data$per > (1000 *365) & 
                           JPL_SmallBody_data$neo_class == "NEO",1])
descriptive_data <- as.data.frame(read_csv('descriptive_data.csv'))
descriptive_data %>% filter(id==X) 

JPL_data[JPL_data$id == X, 1:11]  
rm(X,descriptive_data)

par(mfrow=c(1,4))
for(iter in 15:18) {
  hist(log(JPL_SmallBody_data[,iter]), main=names(JPL_SmallBody_data)[iter],xlab='', ylab='')}

par(mfrow=c(1,4))
X <- JPL_SmallBody_data %>% filter(rms <= 100, per <= 1000*365) %>%
  select(neo_class,per, n, ma, rms)
for(iter in 2:5) { 
  dat <- list(PHO = X[X$neo_class == "PHO",iter],
              NEO = X[X$neo_class == "NEO",iter],SO = X[X$neo_class == "SO",iter])
  boxplot(dat,xlab="neo_class",log='y',cex=0,  main=str_c(names(X)[iter]," (log)"))}

rm(X,dat,iter)

par(mfrow=c(1,4))
X <- JPL_SmallBody_data %>% select(neo_class,per, n, ma, rms)
X <- X[1:k,]
for(iter in 2:5) {
  dat <- list(PHO = X[X$neo_class == "PHO",iter],
              NEO = X[X$neo_class == "NEO",iter],SO = X[X$neo_class == "SO",iter])
  stripchart(dat,vertical=TRUE,method="jitter", pch=16, col=1, main=str_c(names(X)[iter], ""))}
rm(X,dat,iter,k)


## Variable Means

pp <- JPL_SmallBody_data[,3:18]
m_comp <- data_frame()
for(iter in 2:15){
  Mean_PHO <- round(mean(pp[pp$neo_class=="PHO",iter]),2)
  Mean_NEO <- round(mean(pp[pp$neo_class=="NEO",iter]),2)
  Mean_SO <- round(mean(pp[pp$neo_class=="SO",iter]),2)
  m_comp <- bind_rows(m_comp, 
                      data_frame(Variable=colnames(pp[iter]),
                                 PHO = Mean_PHO,NEO = Mean_NEO,SO = Mean_SO))}
m_comp  

rm(pp,m_comp,iter,Mean_PHO,Mean_NEO,Mean_SO)


## Distance

x <- JPL_SmallBody_data[,3:18]

#extract and sweep different categories
ph <- x[x$neo_class =="PHO",-1] %>% as.matrix() %>% sweep(.,2, colMeans(.))
ne <- x[x$neo_class =="NEO",-1] %>% as.matrix() %>% sweep(.,2, colMeans(.))
so <- x[x$neo_class =="SO",-1] %>% as.matrix() %>% sweep(.,2, colMeans(.))

#sweep the data matrix
x <-  as.matrix(x[,-1])
x <- sweep(x,2, colMeans(x))

k <- 42
#hclusters based on data and the transpose
ph_1 <- hclust(dist(ph[1:k,]))
ph_2 <- hclust(dist(t(ph[1:k,])))
ne_1 <- hclust(dist(ne[1:k,]))
ne_2 <- hclust(dist(t(ne[1:k,])))
so_1 <- hclust(dist(so[1:k,]))
so_2 <- hclust(dist(t(so[1:k,])))

#image of the hclust
par(mfrow=c(1,3))
image(x[ph_1$order, ph_2$order],
      col=RColorBrewer::brewer.pal(11,'RdBu'), main = "Potential Hazard")

image(x[ne_1$order, ne_2$order],
      col=RColorBrewer::brewer.pal(11,'RdBu'), main = "Near Earth")

image(x[so_1$order, so_2$order],
      col=RColorBrewer::brewer.pal(11,'RdBu'), main = "Solar Object")

rm(ph_1,ph_2,ne_1,ne_2,so_1,so_2,ph,ne,so,x,k,JPL_SmallBody_data)
gc()


# Train & Test sets

set.seed(131825431, sample.kind = "Rounding")
# test set will be 10% of training set.
test_index <- createDataPartition(JPL_data$neo_class, times = 1, p = 0.1, list = FALSE)
JPL_train <- JPL_data[-test_index,]
JPL_test <- JPL_data[-test_index,]
rm(test_index)

## Process Data

#The variables are modified as mentioned above for the datasets to be used for training and testing
#This data processing will also be done for the validation set.

data_process <- function(preData, tChoice){
  #convert to radians and add additional derived variables
  preData <- preData %>%
    mutate(om=om*(pi/180), w=w*(pi/180), i=as.numeric(i*(pi/180)),
           t_to_p = as.numeric((per - (ma/n))),
           m_d = as.numeric(moid_jup-moid))
  preData <- preData %>% mutate(T_p = as.numeric(round(1-(t_to_p/per),4)))
  preData <- preData %>% select(neo_class,H,q,moid,m_d,e,t_jup,a,i,T_p)
  preData$neo_class <- as.factor(preData$neo_class)
  
  #return as matrix format
  if(tChoice == 1){
    preData <- data.frame(preData)
    x_matrix <- as.matrix(preData[,-1])
    y_matrix <- factor(preData[,1])
    processData <- list(X = x_matrix, Y = y_matrix)}
  #return as data table
  if(tChoice == 2) processData <- preData
  return (processData)
}

## Inaccuracy Details function

descriptive_data <- data.table::fread("descriptive_data.csv", na.strings = c(NA,"NA",""))
descriptive_data <- as.data.frame(descriptive_data)

#function to collate inaccurately classified observations.
inaccurate_details <- tibble()
inaccuracy_check <- function(incorrect_p,method,dat){
  idetails <- data.frame()
  for(each in incorrect_p){ 
    a <- (dat[each,1])
    idetails <- rbind(idetails,data.frame(model_name = method,
                                          descriptive_data[descriptive_data$id==str_trim(a),]))}
  return(idetails)}

### Evaluation Function

#In this case study the confusionmatrix function of the caret package is used. 
#The following function takes the confusion matrix and adds the Recall and the F1 scores to a tibble for the results.

#function to collate the results
results_process<- function(x,name){
  x1 <- data_frame(model_name=name,Object="NEO",
                   Recall=round(x[1,6],3),F1=round(x[1,7],3))
  x2 <- data_frame(model_name=name,Object="PHO",
                   Recall=round(x[2,6],3),F1=round(x[2,7],3))
  x3 <- data_frame(model_name=name,Object="SO",
                   Recall=round(x[3,6],3),F1=round(x[3,7],3))
  return(bind_rows(x1,x2,x3))
}

## PCA

mat_train <- data_process(JPL_train,1)
mat_test <- data_process(JPL_test,1)

set.seed(131825431, sample.kind = "Rounding")
pca <- prcomp(mat_train$X, center = TRUE, scale. = TRUE)
summary(pca)

par(mfrow=c(1,1))
screeplot(pca,type = "line", main="PCA")

#Correlation Plot shows the correlation between the variables.
par(mfrow=c(1,2))
corrplot(cor(mat_train$X), method="circle", type="lower")
corrplot(cor(pca$x), method="circle", type="upper")

#Plot of the the first 4 components of the PCA together even on a random sample

# random sample of 24,576 observations used.

set.seed(131825431, sample.kind = "Rounding")
par(mfrow=c(1,1))
p1 <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], label= mat_train$Y) %>%
  sample_n(24576) %>%  ggplot(aes(PC1,PC2, fill = label)) + 
  geom_point(cex=3,pch=21,alpha=0.7) + theme(legend.position = "right")
p2 <- data.frame(PC3 = pca$x[,3], PC4 = pca$x[,4],label= mat_train$Y) %>%
  sample_n(24576) %>% ggplot(aes(PC3,PC4, fill = label)) + 
  geom_point(cex=3,pch=21,alpha=0.7) + theme(legend.position = "hide")
gridExtra::grid.arrange(p1,p2, ncol=2, nrow=1)

rm(p1,p2)
gc()


### Multinom

#Model Fitting 

#first 7 components
k <- 7
train_pca <- data.frame(pca$x[,1:k], neo_class= as.factor(mat_train$Y))
test_pca <- as.matrix(sweep(mat_test$X,2, colMeans(mat_test$X))) %*% pca$rotation
test_pca <- data.frame(test_pca[,1:k], neo_class= as.factor(mat_test$Y))
set.seed(131825431, sample.kind = "Rounding")
fit_pca_multinom <- multinom(neo_class ~ ., data = train_pca, sumn=1)

#Model Prediction and the confusion matrix: 
y_hat_pca_multinom <- predict(fit_pca_multinom,test_pca, type = "class")
cm <- confusionMatrix(y_hat_pca_multinom, test_pca$neo_class)
results <- results_process(as.data.frame(cm$byClass),"PCA Multinom")
cm$table
cat("Accuracy : ", cm$overall["Accuracy"], "\n")
results %>% filter (model_name == "PCA Multinom")  
rm(pca,fit_pca_multinom,k,test_pca,train_pca,y_hat_pca_multinom,cm)
gc()

## Naive Bayes

#Model Fitting, Prediction and the confusion matrix:

set.seed(131825431, sample.kind = "Rounding")
fit_nb <- naive_bayes(mat_train$X,mat_train$Y, usekernel=FALSE, usepoisson = TRUE)
y_hat_nb <- predict(fit_nb, mat_test$X, type = "class")
cm <- confusionMatrix(y_hat_nb, mat_test$Y)
results <-  bind_rows(results,results_process(as.data.frame(cm$byClass),"Naive Bayes"))
cm$table

results %>% filter (model_name == "Naive Bayes")  
rm(fit_nb,y_hat_nb,cm)
gc()

## Ensemble

#Model Fitting :

set.seed(131825431, sample.kind = "Rounding")
ensemble_train <- data_process(JPL_train[1:16348,],1)
ensemble_test <- data_process(JPL_test[1:1638,],1)
results_ensemble <- data.frame()

models <- c("lda", "qda", "knn", "rpart", "svmRadialCost", "wsrf")

fits <- lapply(models, function(model){
  cat(":- ",model, "\n")
  if (model %in% c("lda","qda")){
    train(ensemble_train$X,ensemble_train$Y, method=model, prior = c(1,1,1)/3)
  }
  else train(ensemble_train$X,ensemble_train$Y, method=model,
             trControl = trainControl(method = "cv", number=5))
}) 

#Prediction and the confusion matrix :  
names(fits) <- models
pred <- sapply(fits,function(object){  predict(object, ensemble_test$X) })

for (each in 1:ncol(pred)){
  cm <- confusionMatrix(factor(pred[,each]), ensemble_test$Y)
  results_ensemble <- bind_rows(results_ensemble,
                                results_process(as.data.frame(cm$byClass),colnames(pred)[each]))
}

#Ensemble Results:
results_ensemble %>% filter (model_name == "lda")  
results_ensemble %>% filter (model_name == "qda")  
results_ensemble %>% filter (model_name == "knn")  
results_ensemble %>% filter (model_name == "rpart")  
results_ensemble %>% filter (model_name == "svmRadialCost")  
results_ensemble %>% filter (model_name == "wsrf")  
rm(ensemble_test,ensemble_train,fits,models,pred,results_ensemble,cm,each)
gc()





## rpart


#Complexity Parameter :
set.seed(131825431, sample.kind = "Rounding")
train_rpart <- train(mat_train$X,mat_train$Y, method="rpart",
                     tuneGrid = data.frame(cp=seq(0.0,0.05, len=25)),
                     trControl = trainControl(method = "cv", number=5))
rpart_cp <- train_rpart$bestTune
rpart_cp

#Variable Importance :
train_rpart$finalModel$variable.importance

# Decision tree chart
plot(train_rpart$finalModel, margin=0.1)
text(train_rpart$finalModel, cex=0.6)


#Model Fitting, Prediction and the confusion matrix:
rtrain <- data_process(JPL_train,2)
rtest <- data_process(JPL_test,2)
set.seed(131825431, sample.kind = "Rounding")
fit_rpart <- rpart(neo_class ~. , data=rtrain, parms = list(split = "gini"),
                   control = rpart.control(cp = rpart_cp))
predcit_rpart <- predict(fit_rpart, rtest)
labels <- colnames(predcit_rpart)[apply(predcit_rpart, 1, which.max)]
cm <- confusionMatrix(factor(labels), rtest$neo_class)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"rpart"))
cm$table

results %>% filter (model_name == "rpart")  

inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  labels != rtest$neo_class ),"rpart",JPL_test))

rm(train_rpart,rtrain,rtest,fit_rpart,predcit_rpart,labels,cm,incorrect_p)
gc()


### Random Forest



#Tuning Function :

gettuned <- function(method_name,grid,dat){
  set.seed(131825431, sample.kind = "Rounding")
  control <- trainControl(method='cv', number = 5)
  train(x = dat$X[2048:8192,], y = dat$Y[2048:8192], nTree=50,
        method=method_name, tuneGrid = grid, trControl = control)$bestTune}


#mtry      :   Number of variables randomly sampled as candidates at each split.
#strata    :   A (factor) variable that is used for stratified sampling.
#sampsize  :   Size of sample to draw.
#nodesize	:   Minimum size of terminal nodes. For classification it is 1.
#nTree     :   Number of trees to grow.

rf_mtry <- gettuned("rf",data.frame(mtry=c(2,3,4,5,6)),mat_train)

#Model Fitting : 
set.seed(131825431, sample.kind = "Rounding")
rf_train <- data_process(JPL_train,2)
rf_test <- data_process(JPL_test,2)
fit_rf <- randomForest(neo_class ~ .,data=rf_train, mtry=rf_mtry$mtry,
                       nodesize = 1, strata = neo_class, nTree=rf_mtry$mtry*50,
                       sampsize = 100000, replace=FALSE)

#Variable Importance of the last measure:

varImpPlot(fit_rf)

#Prediction and the confusion matrix: 

y_hat_rf <- predict(fit_rf,rf_test)
cm <- confusionMatrix(y_hat_rf, rf_test$neo_class)
results <-  bind_rows(results,results_process(as.data.frame(cm$byClass),"Random Forest"))
cm$table

#Results :
results %>% filter (model_name == "Random Forest")  

incorrect_p <- which(y_hat_rf != rf_test$neo_class )
inaccurate_details <- rbind(inaccurate_details,inaccuracy_check(
  incorrect_p,'Random Forest',JPL_test))
rm(fit_rf,rf_test,rf_train,y_hat_rf,cm)
gc()



### WSRF

#mtry        :   number of trial predictors for a split (mtry).
#nTree       :   the number of trees to train.
#minNode     :   minimum number of distinct row references to split a node.
#weights     :   TRUE for weighted subspace selection.
#importance	:   Importance of predictors.

wsrf_mtry <- gettuned("wsrf",data.frame(mtry=c(2,3,4,5,6)),mat_train)

#Model Fitting :
fit_wsrf <- wsrf(x = mat_train$X, y = mat_train$Y,
                 ntree = wsrf_mtry$mtry*100, mtry=wsrf_mtry$mtry+1,
                 weights=TRUE,  importance = TRUE, parallel = TRUE)

#Variable Importance:

fit_wsrf$importance

#Prediction and the confusion matrix: 
y_hat_wsrf <- predict(fit_wsrf, mat_test$X)
y_hat_wsrf_m <- as.matrix(y_hat_wsrf$class)
y_hat_wsrf_m <- ifelse(y_hat_wsrf_m == 1, 'NEO',ifelse(y_hat_wsrf_m == 2, 'PHO','SO'))
cm <- confusionMatrix(as.factor(y_hat_wsrf_m),mat_test$Y)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"WSRF"))
cm$table

#Inaccurate Predictions :
inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  mat_test$Y != y_hat_wsrf_m),"WSRF",JPL_test))
inaccurate_details %>% filter (model_name == "WSRF") %>% .[,3:10] %>% tibble() 

#Results : 
results %>% filter (model_name == "WSRF")  

rm(y_hat_wsrf,y_hat_wsrf_m,cm)
gc()


### Ranger
#classification is set to TRUE
#min.node.size is set to 1.
#importance      :   Variable importance mode.
#splitrule       :   For classification the default "gini".
#regularization.usedepth   :   The depth in regularization.
#num.threads     :   Number of threads / number of CPUs.
#class.weights   :   Weights for the outcome classes in the splitting rule

#Model Fitting : 
set.seed(131825431, sample.kind = "Rounding")
fit_ranger <- ranger(x = mat_train$X, y = mat_train$Y, 
                     classification = TRUE, 
                     min.node.size = 1,
                     mtry=rf_mtry$mtry+1,
                     num.trees = rf_mtry$mtry*50, 
                     splitrule = 'gini',
                     importance = 'impurity', 
                     regularization.usedepth = TRUE,
                     # additional weight of ~ 40:60:0  ~ 2:3:0 ratio from prime numbers. 
                     class.weights = c(7,11,0), 
                     num.threads = 6, verbose = TRUE)

#Variable Importance :
fit_ranger$variable.importance

#Prediction and the confusion matrix : 
y_hat_ranger <- predict(fit_ranger, mat_test$X)

cm <- confusionMatrix(y_hat_ranger$predictions, mat_test$Y)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Ranger"))
cm$table
cm$byClass

#Results : 

results %>% filter (model_name == "Ranger")  

rm(y_hat_ranger,cm)
gc()





## Gradient Boosting
#n.trees             :   Integer specifying the total number of trees to fit.
#class.stratify.cv   :   Logical indicating whether or not the cross-validation should be stratified by class
#bag.fraction        :   The fraction of the training set observations randomly selected
#train.fraction      :   The first train.fraction * nrows(data) observations are used to fit the gbm and 
                        #the remainder are used for computing out-of-sample estimates of the loss function.
#shrinkage :   A shrinkage parameter applied to each tree in the expansion.
#keep.data :   a logical variable indicating whether to keep the data and an index of the data stored with the object.
#interaction.depth	  :   Integer specifying the maximum depth of each tree 
#n.cores             :   The number of CPU cores to use


#Fitting the Model :
gbm_train <- data_process(JPL_train,2)
gbm_test <- data_process(JPL_test,2)
set.seed(131825431, sample.kind = "Rounding")
fit_gbm <- gbm(neo_class~., data = gbm_train, n.trees = 50,
               distribution = "multinomial", class.stratify.cv =TRUE,
               bag.fraction = 0.3, train.fraction = 0.7, shrinkage = 0.1, 
               interaction.depth = 3, keep.data = FALSE, verbose = TRUE, n.cores = 6)

#Visual representation of the model:
par(mfrow=c(1,1))
best.iter <- gbm.perf(fit_gbm, plot.it = TRUE)
plot(fit_gbm)
plot(fit_gbm, i.var = 1:2, n.trees = best.iter)


#Prediction and the confusion matrix:
y_hat_gbm <- predict(fit_gbm, gbm_test, n.trees = best.iter, type='response')
labels <- colnames(y_hat_gbm)[apply(y_hat_gbm, 1, which.max)]
cm <- confusionMatrix(factor(labels), gbm_test$neo_class)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"gbm"))
cm$table

#Results:
results %>% filter (model_name == "gbm")  

inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  labels != gbm_test$neo_class),'gbm',JPL_test))
rm(gbm_train,gbm_test,p1,p2,y_hat_gbm,cm,labels)

rm(mat_test,mat_train,JPL_test, JPL_train)
gc()



## Collated Results

### Recall

F1_score <- results %>% select(model_name, F1, Object) %>% spread(.,Object,F1)
Recall_score <- results %>% select(model_name, Object, Recall) %>% spread(.,Object,Recall)
knitr::kable(Recall_score)
#chart
par(mfrow=c(1,1))
in_m <- c( "rpart","Random Forest","WSRF","gbm","Ranger")
results %>% filter(model_name %in% in_m) %>% filter(Object != "SO") %>%
  ggplot(aes(reorder(model_name,Recall), Recall)) + geom_point(pch=19, cex=3, alpha=0.7)  +
  facet_wrap(~Object, ncol=3,scales = "fixed") +
  theme(axis.text.x = element_text(angle=90,size=8, lineheight = 1.2, hjust=1),
        axis.text.y = element_text(size=8, lineheight = 1.2),
        axis.title.x = element_blank(),
        legend.position = "bottom")

### F1
knitr::kable(F1_score)
#chart
par(mfrow=c(1,1))
in_m <- c( "rpart","Random Forest","WSRF","gbm","Ranger")
results %>% filter(model_name %in% in_m) %>% filter(Object != "SO") %>%
  ggplot(aes(reorder(model_name,F1), F1)) + geom_point(pch=19, cex=3, alpha=0.7)  +
  facet_wrap(~Object, ncol=3,scales = "fixed") +
  theme(axis.text.x = element_text(angle=90,size=8, lineheight = 1.2, hjust=1),
        axis.text.y = element_text(size=8, lineheight = 1.2),
        axis.title.x = element_blank(),
        legend.position = "bottom")

### F1 vs Recall

par(mfrow=c(1,1))
results %>% filter(model_name != "PCA Multinom") %>% 
  ggplot(aes(F1,Recall, col=Object)) + geom_point(pch=19, cex=3)  +
  facet_wrap(~model_name, nrow=2, scales = "free") +
  theme(axis.text.x = element_text(angle=90,size=8, lineheight = 1.2, hjust=1),
        axis.text.y = element_text(size=8, lineheight = 1.2),legend.position = "bottom")
rm(F1_score,Recall_score)





# Final Models

## Validation

### Ranger

#Validation set matrix:
#process as matrix for the model
validation <- data_process(JPL_validation,1)
JPL_F <- data_process(JPL_data,1)

#Model Fitting, Prediction and the confusion matrix : 
set.seed(131825431, sample.kind = "Rounding")
#fit the model for validation
final_ranger <- ranger(x=JPL_F$X, y=JPL_F$Y,
                       classification = TRUE, min.node.size = 1,
                       num.trees = rf_mtry$mtry*50, mtry=rf_mtry$mtry+1, 
                       splitrule = 'gini', importance = 'impurity', 
                       num.threads = 6, regularization.usedepth = TRUE, 
                       verbose = FALSE, class.weights = c(7,11,0))

#predict and the confusion matrix
pred_ranger <- predict(final_ranger, validation$X)
cm <- confusionMatrix(pred_ranger$predictions, validation$Y)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Validation : Ranger"))
cm$table

#Results : 

results %>% filter (model_name =="Validation : Ranger")  

#Inaccurate Predictions :
inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  pred_ranger$predictions != validation$Y),'Validation : Ranger',JPL_validation))
inaccurate_details %>% filter (model_name == "Validation : Ranger") %>% .[,3:10] %>%
  tibble() 


###   WSRF

#Model Fitting, Prediction and the confusion matrix : 
set.seed(131825431, sample.kind = "Rounding")
#fit the model for validation
final_wsrf <- wsrf(x=JPL_F$X, y=JPL_F$Y,
                   ntree = wsrf_mtry$mtry*100, mtry=wsrf_mtry$mtry+1,
                   weights=TRUE,  importance = TRUE, parallel = TRUE)
#predict and the confusion matrix
pred_wsrf <- predict(final_wsrf, validation$X)
pred_wsrf_m <- as.matrix(pred_wsrf$class)
pred_wsrf_m <- ifelse(pred_wsrf_m == 1, 'NEO',ifelse(pred_wsrf_m == 2, 'PHO','SO'))

cm <- confusionMatrix(as.factor(pred_wsrf_m),validation$Y)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Validation : WSRF"))
cm$table
#Results : 
results %>% filter (model_name == "Validation : WSRF")  

#Inaccurate Predictions :
inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  validation$Y != pred_wsrf_m),"Validation : WSRF",JPL_validation))
inaccurate_details %>% filter (model_name == "Validation : WSRF") %>% .[,3:10] %>%
  tibble() 

### Rpart

#Model Fitting, Prediction and the confusion matrix : 

#process as data tables for the model
validation <- data_process(JPL_validation,2)
JPL_F <- data_process(JPL_data,2)
#fit model
Final_rpart <- rpart(neo_class ~. , data=JPL_F,
                     control = rpart.control(cp = rpart_cp, minsplit = 10, minbucket=1))
#predict and confusion matrix
pred_rpart <- predict(Final_rpart, validation)
labels <- colnames(pred_rpart)[apply(pred_rpart, 1, which.max)]
cm <- confusionMatrix(factor(labels), validation$neo_class)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Validation : rpart"))
cm$table

#Results :
results %>% filter (model_name == "Validation : rpart")  

#Inaccurate Predictions :

inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  labels != validation$neo_class),"Validation : rpart",JPL_validation))
inaccurate_details %>% filter (model_name == "Validation : rpart") %>% .[,3:10] %>%
  tibble() 



## As Holdout
#Illustration : Using the fit from the train set and treating the validation data as a holdout data.

### WSRF

#Validation set matrix:

#process as matrix for the model
validation <- data_process(JPL_validation,1)
JPL_F <- data_process(JPL_data,1)

#Prediction and the confusion matrix : 

y_f_wsrf <- predict(fit_wsrf, validation$X)
y_f_wsrf_m <- as.matrix(y_f_wsrf$class)
y_f_wsrf_m <- ifelse(y_f_wsrf_m == 1, 'NEO',ifelse(y_f_wsrf_m == 2, 'PHO','SO'))
cm <- confusionMatrix(as.factor(y_f_wsrf_m),validation$Y)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Holdout : WSRF"))
cm$table

#Results : 

results %>% filter (model_name == "Holdout : WSRF")  


#Inaccurate Predictions :

inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  y_f_wsrf_m != validation$Y ),'Holdout : WSRF',JPL_validation))
inaccurate_details %>% filter (model_name == "Holdout : WSRF") %>% .[,3:10] %>%
  tibble() 

### Ranger

#Prediction and the confusion matrix : 

set.seed(131825431, sample.kind = "Rounding")
#predict using the training fitted model against the validation set.
pred_O_ranger <- predict(fit_ranger, validation$X)
cm <- confusionMatrix(pred_O_ranger$predictions, validation$Y)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Holdout : Ranger"))
cm$table


#Results : 

results %>% filter (model_name =="Holdout : Ranger")  


#Inaccurate Predictions :

inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  pred_O_ranger$predictions != validation$Y ),'Holdout : Ranger',JPL_validation))
inaccurate_details %>% filter (model_name == "Holdout : Ranger") %>% .[,3:10] %>%
  tibble() 

### Gradient Boost

#Validation set data table:

#process as matrix for the model
validation <- data_process(JPL_validation,2)

#Prediction and the confusion matrix:

y_f_gbm <- predict(fit_gbm, validation, n.trees = best.iter, type='response')
labels <- colnames(y_f_gbm)[apply(y_f_gbm, 1, which.max)]
cm <- confusionMatrix(factor(labels), validation$neo_class)
results <- bind_rows(results,results_process(as.data.frame(cm$byClass),"Holdout : gbm"))
cm$table


#Results : 

results %>% filter (model_name == "Holdout : gbm")  


#Inaccurate Predictions :

inaccurate_details <- rbind(inaccurate_details, inaccuracy_check(which(
  labels != validation$neo_class),'Holdout : gbm',JPL_validation))
inaccurate_details %>% filter (model_name == "Holdout : gbm") %>% .[,3:10] %>%
  tibble() 

rm(.Random.seed,inaccuracy_check,JPL_F,labels,pred_ranger,validation,cm,descriptive_data,
   Final_rpart,gettuned,inaccurate_details,JPL_data,JPL_validation,pred_O_ranger,pred_rpart,
   results_process,rpart_cp,y_hat_rf,best.iter,data_process,git_gbm,fit_ranger,fit_wsrf,rf_mtry,
   y_f_gbm,y_f_wsrf,y_f_wsrf_m,fit_gbm)
gc()


#The Final Scores:
  
#Recall

vh_models <- c("Validation : Ranger","Validation : WSRF","Validation : rpart",
               "Holdout : gbm","Holdout : Ranger","Holdout : WSRF")
F1_score <- results %>% filter(model_name %in% vh_models) %>%
  select(model_name, F1, Object) %>% spread(.,Object,F1)
Recall_score <- results %>% filter(model_name %in% vh_models) %>%
  select(model_name, Recall, Object) %>% spread(.,Object,Recall)
Recall_score  
F1_score  

par(mfrow=c(1,1))
results %>%  filter(model_name %in% vh_models) %>%
  ggplot(aes(F1,Recall, col=Object)) + geom_point(pch=19, cex=3)  +
  facet_wrap(~model_name, nrow=2, scales = "free") +
  theme(axis.text.x = element_text(angle=90,size=8, lineheight = 1.2, hjust=1),
        axis.text.y = element_text(size=8, lineheight = 1.2),legend.position = "bottom")


#All results
results 
rm(list = ls(all.names=TRUE))
gc()

#veneet.bhardwaj@gmail.com
cat(as.character(now()))