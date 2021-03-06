#TING LAN

#LAB 4


getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- "mydirectory"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

## Load package
library(igraph)

infile<-"Macrae_table5.5_mjbook.csv"
radio<-read.csv("HamRadioOperators_Killworth.csv",header = TRUE,sep=',')
macrae_frame=read.csv(infile, header = TRUE, sep = ",")
coauthor<-read.csv("Coauthorship_GoyalEtAl.csv",header = TRUE,sep=',')
macrae_frame$nop <-macrae_frame$Number.of.prisoners
# This is the empirical cumulative distribution function; but it's not useful for this dataset
# It calculates F(d) for unaggregated data. But the current dataset is already aggregated; so you 
# should calculate F(d) using the cumulative sum instead
#F_d<-ecdf(macrae_frame$nop)
#plot(F_d)
#average degree of mac
a<-macrae_frame$Degree*macrae_frame$nop
nop.sum<-colSums(macrae_frame)[2]
average.degree<-sum(a)/nop.sum
#ecdf(macrae_frame$Degree)

m.mac<-0.5*average.degree
alpha_0<-0.11
# should calculate F(d) using the cumulative sum instead
csum<-cumsum(macrae_frame$nop)
F_d<-csum/nop.sum

y<-log(1-F_d)
#onmit Inf
y[9]<-NA
d<-macrae_frame$Degree
x_1<-(2*alpha_0*m.mac)/(1-alpha_0)
x_1.2<-d+x_1
x<-log(x_1.2)

#Q3- regress y on x
model.mac<-lm(na.omit(y)~x[1:8])
model.mac$coefficients # value of beta=-1.095


#Q4
alpha_0<-0.1
x_1<-(2*alpha_0*m.mac)/(1-alpha_0)
x_1.2<-d+x_1
x<-log(x_1.2)
model.mac2<-lm(na.omit(y)~x[1:8])
beta<-model.mac2$coefficients[2]
alpha_1<-1+2/beta
alpha_1  #-0.895 for alpha1


# Some useful functions, Suggested help look-ups to learn more:
help(cumsum)
help(lm)
help(coefficients) # Run after lm, to get the value of your Beta slope estimate. Then convert it to the alpha estimate.
help(log)


#Q5
install.packages('rlist')
library(rlist)
alpha_0<-seq(0,0.9,0.1)
alpha_0<-list.append(alpha_0,c(0.99,0.999)) 
# calculate x values for each alpha_0  and plot the x lists
xset<-list()
par(mfrow=c(2,6))
for (i in 1:12){
  x_1<-(2*alpha_0[i]*m.mac)/(1-alpha_0[i])
  x_1.2<-d+x_1
  x<-log(x_1.2)
  boxplot(x)
  xset<-list.append(xset,x)
  
}

#now calulate beta value and alpha_1

#remove any inf in x 
x_1<-xset[1]
x_1[[1]][1]<-NA
xset[1]<-x_1

alpha1_list<-list()

for(i in 1:12){
  model.mac<-lm(y~as.matrix(xset[[i]]))
  beta<-model.mac$coefficients[2]
  alpha_1<- 1+(2/beta)
  alpha1_list<-list.append(alpha1_list,alpha_1)
  
}

a<-as.data.frame(alpha1_list)
#visulize the trend of alpha_1

plot(unlist(a)~alpha_0,xlab='alpha_0',ylab='alpha_1 estimated')





#Q7(coauthor)
alpha0.coa<-seq(0.2,0.9,0.1) 
alpha0.coa[9:10]<-c(0.99,0.999)
alpha0.list<-seq(0.1,0.19,0.01)
alpha0.list<-list.append(alpha0.list,alpha0.coa)
#coauthor$noa<-coauthor$Number.of.authors
#ecdf(coauthor$noa)
csum.ca<-cumsum(coauthor$noa)
F_d.ca<-csum.ca/sum(coauthor$noa)
col.sum<-colSums(coauthor)
avg.degree_ca<-sum(coauthor$Degree*coauthor$noa)/col.sum[3]
m.ca<-0.5*avg.degree_ca
#compute x and y respectively
y.ca<-log(1-F_d.ca)
xlist<-list()
#compute x list for different alpha0
for (i in 1:length(alpha0.list)){
  x_1.ca<-(2*alpha0.list[i]*m.ca)/(1-alpha0.list[i])
  x_1.2_ca<-coauthor$Degree + x_1.ca
  x.ca<-log(x_1.2_ca)
  xlist<-list.append(xlist,x.ca)
  
}
#eliminate inf/NA in x or y
y.ca[62]<-NA
#regress y on different x
alpha1.ca<-list()
for (i in 1:length(xlist)){
  model.ca<-lm(na.omit(y.ca)~xlist[[i]][1:61])
  beta.ca<-model.ca$coefficients[2]
  alpha_1<- 1+(2/beta.ca)
  alpha1.ca<-list.append(alpha1.ca,alpha_1)

}

#alpha1 list
unlist(alpha1.ca)
#visualize alpha1 and alpha0
plot(unlist(alpha1.ca)~alpha0.list,xlab='value of alpha0',ylab='value of alpha1')

#from the graph we can see the slope is almost 1 , which means
#alpha 0 and alpha1 becomes almost the same around 0.4 -0.41



#Q8 guessing alpha for the ham radio
alpha0.radio<-seq(0.1,0.9,0.1)
avg.degree_ra<-sum(radio$Degree*radio$Number.of.Operators)/sum(radio$Number.of.Operators)
m.ra<-0.5*avg.degree_ra
F_d.ra<-cumsum(radio$Number.of.Operators)/sum(radio$Number.of.Operators)
y.ra<-log(1-F_d.ra)
#exclude inf
y.ra[28]<-NA

#compute x list for different alpha0
xlist_ra<-list()
for (i in 1:length(alpha0.radio)){
  x_1.ra<-(2*alpha0.radio[i]*m.ra)/(1-alpha0.radio[i])
  x_1.2_ra<-radio$Degree + x_1.ra
  x.ra<-log(x_1.2_ra)
  xlist_ra<-list.append(xlist_ra,x.ra)
  
}


#regress y on x for different alpha0
alpha1.ra<-list()
for (i in 1:length(xlist_ra)){
  model.ra<-lm(na.omit(y.ra)~xlist_ra[[i]][1:27])
  beta.ra<-model.ra$coefficients[2]
  alpha_1<- 1+(2/beta.ra)
  alpha1.ra<-list.append(alpha1.ra,alpha_1)
  
}


unlist(alpha1.ra)
