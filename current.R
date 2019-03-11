d1<-read.csv("trainCancer.csv")
levels(d1$menopause)
library(dummies)
d1<-cbind(d1,dummy(d1$menopause,sep ="_"))
d1$menopause<-NULL
d1<-cbind(d1,dummy(d1$class,sep = "_"))
d1$class<-NULL
levels(d1$age)
d1<-cbind(d1,dummy(d1$age,sep="_"))
d1$age<-NULL
levels(d1$tumorsize)
d1<-cbind(d1,dummy(d1$tumorsize,sep="_"))
d1$tumorsize<-NULL
levels(d1$invnodes)
d1<-cbind(d1,dummy(d1$invnodes,sep="_"))
d1$invnodes<-NULL
d1<-cbind(d1,dummy(d1$breast,sep="_"))
d1$breast<-NULL
levels(d1$nodecaps)
d1<-cbind(d1,dummy(d1$breastquad,sep="_"))
d1$breastquad<-NULL
d1<-cbind(d1,dummy(d1$nodecaps,sep="_"))
d1$nodecaps<-NULL
d1<-cbind(d1,dummy(d1$degmalig,sep="_"))
d1$degmalig<-NULL
d1<-cbind(d1,dummy(d1$irradiat,sep="_"))
d1$irradiat<-NULL
library(neuralnet)
d2<-data.frame(d1[39],d1[38])

d1$Y<-d2$d1_yes
d1$N<-d2$d1_no
d1[39]<-NULL
d1[38]<-NULL
ind <-sample(1:nrow(d1),175)
trainDF <- d1[ind,]
testDF <- d1[-ind,]
d3<-data.frame(testDF$d1_yes,testDF$d1_no)

neuralmodel <- neuralnet(d1$d1_yes+d1$d1_no~d1$d1_ge40+d1$d1_lt40+d1$d1_premeno+d1$`d1_no-recurrence-events`+d1$`d1_recurrence-events`+d1$`d1_20-29`+d1$`d1_30-39`+d1$`d1_40-49`+d1$`d1_50-59`+d1$`d1_60-69`+d1$`d1_70-79`+d1$`d1_0-4`+d1$`d1_05-Sep`+d1$`d1_15-19`+d1$`d1_20-24`+d1$`d1_25-29`+d1$`d1_30-34`+d1$`d1_35-39`+d1$`d1_40-44`+d1$`d1_45-49`+d1$`d1_50-54`+d1$`d1_Oct-14`+d1$`d1_0-2`+d1$`d1_03-May`+d1$`d1_06-Aug`+d1$`d1_09-Nov`+d1$`d1_15-17`+d1$`d1_24-26`+d1$`d1_Dec-14`+d1$d1_1+d1$d1_2+d1$d1_3+d1$Y+d1$N,data = trainDF,hidden = 3,algorithm = "backprop",err.fct="sse",act.fct = "logistic",linear.output = FALSE,learningrate = 0.01,stepmax = 1000000,rep = 1)
plot(neuralmodel)

d1[36]<-NULL
d1[35]<-NULL
d1[34]<-NULL
d1[33]<-NULL
d1[32]<-NULL
d1[31]<-NULL
d1[30]<-NULL
testDF$d1_yes<-NULL
testDF$d1_no<-NULL



predictions <- compute(neuralmodel,testDF)
p<-data.frame(pred = round(predictions$net.result))
predictions$net.result
a <- cbind(d3$testDF.d1_yes,d3$testDF.d1_no,p)
mean((a$`d3$testDF.d1_yes` - a$pred.1)^2)


test<-read.csv("testCancer.csv")









