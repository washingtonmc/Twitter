
library(randomForest)
library(leaps)
library(car)
library(class)
library(MASS)
library(caTools)
library(boot)
library(ROCR)
library(nnet)
library(pROC)

project=read.csv("C:/Users/lucky/OneDrive/Documents/School stuff/COFC/FALL_2018/CSCI390/Project/project.csv",header=T)
attach(project)
head(project)
dim(project)
twit.fit=glm(spambot~statuses_count+followers_count+favourites_count+listed_count+friends_count,data=project,family=binomial)
summary(twit.fit)
glm.probs=predict(twit.fit,type="response")
glm.probs[1:10]
contrasts(spambot)
glm.pred=rep("No",140)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,spambot)
par(mfrow=c(2,2))
plot(twit.fit)
pairs(project)

train=sample.split(spambot,SplitRatio = 1/3)
train.project=project[train,]
test.project=project[!train,]
spam.test=spambot[!train]
dim(test.project)

project.preds=cbind(followers_count,friends_count,statuses_count,favourites_count,listed_count)
b=regsubsets(as.matrix(project.preds),spambot)
rs=summary(b)
par(mfrow=c(1,1))
plot(1:5,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared", pch = 19)
subsets(b,statistic=c("adjr2"))
m1=glm(spambot~statuses_count,data=train.project,family=binomial)
n <- length(m1$residuals)
m7=glm(spambot~statuses_count+followers_count+friends_count+favourites_count+listed_count,family=binomial)
backAIC <- step(m7,direction="backward", data=project)
backBIC <- step(m7,direction="backward", data=project, k=log(n))

#logistic
glm.fits=glm(spambot~followers_count+friends_count+statuses_count+favourites_count+listed_count,data=test.project,
             family=binomial,subset=train)
glm.probs=predict(glm.fits,test.project,type="response")
glm.pred=rep("No",93)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred,spam.test)
1-mean(glm.pred==spam.test)
summary(glm.fits)
avPlots(glm.fits, terms=~.,ask=F,pch=19)

glm.fits2=glm(spambot~followers_count+friends_count,data=project,family=binomial,subset=train)
glm.probs2=predict(glm.fits2,test.project,type="response")
glm.pred2=rep("No",93)
glm.pred2[glm.probs2>0.5]="Yes"
table(glm.pred2,spam.test)
1-mean(glm.pred2==spam.test)
summary(glm.fits2)

#lda
lda.fit=lda(spambot~followers_count+friends_count,data=project,subset=train)
lda.fit
lda.pred=predict(lda.fit,test.project)
lda.class=lda.pred$class
table(lda.class,spam.test)
1-mean(lda.class==spam.test)

#knn
train.x=cbind(followers_count,friends_count)[train,]
test.x=cbind(followers_count,friends_count)[!train,]
train.spam=spambot[train]
set.seed(1)
knn.pred=knn(train.x,test.x,train.spam,k=1)
table(knn.pred,spam.test)
1-mean(knn.pred==spam.test)

knn.pred=knn(train.x,test.x,train.spam,k=3)
table(knn.pred,spam.test)
1-mean(knn.pred==spam.test)


# project=data.frame(project,spambot)
# project
# set.seed(1)
# bag.project=randomForest(project$spambot~.,data=train.project,mtry=2,importance=T)
# dim(project)
# project$id
# avPlots(twit.fit1)
# d.twit=data.frame(followers_count,favourites_count,statuses_count,listed_count,friends_count,spambot)
# na.omit(d.twit)
# tree.twit=tree(spambot~.,data=train.project)
# summary(tree.twit)
# plot(tree.twit)

# powerTransform(d.twit)
# tfol=followers_count^0.24571044
# tfav=favourites_count^0.09553096
# tstat=statuses_count^0.09835868
# tlist=listed_count^0.202860649
# tfriend=friends_count^0.32422630
# tspam=spambot^-0.41305501
# twit.fit2=lm(tspam~tfol+tfav+tstat+tlist+tfriend)
# summary(twit.fit1)
# summary(twit.fit2)

spam.test=spambot[!train]
lda.spam=lda(spambot~statuses_count+followers_count+friends_count+listed_count+favourites_count,subset=train)
lda.spam
plot(lda.spam)
lda.pred=predict(lda.spam,test.project)
lda.class=lda.pred$class
table(lda.class,test.project)
1-mean(lda.class==test.project)

#Leave one out
set.seed(17)
cv.error.10=rep(0,10)
glm.fits3=glm(spambot~friends_count+followers_count,data=project,family=binomial)
cv.err=cv.glm(project,glm.fits3)
cv.err$delta
plot(cv.err$delta)
cv.error=rep (0,5)
for (i in 1:5){
  glm.fit=glm(spambot~(poly(friends_count ,i),data=project))
  cv.error[i]=cv.glm (project ,glm.fit)$delta [1]
}
cv.error

#ROC curves
mymodel=multinom(spambot1~.,data=project)
p=predict(mymodel,project)
tab=table(p,project$spambot1)
tab
pred=predict(mymodel,project, type='prob')
head(pred)
hist(pred)
pred=prediction(pred,project$spambot1)
eval=performance(pred,"acc")
plot(eval)
max=which.max(slot(eval,"y.values")[[1]])
max
acc=slot(eval,"y.values")[[1]][max]
acc
cut=slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc,Cutoff=cut))

attributes(mod)$prob

roc(test.x$Class, )

#marginal model plots
mod1 <- lm(spambot~friends_count)
par(mfrow=c(1,1))
mmp(mod1,friends_count,xlab="Number of friends",key=NULL)
