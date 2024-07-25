library(caret)
library(pROC)
library(dplyr)

credit1 = read.csv("creditcard.csv")
str(credit1)
summary(credit1)

missing = credit1 %>% summarise_all(funs(sum(is.na(.))/n()))
missing
# Let's remove the ID variable it is not needed
credit=credit1[,-1]

# preliminary check of the data

credit %>% 
  group_by(default.payment.next.month) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(default.payment.next.month, -percent), percent), fill = default.payment.next.month)+
  geom_col(fill = c("grey", "light blue"))+
  geom_text(aes(label = sprintf("%.1f%%", percent)), hjust = 0.2, vjust = 2, size = 5)+ 
  theme_bw()+  
  xlab("Default (0:No, 1:Yes)") + ylab("Percent") + ggtitle("Default Percent")

ggplot(credit, aes(x = LIMIT_BAL)) + geom_histogram()
ggplot(credit, aes(x = SEX)) + geom_histogram()
ggplot(credit, aes(x = EDUCATION)) + geom_histogram()
ggplot(credit, aes(x = MARRIAGE)) + geom_histogram()
ggplot(credit, aes(x = AGE)) + geom_histogram()
ggplot(credit, aes(x = PAY_0)) + geom_histogram()
ggplot(credit, aes(x = PAY_2)) + geom_histogram()
table(credit$PAY_0)

# There are some categories with very few observations. Let's group them.
credit$EDUCATION[credit$EDUCATION == 0] = 4
credit$EDUCATION[credit$EDUCATION == 5] = 4
credit$EDUCATION[credit$EDUCATION == 6] = 4
credit$MARRIAGE[credit$MARRIAGE == 0] = 3

ggplot(credit, aes(x = EDUCATION)) + geom_histogram()
ggplot(credit, aes(x = MARRIAGE)) + geom_histogram()

# Make some numeric variables into factors
credit$default.payment.next.month=as.factor(credit$default.payment.next.month)
credit$SEX=as.factor(credit$SEX) 
credit$EDUCATION=as.factor(credit$EDUCATION)
credit$MARRIAGE=as.factor(credit$MARRIAGE)

# Some EDA
# Default against categorical variables
ggplot(credit, aes(x=SEX,fill=default.payment.next.month))+ geom_bar()+ theme_bw() 
ggplot(credit, aes(x=EDUCATION,fill=default.payment.next.month))+ geom_bar()+theme_bw()
ggplot(credit, aes(x=MARRIAGE,fill=default.payment.next.month))+ geom_bar()+theme_bw()

# Default against numeric variables
ggplot(credit, aes(x=default.payment.next.month, y=LIMIT_BAL, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Amount of given credit in NT dollars")
ggplot(credit, aes(x=default.payment.next.month, y=AGE, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Age")
ggplot(credit, aes(x=default.payment.next.month, y=PAY_0, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Repayment status in September, 2005")
ggplot(credit, aes(x=default.payment.next.month, y=BILL_AMT1, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Amount of bill statement in September, 2005 (NT dollar)")+
  ylim(0,125000)
ggplot(credit, aes(x=default.payment.next.month, y=PAY_AMT1, fill=default.payment.next.month)) + geom_violin()+
  geom_boxplot(width=0.1, fill="white") + labs(title="Amount of previous payment in September, 2005 (NT dollar)")+
  ylim(0,10000)


# Logistic model
logit.1 = glm(default.payment.next.month~., data=credit, family="binomial")
summary(logit.1)

pred = predict(logit.1, type = "response", credit)
summary(pred)


# ROC and AUC
glm.roc = roc(response = credit$default.payment.next.month, predictor = pred)
plot(glm.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
coords(glm.roc, "best", "threshold")

# Threshold = 0.5
pred_default = factor(ifelse(pred >=0.5, "Yes", "No"))
credit$default.payment.next.month = factor(ifelse(credit$default.payment.next.month==1, "Yes","No"))
confusionMatrix(pred_default, credit$default.payment.next.month, positive = "Yes")

# In search of a better threshold
perform_fn = function(cutoff) 
{
  pred_default = factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(pred_default, credit$default.payment.next.month, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(0.01,0.99,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=3,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=3)
lines(s,OUT[,3],col=4,lwd=3)
box()
legend("right",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.5, col="black", lwd=2, lty=2)
abline(v = 0.3, col="black", lwd=2, lty=3)
abline(v = 0.2, col="black", lwd=2, lty=4)
axis(1, at = seq(0.1, 1, by = 0.1))
grid()

# Alternative Way:
library(ROCR)
glm.roc2 = prediction(pred, credit$default.payment.next.month)
glm.roc2a = performance(glm.roc2, "tpr", "fpr")

plot(glm.roc2a, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

## Cut-off point seems to be better between 0.2 and 0.3 than in 0.5

##
## Remove the dependent variable
##
credit2=credit1[!names(credit1) %in% c("ID","default.payment.next.month")] 

# Principal Component Analysis
str(credit2)  # Make sure all variables are numerical
pr.out=prcomp(credit2, scale=TRUE)
summary(pr.out)
pr.out$rotation[,1:5] 
pr.out$x

##
## K-Means Cluster
##
km1=kmeans(credit2,4,nstart=111)
km1$cluster

plot(credit2[,c("LIMIT_BAL","BILL_AMT1")], col=(km1$cluster+1), 
     main="K-Means Clustering Results with K=4", 
     xlab="LIMIT_BAL", ylab="BILL_AMT1", pch=20, cex=2)

plot(credit2[,c("LIMIT_BAL","PAY_AMT1")], col=(km1$cluster+1), 
     main="K-Means Clustering Results with K=4", 
     xlab="LIMIT_BAL", ylab="PAY_AMT1", pch=20, cex=2)

plot(credit2[,c("PAY_AMT1","BILL_AMT1")], col=(km1$cluster+1), 
     main="K-Means Clustering Results with K=4", 
     xlab="PAY_AMT1", ylab="BILL_AMT1", pch=20, cex=2)

plot(credit2[,c("LIMIT_BAL","AGE")], col=(km1$cluster+1), 
     main="K-Means Clustering Results with K=4", 
     xlab="LIMIT_BAL", ylab="AGE", pch=20, cex=2)

plot(credit2[,c("BILL_AMT1","AGE")], col=(km1$cluster+1), 
     main="K-Means Clustering Results with K=4", 
     xlab="BILL_AMT1", ylab="AGE", pch=20, cex=2)

