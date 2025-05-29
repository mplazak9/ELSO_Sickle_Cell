Sickle <- read_excel("Desktop/Sickle.xlsx")

Trait <- Sickle[Sickle$Trait=="1",]
Sickle <- Sickle[Sickle$Trait=="0",]

VA <- Sickle[Sickle$Mode_combo=="VA",]
VV <- Sickle[Sickle$Mode_combo=="VV",]

VAimp2 <- VA
VVimp2 <- VV

set.seed(120)
VAimp2.train <- as.data.frame(VAimp2)
VVimp2.train <- as.data.frame(VVimp2)

Va.imputed <- missForest(VAimp2.train)
Vv.imputed <- missForest(VVimp2.train)

head(Va.imputed$ximp)
head(Vv.imputed$ximp)
length(VAimp2)
length(VVimp2)

VA.new.data <- Va.imputed$ximp
VV.new.data <- Vv.imputed$ximp

class(Va.imputed)
summary(VV.new.data)

set.seed(121)
X <- model.matrix(DischargedAlive ~ .,data=VA.new.data)
Y <- VV.new.data[,"DischargedAlive"]=="No"
cv.model <- cv.glmnet(x=X, y=Y, family = "binomial", alpha=1)
plot(cv.model)
l.min <- cv.model$lambda.min
l.min
va.lasso.model <- glmnet(x=X, y=Y, family = "binomial", alpha=1, lambda = l.min)
va.lasso.model$beta
assess.glmnet(va.lasso.model, newx=X, newy=Y)
plot(roc.glmnet(va.lasso.model,newx=X, newy=Y),type="l")
va.lasso.model

set.seed(122)
X <- model.matrix(DischargedAlive ~ .,data=VV.new.data)
Y <- VV.new.data[,"DischargedAlive"]=="No"
cv.model <- cv.glmnet(x=X, y=Y, family = "binomial", alpha=1)
plot(cv.model)
l.min <- cv.model$lambda.min
l.min
vv.lasso.model <- glmnet(x=X, y=Y, family = "binomial", alpha=1, lambda = l.min)
vv.lasso.model$beta
assess.glmnet(vv.lasso.model, newx=X, newy=Y)
plot(roc.glmnet(vv.lasso.model,newx=X, newy=Y),type="l")
vv.lasso.model