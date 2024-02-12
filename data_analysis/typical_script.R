#load data
require(titanic)
df = titanic_train

#prepare data
df$Sex = factor(df$Sex, levels=c("male", "female"))
df$Survived = factor(df$Survived, levels=c(0, 1), labels=c("No", "Yes"))
df$Pclass = factor(df$Pclass, levels=c(1,2,3), ordered=TRUE)

#model
m1 = glm(Survived ~ Sex + Pclass + Age, family = 'binomial', data=df)
summary(m1)