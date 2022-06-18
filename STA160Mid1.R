heart_disease_health_indicators_BRFSS2015 <- read.csv("~/Desktop/davis insuranve/heart_disease_health_indicators_BRFSS2015.csv")
View(heart_disease_health_indicators_BRFSS2015)

data = heart_disease_health_indicators_BRFSS2015

library(ggplot2)

mean_BP= aggregate(HeartDiseaseorAttack ~ HighBP, data = data, mean)

sds_BP= aggregate(HeartDiseaseorAttack ~ HighBP, data = data, sd)


ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(HighBP ~.) + ggtitle("II: People with Heart Disease by HighBP")



mean_Chol= aggregate(HeartDiseaseorAttack ~ HighChol, data = data, mean)

sds_Chol= aggregate(HeartDiseaseorAttack ~ HighChol, data = data, sd)


ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(HighChol ~.) + ggtitle("II: People with Heart Disease by HighChol")



mean_Sex= aggregate(HeartDiseaseorAttack ~ Sex, data = data, mean)

sds_Sex= aggregate(HeartDiseaseorAttack ~ Sex, data = data, sd)


ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(Sex ~.) + ggtitle("II: People with Heart Disease by Sex")



mean_Age= aggregate(HeartDiseaseorAttack ~ Age, data = data, mean)

sds_Age= aggregate(HeartDiseaseorAttack ~ Age, data = data, sd)


ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(Age ~.) + ggtitle("II: People with Heart Disease by Age")



mean_HC= aggregate(HeartDiseaseorAttack ~ AnyHealthcare, data = data, mean)

sds_HC= aggregate(HeartDiseaseorAttack ~ AnyHealthcare, data = data, sd)

boxplot(HeartDiseaseorAttack ~ AnyHealthcare, data,
        main = "I: People Attack by High BP")

ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(AnyHealthcare ~.) + ggtitle("II: People with Heart Disease by HC")





model.one <- glm(HeartDiseaseorAttack~HighBP, family=binomial, data=data)
alpha.one = coefficients(model.one)[1]
beta.one = coefficients(model.one)[2]
plogis(-3.147744 + 1.524337*6.5)
logOdds.one <- function(x){predict(model.one,data.frame(HighBP=x))}
pi.one <- function(x){plogis(predict(model.one,data.frame(HighBP=x)))}
plot(logOdds.one,
     ylab="Log odds of Heart Disease/Attack",
     xlab="age",
     main="Figure 1: Log odds linear in High BP")
plot(pi.one,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in High BP")


model.two <- glm(HeartDiseaseorAttack~HighChol, family=binomial, data=data)
alpha.two = coefficients(model.two)[1]
beta.two = coefficients(model.two)[2]
plogis(-2.968394 + 1.277894*6.5)
logOdds.two <- function(x){predict(model.two,data.frame(HighChol=x))}
pi.two <- function(x){plogis(predict(model.two,data.frame(HighChol=x)))}
plot(logOdds.two,
     ylab="Log odds of Heart Disease/Attack",
     xlab="age",
     main="Figure 1: Log odds linear in HighChol")
plot(pi.two,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in HighChol")




model.three <- glm(HeartDiseaseorAttack~Sex, family=binomial, data=data)
alpha.three = coefficients(model.three)[1]
beta.three = coefficients(model.three)[2]
plogis(-2.558173 + .589541*6.5)
logOdds.three <- function(x){predict(model.three,data.frame(Sex=x))}
pi.three <- function(x){plogis(predict(model.three,data.frame(Sex=x)))}
plot(logOdds.three,
     ylab="Log odds of Heart Disease/Attack",
     xlab="age",
     main="Figure 1: Log odds linear in Sex")
plot(pi.three,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in Sex")





model.four <- glm(HeartDiseaseorAttack~Age, family=binomial, data=data)
alpha.four = coefficients(model.four)[1]
beta.four = coefficients(model.four)[2]
plogis(-5.091596 + .3117941*6.5)
logOdds.four <- function(x){predict(model.four,data.frame(Age=x))}
pi.four <- function(x){plogis(predict(model.four,data.frame(Age=x)))}
plot(logOdds.four,
     ylab="Log odds of Heart Disease/Attack",
     xlab="age",
     main="Figure 1: Log odds linear in Age")
plot(pi.four,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in Age")






model.five <- glm(HeartDiseaseorAttack~AnyHealthcare, family=binomial, data=data)
alpha.five = coefficients(model.five)[1]
beta.five = coefficients(model.five)[2]
plogis(-2.585688 + .3365861*6.5)
logOdds.five <- function(x){predict(model.five,data.frame(AnyHealthcare=x))}
pi.five <- function(x){plogis(predict(model.five,data.frame(AnyHealthcare=x)))}
plot(logOdds.five,
     ylab="Log odds of Heart Disease/Attack",
     xlab="age",
     main="Figure 1: Log odds linear in Healthcare")
plot(pi.five,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in Healthcare")





model.six = glm(HeartDiseaseorAttack~HighBP + HighChol + Sex + Age
                + AnyHealthcare, family=binomial, data=data)
alpha.six = coefficients(model.six)[1]
beta1.six = coefficients(model.six)[2]
beta2.six = coefficients(model.six)[3]
beta3.six = coefficients(model.six)[4]
beta4.six = coefficients(model.six)[5]
beta5.six = coefficients(model.six)[6]





sample_size = floor(0.5*nrow(data))
set.seed(84)

picked = sample(seq_len(nrow(data)),size = sample_size)
test.data = data[picked,]
train.data = data[-picked,]

result <- step(glm(HeartDiseaseorAttack~1, binomial, test.data),
               scope = ~HighBP*HighChol*Sex*Age*AnyHealthcare,
               k=2,
               trace=0,
               direction = "forward")

summary(result)

testFit <- glm(result$model, binomial, test.data)
summary(testFit)$coefficients



par(mfrow=c(2,3))
plot(logOdds.one,
     ylab="Log odds of Heart Disease/Attack",
     xlab="High BP",
     main="Figure 1: Log odds linear in High BP")
plot(logOdds.two,
     ylab="Log odds of Heart Disease/Attack",
     xlab="HighChol",
     main="Figure 1: Log odds linear in HighChol")
plot(logOdds.three,
     ylab="Log odds of Heart Disease/Attack",
     xlab="Sex",
     main="Figure 1: Log odds linear in Sex")
plot(logOdds.four,
     ylab="Log odds of Heart Disease/Attack",
     xlab="Age",
     main="Figure 1: Log odds linear in Age")
plot(logOdds.five,
     ylab="Log odds of Heart Disease/Attack",
     xlab="age",
     main="Figure 1: Log odds linear in Healthcare")

par(mfrow=c(2,3))
plot(pi.one,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in High BP")
plot(pi.two,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in HighChol")
plot(pi.three,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in Sex")
plot(pi.four,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in Age")
plot(pi.five,
     ylab="Probability of Heart Disease/Attack",
     xlab="age",
     main="Figure 2: Log odds linear in Healthcare")



par(mfrow=c(6,6))
ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(HighBP ~.) + ggtitle("II: People with Heart Disease by HighBP")
ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(HighChol ~.) + ggtitle("II: People with Heart Disease by HighChol")
ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(Sex ~.) + ggtitle("II: People with Heart Disease by Sex")
ggplot(data, aes(x = HeartDiseaseorAttack)) +
  geom_histogram(binwidth = 1,color = "black",fill = "white") +
  facet_grid(AnyHealthcare ~.) + ggtitle("II: People with Heart Disease by HC")



