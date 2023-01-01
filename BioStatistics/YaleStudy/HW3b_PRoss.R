# Homework 3 part B
### Exercise 1

# don't forget to set the working directory to where you have these data stored

library(tidyverse)
setwd("~/SpatialAnalysis/01-2022_PHStatsII/Assignments/A3/A3ptB")

casData <- read.csv("casData.csv")
head(casData)

# Part A:  GPA by drinking
  class(casData$DRINK)
  table(casData$DRINK)
  
  # make DRINK a factor with labels (helpful for visual displays, cuts down on code needed to label groups)
  casData <- casData %>%
    mutate(DRINK = factor(DRINK, levels = c(0, 1), labels = c("Non-drinker", "Drinker")))

  # no frills boxplot of GPA by drinking
  ggplot(casData, aes(x = DRINK, y = GPA)) +
    geom_boxplot()

  # Simple linear regression of GPA on drinking
  model1 <- lm(GPA ~ DRINK, data = casData)
  summary(model1)

  model1$coefficients[2]
  confint(model1)[2,]

#  Part B:GPA by age
  
   # no frills scatterplot of GPA by age
    ggplot(casData, aes(x = age, y = GPA)) +
    geom_point()
  
    # since age is discrete, may a boxplot is more helpful?
    casData <- casData %>%
      mutate(agef = factor(age))
    
    ggplot(casData, aes(x = agef, y = GPA)) +
      geom_boxplot()

    # model that assumes linaerity of mean GPA with age
    model2 <- lm(GPA ~ DRINK + age, data = casData)
    summary(model2)

    # The estimated mean GPA for 20-year-old students who drink
    model2$coefficients[1] + model2$coefficients[2]*1 + model2$coefficients[3]*20
    
    # The estimated mean GPA for 20-year-old students who don't drink
    model2$coefficients[1] + model2$coefficients[2]*0 + model2$coefficients[3]*20

    # The estimated mean difference in GPA for students who drink, compared to students of the same age who do not

    model2$coefficients[2]
    confint(model2)[2,]

# Part C: GPA and drinking, adjusting for year in school

    #  created factor version of year in school
    casData <- casData %>%
      mutate(schoolyr = factor(schoolyr, 
                               labels = c("freshman", "sophomore", "junior", "senior", "5th year plus")))
    
    # no frills boxplot of GPA by year in school
    ggplot(casData, aes(x = schoolyr, y = GPA)) +
      geom_boxplot()
    
    # table of drinking status by year in school
    casData %>% count(schoolyr, DRINK) %>%
      group_by(schoolyr) %>%
      mutate(prop = n / sum(n)) %>%
      select(-n) %>%
      spread(DRINK, prop)
    
    # multiple regression of GPA on drinking and school year
    model3 <- lm(GPA ~ DRINK + as.factor(schoolyr), data = casData)
    summary(model3)

    # The estimated mean GPA for freshman students who drink
    model3$coefficients[1] + model3$coefficients[2]*1
    
    # The estimated mean GPA for senio students who drink
    model3$coefficients[1] + model3$coefficients[2]*1 + model3$coefficients[5]*1

    # The estimated mean difference in GPA for students who drink, compared to students of the same school year who do not
    model3$coefficients[2]
    confint(model3)[2,]

# Part D: GPA and sex, adjusting for drinking


    # no frills boxplot of GPA by sex
    ggplot(casData, aes(x = male, y = GPA))+
      geom_boxplot()
    
    # Why the warning message?
    # explicitly express male as a factor variable
    # Could you also give nicer labels?
    ggplot(casData, aes(x = as.factor(male), y = GPA))+
      geom_boxplot()
    
    # table of drinking status by sex
    casData %>% count(male,DRINK) %>%
      group_by(male) %>%
      mutate(prop = n / sum(n)) %>%
      select(-n) %>%
      spread(male, prop)
    
    # regression results: you'll need to figure out which code answers which items
      model4 <- lm(GPA ~ DRINK + male, data=casData)
      summary(model4)
      
      model4$coefficients[2]
      confint(model4)[2,]
      
      model4$coefficients[3]
      confint(model4)[3,]

### Exercise 2

      nmesData <- read.csv("nmesData.csv")
      head(nmesData)

      # Part A: High Medical Expenditures and Smoking History
      
      # table of high expenditures indicator by smoking status
      nmesData %>% count(eversmk, highexp) %>%
        group_by(eversmk) %>%
        mutate(prop = n / sum(n)) %>%
        select(-n) %>%
        spread(eversmk, prop)

        # logistic regression relating high med expenditures to smoking history
        model5 <- glm(highexp ~ eversmk, family=binomial(link=logit), data=nmesData)
        summary(model5)

        # estimate prob/proportion of high med exp among non-smokers (how does this compare to the result from your 2X2 talve)
        exp(model5$coefficients[1])/(1+exp(model5$coefficients[1]))
    
        # estimate prob/proportion of high med exp among smokers (how does this compare to the result from your 2X2 table)
          exp(model5$coefficients[1] + model5$coefficients[2]*1)/(1+exp(model5$coefficients[1]+ model5$coefficients[2]*1))
          
        # odds ratio and 95% CI of high med exp for smokers compared to non-smokers

          exp(model5$coefficients)[2]
          exp(confint(model5))[2,]

        # Part B: high medical expenditures and smoking history taking into account (adjusting for) age
                    
          model6 <- glm(highexp ~ eversmk + age, family=binomial(link=logit), data=nmesData)
          summary(model6)

        # The estimated probability of a high expenditure for a 50-year-old smoker
          exp(model6$coefficients[1] + model6$coefficients[2]*1 + model6$coefficients[3]*50)/(1+exp(model6$coefficients[1] + model6$coefficients[2]*1 + model6$coefficients[3]*50))

        #  The estimated probability of a high expenditure for a 60-year-old non-smoker
          exp(model6$coefficients[1] + model6$coefficients[2]*0 + model6$coefficients[3]*60)/(1+exp(model6$coefficients[1] + model6$coefficients[2]*0 + model6$coefficients[3]*60))

        # The estimated odds ratio of a high expenditure, comparing smokers to nonsmokers of the same age  (and 95% CI)
          exp(model6$coefficients[2])
          exp(confint(model6)[2,])

        # The estimated odds ratio (and 95% CI)  of a high expenditure for two groups of the same smoking status but whose age differs by 10 years
          
        exp(10*model6$coefficients[3])
        exp(10*confint(model6)[3,])

