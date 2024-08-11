# INSTALL LIBRARIES
library(psych)
library(lm.beta)
library(readr)
library(knitr)
library(corrgram)
library(questionr)
library(ltm)
library(eRm)
library(mirt)
#library(WrightMap)


# GET RAW DATA
library(haven)
Pew_RawData <- read_sav("data/Pew_RawData.sav")
write.csv(Pew_RawData, "data/Pew_RawData.csv")
pew <- read.csv("data/Pew_RawData.csv", header = TRUE, sep = ",")

# Code 9s to missing
pew$qa1[pew$qa1==9] <- NA
pew$qa2a[pew$qa2a==9] <- NA
pew$qa2b[pew$qa2b==9] <- NA
pew$qi4a[pew$qi4a==9] <- NA
pew$qi4b[pew$qi4b==9] <- NA
pew$qi4c[pew$qi4c==9] <- NA
pew$qi4d[pew$qi4d==9] <- NA
pew$marital[pew$marital==9] <- NA
pew$attend[pew$attend==9] <- NA
pew$qf2[pew$qf2==9] <- NA
pew$qf5[pew$qf5==9] <- NA
pew$qg1[pew$qg1==9] <- NA
pew$qg1c[pew$qg1c==9] <- NA
pew$qg5[pew$qg5==9] <- NA
pew$qg6[pew$qg6==9] <- NA
pew$qi1[pew$qi1==9] <- NA
pew$childrenrec[pew$childrenrec==99] <- NA
pew$agerec[pew$agerec==99] <- NA
pew$educ[pew$educ==9] <- NA
pew$income[pew$income==99] <- NA
pew$qp99[pew$qp99==9] <- NA

# Reverse code
pew$happy <- 4-pew$qa1
pew$sat_family <- 5-pew$qa2a
pew$sat_health <- 5-pew$qa2b
pew$sense_peace <- 6-pew$qi4a
pew$sense_wonder <- 6-pew$qi4b
pew$sense_gratitude <- 6-pew$qi4c
pew$sense_purpose <- 6-pew$qi4d
pew$attendr <- 7-pew$attend
pew$religimp <- 5-pew$qf2
pew$prayfreq <- 9-pew$qi1

# create dummy variables
pew$d_cath_adult <- ifelse(pew$RELTRAD == 10000,c(1),c(0))
pew$d_cath_child <- ifelse(pew$CHRELTRAD == 10000, c(1), c(0))
pew$d_male <- ifelse(pew$SEX =='1',c(1),c(0))
pew$d_evang_adult <- ifelse(pew$RELTRAD == 1100, c(1), c(0))
pew$d_mainline_adult <- ifelse(pew$RELTRAD == 1200, c(1), c(0))
pew$d_none_adult <- ifelse(pew$RELTRAD == 100000, c(1), c(0))
pew$d_hispanic <- ifelse(pew$hisp == 1, c(1), c(0))
pew$d_married <- ifelse(pew$marital == 1, c(1),c(0))
pew$d_living <- ifelse(pew$marital == 2, c(1),c(0))
pew$d_divorced <- ifelse(pew$marital == 3, c(1),c(0))
pew$d_separated <- ifelse(pew$marital == 4, c(1),c(0))
pew$d_widowed <- ifelse(pew$marital == 5, c(1),c(0))
pew$d_member <- ifelse(pew$qf5 == 1, c(1), c(0))
pew$d_theist <- ifelse(pew$qg1 == 1,c(1),c(0))
pew$d_godperson <- ifelse(pew$qg1c == 1,c(1),c(0))
pew$d_heaven <- ifelse(pew$qg5 == 1,c(1),c(0))
pew$d_hell <- ifelse(pew$qg6 == 1,c(1),c(0))
pew$d_knowgay <- ifelse(pew$qp99 == 1,c(1),c(0))

pew$d_peace <- ifelse(pew$sense_peace == 5, c(1),c(0))
pew$d_gratitude <- ifelse(pew$sense_gratitude == 5, c(1),c(0))
pew$d_wonder <- ifelse(pew$sense_wonder == 5, c(1),c(0))
pew$d_purpose <- ifelse(pew$sense_purpose == 5, c(1),c(0))

freq(pew$d_peace, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$d_gratitude, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$d_wonder, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$d_purpose, cum = TRUE, sort = "dec", total = TRUE)


Frequencies are presented here.
```{r echo=FALSE, fig.cap=""}
library(questionr)
freq(wb$sense_peace, cum = TRUE, sort = "dec", total = TRUE)
freq(wb$sense_wonder, cum = TRUE, sort = "dec", total = TRUE)
freq(wb$sense_gratitude, cum = TRUE, sort = "dec", total = TRUE)
freq(wb$sense_purpose, cum = TRUE, sort = "dec", total = TRUE)
```



Frequencies of the items show considerable positive skew as most respondents report more frequent feelings of deep peace, wonder, gratitude and purpose. This skew will pose a challenge to classical statistics based on the assumption of normally distributed data.

Internal reliability assumes the items are redundant.


pew$d_peace <- ifelse(pew$sense_peace == 5, c(1),c(0))
pew$d_gratitude <- ifelse(pew$sense_gratitude == 5, c(1),c(0))
pew$d_wonder <- ifelse(pew$sense_wonder == 5, c(1),c(0))
pew$d_purpose <- ifelse(pew$sense_purpose == 5, c(1),c(0))
freq(pew$d_peace, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$d_gratitude, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$d_wonder, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$d_purpose, cum = TRUE, sort = "dec", total = TRUE)

data.frame(colnames(pew))           #Returns column index numbers in table format,df=DataFrame name
newwellbeingitems <- pew[,37:40]        #select variables to include in fa





data.frame(colnames(pew))           #Returns column index numbers in table format,df=DataFrame name
newwellbeingitems <- pew[,37:40]        #select variables to include in fa
library(mirt)
model.rasch <- 'well.being = 1-4'
results.rasch <- mirt(data=newwellbeingitems, model=model.rasch, itemtype="Rasch", verbose=FALSE)
coef.rasch <- coef(results.rasch, simplify=TRUE)
items.rasch <- as.data.frame(coef.rasch$items)
print(items.rasch)
plot(results.rasch, type = 'trace', which.items = c(1,2,3,4),
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) # option curves
plot(results.rasch, type = 'infotrace', which.items = c(1,2,3,4),
     main = "", par.settings = simpleTheme(lwd=2)) # item information functions (IIF)
plot(results.rasch, type = 'info', theta_lim = c(-4,4), lwd=2) # test information curve
plot(results.rasch, type = 'itemscore', theta_lim = c(-4,4), lwd=2, facet_items = FALSE) # item scoring traceline
plot(results.rasch, type = 'SE', theta_lim = c(-4,4), lwd=2) # test standard errors




# exclude variables v1, v2, v3
deleteme <- names(pew) %in% c("qa1",
                              "qa2a",
                              "qa2b",
                              "qi4a",
                              "qi4b",
                              "qi4c",
                              "qi4d",
                              "marital",
                              "attend",
                              "qf2",
                              "qf5",
                              "qg1",
                              "qg1c",
                              "qg5",
                              "qg6",
                              "qi1",
                              "childrenrec",
                              "agerec",
                              "educ",
                              "income",
                              "qp99")
pew <- pew[!deleteme]
data.frame(colnames(pew)) #Returns column index numbers in table format,df=DataFrame name
pew <- pew[ -c(1,3:6,8:70,72:115,117:146,148:161,163:179) ]
pew <- pew[ which(pew$d_cath_child==1), ] # select only former Catholics
write.csv(pew, "data/MyPew_CathKids.csv") # write new data file

# Frequencies
freq(pew$happy, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$qa1, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$sense_peace, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$sense_wonder, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$sense_gratitude, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$sense_purpose, cum = TRUE, sort = "dec", total = TRUE)
freq(pew$wellbeing, cum = TRUE, sort = "dec", total = TRUE)

# select variables for wellbeing
attach(pew)
myvars <- c("sense_peace", "sense_wonder", "sense_gratitude", "sense_purpose")
wellbeing_file <- pew[myvars]
detach(pew)

rm(bigscale)
rm(myvars)
rm(mytable)
rm(table)

# classical reliability of wellbeing
alpha(wellbeing_file)
corrgram(wellbeing_file, order=NULL, lower.panel = panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Well-Being Variables") # correlogram

pew$wellbeing <- pew$sense_gratitude + pew$sense_wonder + pew$sense_purpose + pew$sense_peace
hist(pew$wellbeing)

pew <- pew[ which(pew$wellbeing > 4), ] # select only cases with complete wellbeing data


# IRT reliability
data.frame(colnames(pew))           #Returns column index numbers in table format,df=DataFrame name
data.frame(colnames(wb))           #Returns column index numbers in table format,df=DataFrame name
wellbeingitems <- pew[,10:13]        #select variables to include in fa
library(mirt)
model.rsm <- 'well.being = 1-4'
results.rsm <- mirt(data=wellbeingitems, model=model.rsm, itemtype="rsm", verbose=FALSE)
coef.rsm <- coef(results.rsm, simplify=TRUE)
items.rsm <- as.data.frame(coef.rsm$items)
print(items.rsm)
plot(results.rsm, type = 'trace', which.items = c(1,2,3,4),
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) # option curves
plot(results.rsm, type = 'infotrace', which.items = c(1,2,3,4),
     main = "", par.settings = simpleTheme(lwd=2)) # item information functions (IIF)
plot(results.rsm, type = 'info', theta_lim = c(-4,4), lwd=2) # test information curve
plot(results.rsm, type = 'itemscore', theta_lim = c(-4,4), lwd=2, facet_items = FALSE) # item scoring traceline
plot(results.rsm, type = 'SE', theta_lim = c(-4,4), lwd=2) # test standard errors
tabscores <- fscores(results.rsm) # get theta values
head(tabscores) # see first five cases
hist(tabscores) # histogram of theta values
pew <- cbind(tabscores,pew) # try to attach theta scores to data frame - this looks promising
pew$wellbeing <- pew$sense_peace + pew$sense_wonder + pew$sense_gratitude + pew$sense_purpose # wellbeing is sum of four items
head(pew)

#library(psych)
#bfi <- bfi[,1:24]                 #select variables to include in fa
#fit <- fa(bfi, 2)                 #estimate model with 2 factors
#fs <- factor.scores(bfi, fit)     #obtain factor scores
#fs <- fs$scores                   #get the columns of factor scores for each case
#bfi <- cbind(bfi,fs)              #append factor scores to dataset (you can also
#use merge()) or something comparable.

# save derived data
write.csv(wellbeing_file, file='wellbeing_file.csv')
write.csv(pew, file = 'data/MyPew.csv')
rm(wellbeing_file)

# histograms
hist(pew$happy)
hist(pew$sat_family)
hist(pew$sat_health)
hist(pew$sense_peace)
hist(pew$sense_wonder)
hist(pew$sense_gratitude)
hist(pew$sense_purpose)
hist(pew$wellbeing)

boxplot(wellbeing_file,main="Spiritual Well-Being") # boxplots

# T TESTS ------------------------------------------------------------------------------
t.test(happyscale ~ left_catholic,data=pew) # where y is numeric and x is a binary factor
t.test(happyscale ~ left_none,data=pew) # where y is numeric and x is a binary factor
t.test(happyscale ~ left_evangelical,data=pew) # where y is numeric and x is a binary factor
t.test(happyscale ~ left_mainline,data=pew) # where y is numeric and x is a binary factor

t.test(happy ~ left_catholic,data=pew) # where y is numeric and x is a binary factor
t.test(happy ~ left_none,data=pew) # where y is numeric and x is a binary factor
t.test(happy ~ left_evangelical,data=pew) # where y is numeric and x is a binary factor
t.test(happy ~ left_mainline,data=pew) # where y is numeric and x is a binary factor

t.test(satisfiedfamily ~ left_catholic,data=pew) # where y is numeric and x is a binary factor
t.test(satisfiedfamily ~ left_none,data=pew) # where y is numeric and x is a binary factor
t.test(satisfiedfamily ~ left_evangelical,data=pew) # where y is numeric and x is a binary factor
t.test(satisfiedfamily ~ left_mainline,data=pew) # where y is numeric and x is a binary factor

t.test(satisfiedhealth ~ left_catholic,data=pew) # where y is numeric and x is a binary factor
t.test(satisfiedhealth ~ left_none,data=pew) # where y is numeric and x is a binary factor
t.test(satisfiedhealth ~ left_evangelical,data=pew) # where y is numeric and x is a binary factor
t.test(satisfiedhealth ~ left_mainline,data=pew) # where y is numeric and x is a binary factor

t.test(peacescale ~ left_catholic,data=pew) # where y is numeric and x is a binary factor
t.test(peacescale ~ left_none,data=pew) # where y is numeric and x is a binary factor
t.test(peacescale ~ left_evangelical,data=pew) # where y is numeric and x is a binary factor
t.test(peacescale ~ left_mainline,data=pew) # where y is numeric and x is a binary factor


# Multiple Linear Regression Example
spw_model <- lm(peacescale ~
            + left_evangelical
            + left_mainline
            + left_other
            + left_none
            + age
            + d_male
            + d_hispanic
            + highested
            + income
            + d_married, data=pew)
spw_model_beta <- lm.beta(spw_model)
summary(spw_model_beta)

            + age
            + highested
            + d_married
            + childrenrec
            + prayfreq
            + d_heaven
            + d_hell
            + religimportance
            + happyscale
            + praygroupfreq
            + d_knowgay, data=pew)
summary(peace) # show results

summary(pew)


model <- glm(left_none ~ age
             + d_male
             + highesteduc
             + income
             + d_hispanic
             + d_married
             + d_personalgod
             + attend,
             family=binomial(link='logit'), data=pew)
summary(model)
