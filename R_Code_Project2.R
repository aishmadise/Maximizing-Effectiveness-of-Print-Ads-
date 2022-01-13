str(project2)
attach(project2)

# Mean - Descriptive Stats 
summary(cbind(brand_fix,pic_fix,brand_size,pic_size,page_num))

# Correlation - cor(x, method = "pearson", use = "complete.obs")
# the usual correlation does not work here as there are factor variables in the data
# sapply() function has to be used to identify the factor variables in the model
cor(project2[sapply(project2, function(x) !is.factor(x))])

# Frequency - List of all Brand Names
table(brand)

# Poisson Regression
# BRAND_FIX - Fixation Counts of the Brand Element
model1 <- glm(brand_fix ~ brand_size+page_pos+page_num , poisson(link="log"))
summary(model1)
# PIC_FIX - Fixation Counts of the Pic Element
model2 <- glm(pic_fix ~ pic_size+page_pos+page_num , poisson(link = "log"))
summary(model2)
# RECALL_ACCU - Binary Logit Model
model3 <- glm(recall_accu ~ brand_fix+pic_fix+page_pos+page_num ,binomial(link = "logit"))
summary(model3)