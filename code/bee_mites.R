# GLM Poisson Practice

# How does increasing concentration of pesticides
# impact number of dead mites? 

# Mites exposed to 4 commercially avaible acardicides
# dead mites counted over 24 hrs

mites <- read.csv("~/Downloads/bee_mites.csv", stringsAsFactors=TRUE)
                  
colnames(mites)[4] <- "Dead"


# FIt model
M1 <- glm(Dead~Concentration, data=mites, family="poisson")
