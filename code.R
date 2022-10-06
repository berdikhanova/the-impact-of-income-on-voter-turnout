#install.packages('multiwayvcov')

library(multiwayvcov)
library(quantreg)
library(haven)

#Load in replication data from B&W
#link to download the dataset: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/M7JKP6/MEONOT&version=1.0
county.data <- read_dta("/Users/marinaberdikhanova/Downloads/BW_20JOP_county_replication_data.dta")
head(county.data)


##________________________REPLICATION_______________________##

#TABLE 1
#Table 1 Model 1 (Unweighted) - correct coefficients and standard errors (except intercept)
table1.model1 <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                      AdjIncome + closeness + GubElection + SenElection +
                      Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84+ Yr80 +  
                    factor(FIPS_County), data=county.data)
                      
                      
table1.model1$coefficients[1:9]

#SE
m1.vcovCL <- cluster.vcov(table1.model1, county.data$FIPS_County, df_correction = FALSE)
ses.m1u <- sqrt(diag(m1.vcovCL))
ses.m1u[1:13]


#Table 1 Model 1 (Weighted) - correct coefficients and standard errors (except intercept)
table1.model1w <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                       AdjIncome + closeness + GubElection + SenElection +
                       Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                       factor(FIPS_County), data=county.data, weights=as.numeric(pop))


table1.model1w$coefficients[1:9]

#SE
m1w.vcovCL <- cluster.vcov(table1.model1w, county.data$FIPS_County, df_correction = TRUE)
ses.m1w <- sqrt(diag(m1w.vcovCL))
ses.m1w[1:9]

#Table 1 Model 2 (Unweighted) - correct coefficients and standard errors (except intercept)
table1.model2u <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                       AdjIncome + closeness + GubElection + SenElection + Turnout_Lag +
                       Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                       factor(FIPS_County), data=county.data)
table1.model2u$coefficients[1:10]


#SE
m2u.vcovCL <- cluster.vcov(table1.model2u, county.data$FIPS_County, df_correction = FALSE)
ses.m2u <- sqrt(diag(m2u.vcovCL))
ses.m2u[1:10]

#Table 1 Model 2 (Weighted) - correct coefficients and standard errors (except intercept)
table1.model2w <- lm(Turnout ~ Unemploy_County_new + Unemploy_State + PcntBlack + ZPcntHSGrad +
                       AdjIncome + closeness + GubElection + SenElection + Turnout_Lag +
                       Yr2008 + Yr2004 + Yr2000 + Yr96 + Yr92 + Yr88 + Yr84 + Yr80 +  
                       factor(FIPS_County), data=county.data, weights=as.numeric(pop))
table1.model2w$coefficients[1:10]


#SE
m2w.vcovCL <- cluster.vcov(table1.model2w, county.data$FIPS_County, df_correction = TRUE)
ses.m2w <- sqrt(diag(m2w.vcovCL))
ses.m2w[1:10]


#Creating a table to combine the data from four models#
coef_names <- c('County Unemployment rate', 'State unemployment rate', '% Black','% High school graduates', 'Median Income', 'Presidential campaign competitiveness', 'Simultaneous Gubernatorial election', 'Simultaneous senatorial election','Lagged turnout', 'Constant')
coef_num <- length(coef_names)



#Combining Model 1 unweighted and weighted 
mod1 <- matrix(NA, nrow = coef_num, ncol = 2)
rownames(mod1) <- coef_names
colnames(mod1) <- c("Mod 1 | Unweighted", "Weighted")

for (i in c(2,3,4,5,6,7,8,9,10,1)){
  j = i-1
  if (j>0&j<9) {
    mod1[j, 1] <- table1.model1$coefficients[i]
    mod1[j, 2] <- table1.model1w$coefficients[i]  
  } else { if (j==9){
    mod1[j, 1] <- NA
    mod1[j, 2] <- NA
  } else{
    mod1[coef_num, 1] <- table1.model1$coefficients[i]
    mod1[coef_num, 2] <- table1.model1w$coefficients[i]
  }
      
  }
  
}

#Combining Model 1 unweighted and weighted 
mod2 <- matrix(NA, nrow = coef_num, ncol = 2)
rownames(mod2) <- coef_names
colnames(mod2) <- c("Mod 2 | Unweighted", "Weighted")

for (i in c(2,3,4,5,6,7,8,9,10,1)){
  j = i-1
  if (j>0){
    mod2[j, 1] <- table1.model2u$coefficients[i]
    mod2[j, 2] <- table1.model2w$coefficients[i] 
  } else{
    mod2[coef_num, 1] <- table1.model2u$coefficients[i]
    mod2[coef_num, 2] <- table1.model2w$coefficients[i] 
  }
  
}

#Combining Model 1 and 2 
table_1 <- cbind(mod1, mod2)
table_1 <- round(table_1, 3)
show("Table 1. Replication of Cross-Sectional Models of County Turnout", table_1)


#EXTENSION


library(quantreg)
attach(county.data)
Y = cbind(Turnout)
X = cbind(AdjIncome, Unemploy_State, Unemploy_County_new)

quantile_rg_national <- rq(Y ~ X, tau = seq(0.05,0.95, by=0.1),data=county.data)
quantile_rg_plot <- summary(quantile_rg_national)

#National Data Quantile Regression Plot
plot(quantile_rg_plot, main = c("Intercept ", "Adjusted Median Income", "State Unmployment", "County Unemployment" ))
summary(quantile_rg_national, cov = TRUE)

#Virginia State Quantile Regression Plot
county.data_va <- county.data[county.data$state == "VA",] 

#Regression 
attach(county.data_va)

quantreg_virginia <- rq(Turnout ~ AdjIncome + Unemploy_State+ Unemploy_County_new , tau = seq(0.05,0.95,by=0.1),data=county.data_va)
quantreg_virginia_plot <- summary(quantreg_virginia)

plot(quantreg_virginia_plot, main = c("Intercept ", "Adjusted Median Income", "State Unmployment", "County Unemployment")) 
summary(quantreg_virginia, cov=TRUE)
