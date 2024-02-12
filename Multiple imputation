library("mice")
library("foreign")
library("readxl")
library("matrixStats")
library("tableone")
library("haven")

##A. Split boy/girl
##mcs$FCCSEX00<- labelled::to_character(mcs$FCCSEX00)
MCS_ACE<-zap_labels(mcs)
MCS_ACEboys<-MCS_ACE[
  grepl(1,MCS_ACE$FCCSEX00),]
MCS_ACEgirls<-MCS_ACE[
  grepl(2,MCS_ACE$FCCSEX00),]
# We run the mice code with 0 iterations 
ini <- mice(MCS_ACE, maxit=0, pri=F)
meth <- ini$method
#Extract predictorMatrix and methods of imputation 
predM <- ini$predictorMatrix
meth <- ini$method
#keep gender variable in but do not use for imputation (want to use in modelling but nothing else)
predM[,"FCCSEX00"] <- 0
meth["FCCSEX00"] <- ""
length(meth);dim(predM)

# If you like, view the first few rows of the predictor matrix
head(predM)
meth

# doing 50 imputations and 30 iterations 
m <- 50
maxit <-30
require(mice)
imp_boys <- mice(MCS_ACEboys, m = m,maxit=maxit,print=TRUE, method=meth,
                 predictorMatrix=predM,stringsAsFactor = TRUE, seed=140817)

imp_girls <- mice(MCS_ACEgirls, m = m,maxit=maxit,print=TRUE, method=meth,
                  predictorMatrix=predM,stringsAsFactor = TRUE, seed=140817)

##combine genders
imp_all<-rbind(imp_boys,imp_girls)
save(imp_all,file=".../imp_all.Rdata")

# Save
com_all <- complete(imp_all, "long", include = TRUE)
table(is.na(com_all))
nrow(com_all[com_all$.imp==0,]);colSums(is.na(com_all[com_all$.imp==0,]))
save(com_all,file=".../com_all.Rdata")
## Save stata format
write.dta(com_all,file="final_MCSimptd.dta")
