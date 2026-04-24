### Download data:
# You may download the dataset from the website: knit.ucsd.edu/ranchobernardostudy
# Go the website and click on the "Data Access" tab. 
# Then click on "Data Use Agreement" and click on "I agree to the terms and conditions. Please take me to the download page."
# Download the datasets "Demographics" and "Health Behaviors".
# You can find both the data files and the data dictionaries there.




### Load the dataset in R
rm(list = ls())     # Remove everything in the environment.
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   # set the working directory to where the current file is

library("readxl")
dat.raw = read_excel("RBS_DEMO_DATA.xlsx")


# Extract the columns "SUBJID", "LASTCONTAGE", "STATUS"
# as well as the 4 demographic variables: "SEX", "RACE_WHITE", "EDUCATION", "BIRTH_COHORT"
dat = dat.raw[,c("SUBJID", "LASTCONTAGE", "STATUS", 
                 "SEX", "RACE_WHITE", "EDUCATION", "BIRTH_COHORT")]
dim(dat)   # 6726*7






## Add the Health Behaviors to the dataset
dat_hb.raw = read_excel("RBS_HLTHBEH_DATA.xlsx")

dat_hb = dat_hb.raw[,c("SUBJID", "VISIT",
                       "EVERSMOKE", "EVERALC", "ALCO", 
                       "EX3XWK", "MEDSCORE")]
UID = unique(dat_hb$SUBJID)
length(UID)   # 6726

# Take the variables at Visit 4 (1984-1987)
# Truncate subjects who did not survival to visit 4, and remove those did not have visit 4
dat_hb_v4 = dat_hb[dat_hb$VISIT == 4,]
dim(dat_hb_v4)  # 2480*7

id.match = match(dat_hb_v4$SUBJID, dat$SUBJID)
dat_demo = dat[id.match,]

# Variables for smoking, alcohol, exercise and diet.
dat_smk_alc_ex_diet = dat_hb_v4[,c("EVERSMOKE", "EVERALC", "ALCO", 
                                   "EX3XWK", "MEDSCORE")]
dat_demo_hb = cbind(dat_demo, dat_smk_alc_ex_diet)


dat_demo_hb = dat_demo_hb[complete.cases(dat_demo_hb),]
dim(dat_demo_hb)    # 1024*12


# Change to numerical values
dat_demo_hb$BIRTH_COHORT = as.numeric(dat_demo_hb$BIRTH_COHORT)

# For EDUCATION, please treat the values 1,2,3,4,5 as numerical when fitting the models.
# For BIRTH_COHORT, please also treat the values as numerical.



## Approximate the age at visit 4 (the left truncation time)
(1984+1987)/2   # median of visit 4 year
dat_demo_hb$AgeV4 = 1985.5 - (dat_demo_hb$BIRTH_COHORT+5)

sum(dat_demo_hb$LASTCONTAGE <= dat_demo_hb$AgeV4)  #13
dat_demo_hb = dat_demo_hb[dat_demo_hb$AgeV4 < dat_demo_hb$LASTCONTAGE, ]  # remove these 13 subjects
dim(dat_demo_hb)   # 1011 * 13






# censoring rate
1-mean(dat_demo_hb$STATUS)    # 28.78%

