library(RSQLite)

#Connect to the initial database
sql.lite.db <- dbConnect(SQLite(), dbname="NTP_SHORT_TERMDB.sqlite")
dbWriteTable(conn = sql.lite.db, name = 'shortterm',value= df4[,-(23:33)])

#Query the DB for all tables/rows
result <- dbSendQuery(sql.lite.db,"SELECT * FROM shortterm;")
#list the output

QUERY <-  fetch(result)

result <- dbSendQuery(sql.lite.db,"SELECT * 
                                   FROM shortterm
                                   WHERE NTP_TDMS_NUMBER IN ('55301-02','55301-03');")
RAT <-  fetch(result)


result <- dbSendQuery(sql.lite.db,"SELECT * 
                                   FROM shortterm
                                   WHERE NTP_TDMS_NUMBER = '55301-01';")
MOUSE <-  fetch(result)
################################################################
# Show some examples of what you can do
# Take one of the Continuous columns
BF.R = RAT$`Erythrocytes (RBC) (million/microliter)`
# Take one of the Dichotomous Columns
BI.R = RAT$Inflammation

DS.R = RAT$DOSE
DS.R[is.na(DS.R)] = 0
###############################################################
#Fit Logistic Regression To Dichotomous Data
# 
logistic.RAT <- glm(BI.R~DS.R,family = binomial(link='logit'))
###############################################################
#Find A Better way to get the probability of response + Summary STATS
doses <- seq(0,1000,20)
prob  <- 1/(1+exp(2.538-0.005228*doses))
################################################################
#Plot the logistic Estimate
################################################################
plot(doses,prob,type='l',ylab = "Probability of Inflammation",xlab="Dose")
plot(DS.R,BF.R,col=1,pch=16,ylab = "Erythrocytes (RBC)(million/microliter)",xlab="Dose")

