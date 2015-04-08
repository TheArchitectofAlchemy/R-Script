require(RODBC)
MVPS <- odbcConnect("MVPS", uid="kuhns", pwd="!974L3dz3p") # connect to sql must be admin in ODBC connect on comp

list(MVPS) # displays connection
sqlTables(MVPS) # retrieve list of tables in sql dbase
sqlFetch(MVPS, "tblPlayers") # retrieve contents of a sql table

sqlFetchMore(MVPS, "tblPlayers") #this function is not working at this point

Players <- sqlQuery(MVPS, "SELECT * FROM tblPlayers", stringsAsFactors=FALSE) #call table to query
Workouts <- sqlQuery(MVPS, "SELECT * FROM vw_PlayerMeasureables", stringsAsFactors=FALSE) #copy of above
head(Players) #retrieve first few results
list(Players) #prints the table
names(Players) #retrives column header names

longQuery <- "SELECT * FROM tblPlayers, vw_PlayerMeasureables WHERE tblPlayers.vikingID = vw_PlayerMeasurables.vikingID" # join two tables on a common column 
joinPW <- sqlQuery(MVPS, longQuery, stringAsFactors=FALSE) # view of the joined tables not currently working

joinEx <- sqldf("SELECT vikingID FROM tblPlayers JOIN vw_PlayerMeasurables USING(vikingID)")

AvgDraftRd <- mean(Players$10.Yd, na.rm=TRUE)

OTrpart <- rpart(High ~ Fly20 + Ht+ Wt + Hand + Arm + Fat.pct + X3Cone + Front + Back + Fat.lbs + BJ + X10Y + SS20Y + Groin + VJ + X10Y + X20Y + X40Y + Vk.Grd, data = OT)

getwd()
# my directory

setwd("c:/data")
# my new directory

list.files()
# list files in directory

search()
# calls packages & tools

View(OG)
# allows to see data frame of called "object"

# <- gets operator or assignment symbol
# ; operator to run next part of an argument 
# print function displays the called "object"

OG <- read.table("C:/data/OG.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)

OG$High = ifelse(OG$YrsStart > 2, "Yes", "No")
# example of if else to create a new colum resulting in a binary output

OGArpart <- rpart(High ~ a3C + aBJ +aVJ + aSS20 + a60Y + a10Y + a20Y + aF20 + a40Y, data = OG)

OGSrpart <- rpart(High ~ mHt+ mWt + +mWing + mHand + mArm + mFatPct + mFattMass + mBShoulder + 
                   + mFShoulder + mHamstring + mGroin +aReps, data = OG)

OGMrpart <- rpart(High ~ pWonderlic + pMatricies + pMentalQuickness + pDedication + pSelfEfficacy + 
                    pFocus + pOverallHRT + pSocialMaturity + pInterpersonalStyle + pReceptivityCoaching +
                    + pAffectiveCommitment + pCombativeAttitude + pOverallSigma + pDepression +
                    pBipolar + PAnxietyDisorder + pAlcoholAbuse + pADHD + pLearningDisability, data = OG)

OGTrpart <- rpart(High ~ a3C + aBJ +aVJ + aSS20 + a60Y + a10Y + a20Y + aF20 + a40Y +
                   pWonderlic + pMatricies + pMentalQuickness + pDedication + pSelfEfficacy +
                   pFocus + pOverallHRT + pSocialMaturity + pInterpersonalStyle + pReceptivityCoaching +
                   + pAffectiveCommitment + pCombativeAttitude + pOverallSigma + pDepression +
                   pBipolar + PAnxietyDisorder + pAlcoholAbuse + pADHD + pLearningDisability +
                   mHt+ mWt + +mWing + mHand + mArm + mFatPct + mFattMass + mBShoulder + 
                   mFShoulder + mHamstring + mGroin +aReps, data = OG)

OGTree <- rpart.plot(OGTrpart, type = 4, extra = 6, digits = 4) #decision tree must require rpart.plot

charplot(OG$mWt, OG$Xa40Y, 16, "Red") #function not working at this point cannot find package for function

na.rm=TRUE

#Bootstrap

trees <- vector(mode = "list", length = 100)
n <- nrow(HB)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
str(HB)
mod <- rpart(High ~ Fly20 + Ht+ Wt + Hand + Arm + Fat.pct + X3Cone + Front + Back + Fat.lbs + BJ + X10Y + SS20Y + Groin + VJ + X10Y + X20Y + X40Y, data = HB)
control = rpart.control(xval = 0)
for (i in 1:length(trees))
trees[[i]] <- update(mod, weights = bootsamples[, i])
table(sapply(trees, function(x) as.character(x$frame$var[1])))

require(partykit)
plot(as.party(OGrpart), tp_args = list(id = FALSE))
print(HBrpart$cptable)

# color plot two variables break on a 3rd indicator with color value
> plot(OG$aSS20, OG$aF20)
> plot(OG$aSS20, OG$aF20, pch=16, col=ifelse(OG$YrsStart > 3, "blue", "red"))