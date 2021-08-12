library(GOESDiurnalNDVI)

siteName <- "RussellSage"
lat <- 32.457
long <- -91.9743
year <- 2017
TZ <- 5 #Time zone
day <- 233 #Day of year
dataPath <- "Data" #Folder where the data is located
savePath <- paste(getwd(),"/",sep="")
mVersion <- 3

siteData <- cbind(siteName,lat,long,TZ)

ptm <- proc.time()
calculateNDVI_GOES_MAIN(day,siteData, year, TZ, dataPath,TZ_name="America/New_York", savePath, mVersion)
proc.time() - ptm
#795.047

GOESdat<- read.csv("GOES_NDVI_DiurnalRussellSage_2017233.csv",header=FALSE)

plot(as.numeric(GOESdat[3,]),as.numeric(GOESdat[2,]),pch=20,xlim=c(0,20),ylim=c(0,1),
     ylab="NDVI",xlab="Time (Hour)")

data <- list(x=as.numeric(GOESdat[3,]),y=as.numeric(GOESdat[2,]))

j.model <- createDiurnalModel(siteName=siteName,data=data)

var.burn <- runMCMC_Model(j.model=j.model,variableNames=c("a","c","k","prec"),
                          baseNum=20000,iterSize =10000) #The baseNum and iterSize can be increased/decreased to make the code run faster if you know it will converge easier


modelFitFileName <- paste(savePath,siteName,"_",year,day,"_varBurn.RData",sep="")
save(var.burn,file=modelFitFileName)


load(modelFitFileName)
plotCI(siteName=siteName,year=year,day=day,savePath=savePath)
