## Reading and checking of data from KLIM to be used in Advanced time series analysis cex. 4 wind power case
## Reads the data files:
## wind_pow.dep   - power measuremnts
## wind_nwp.dep   - weather forecasts
##
## For the wind power measurements linear interpolation of gaps shorter than or equal to 28 hours is carried out.
## For the Weather forecast data missing data is replaced with older forecast,
## and afterwards linear interpolation of gaps shorter than or equal to 26 hours is carried out.
##
## Output is two files one with the interpolated data and one with the non-interpolated data, each
## containing series to be used for respectively 1,2, and 3 hour forecasting. The wind power series
## is named "p" and the forecasts of, which are older than 1 hour, is called: Windspeed "Ws1",
## Winddirection "Wd1", and Temperature "T1". The forecasts which are all older than 2 hours (i.e.
## to be used as input for 2 hour forecasting) is "Ws2", Wd2", ...
##
## In order to run the script on another computer, a few paths must be altered.
##
## Peder Bacher, pb@imm.dtu.dk, Dec. 2010.

## Start
rm(list=ls())
setwd("/Volumes/GoogleDrive/Mit drev/Matematisk modellering/9. semester/Adv. Time Series/exercises/02427-wind-power-forecast")

##----------------------------------------------------------------
## Control weather to plot
plotIt <- FALSE

##----------------------------------------------------------------
## Function for converting the times in the data files to POSIXct class (Rs time class)
convertToPOSIXct <- function(time)
{
  ## Test
  #time <- P$t
  ## Check the time points
  hour <- (time %% 1E4) / 1E2
  
  ## Day
  day <- floor((time %% 1E6) / 1E4)
  
  ## Month
  month <- floor((time %% 1E8) / 1E6)
  
  ## Year
  year <- floor(time / 1E8)
  
  return(ISOdatetime(year, month, day, hour, 0, 0, tz = "GMT"))
}

## Helping function converting strings to time values
asP <- function(timeStr, tz="GMT", ...)
  {
    as.POSIXct(timeStr,tz=tz, ...)
  }

## Helping function for breaking between many plots
continue <- function() { 
    cat("Hit enter to continue, or s and then enter to stop...")
    x <- readline()
    if(x=="s") return(FALSE)
    else return(TRUE)
    #print(locator(1))
    return(TRUE)
}

## Function for linear interpolation
interpol <- function(x, maxGap)
{  
  D <- data.frame(i=1:length(x), x=x)
  ## Find each gap
  grp <- cumsum(abs(diff(c( is.na(D$x[1]), is.na(D$x) ))))
  
  L <- lapply(split(D, grp), function(y){
    if(length(y$x)<=maxGap & is.na(y$x[1]))
      {
        return(y$i)
      }
    else return(NA)
  })
  sel <- do.call("c",L)
  sel <- sel[!is.na(sel)]
  D$x[sel] <- approx(D$x, xout=sel)$y
  return(D$x)  
}

## Function for plotting around the gaps
plotAroundGaps <- function(D,cln,width,ylimAll=TRUE, i=NA)
  {
    ## Find each gap and plot around it
    grp <- cumsum(abs(diff(c( is.na(D[1,cln]), is.na(D[,cln]) ))))
    grpLen <- sapply(split(D[,cln],grp), length)
    grpNA <- sapply(split(D[,cln],grp), function(x){is.na(x[1])})
    if(is.na(i)) i <- cumsum(grpLen)[grpNA]

    if(plotIt)
      {
        for(ii in 1:length(i))
          {
            X <- D[(i[ii]-width):(i[ii]+width), ]
            if(ylimAll){ ylim <- range(D[,cln], na.rm=TRUE) }else{ ylim <- range(X[,cln], na.rm=TRUE)}
            plot(X$t, X[,cln], type="b", ylim=ylim)
            mtext(paste("Number of NAs in the gap", grpLen[grpNA][ii]), line=-2)
            if(!continue()) break
          }
      }
    invisible(i)
  }

## Function picking forecasts older than k, and replacing NAs with the newest forecasts (also older than k, of course)
replaceNAs <- function(cln,k,NWP)
  {
    ## cln is the column name the replacement
    ## k the prediction horizon, i.e. only forecasts with age below or equal to k is kept
    n <- length(unique(NWP$tPred))
    i <- rep(0:(n-1), each=6)*49 + k + (1:6)
    x <- NWP[i, c("t","toy","tPred",cln)]
    
    isNA <- which(is.na(x[,cln]))
    X <- NWP[NWP$t %in% x[isNA,"t"], c("t","toy","tPred",cln)]
    for(ii in 1:length(isNA))
      {
        if(ii%%100==1) cat("Replacing number ", ii," out of ",length(isNA),"\n")
        ## Find the newest replacement
        y <- X[X$t == x[isNA[ii],"t"], ]
        ## And where the "age" is not below k hours
        age <- as.numeric(y$t - y$tPred, units="hours")
        y <- y[!is.na(y[,cln]) & age>=k, ]
        if(nrow(y) > 0)
          {
            ## Use this one
            x[isNA[ii],] <- y[which.max(y$tPred),]
          }
      }
    
    cat("Number of missing forecast values of",cln,"left:",sum(is.na(x[,cln])),"\n")
    ## Make tPred for the cln, such that the "age" of the forecast can be calculated
    names(x)[names(x)=="tPred"] <- paste("tPred",cln,sep="")
    
    return(x)
  }

##----------------------------------------------------------------
## Read the wind power data
P = read.table("data/data/wind_pow.dep", header=FALSE,col.names=c("t","toy","p"), skip = 12)

P$t <- convertToPOSIXct(P$t)

## Check around summer time and winter time limits, if there are missing values it can be that the data is in CEST
P[asP("1999-03-27 20:00:00") <= P$t & P$t <= asP("1999-03-28 06:00:00"), ]
P[asP("1999-10-30 20:00:00") <= P$t & P$t <= asP("1999-10-31 06:00:00"), ]
## Suspecious with NaNs at the shifts to and from summer time
P[asP("2000-03-25 20:00:00") <= P$t & P$t <= asP("2000-03-26 10:00:00"), ]
P[asP("2000-10-28 20:00:00") <= P$t & P$t <= asP("2000-10-29 10:00:00"), ]
## No problems
P[asP("2001-03-24 20:00:00") <= P$t & P$t <= asP("2001-03-25 10:00:00"), ]
P[asP("2001-10-26 20:00:00") <= P$t & P$t <= asP("2001-10-30 10:00:00"), ]
## Hmm, bigger gap...
P[asP("2002-03-23 20:00:00") <= P$t & P$t <= asP("2002-03-24 10:00:00"), ]
P[asP("2002-10-26 20:00:00") <= P$t & P$t <= asP("2002-10-27 10:00:00"), ]
P[asP("2003-03-29 20:00:00") <= P$t & P$t <= asP("2003-03-30 10:00:00"), ]
## No problems

## Check for missing sample points
plot(P$t, type="l")
plot(diff(P$t), type="l")
unique(diff(P$t))
## No problems.

## See what is in P
summary(P)

## Transform the power to kW
P$p = P$p/1000

## Plot to see the where the NAs are
plot(P$t, is.na(P$p), type="l")

## Plot around the gaps
plotAroundGaps(P,"p",48)

## Seems reasonable to do simple linear interpolation, where the periods with missing
## observations are smaller than 28 hours. This will remove most of the gaps, which
## is ok since this first stage in modelbuilding, i.e. the aim is to find good models,
## and not too much deal to much with operational issues, i.e. computation time and missing values etc.
PNoInterPol <- P
P$p <- interpol(P$p, 28)

## Plot to see the gaps left
plot(P$t, is.na(P$p), type="l")

## Plot around the gaps
plotAroundGaps(P,"p",80)


##----------------------------------------------------------------
# Klim NWP
NWP = read.table("data/data/wind_nwp.dep",skip = 16, header=FALSE, col.names=c("t","toy","tPred","Ws","Wd","T"))
summary(NWP)
nrow(NWP) #37941

## Convert times into POSIXct
NWP$t <- convertToPOSIXct(NWP$t)
NWP$tPred <- convertToPOSIXct(NWP$tPred)

## Check the time between the sample points
unique(diff(NWP$t))
## No problems

## How many unique forecasts are there?
(n <- length(unique(NWP$tPred)))
## Does that match with the number of samples for each forecast?
nrow(NWP)/n # Yes

##----------------------------------------------------------------
## Replace NAs for the each forecast series, with the newest available forecast
## such that the oldest forecasts are 2 hours old
xWs <- replaceNAs("Ws",1,NWP)
xWd <- replaceNAs("Wd",1,NWP)
xT <- replaceNAs("T",1,NWP)
D <- merge(xWs,xWd, all=TRUE)
D <- merge(D,xT, all=TRUE)

names(D)
summary(D)

## Keep the non interpolated data
DNoInterPolate1 <- D

## See if remaining gaps can be filled by interpolation for Wind Speed
sum(is.na(D$Ws))
plot(D$t, is.na(D$Ws), type="l")
plotAroundGaps(D,"Ws",150)
## Fair to do linear interpolate for gaps below or equal to 24 missing samples
D$Ws <- interpol(D$Ws, 24)
plot(D$t, is.na(D$Ws), type="l")

## See if remaining gaps can be filled by interpolation for Wind direction
sum(is.na(D$Wd))
plot(D$t, is.na(D$Wd), type="l")
iGaps <- plotAroundGaps(D,"Wd",150)
## Hmm, problems since it is limited between 0 and 360, so
xOr <- D$Wd
dWd <- diff(D$Wd[!is.na(D$Wd)])
i <- abs(dWd)>200
dWd[i] <- dWd[i] - sign(dWd[i]) * 360
D$Wd[!is.na(D$Wd)] <- c(D$Wd[1],D$Wd[1]+cumsum(dWd))
## See what it looks like
plot(D$Wd, type="l")
plotAroundGaps(D,"Wd",150,FALSE)
## Do the interpolation
D$Wd <- interpol(D$Wd,24)
plot(D$t, is.na(D$Wd), type="l")
## Crop it back between 0 and 360
D$Wd <- round(D$Wd%%360, 1)
D$Wd[D$Wd==360] <- 0
## Check how the interpolation has worked out
plotAroundGaps(D,"Wd",36,FALSE,iGaps)
## Check if anything has gone wrong
plot(D$Wd-xOr, type="l")
unique(D$Wd-xOr)

## See if remaining gaps can be filled by interpolation for Temperature
sum(is.na(D$T))
plot(D$t, is.na(D$T), type="l")
plotAroundGaps(D,"T",150)
## Fair to do linear interpolate for gaps below or equal to 24 missing samples
D$T <- interpol(D$T, 24)
plot(D$t, is.na(D$T), type="l")

## Keep it
D1 <- D


##----------------------------------------------------------------
## Replace NAs for the each forecast series, with the newest available forecast,
## such that the oldest forecasts are 2 hours old
xWs <- replaceNAs("Ws",2,NWP)
xWd <- replaceNAs("Wd",2,NWP)
xT <- replaceNAs("T",2,NWP)
D <- merge(xWs,xWd, all=TRUE)
D <- merge(D,xT, all=TRUE)

## Check the age
range(D$t - D$tPredWs)

## Keep the non interpolated data
DNoInterPolate2 <- D

## See if remaining gaps can be filled by interpolation for Wind Speed
plot(D$t, is.na(D$Ws), type="l")
plotAroundGaps(D,"Ws",150)
## Fair to do linear interpolate for gaps below or equal to 25 missing samples
D$Ws <- interpol(D$Ws, 25)

## See if remaining gaps can be filled by interpolation for Wind direction
plot(D$t, is.na(D$Wd), type="l")
iGaps <- plotAroundGaps(D,"Wd",150)
## Hmm, problems since it is limited between 0 and 360, so
xOr <- D$Wd
dWd <- diff(D$Wd[!is.na(D$Wd)])
i <- abs(dWd)>200
dWd[i] <- dWd[i] - sign(dWd[i]) * 360
D$Wd[!is.na(D$Wd)] <- c(D$Wd[1],D$Wd[1]+cumsum(dWd))
## Do the interpolation
D$Wd <- interpol(D$Wd,25)
## Crop it back between 0 and 360
D$Wd <- round(D$Wd%%360, 1)
D$Wd[D$Wd==360] <- 0
## Check how the interpolation has worked out
plotAroundGaps(D,"Wd",36,FALSE,iGaps)
## Check if anything has gone wrong
unique(D$Wd-xOr)

## See if remaining gaps can be filled by interpolation for Temperature
plotAroundGaps(D,"T",150)
## Fair to do linear interpolate for gaps below or equal to 25 missing samples
D$T <- interpol(D$T, 25)

## Keep it
D2 <- D


##----------------------------------------------------------------
## Replace NAs for the each forecast series, with the newest available forecast,
## such that the oldest forecasts are 3 hours old
xWs <- replaceNAs("Ws",3,NWP)
xWd <- replaceNAs("Wd",3,NWP)
xT <- replaceNAs("T",3,NWP)
D <- merge(xWs,xWd, all=TRUE)
D <- merge(D,xT, all=TRUE)

## Check the age
range(D$t - D$tPredT)

## Keep the non interpolated data
DNoInterPolate3 <- D

## See if remaining gaps can be filled by interpolation for Wind Speed
plot(D$t, is.na(D$Ws), type="l")
plotAroundGaps(D,"Ws",150)
## Fair to do linear interpolate for gaps below or equal to 26 missing samples
D$Ws <- interpol(D$Ws, 26)

## See if remaining gaps can be filled by interpolation for Wind direction
plot(D$t, is.na(D$Wd), type="l")
iGaps <- plotAroundGaps(D,"Wd",150)
## Hmm, problems since it is limited between 0 and 360, so
xOr <- D$Wd
dWd <- diff(D$Wd[!is.na(D$Wd)])
i <- abs(dWd)>200
dWd[i] <- dWd[i] - sign(dWd[i]) * 360
D$Wd[!is.na(D$Wd)] <- c(D$Wd[1],D$Wd[1]+cumsum(dWd))
## Do the interpolation
D$Wd <- interpol(D$Wd,26)
## Crop it back between 0 and 360
D$Wd <- round(D$Wd%%360, 1)
D$Wd[D$Wd==360] <- 0
## Check how the interpolation has worked out
plotAroundGaps(D,"Wd",36,FALSE,iGaps)
## Check if anything has gone wrong
unique(D$Wd-xOr)

## See if remaining gaps can be filled by interpolation for Temperature
plotAroundGaps(D,"T",150)
## Fair to do linear interpolate for gaps below or equal to 26 missing samples
D$T <- interpol(D$T, 26)

## Keep the data
D3 <- D

Ds <- vector(mode="list",length=12 )

D_names <- c('D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10', 'D12')

for (j in 1:12) {
  xWs <- replaceNAs('Ws',j,NWP)
  xWd <- replaceNAs('Wd',j,NWP)
  xT <- replaceNAs('T',j,NWP)
  D <- merge(xWs,xWd, all=TRUE)
  D <- merge(D,xT, all=TRUE)
  
  ## Check the age
  range(D$t - D$tPredT)
  
  ## Keep the non interpolated data
  DNoInterPolate3 <- D
  
  ## See if remaining gaps can be filled by interpolation for Wind Speed
  plot(D$t, is.na(D$Ws), type="l")
  plotAroundGaps(D,"Ws",150)
  ## Fair to do linear interpolate for gaps below or equal to 26 missing samples
  D$Ws <- interpol(D$Ws, 26)
  
  ## See if remaining gaps can be filled by interpolation for Wind direction
  plot(D$t, is.na(D$Wd), type="l")
  iGaps <- plotAroundGaps(D,"Wd",150)
  ## Hmm, problems since it is limited between 0 and 360, so
  xOr <- D$Wd
  dWd <- diff(D$Wd[!is.na(D$Wd)])
  i <- abs(dWd)>200
  dWd[i] <- dWd[i] - sign(dWd[i]) * 360
  D$Wd[!is.na(D$Wd)] <- c(D$Wd[1],D$Wd[1]+cumsum(dWd))
  ## Do the interpolation
  D$Wd <- interpol(D$Wd,26)
  ## Crop it back between 0 and 360
  D$Wd <- round(D$Wd%%360, 1)
  D$Wd[D$Wd==360] <- 0
  ## Check how the interpolation has worked out
  plotAroundGaps(D,"Wd",36,FALSE,iGaps)
  ## Check if anything has gone wrong
  unique(D$Wd-xOr)
  
  ## See if remaining gaps can be filled by interpolation for Temperature
  plotAroundGaps(D,"T",150)
  ## Fair to do linear interpolate for gaps below or equal to 26 missing samples
  D$T <- interpol(D$T, 26)
  
  print('save data')
  
  names(D) <- c('t', 'toy', 'tPredWs', paste('Ws', as.character(j), sep = ''), 'tPredWd',
                paste('Wd', as.character(j), sep = ''), 'tPredWdT', paste('T', as.character(j), sep = ''))
  ## Keep the data
  Ds[[j]] <- D
}

##----------------------------------------------------------------
## Compile the final dataset
## Merge the interpolated data
X <- merge(P,Ds[[1]],by="t")
X <- X[,-4]
names(X)[2] <- "toy"

for (i in 2:12) {
  X <- merge(X,Ds[[i]],by="t")
}

names(X)
X <- X[,c("t","toy.x","p","Ws1","Wd1","T1","Ws2","Wd2","T2","Ws3","Wd3","T3", 
          "Ws4","Wd4","T4", "Ws5","Wd5","T5", "Ws6","Wd6","T6", "Ws7","Wd7","T7",
          "Ws8","Wd8","T8", "Ws9","Wd9","T9", "Ws10","Wd10","T10","Ws11","Wd11","T11",
          "Ws12","Wd12","T12")]
names(X) <- c("t","toy","p","Ws1","Wd1","T1","Ws2","Wd2","T2","Ws3","Wd3","T3", 
              "Ws4","Wd4","T4", "Ws5","Wd5","T5", "Ws6","Wd6","T6", "Ws7","Wd7","T7",
              "Ws8","Wd8","T8", "Ws9","Wd9","T9", "Ws10","Wd10","T10","Ws11","Wd11","T11",
              "Ws12","Wd12","T12")
summary(X)

## Write the data file
write.table(X, "data/data/cex4WindDataInterpolated12Hours.csv", sep=",", row.names=FALSE)

## See the gaps left
plot(X$t, apply(X,1,function(x){ any(is.na(x)) }), type="l")


##----------------------------------------------------------------
## Compile the final non interpolated dataset
## Merge the interpolated data
names(DNoInterPolate1)
X <- merge(P,DNoInterPolate1,by="t")
names(X)
X <- X[,-4]
names(X)[2] <- "toy"
X <- merge(X,DNoInterPolate2,by="t")
X <- merge(X,DNoInterPolate3,by="t")
names(X)
X <- X[,c("t","toy.x","p","Ws.x","Wd.x","T.x","Ws.y","Wd.y","T.y","Ws","Wd","T")]
names(X) <- c("t","toy","p","Ws1","Wd1","T1","Ws2","Wd2","T2","Ws3","Wd3","T3")
summary(X)

write.table(X, "~/courses/ats02427/computerExercise4Wind/cex4WindDataNoInterpolation.csv", sep=",", row.names=FALSE)

## See the gaps left
plot(X$t, apply(X,1,function(x){ any(is.na(x)) }), type="l")

