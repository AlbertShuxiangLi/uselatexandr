Wt1 <- paste("Program Started @",format(Sys.time(),"%d%B%Y %H:%M:%S"))
print(Wt1)
workDir <- "/home/albert/simplifiedthesis/data"

## INITIALIZE DATA HOLDER OF MODEL PARAMETERS
f_para <- data.frame()
AR <- 14
MA <- 6
Num <- 4           ## number of flights combined
## Best fit ##
## 1-(12,0,4); 2-(16,0,6); 3-(12,0,4); 4-(14,0,6); 5-(16,0,4); 6-(12,0,2); 7-(16,0,2) ##

combNFli <- paste("Negative_comb_", Num, sep="")
combPFli <- paste("Positive_comb_", Num, sep="")
filezN <- list.files(path=workDir, pattern=combNFli, full.names = TRUE)
filezP <- list.files(path=workDir, pattern=combPFli, full.names = TRUE)

## CURRENT SAMPLE SIZE - NEGATIVE 22; POSITIVE 6 
TurbofanEngine <- data.frame()
for (i in 1:length(filezN)) {
        Dat <- data.frame()
        Dat <- read.csv(filezN[i])
        Dat <- Dat[which(Dat[,2] < 9.999 & Dat[,2] > -9.999), 2]
        Dat <- ts(Dat)
        Dat <- diff(Dat)

## IMPORTANT NOTICE: DO NOT CHANGE THE FOLLOWING ONE LINE OF CODE        
        Dat.fit <- arima(Dat, order = c(AR, 0, MA), optim.method = "Nelder-Mead")
        TurbofanEngine[i,1] <- as.numeric(0)
        print(paste("@ ",format(Sys.time(),"%H:%M:%S")," filezN[",
                    i,"] arima(x,optim.methode='Nelder-Mead',order=c(",
                    AR,",0,",MA,")) Processed!",sep=""))
for (k in 1:(AR + MA)) { TurbofanEngine[i, k + 1] <- as.numeric(Dat.fit$coef[k]) }
}

for (j in 1:length(filezP)) {
        Dat <- data.frame()
        Dat <- read.csv(filezP[j])
        Dat <- Dat[which(Dat[,2] < 9.999 & Dat[,2] > -9.999), 2]
        Dat <- ts(Dat)
        Dat <- diff(Dat)

## IMPORTANT NOTICE: DO NOT CHANGE THE FOLLOWING ONE LINE OF CODE        
        Dat.fit <- arima(Dat, order = c(AR, 0, MA), optim.method = "Nelder-Mead")
        TurbofanEngine[j + length(filezN),1] <- as.numeric(1)
        print(paste("@ ",format(Sys.time(),"%H:%M:%S")," filezP[",
                    j,"] arima(x,optim.methode='Nelder-Mead',order=c(",
                    AR,",0,",MA,")) Processed!",sep=""))
for (k in 1:(AR + MA)) { TurbofanEngine[j + length(filezN), k + 1] <- as.numeric(Dat.fit$coef[k]) }
}

## CONSTRUCT COLUMN NAMES FOR 'TurbofanEngine'
term <- c("EngineStallFault",
          "AR01","AR02","AR03","AR04","AR05","AR06","AR07","AR08",
          "AR09","AR10","AR11","AR12","AR13","AR14","AR15","AR16",
          "AR17","AR18","AR19","AR20",
          "MA01","MA02","MA03","MA04","MA05","MA06")
colnames(TurbofanEngine) <- c(term[1],term[2:(AR+1)],term[22:(MA+21)])

## MAKE LRM MODEL FOR ALL EXPLANATORY VARIABLES
fm1 <- lm(EngineStallFault ~ ., data = TurbofanEngine, na.action = NULL)
x <- summary(fm1)$fstatistic
f_para <- data.frame()

## RETRIEVE MODELING PARAMETERS AS REQUIRED
f_para[1,1] <- paste("arima(",AR,",0,",MA,")",sep="")
f_para[1,2] <- format(summary(fm1)$r.squared,digits=4)
f_para[1,3] <- format(summary(fm1)$adj.r.squared,digits=4)
f_para[1,4] <- format(summary(fm1)$sigma,digits=4)

## CALCULATE THE P-VALUE OF MODEL FROM F-STATISTICS
f_para[1,5] <- format(pf(x[1],x[2],x[3],lower.tail=FALSE),digits=4)
colnames(f_para) <- c("ARIMA","r_sqare","r_sqaure_adj","sigma^2","model_p_value")
library(xtable)
xtable(fm1, caption="LRM Coefficients of arim(12,0,4) for diff(ts(VG(t)))", label="tab:LRMcoef")
xtable(f_para[1,1:5], caption="Model Adequacy Parameters", label="tab:modeladq")

## print(f_para)
para_text <- paste("For ",f_para[1,1],
                    "R_squared = ",f_para[1,2],
                   "; R_squared_adj = ",f_para[1,3],
                   "; Sigma^2 = ",f_para[1,4],
                   "; Model_p-value = ",f_para[1,5],sep="")
print(para_text)
y_hat <- data.frame()
for (y in 1:length(filezN)) {
  y_hat[y,1] <- (-1)*fm1$residuals[y] 
  y_hat[y,2] <- as.numeric(0)
  y_hat[y,3] <- y
}
for (y in (length(filezN) + 1):(length(filezN) + length(filezP))) {
  y_hat[y,1] <- as.numeric(1) - fm1$residuals[y]
  y_hat[y,2] <- as.numeric(1)
  y_hat[y,3] <- y
}
colnames(y_hat) <- c("y_hat","response","data_point")
## TO AUTOMATICALLY SAVE PLOTS PER SCREEN DISPLAY
mypath <- paste("/home/albert/simplifiedthesis/data/LRM_arima(",
                AR,",0,",MA,")_", Num, "-CombinedFlights_",
                format(Sys.time(),"%d%b%Y_%H%M%S"),".png", sep="")
par(mfrow=c(1,1))
plot(y_hat[,3],y_hat[,1], xlab="Data_Points",ylab="y[BLU], y_hat[BLK]",
     main=paste("Linear Regression Model Based On ",f_para[1,1]," For ",
                Num,"-Combined-Flights Data", sep=""),   
     xlim=c(0,30),ylim=c(-0.2,1.2),pch=15,col="black",cex=1.5)
lines(y_hat[,3],y_hat[,1],col="black",lwd=0.1)
points(y_hat[,3],y_hat[,2],pch=16,col="blue",cex=1.5)
lines(y_hat[,3],y_hat[,2], col="blue",lty=2,lwd=0.1)
text(14,0.8,labels=para_text,cex=1.25)
mtext(paste("Plotted ",format(Sys.time(),"%d%b%Y %H:%M:%S")),cex=1.05)
dev.copy(png, file=mypath, width=1680, height=988)
dev.off()

Wt4 <- paste("Program Completed @",format(Sys.time(),"%d%B%Y %H:%M:%S"))
print(Wt4)