NET <- read.csv("~/R/NetNetSales.csv", header=T)
library(psych)
library(TSA)
library(vars)
library(fUnitRoots)
library(FactoMineR)
library(astsa)
library(strucchange)

#PRINCIPAL COMPONENT ANALYSIS
{
     #Generate PCA Matrix
     pcamat <- data.frame(
          NET[["BRENT"]], NET[["MTL_INDX"]], NET[["RUS2000"]], NET[["PHLX"]], 
          NET[["NASDAQ"]], NET[["PMI"]], NET[["DURGOOD"]], NET[["RTWD_INDX"]], 
          NET[["TED"]], NET[["NIKKEI"]], NET[["SHANGHAI"]], NET[["FTSE100"]], 
          NET[["DAX"]], NET[["CAC40"]], NET[["FTSEMIB"]], NET[["IBEX35"]], 
          NET[["CONSSENT_4m"]], NET[["USGDP_REAL"]])
     
     #Conduct Diagnostic Tests
     PCAcor <- cor(pcamat)
     cortest.bartlett(PCAcor, n=156) #Null Hypothesis: No Inter-Correlations
     det(PCAcor) #Determinant < 0.00001 implies extreme multicollinearity
     KMO(PCAcor) #Overall MSA should be > 0.5
     
     #If problems exist, adjust PCA matrix accordingly
     pcamat <- data.frame(
          NET[["BRENT"]], NET[["MTL_INDX"]], NET[["RUS2000"]], NET[["PHLX"]], 
          NET[["NASDAQ"]], NET[["DURGOOD"]],
          NET[["CAC40"]], NET[["NIKKEI"]] )
     
     PCAcor <- cor(pcamat)
     cortest.bartlett(PCAcor, n=156) #Null Hypothesis: No Inter-Correlations
     det(PCAcor) #Determinant < 0.00001 implies extreme multicollinearity
     KMO(PCAcor) #Overall MSA should be > 0.5
     
     #Estimate first component extractions          
     estim_ncp(pcamat, ncp.min = 0, ncp.max = NULL, scale = T, method = "GCV")
     estim_ncp(pcamat, ncp.min = 0, ncp.max = NULL, scale = T, method = "Smooth")
     
     #Extract Components - No Rotation Initially
     PCA <- principal(pcamat, nfactors = 3, residuals = F, rotate = "none", 
                      covar = F, scores = T, oblique.scores = T, method = "tenBerge")
     
     print.psych(PCA, cut = 0.3, sort = T)
     
     #Scree Plot 
     scree(pcamat, factors = F, pc=T, hline = 1)
     
     #Utilize 2 Principal Components and extract scores
     PCA <- principal(pcamat, nfactors = 2, residuals = F, rotate = "oblimin", 
                      covar = F, scores = T, oblique.scores = T, method = "tenBerge")
     print.psych(PCA, cut = 0.3, sort = T)
     
     us <- PCA$scores[,1]
     globe <- PCA$scores[,2]
}

## GLOBAL CERAMIC NET-NET SALES
{
#UNIT ROOT TESTING
{
     ts(NET, frequency =12, start=c(2001,4))
     
     #Ceramic Business Group Sales
     plot(NET[["CBG_000"]], type="o")
     pacf(NET[["CBG_000"]])
     acf(NET[["CBG_000"]])
     urersTest(NET[["CBG_000"]], type="DF-GLS", model="trend", lag.max=4, doplot=T)
     urkpssTest(NET[["CBG_000"]], type="tau", lags = "short", use.lag = 4, doplot= T)
     #Unit Root Present
     
     #Global Semiconductor Sales
     plot(NET[["SEMISALES"]], type="o")
     pacf(NET[["SEMISALES"]])
     acf(NET[["SEMISALES"]])
     urersTest(NET[["SEMISALES"]], type="DF-GLS", model="trend", lag.max=5, doplot=T)
     urkpssTest(NET[["SEMISALES"]], type="tau", lags = "short", use.lag = 5, doplot= T)
     #Unit Root Present
     
     #Global Ceramic Capacitor Sales
     plot(NET[["W_CBG"]], type="o")
     pacf(NET[["W_CBG"]], na.action=na.omit)
     acf(NET[["W_CBG"]], na.action=na.omit)
     urersTest(NET[["W_CBG"]], type="DF-GLS", model="trend", lag.max=4, doplot=T)
     urkpssTest(NET[["W_CBG"]], type="tau", lags = "short", use.lag = 4, doplot= T)
     #Tests confirm stationarity
     #Utilize Zivot-Andrews Test
     urzaTest(NET[["W_CBG"]], model= c("both"), lag=4, doplot=T)
     #Unit-Root Present.  Take first difference
     
     #US Economic Conditions
     plot(us, type="o")
     pacf(us, na.action=na.omit)
     acf(us, na.action=na.omit)
     urersTest(us, type="DF-GLS", model="trend", lag.max=3, doplot=T)
     urkpssTest(us, type="tau", lags = "short", use.lag = 3, doplot= T)
     #Tests confirm stationarity
     #Utilize Zivot-Andrews Test
     urzaTest(us, model= c("both"), lag=3, doplot=T)
     #Unit-Root Not Present.  Caution in VAR Models
     
     #Global Economic Conditions
     plot(globe, type="o")
     pacf(globe, na.action=na.omit)
     acf(globe, na.action=na.omit)
     urersTest(globe, type="DF-GLS", model="constant", lag.max=2, doplot=T)
     urkpssTest(globe, type="mu", lags = "short", use.lag = 2, doplot= T)
     #Unit Root Present
}

#VAR MODELING

#Generate Variables
{
     cbg <-NET[["CBG_000"]]
     D_cbg <- diff(cbg, differences = 1)
     semi <- NET[["SEMISALES"]]
     D_semi <- diff(semi, differences = 1)
     wcbg <- NET[["W_CBG"]]
     D_wcbg <- diff(wcbg, differences = 1)
     D_us <- diff(us, differences = 1)
     D_globe <- diff(globe, differences = 1)
     m<-NET[["m"]]
     y<-NET[["y"]] 
}

#Cross-Correlations
{
     ccf(cbg,cbg)
     ccf(semi,cbg)
     ccf(wcbg,cbg, na.action = na.omit)
     ccf(us, cbg)
     ccf(globe, cbg)
     
     ccf(D_cbg,D_cbg)
     ccf(D_semi, D_cbg)
     ccf(D_wcbg, D_cbg, na.action = na.omit)
     ccf(D_us, D_cbg)
     ccf(D_globe, D_cbg)
}

#Exogenous Variables
{
     jan <- ifelse(NET[["m"]]==1,1,0)
     feb <- ifelse(NET[["m"]]==2,1,0)
     mar <- ifelse(NET[["m"]]==3,1,0)
     apr <- ifelse(NET[["m"]]==4,1,0)
     may <- ifelse(NET[["m"]]==5,1,0)
     jun <- ifelse(NET[["m"]]==6,1,0)
     jul <- ifelse(NET[["m"]]==7,1,0)
     aug <- ifelse(NET[["m"]]==8,1,0)
     sep <- ifelse(NET[["m"]]==9,1,0)
     oct <- ifelse(NET[["m"]]==10,1,0)
     nov <- ifelse(NET[["m"]]==11,1,0)
     dec <- ifelse(NET[["m"]]==12,1,0)
     orsale <- NET[["OR_SALE_000"]]
     D_orsale <- diff(NET[["OR_SALE_000"]], differences=1)
     exog <- data.frame(mar, apr, jul, aug, nov)
     exog<- data.frame(cbind(exog[2:156,],D_orsale))
}

#Cointigration Testing
{
     VAR<- data.frame(cbg,semi,wcbg)
     VARselect(VAR[1:154,],lag.max = 15, type = "trend", exogen = exog[1:154,])
     summary(ca.jo(VAR, type = "trace", ecdet = "trend", K = 6, spec = "longrun"))
     #Variables not Cointegrated
}

#VAR MOdel in Differences
DVAR <- data.frame(D_cbg, D_semi, D_wcbg)
VARselect(DVAR[1:143,], type="const", lag.max=14, exogen = exog[1:143,])
var <- VAR(DVAR[1:143,], type="const", p=5, exogen = exog[1:143,])
res <-resid(var)
summary(var)


##Diagnostic Tests - Serial Correlation
serVar1 <- serial.test(var, lags.pt = 24, type = "PT.asymptotic")
serVar1$serial

##Diagnostics Tests - Normality of Residuals
normVar1 <- normality.test(var)
normVar1$jb.mul

##Diagnostics Tests - ARCH test for Heteroskedasticity
archVar1 <- arch.test(var, lags.multi = 12)
archVar1$arch.mul
plot(archVar1)
plot(stability(var), nc=1)

##Diagnostic Tests - Stability Test
stability(var, type= c("Rec-CUSUM"), h=0.15, dynamic = T, rescale = T)
plot(stability(var, type= c("Rec-CUSUM")), nc=2)
roots(var)


summary(var)
restrict <- matrix (c(1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,1,1,0,0,0,1,0,
                      1,1,1,0,0,0,1,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,
                      0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0),
                    nrow = 3, ncol = 22, byrow = T)

var1_restrict <- coef(restrict(var, method ="man", resmat = restrict))
var1_restrict

#Restricted VAR Expost     
expostrestrict <- predict(restrict(var, method ="man", resmat = restrict), n.ahead = 12, ci=.95, dumvar=exog[144:155,])     
print(expostrestrict)
#Create single matrix for ExPost Data and Forecasts
post_fcast <- expostrestrict$fcst$D_cbg
test_cbg <- data.matrix(D_cbg[144:155],)
evaldata <- cbind(test_cbg, post_fcast)
View(evaldata)

}


## GLOBAL TANTALUM NET-NET SALES
{
     #Unit Root testing
     {
          #Global Tantalum Sales
          plot(NET[["TBG_000"]], type="o")                  
          acf(NET[["TBG_000"]])
          pacf(NET[["TBG_000"]])
          urersTest(NET[["TBG_000"]], type="DF-GLS", model="const", lag.max=4, doplot=T)
          urkpssTest(NET[["TBG_000"]], type="mu", lags="short", use.lag=4, doplot=T)
          #Unit Root Present - Take first difference
     
          #Tantalum Orders Received
          plot(NET[["TBG_ORSALE_000"]], type="o")                  
          acf(NET[["TBG_ORSALE_000"]])
          pacf(NET[["TBG_ORSALE_000"]])
          urersTest(NET[["TBG_ORSALE_000"]], type="DF-GLS", model="const", lag.max=2, doplot=T)
          urkpssTest(NET[["TBG_ORSALE_000"]], type="mu", lags="short", use.lag=2, doplot=T)
          #Unit Root Present - Take first difference
     }
     
     #Vector Autoregressive Models
          
          #Generate Variables
          {
          D_tbg <- diff(NET[["TBG_000"]],differences = 1)
          D_semi <- diff(NET[["SEMISALES"]],differences = 1)
          D_wtbg <- diff(NET[["W_TBG"]],differences = 1)
          D_us <- diff(us, differences = 1)
          D_globe <- diff(globe, differences = 1)
          VAR_tbg <-data.frame(D_tbg[1:153], 
                              D_semi[1:153]) 
          ts(VAR_tbg, frequenc =12, start=c(2001,5))
          jan <- ifelse(NET[["m"]]==1,1,0)
          feb <- ifelse(NET[["m"]]==2,1,0)
          mar <- ifelse(NET[["m"]]==3,1,0)
          apr <- ifelse(NET[["m"]]==4,1,0)
          may <- ifelse(NET[["m"]]==5,1,0)
          jun <- ifelse(NET[["m"]]==6,1,0)
          jul <- ifelse(NET[["m"]]==7,1,0)
          aug <- ifelse(NET[["m"]]==8,1,0)
          sep <- ifelse(NET[["m"]]==9,1,0)
          oct <- ifelse(NET[["m"]]==10,1,0)
          nov <- ifelse(NET[["m"]]==11,1,0)
          dec <- ifelse(NET[["m"]]==12,1,0)
          evora <- ifelse(NET[["evora"]]==1,1,0)
          recession <- ifelse(NET[["US_RECESSION"]]==1,1,0)
          D_tbgorsale <- diff(NET[["TBG_ORSALE"]], differences=1)
          exog <- data.frame(mar[2:154], apr[2:154])
          #exog<- data.frame(cbind(exog[2:156,],D_tbgorsale))
          #exog <- exog[1:153,]
          }
     
     #ExPost Model and Forecast
     {
     VARselect(VAR_tbg[1:141,], type="const", lag.max = 15, exogen=exog[1:141,])

     tbg_var <- VAR(VAR_tbg[1:141,], type="const", p=5, exogen=exog[1:141,])
     summary(tbg_var)
     res <- resid(tbg_var)
     hist(res[,1], breaks = 20)
     plot(tbg_var, names="D_tbg")
     
     ##Diagnostic Tests - Serial Correlation
     serVar1 <- serial.test(tbg_var, lags.pt = 12, type = "PT.asymptotic")
     serVar1$serial
     
     ##Diagnostics Tests - Normality of Residuals
     normVar1 <- normality.test(tbg_var)
     normVar1$jb.mul
     
     ##Diagnostics Tests - ARCH test for Heteroskedasticity
     archVar1 <- arch.test(tbg_var, lags.multi = 12)
     archVar1$arch.mul
     plot(archVar1, names ="D_tbg")
     plot(stability(tbg_var), nc=1)
     
     ##Diagnostic Tests - Stability Test
     stability(tbg_var, type= c("Rec-CUSUM"), h=0.15, dynamic = T, rescale = T)
     plot(stability(tbg_var, type= c("Rec-CUSUM")), nc=2)
     roots(tbg_var)
     
               ##EX-POST FORECASTS
               
               ##Separate models based on Training and Testing Data
               ##Create training set
               expost <- predict(tbg_var, n.ahead=12, ci=0.95, dumvar=exog[142:153,])
               plot(expost, names= "D_tbg")
               print(expost)
               #Create single matrix for ExPost Data and Forecasts
               post_fcast <- expost$fcst$D_tbg
               test_tbg <- data.matrix(D_tbg[142:153],)
               evaldata <- cbind(test_tbg, post_fcast)
               View(evaldata)
     
     }
     
     
     #ExAnte Model and Forecasts
     {
          VARselect(VAR_tbg, type="const", lag.max = 15, exogen=exog)
          
          tbg_var <- VAR(VAR_tbg, type="const", p=5, exogen=exog)
          summary(tbg_var)
          res <- resid(tbg_var)
          hist(res[,1], breaks = 20)
          plot(tbg_var, names="D_tbg")
          
          ##Diagnostic Tests - Serial Correlation
          serVar1 <- serial.test(tbg_var, lags.pt = 12, type = "PT.asymptotic")
          serVar1$serial
          
          ##Diagnostics Tests - Normality of Residuals
          normVar1 <- normality.test(tbg_var)
          normVar1$jb.mul
          
          ##Diagnostics Tests - ARCH test for Heteroskedasticity
          archVar1 <- arch.test(tbg_var, lags.multi = 12)
          archVar1$arch.mul
          plot(archVar1, names ="D_tbg")
          plot(stability(tbg_var), nc=1)
          
          ##Diagnostic Tests - Stability Test
          stability(tbg_var, type= c("Rec-CUSUM"), h=0.15, dynamic = T, rescale = T)
          plot(stability(tbg_var, type= c("Rec-CUSUM")), nc=2)
          roots(tbg_var)    
     }
     
}






export <- data.frame(y,m,cbg,semi,wcbg,us,globe,orsale,jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)


#Export
