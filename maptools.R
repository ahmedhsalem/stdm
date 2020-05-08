library(tmap)
library(tmaptools)
library(ggplot2)
library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(ggpmisc)
xmin<- -0.15279
ymin<- 51.51260
xmax<- -0.12221
ymax<- 51.52266

bboxt <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
tm_shape(LCAPShp, bbox=bboxt)+tm_lines()+tm_scale_bar()+tm_grid(labels.size =0.6)+tm_text("LCAP_ID",size=0.9, remove.overlap = TRUE)
ttm()

hist(LCAPShp@data$SPEED)
data<-LCAPShp@data

#Plotting Functions

focusLCAPIDs<-c(1883,1884,420,423,1576,1593,1613,1616,1412,1413,2112,2364,2363,2173,2318,2433,435,524,437,469,425,463)
focusShapeMatrix<-matrix(,nrow=0,ncol=42)

for(ID in focusLCAPIDs)
  focusShapeMatrix<-rbind(focusShapeMatrix,subset(data,LCAP_ID==ID))

UJTFocusMatrix<-subset(UJT,select=as.character(focusLCAPIDs))
AdjFocusMatrix<-subset(LCAPAdj,select=as.character(focusLCAPIDs))


merge(UJTFocusMatrix,dates)
AnalysisTable<-cbind(dates,UJTFocusMatrix)

avg<-colMeans(UJT[,])

avgfocus<-rowMeans(AnalysisTable[,3:ncol(AnalysisTable)])

rowsbyDate<-aggregate(avgfocus,by=list(Date=AnalysisTable$Date),FUN=max)
rowsbyTime<-aggregate(avgfocus,by=list(Time=AnalysisTable$Time),FUN=max)
#rowsbyWeek<-apply.weekly(avgfocus,mean)
plot(rowsbyDate)
ggplot(rowsbyDate, aes(x=Date, y=x))+geom_point()+geom_smooth(method="lm")+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)         
ggplot(rowsbyTime, aes(x=Time, y=x))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) 

#AutoCorrelationFunctions Temporal
acf(rowsbyDate, lag.max = 30)
acf(rowsbyTime, lag.max = 180)

pacf(rowsbyDate,lag.max=15)
pacf(rowsbyTime, lag.max=50)

#kable(LCAPAdj)
#knn2nb(LCAPAdj)
#graphneigh(LCAPAdj)

#Moran Test

Weights<-mat2listw(LCAPAdj, style="M")
#kable(listw2mat(Weights))

moran.test(x=avg, listw=Weights,zero.policy = TRUE)

#Semi-Variogram
avgfocuscol<-colMeans(UJTFocusMatrix)
a<-LCAPShp@data[,1:2]
b<-ggplot2::fortify(LCAPShp)
b1<-aggregate(b, by = list(OBJECTID=b$id), FUN=mean)
c<-merge(a,b1)
c1<-matrix(,nrow=0,ncol=8)
for(ID in focusLCAPIDs)
  c1<-rbind(c1,subset(c,LCAP_ID==ID))
coords = list(projectMercator(c1[,3], c1[,4]))

plot(variogram(list(avgfocuscol), locations=coords))
       
