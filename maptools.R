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
       
