SixAirlinesDataV2 <- read_csv("C:/Users/snskr95/Downloads/SixAirlinesDataV2.csv")

summary(SixAirlinesDataV2)

par(mfrow=c(1,2))

boxplot(SixAirlinesDataV2$SeatsEconomy,xlab='number of economy seats',horizontal = TRUE) # box plot for number of economy seats.


boxplot(SixAirlinesDataV2$SeatsPremium,xlab="Number of premium seats" ,horizontal = TRUE)




plot(SeatsEconomy,SeatsPremium,main="relation between premium and economy seats",data=SixAirlinesDataV2)

par(mfrow=c(2,1))


plot(PitchEconomy,PitchPremium,main="relation between premium and economy seats",data=SixAirlinesDataV2)



plot(WidthEconomy,WidthPremium,main="relation between premium and economy seats",data=SixAirlinesDataV2)


library(corrgram)


corrgram(SixAirlinesDataV2, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="airlines data and their correlations ")




pairs(~SeatsEconomy+SeatsPremium+PitchEconomy+PitchPremium+WidthEconomy+WidthPremium+PitchDifference+WidthDifference,data = SixAirlinesDataV2,
      main = "Scatterplot Matrix")



// To implement the linear regression model we can either choose the price of economy seat or the price of premium seat as y.


// For the below case the price of economy seat is chosen y and it depends on the factors like no. of economy seats, number of premium seats ,pitch ,width.
//here instead of taking pitch and width of two categories their differences are considered.



fit<-lm(PriceEconomy~SeatsEconomy+SeatsPremium+PriceEconomy+PricePremium+PitchDifference+WidthDifference, data=SixAirlinesDataV2)


summary(fit)


