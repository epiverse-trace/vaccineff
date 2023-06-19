library(DOVE)
data("doveData")


nrow(doveData)
names(doveData)
table(doveData$vaccine.status)

# Basic KM model
km_model <- survfit( Surv(doveData$event.time, doveData$event.status) ~ doveData$vaccine.status)
km_model
summary(km_model)

plot(km_model, conf.int = FALSE, 
     xlab =  'Time-to-event (Days)',
     ylab = '% Alive: S(t)',
     main = 'KM-model',
     col = c('red','blue'),
     las = 1,
     linewidth = 2,
     mark.time = TRUE
) 
legend(18, 0.95, legend = c('Unvaccinated', 'Vaccinated'), lty = 1, lwd = 2, col = c('red', 'blue'), bty = ' ', cex = 0.6)