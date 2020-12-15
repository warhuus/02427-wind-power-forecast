library(MSwM)

mod <- lm(p~1, training_data)
hi <- msmFit(mod, k=2, p=1, sw=rep(TRUE, 3))
