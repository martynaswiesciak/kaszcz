library('tseries')

rm(list=ls())
graphics.off()

load("/Users/martynaswiesciak/Desktop/Studia/sem5/KASZCZ/kaszcz/residua.Rdata")

result <- jarque.bera.test(e)