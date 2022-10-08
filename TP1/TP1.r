
identical(pi, log(640320^3)/sqrt(163))

notes <- c(18, 1.5, 9.5, 15.5, 15, 15.5, 0.5, 14.5, 10)
notes <- c(notes, 4)

notes_sup_10 <- notes > 10
length(notes[notes_sup_10])

choix <- notes == floor(notes)
min(notes[choix])

notes2 <- notes - 2



notes2[notes2 < 0] <- 0

ADN <- c("A", "C", "A", "A", "G", "A", "T", "G", "C", "C", "A", "T", "T", "G", "T", "C")
ADN <- factor(ADN)
levels(ADN)
nlevels(ADN)

X <- read.csv("sy02.data")

mean(X[X$correcteur.median == "EG", 'median'])
nrow(X[X$median < X$final,])/nrow(X)
moy <- X[, "median"]
moy
moy_sort <- sort(moy)
moy_sort

breaks = c(0,1,5,10,15,20)
diff(breaks)

plot(X$median, X$final)
boxplot(final ~ correcteur.final, data=X)

stripchart(final ~ correcteur.final, data=X,method = "jitter")
