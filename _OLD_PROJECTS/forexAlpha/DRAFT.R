pair <- PAIR$Symbol

base <- substr(pair, 1, 3)
quote <- substr(pair, 4, 6)

PAIR <- cbind(PAIR, Base = base, Quote = quote)

PAIR <- subset(PAIR, select = 1:3)

group <- as.factor(c(rep('Major', 7), rep('Minor', 21), rep('Metal', 2)))

PAIR$Group <- group
PAIR$Group[1:7] <- as.factor('Major')
PAIR$Group[8:28] <- 'Minor'
PAIR$Group[29:30] <- 'Metal'
