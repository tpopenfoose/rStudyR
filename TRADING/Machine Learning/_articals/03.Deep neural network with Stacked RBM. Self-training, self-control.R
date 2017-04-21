#-----------------------------------------------------------------------------#
# Title:  Deep neural network with Stacked RBM. Self-training, self-control   #
# Source: https://www.mql5.com/zh/articles/1628                               #
# Date:   2016-03-31                                                          #
# Autor:  Vladimir Perervenko                                                 #
# Packages: c()                                                               #
#-----------------------------------------------------------------------------#

required.packages <- c('magrittr', 'data.table', 'TTR', 'zoo', 'randomUniformForest',
                       'caret', 'rminer', 'labeling', 'darch')
bars <- 6000

suppressPackageStartupMessages({
  sapply(required.packages, require, quietly = FALSE, character.only = TRUE)
})

PRICE <- load('eurusd_m30.rdata') %>% get %>% tail(bars)

#-----------------------------------------------------------------------------
## compatibility fix for ggplot2: partialDependenceOverResponses()
partialDependenceOverResponses <- 
  function (Xtest, importanceObject, whichFeature = NULL, whichOrder = c("first", "second", "all"),
            outliersFilter = FALSE, plotting = TRUE, 
            followIdx = FALSE, maxClasses = if (is.null(whichFeature)) {
              10
            } else {
              max(10, which.is.factor(Xtest[, whichFeature, drop = FALSE], 
                                      count = TRUE))
            }, bg = "lightgrey") {
    FeatureValue = Response = Class = Observations = NULL
    if (!is.null(whichFeature)) {
      if (is.character(whichFeature)) {
        whichFeature = which(colnames(Xtest) == whichFeature)
      }
      if (length(whichFeature) > 1) {
        whichFeature = whichFeature[1]
        cat("Only one variable can be computed at the same time\n")
      }
    }
    if (whichOrder[1] == "first") {
      idxOrder = 2
    }
    if (whichOrder[1] == "second") {
      idxOrder = 3
    }
    if (whichOrder[1] == "all") {
      if (is.matrix(importanceObject$localVariableImportance)) {
        idxOrder = 2:length(grep("localVariableFrequency", 
                                 colnames(importanceObject$localVariableImportance)))
      }
      else {
        idxOrder = 2:length(grep("localVariableFrequency", 
                                 colnames(importanceObject$localVariableImportance$obsVariableImportance)))
      }
    }
    idx = list()
    if (is.matrix(importanceObject$localVariableImportance)) {
      importanceObjectMatrix = importanceObject$localVariableImportance
    }
    else {
      importanceObjectMatrix = importanceObject$localVariableImportance$obsVariableImportance
    }
    if (is.null(whichFeature)) {
      whichFeature = as.numeric(names(which.max(table(importanceObjectMatrix[, 
                                                                             idxOrder[1]]))))
    }
    idx[[1]] = which(importanceObjectMatrix[, idxOrder[1]] == 
                       whichFeature)
    if (length(idxOrder) > 1) {
      for (i in 1:length(idxOrder)) {
        idx[[i + 1]] = which(importanceObjectMatrix[, 1 + 
                                                      idxOrder[i]] == whichFeature)
      }
    }
    partialDependenceMatrix = cbind(Xtest[unlist(idx), whichFeature], 
                                    importanceObjectMatrix[unlist(idx), 1], unlist(idx))
    partialDependenceMatrix = sortMatrix(partialDependenceMatrix, 
                                         1)
    NAIdx = which(is.na(partialDependenceMatrix))
    if (length(NAIdx) > 0) {
      partialDependenceMatrix = partialDependenceMatrix[-NAIdx, 
                                                        ]
    }
    if (outliersFilter & (!is.factor(Xtest[, whichFeature]))) {
      highOutlierIdx = which(partialDependenceMatrix[, 1] > 
                               quantile(partialDependenceMatrix[, 1], 0.95))
      lowOutlierIdx = which(partialDependenceMatrix[, 1] < 
                              quantile(partialDependenceMatrix[, 1], 0.05))
      if (length(highOutlierIdx) > 0 | length(lowOutlierIdx) > 
          0) {
        partialDependenceMatrix = partialDependenceMatrix[-c(lowOutlierIdx, 
                                                             highOutlierIdx), ]
      }
    }
    if (is.vector(partialDependenceMatrix)) {
      stop("Not enough points to plot partial dependencies. Please increase order of interaction when computing importance.")
    }
    if (dim(partialDependenceMatrix)[1] < 10) {
      stop("Not enough points to plot partial dependencies. Please increase order of interaction when computing importance.")
    }
    else {
      idx = partialDependenceMatrix[, 3]
      partialDependenceMatrix = partialDependenceMatrix[, -3]
      flagFactor = 0
      smallLength = length(unique(Xtest[, whichFeature]))
      n = nrow(Xtest)
      if (((smallLength < maxClasses) | is.factor(Xtest[, whichFeature])) & 
          (smallLength/n < 1/5)) {
        featureLevels = levels(as.factor(Xtest[, whichFeature]))
        testNumeric = is.numeric(as.numeric(featureLevels))
        if (testNumeric & length(rmNA(as.numeric(featureLevels))) > 
            0) {
          flagFactor = 1
        } else {
          if (is.matrix(importanceObject$localVariableImportance)) {
            classFeature = unique(partialDependenceMatrix[, 1])
            B = round(as.numeric(partialDependenceMatrix[, 2]), 4)
            A = as.numeric(factor2matrix(partialDependenceMatrix[, 1, drop = FALSE]))
            partialDependenceMatrix = cbind(A, B)
            colnames(partialDependenceMatrix) = c("Class", "Response")
            flagFactor = 1
            valueFeature = unique(A)
            referenceTab = cbind(classFeature, valueFeature)
            colnames(referenceTab) = c("category", "numeric value")
            cat("categorical values have been converted to numeric values :\n")
            print(referenceTab)
            cat("\n")
          }
          else {
            partialDependenceMatrix[, 1] = featureLevels[as.numeric(as.factor(partialDependenceMatrix[, 1]))]
          }
        }
      }
    }
    if (plotting) {
      if (dim(partialDependenceMatrix)[1] < 1) {
        stop("Not enough points to plot partial dependencies. Please increase order of interaction when computing importance.")
      }
      if (is.matrix(importanceObject$localVariableImportance)) {
        if (((smallLength < maxClasses) | is.factor(Xtest[, whichFeature])) & (smallLength/n < 1/5)) {
          A = if (flagFactor) {
            as.factor(partialDependenceMatrix[, 1])
          } else {
            partialDependenceMatrix[, 1]
          }
          B = round(as.numeric(partialDependenceMatrix[, 2]), 4)
          partialDependenceMatrix = data.frame(A, B)
          colnames(partialDependenceMatrix) = c("Class", "Response")
          plot(qplot(Class, Response, data = partialDependenceMatrix, 
                     geom = c("boxplot", "jitter"),# outlier.colour = "green", outlier.size = 2.5,
                     fill = Class, main = "Partial dependence over predictor", 
                     xlab = colnames(Xtest)[whichFeature], ylab = "Response"))
        } else {
          colnames(partialDependenceMatrix) = c("FeatureValue", "Response")
          partialDependenceMatrix = data.frame(partialDependenceMatrix)
          tt <- ggplot(partialDependenceMatrix, aes(x = FeatureValue, 
                                                    y = Response))
          plot(tt + geom_point(colour = "lightblue") + stat_smooth(fill = "green", colour = "darkgreen", 
                                                                   size = 1) + labs(title = "Partial dependence over predictor", 
                                                                                    x = colnames(Xtest)[whichFeature], y = "Response"))
        }
      } else {
        colnames(partialDependenceMatrix) = c("Observations", 
                                              "Class")
        partialDependenceMatrix = data.frame(partialDependenceMatrix)
        variablesNames = unique(partialDependenceMatrix$Class)
        partialDependenceMatrix$Class = factor(partialDependenceMatrix$Class)
        levels(partialDependenceMatrix$Class) = colnames(importanceObject$localVariableImportance$classVariableImportance)[sort(variablesNames)]
        if (((smallLength < maxClasses) | is.factor(Xtest[, whichFeature])) & (smallLength/n < 1/5)) {
          par(las = 1)
          if (bg != "none") 
            par(bg = bg)
          mosaicplot(t(table(partialDependenceMatrix)), 
                     color = sort(heat.colors(length(featureLevels)), 
                                  decreasing = FALSE), border = NA, ylab = colnames(Xtest)[whichFeature], 
                     xlab = "Class", main = "Partial dependence over predictor")
        }
        else {
          plot(qplot(Class, Observations, data = partialDependenceMatrix, 
                     geom = c("boxplot", "jitter"), #outlier.colour = "green", outlier.size = 2.5,
                     fill = Class, main = "Partial dependence over predictor", 
                     xlab = "", ylab = colnames(Xtest)[whichFeature]))
        }
      }
    }
    else {
      if (!is.matrix(importanceObject$localVariableImportance)) {
        colnames(partialDependenceMatrix) = c("Observations", 
                                              "Class")
        partialDependenceMatrix = data.frame(partialDependenceMatrix)
        variablesNames = unique(partialDependenceMatrix$Class)
        partialDependenceMatrix$Class = factor(partialDependenceMatrix$Class)
        levels(partialDependenceMatrix$Class) = colnames(importanceObject$localVariableImportance$classVariableImportance)[sort(variablesNames)]
      }
    }
    if (followIdx) {
      return(list(partialDependenceMatrix = partialDependenceMatrix, 
                  idx = as.numeric(idx)))
    }
    else {
      return(partialDependenceMatrix)
    }
  }

#-----------------------------------------------------------------------------
add.price.factor <- function(price.data.table) {
  price.data.table[, c('TIME', 'MEDIAN', 'DIFF') := list(NULL, (HIGH + LOW) * 0.5, CLOSE - OPEN)]
}

PRICE.with.median.diff <- add.price.factor(PRICE)


#-----------------------------------------------------------------------------
input <- function(price, period = 16) {
  adx <-ADX(price[, .(HIGH, LOW, CLOSE)], n = period) %>% as.data.table %>%
    extract(
      j = .(DX, ADX, oscDX = DIp - DIn)
    )
  aroon <- aroon(price[, .(HIGH, LOW)], n = period) %>% as.data.table %>% extract(j = .(oscillator))
  atr <- ATR(price[, .(HIGH, LOW, CLOSE)], n = period, maType = 'EMA') %>% as.data.table %>%
    extract(j = c(1:2), with = FALSE)
  cci <- CCI(price[, .(HIGH, LOW, CLOSE)], n = period)
  chv <- chaikinVolatility(price[, .(HIGH, LOW)], n = period)
  cmo <- CMO(price[, MEDIAN], n = period)
  macd <- MACD(price[, MEDIAN], 12, 26, 9) %>% as.data.table %>% extract(
    j = .(sign = signal, vsig = signal %>% diff %>% c(NA,.) %>% multiply_by(10))
  )
  rsi <- RSI(price[, MEDIAN], n = period)
  stoh <- stoch(price[, .(HIGH, LOW, CLOSE)], nFastK = period, nFastD =3, nSlowD = 3,
                maType = "EMA")%>% as.data.table %>%
    extract(
      j = .(slowD, oscK = fastK - fastD)
    )
  smi <- SMI(price[, .(HIGH, LOW, CLOSE)],n = period, nFast = 2, nSlow = 25, nSig = 9)
  vol <- volatility(price[, .(OPEN, HIGH, LOW, CLOSE)], n = period, calc = "yang.zhang", N = 144)
  cbind(adx, aroon, atr, cci, chv, cmo, macd, rsi, stoh, smi, vol)
}

INPUT <- input(PRICE.with.median.diff)

#-----------------------------------------------------------------------------
output <- function(price, ch = ch, mode='m') {
  using.price <-
    switch(
      mode,
      'm' = price[, MEDIAN],
      'hl' = price[, .(HIGH, LOW)],
      'c' = price[, CLOSE]
    )
  zz <- ZigZag(using.price, change = ch, percent = FALSE, retrace = FALSE, lastExtreme = TRUE)
  sig <- zz %>% diff %>% c(., NA) %>% na.locf %>% sign
  cbind(zz, sig)
}

OUTPUT <- output(PRICE.with.median.diff, 0.00500)

#-----------------------------------------------------------------------------

DATA <- cbind(INPUT, SIGNAL = OUTPUT[, 2]) %>% na.omit

table(DATA$SIGNAL) %T>% print

descCor <- cor(DATA[, !'SIGNAL', with = FALSE])
summary(descCor[upper.tri(descCor)])

highCor <- caret::findCorrelation(descCor, cutoff = .90) %T>% print
high.cor.names <- colnames(DATA)[highCor] %T>% print

DATA.F <- DATA[SIGNAL != 0, !high.cor.names, with = FALSE]

idx <- rminer::holdout(y = DATA.F$SIGNAL) %T>% print
prep <- caret::preProcess(x = DATA.F[idx$tr, !'SIGNAL', with = FALSE],
                          method = c("center","spatialSign")) %T>% print

x.train <- predict(prep, DATA.F[idx$tr, !'SIGNAL', with = FALSE]) %>% as.data.frame
x.test <- predict(prep, DATA.F[idx$ts, !'SIGNAL', with = FALSE]) %>% as.data.frame
y.train <- DATA.F[idx$tr, SIGNAL] %>% as.factor
y.test <- DATA.F[idx$ts, SIGNAL] %>% as.factor

ruf <- randomUniformForest(X = x.train, Y = y.train, xtest = x.test, ytest = y.test,
                           mtry = 1, ntree = 300, threads = 2, nodesize = 1)

imp.ruf <- importance(ruf, Xtest = x.test, maxInteractions = 3)

best <- imp.ruf$localVariableImportance$classVariableImportance %>% rownames %>% head(10)

best.sell <- partialImportance(X = x.test, imp.ruf, whichClass = "-1", nLocalFeatures = 7) %>% 
  row.names %>% as.numeric %>% colnames(x.test)[.]

best.buy <- partialImportance(X = x.test, imp.ruf, whichClass = "1", nLocalFeatures = 7) %>% 
  row.names %>% as.numeric %>% colnames(x.test)[.]

# balance <- FALSE

####
actFun <- list(
  sig = sigmoidUnit,
  tnh = tanhUnit,
  lin = linearUnit,
  soft = softmaxUnit
) # 7 act fun total

nIn <- length(best)
nOut <- 1
Layers = c(nIn, 2 * nIn , nOut)
Bath = 50
nEp = 20
ncd = 3

####
dbn <- darch(layers = Layers, batchSize = Bath, logLevel = 5, darch.initialMomentum = 0.5, darch.finalMomentum = 0.9,
             darch.momentumRampLength = 0.8 * nEp %>% round, x = x.train, y = y.train, darch.numEpochs = nEp,
             rbm.numCD = ncd)


#### not finished ####
