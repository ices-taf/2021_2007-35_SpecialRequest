minus9toNA <- function(x){
    for(i in 1:ncol(x)){
        if(is.numeric(x[,i])){
            is9 <- which(abs(x[,i]+9)<1e-14)
            if(length(is9)>0) x[,i][is9] <- NA
        }
    }
    return(x)
}


## more robust version of getDATRAS  from the icesDatras package
getDATRAS_rob <- function (record = "HH", survey, years, quarters)
{
    if (!record %in% c("HH", "HL", "CA")) {
        message("Please specify record type:", "\n\t\tHH (haul data)",
            "\n\t\tHL (length-based data)", "\n\t\tCA (age-based data)")
        return(FALSE)
    }
    if (!checkSurveyOK(survey))
        return(FALSE)
    available_years <- getSurveyYearList(survey)
    available_years_req <- intersect(years, available_years)
    if (length(available_years_req) == 0) {
        message("Supplied years (", paste(years, collapse = ", "),
            ") are not available.\n  Available options are:\n",
            paste(capture.output(print(available_years)), collapse = "\n"))
        return(FALSE)
    }
    else if (length(available_years_req) < length(years)) {
        message("Some supplied years (", paste(setdiff(years,
            available_years), collapse = ", "), ") are not available.")
    }
    mat <- sapply(as.character(available_years_req), function(y) getSurveyYearQuarterList(survey,
        as.integer(y)), simplify = FALSE)
    mat <- sapply(mat, function(x) as.integer(1:4 %in% x))
    row.names(mat) <- 1:4
    if (sum(mat[quarters, ]) == 0) {
        message("Supplied quarters (", paste(quarters, collapse = ", "),
            ") are not available.\n  Available options are:\n",
            paste(capture.output(print(mat)), collapse = "\n"))
        return(FALSE)
    }
    else if (sum(mat[quarters, ] == 0) > 0) {
        message("Some supplied quarter and year combinations are not available.")
    }
    amat <- mat[quarters, , drop = FALSE]
    qvec <- quarters[row(amat)[amat == 1]]
    yvec <- available_years_req[col(amat)[amat == 1]]
    message("Data being extracted for:\n", paste(capture.output(print(cbind.data.frame(survey = survey,
        year = yvec, quarter = qvec))), collapse = "\n"))
    url <- sprintf("https://datras.ices.dk/WebServices/DATRASWebService.asmx/get%sdata?survey=%s&year=%i&quarter=%i",
        record, survey, yvec, qvec)
    out <- lapply(url, function(x) {
        x <- icesDatras:::readDatras(x)
        icesDatras:::parseDatras(x)
    })
    ind <- which(sapply(out, function(x) any(names(x) != names(out[[1]]))))
    if(length(ind) > 0){
        print(paste0("Problem with following column names: "))
        for(i in 1:length(ind)){
            ind2 <- which(colnames(out[[1]]) != colnames(out[[ind[i]]]))
            print(cbind(colnames(out[[1]])[ind2], colnames(out[[ind[i]]])[ind2]))
            colnames(out[[ind[i]]]) <-  colnames(out[[1]])
        }
        print(paste0("Overwriting column names!"))
    }
    out <- do.call(rbind, out)
    out
}


## adjusting getGrid from SurveyIndex to run with not-DATRASraw
getGrid_df <- function(dd, nLon = 20, opt = FALSE, gridSize = 20){
    getBps <- function(labs) {
        cbind(lower = as.numeric(sub("\\((.+),.*", "\\1", labs)),
              upper = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1",
                                     labs)))
    }
    mlon = mean(dd$Lon)
    mlat = mean(dd$Lat)
    kmPerDegLon = surveyIndex:::gcd.hf(surveyIndex:::deg2rad(mlon), surveyIndex:::deg2rad(mlat),
                                       surveyIndex:::deg2rad(mlon + 1), surveyIndex:::deg2rad(mlat))
    kmPerDegLat = surveyIndex:::gcd.hf(surveyIndex:::deg2rad(mlon), surveyIndex:::deg2rad(mlat),
                                       surveyIndex:::deg2rad(mlon), surveyIndex:::deg2rad(mlat + 1))

    if(opt){
        fn <- function(x, dd, gridSize){
            lonf = cut(dd$Lon, x, dig.lab = 8)
            lonbps = getBps(levels(lonf))
            gridsize = diff(lonbps[1, ]) * kmPerDegLon
            nLat = round(diff(range(dd$Lat)) * kmPerDegLat/gridsize)
            latf = cut(dd$Lat, nLat, , dig.lab = 8)
            dd$StatRec2 = as.factor(paste(lonf, latf))
            latbps = getBps(levels(latf))
            ##
            (gridSize - mean(c(diff(lonbps[1, ]) * kmPerDegLon, diff(latbps[1, ]) * kmPerDegLat)))^2
        }
        opti <- optimise(fn, interval = c(1,200), dd = dd, gridSize = gridSize)
        nLon <- round(opti$minimum,20)
    }

    lonf = cut(dd$Lon, nLon, dig.lab = 8)
    lonbps = getBps(levels(lonf))
    gridsize = diff(lonbps[1, ]) * kmPerDegLon
    nLat = round(diff(range(dd$Lat)) * kmPerDegLat/gridsize)
    latf = cut(dd$Lat, nLat, , dig.lab = 8)
    dd$StatRec2 = as.factor(paste(lonf, latf))
    latbps = getBps(levels(latf))
    cat("Approximate grid size: ", mean(c(diff(lonbps[1, ]) *
                                          kmPerDegLon, diff(latbps[1, ]) * kmPerDegLat)), " km\n")
    uRecs = unique(as.character(dd$StatRec2))
    N = length(uRecs)
    mylon = numeric(N)
    mylat = numeric(N)
    myids = numeric(N)
    k = 0
    for (rec in uRecs) {
        k = k + 1
        tmp = subset(dd, StatRec2 == rec) ## HERE:
        mlon = mean(tmp$Lon)
        mlat = mean(tmp$Lat)
        dist = sqrt((mlon - tmp$Lon)^2 + (mlat - tmp$Lat)^2)
        sel = which.min(dist)
        mylon[k] = tmp$Lon[sel]
        mylat[k] = tmp$Lat[sel]
        myids[k] = as.character(tmp$haul.id[sel])
    }
    ret <- list(mylon, mylat, myids, lonbps, latbps)
    class(ret) <- "surveyIndexGrid"
    return(ret)
}


## Adjusting getSurveyIdxStratMean
getSurveyIdxStratMean_df <- function (x, ageCols, doLog = FALSE)
{
    x <- x[order(x$Year),] ## not really necessary, ysplit seems to order too
    ysplit = split(x, x$Year)
    res = matrix(NA, nrow = length(ysplit), ncol = length(ageCols))
    for (y in 1:length(ysplit)) {
        if (!doLog) {
            byRec = aggregate(ysplit[[y]]$Nage[, ageCols],
                by = list(ysplit[[y]]$StatRec), FUN = "mean")
        }
        else {
            byRec = aggregate(log(ysplit[[y]]$Nage[, ageCols] +
                1), by = list(ysplit[[y]]$StatRec), FUN = "mean")
        }
        if(length(ageCols) > 1){
            res[y, ] = colMeans(byRec[, -1])
        }else{
            res[y, ] = mean(byRec[, -1])
        }
    }
    years <- seq(min(as.numeric(as.character(x$Year))),
                 max(as.numeric(as.character(x$Year))), 1)
    ret <- data.frame(year = years, stratMean = NA)
    ret$stratMean[ret$year %in% sort(unique(as.numeric(as.character(x$Year))))] <- res
    return(ret)
}



## adjusting getSurveyIdx
getSurveyIdx_df <- function (x, ages, myids, kvecP = rep(12 * 12, length(ages)),
    kvecZ = rep(8 * 8, length(ages)), gamma = 1.4, cutOff = 1,
    fam = "Gamma", useBIC = FALSE, nBoot = 1000, mc.cores = 1,
    method = "ML", predD = NULL, modelZ = rep("Year+s(lon,lat,k=kvecZ[a],bs='ts')+s(Ship,bs='re',by=dum)+s(Depth,bs='ts')+s(TimeShotHour,bs='cc')",
        length(ages)), modelP = rep("Year+s(lon,lat,k=kvecP[a],bs='ts')+s(Ship,bs='re',by=dum)+s(Depth,bs='ts')+s(TimeShotHour,bs='cc')",
        length(ages)), knotsP = NULL, knotsZ = NULL, predfix = NULL,
    linkZ = "logit", CIlevel = 0.95, ...)
{
    if (is.null(x$Nage))
        stop("No age matrix 'Nage' found.")
    if (is.null(colnames(x$Nage)))
        stop("No colnames found on 'Nage' matrix.")
    if (length(modelP) < length(ages))
        stop(" length(modelP) < length(ages)")
    if (length(kvecP) < length(ages))
        stop(" length(kvecP) < length(ages)")
    if (fam[1] != "Tweedie") {
        if (length(modelZ) < length(ages))
            stop(" length(modelZ) < length(ages)")
        if (length(kvecZ) < length(ages))
            stop(" length(kvecZ) < length(ages)")
    }
    stopifnot(fam[1] %in% c("Gamma", "LogNormal", "Tweedie",
        "negbin"))
    if (length(fam) < length(ages)) {
        famVec = rep(fam[1], length(ages))
    }
    else famVec = fam
    dataAges <- as.numeric(gsub("[+]", "", colnames(x$Nage)))
    if (!all(ages %in% dataAges))
        stop(paste0("age(s) ", setdiff(ages, dataAges), " not found in 'Nage' matrix"))
    x$Year = as.factor(x$Year)
    pModels = list()
    zModels = list()
    gPreds = list()
    gPreds2 = list()
    allobs = list()
    resid = list()
    predDc = predD
    if (exists(".Random.seed")) {
        oldseed <- get(".Random.seed", .GlobalEnv)
        oldRNGkind <- RNGkind()
        on.exit({
            do.call("RNGkind", as.list(oldRNGkind))
            assign(".Random.seed", oldseed, .GlobalEnv)
        })
    }
    set.seed(314159265)
    yearNum = as.numeric(as.character(x$Year))
    yearRange = min(yearNum):max(yearNum)
    gearNames = names(xtabs(~Gear, data = x))
    myGear = names(xtabs(~Gear, data = x))[which.max(xtabs(~Gear, data = x))]
    resMat = matrix(NA, nrow = length(yearRange), ncol = length(ages))
    upMat = resMat
    loMat = resMat
    sdMat = resMat
    do.one.a <- function(a) {
        age = which(dataAges == ages[a])
        ddd = x
        ddd$dum = 1
        ddd$A1 = ddd$Nage[, age]
        gammaPos = gamma
        gammaZ = gamma
        if (useBIC) {
            nZ = nrow(ddd)
            nPos = nrow(subset(ddd, A1 > cutOff))
            gammaPos = log(nPos)/2
            gammaZ = log(nZ)/2
            cat("gammaPos: ", gammaPos, " gammaZ: ", gammaZ,
                "\n")
        }
        pd = subset(ddd, A1 > cutOff)
        if (famVec[a] == "LogNormal") {
            f.pos = as.formula(paste("log(A1) ~", modelP[a]))
            f.0 = as.formula(paste("A1>", cutOff, " ~", modelZ[a]))
            print(system.time(m.pos <- DATRAS:::tryCatch.W.E(gam(f.pos,
                data = subset(ddd, A1 > cutOff), gamma = gammaPos,
                method = method, knots = knotsP, na.action = na.fail,
                ...))$value))
            if (class(m.pos)[2] == "error") {
                print(m.pos)
                stop("Error occured for age ", a, " in the positive part of the model\n",
                  "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
            }
            print(system.time(m0 <- DATRAS:::tryCatch.W.E(gam(f.0,
                gamma = gammaZ, data = ddd, family = binomial(link = linkZ),
                method = method, knots = knotsZ, na.action = na.fail,
                ...))$value))
            if (class(m0)[2] == "error") {
                print(m0)
                stop("Error occured for age ", a, " in the binomial part of the model\n",
                  "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
            }
        }
        else if (famVec[a] == "Gamma") {
            f.pos = as.formula(paste("A1 ~", modelP[a]))
            f.0 = as.formula(paste("A1>", cutOff, " ~", modelZ[a]))
            print(system.time(m.pos <- DATRAS:::tryCatch.W.E(gam(f.pos,
                data = subset(ddd, A1 > cutOff), family = Gamma(link = "log"),
                gamma = gammaPos, method = method, knots = knotsP,
                na.action = na.fail, ...))$value))
            if (class(m.pos)[2] == "error") {
                print(m.pos)
                stop("Error occured for age ", a, " in the positive part of the model\n",
                  "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
            }
            print(system.time(m0 <- DATRAS:::tryCatch.W.E(gam(f.0,
                gamma = gammaZ, data = ddd, family = binomial(link = linkZ),
                method = method, knots = knotsZ, na.action = na.fail,
                ...))$value))
            if (class(m0)[2] == "error") {
                print(m0)
                stop("Error occured for age ", a, " in the binomial part of the model\n",
                  "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
            }
        }
        else if (famVec[a] == "Tweedie") {
            ddd$A1[ddd$A1 < cutOff] = 0
            pd = ddd
            f.pos = as.formula(paste("A1 ~", modelP[a]))
            print(system.time(m.pos <- DATRAS:::tryCatch.W.E(gam(f.pos,
                data = ddd, family = tw, gamma = gammaPos, method = method,
                knots = knotsP, na.action = na.fail, ...))$value))
            if (class(m.pos)[2] == "error") {
                print(m.pos)
                stop("Error occured for age ", a, ".\n", "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
            }
            m0 = NULL
        }
        else if (famVec[a] == "negbin") {
            pd = ddd
            f.pos = as.formula(paste("A1 ~", modelP[a]))
            print(system.time(m.pos <- DATRAS:::tryCatch.W.E(gam(f.pos,
                data = ddd, family = nb, gamma = gammaPos, method = method,
                knots = knotsP, na.action = na.fail, ...))$value))
            if (class(m.pos)[2] == "error") {
                print(m.pos)
                stop("Error occured for age ", a, ".\n", "Try reducing the number of age groups or decrease the basis dimension of the smooths, k\n")
            }
            m0 = NULL
        }
        if (famVec[a] == "Tweedie" || famVec[a] == "negbin") {
            totll = logLik(m.pos)[1]
        }
        else {
            p0p = (1 - predict(m0, type = "response"))
            ppos = p0p[ddd$A1 > cutOff]
            p0m1 = p0p[ddd$A1 <= cutOff]
            if (famVec[a] == "Gamma")
                totll = sum(log(p0m1)) + sum(log(1 - ppos)) +
                  logLik(m.pos)[1]
            if (famVec[a] == "LogNormal")
                totll = sum(log(p0m1)) + sum(log(1 - ppos)) +
                  logLik(m.pos)[1] - sum(m.pos$y)
        }
        if (is.null(predD))
            predD = subset(ddd, haul.id %in% myids)
        res = numeric(length(yearRange))
        lores = res
        upres = res
        sdres = res
        gp2 = list()
        for (y in levels(ddd$Year)) {
            if (!any(ddd$A1[ddd$Year == y] > cutOff)) {
                res[which(as.character(yearRange) == y)] = 0
                upres[which(as.character(yearRange) == y)] = 0
                lores[which(as.character(yearRange) == y)] = 0
                sdres[which(as.character(yearRange) == y)] = 0
                next
            }
            if (is.list(predDc) && !class(predDc) %in% c("data.frame",
                "DATRASraw"))
                predD = predDc[[as.character(y)]]
            if (is.null(predD))
                stop(paste("Year", y, " not found in predD"))
            predD$Year = y
            predD$dum = 0
            predD$ctime = as.numeric(as.character(y))
            predD$TimeShotHour = mean(ddd$TimeShotHour)
            predD$Ship = names(which.max(summary(ddd$Ship)))
            predD$timeOfYear = mean(ddd$timeOfYear)
            predD$HaulDur = 30
            predD$Gear = myGear
            if (!is.null(predfix)) {
                stopifnot(is.list(predfix))
                for (n in names(predfix)) {
                  predD[, n] = predfix[[n]]
                }
            }
            p.1 <- p.0 <- NULL
            try({
                Xp.1 = predict(m.pos, newdata = predD, type = "lpmatrix")
                OS.pos = numeric(nrow(predD))
                terms.pos = terms(m.pos)
                if (!is.null(m.pos$offset)) {
                  off.num.pos <- attr(terms.pos, "offset")
                  for (i in off.num.pos) OS.pos <- OS.pos + eval(attr(terms.pos,
                    "variables")[[i + 1]], predD)
                }
                p.1 = Xp.1 %*% coef(m.pos) + OS.pos
                if (!famVec[a] %in% c("Tweedie", "negbin")) {
                  Xp.0 = predict(m0, newdata = predD, type = "lpmatrix")
                  brp.0 = coef(m0)
                  OS0 = numeric(nrow(predD))
                  terms.0 = terms(m0)
                  if (!is.null(m0$offset)) {
                    off.num.0 <- attr(terms.0, "offset")
                    for (i in off.num.0) OS0 <- OS0 + eval(attr(terms.0,
                      "variables")[[i + 1]], predD)
                  }
                  p.0 = m0$family$linkinv(Xp.0 %*% brp.0 + OS0)
                }
            })
            if (!is.numeric(p.1) | (!famVec[a] %in% c("Tweedie",
                "negbin") && !is.numeric(p.0))) {
                res[which(as.character(yearRange) == y)] = 0
                upres[which(as.character(yearRange) == y)] = 0
                lores[which(as.character(yearRange) == y)] = 0
                sdres[which(as.character(yearRange) == y)] = 0
                next
            }
            sig2 = m.pos$sig2
            if (famVec[a] == "Gamma") {
                res[which(as.character(yearRange) == y)] = sum(p.0 *
                  exp(p.1))
                gPred = p.0 * exp(p.1)
            }
            if (famVec[a] == "LogNormal") {
                res[which(as.character(yearRange) == y)] = sum(p.0 *
                  exp(p.1 + sig2/2))
                gPred = p.0 * exp(p.1 + sig2/2)
            }
            if (famVec[a] %in% c("Tweedie", "negbin")) {
                res[which(as.character(yearRange) == y)] = sum(exp(p.1))
                gPred = exp(p.1)
            }
            gp2[[y]] = gPred
            if (nBoot > 10) {
                brp.1 = mvrnorm(n = nBoot, coef(m.pos), m.pos$Vp)
                if (!famVec[a] %in% c("Tweedie", "negbin")) {
                  brp.0 = mvrnorm(n = nBoot, coef(m0), m0$Vp)
                  OS0 = matrix(0, nrow(predD), nBoot)
                  terms.0 = terms(m0)
                  if (!is.null(m0$offset)) {
                    off.num.0 <- attr(terms.0, "offset")
                    for (i in off.num.0) OS0 <- OS0 + eval(attr(terms.0,
                      "variables")[[i + 1]], predD)
                  }
                  rep0 = m0$family$linkinv(Xp.0 %*% t(brp.0) +
                    OS0)
                }
                OS.pos = matrix(0, nrow(predD), nBoot)
                terms.pos = terms(m.pos)
                if (!is.null(m.pos$offset)) {
                  off.num.pos <- attr(terms.pos, "offset")
                  for (i in off.num.pos) OS.pos <- OS.pos + eval(attr(terms.pos,
                    "variables")[[i + 1]], predD)
                }
                if (famVec[a] == "LogNormal") {
                  rep1 = exp(Xp.1 %*% t(brp.1) + sig2/2 + OS.pos)
                }
                else {
                  rep1 = exp(Xp.1 %*% t(brp.1) + OS.pos)
                }
                if (!famVec[a] %in% c("Tweedie", "negbin")) {
                  idxSamp = colSums(rep0 * rep1)
                }
                else {
                  idxSamp = colSums(rep1)
                }
                halpha = (1 - CIlevel)/2
                upres[which(as.character(yearRange) == y)] = quantile(idxSamp,
                  1 - halpha)
                lores[which(as.character(yearRange) == y)] = quantile(idxSamp,
                                                                      halpha)
                sdres[which(as.character(yearRange) == y)] = sd(idxSamp)
            }
        }
        list(res = res, m.pos = m.pos, m0 = m0, lo = lores, up = upres,
             sd = sdres,
            gp = gPred, ll = totll, pd = pd, gp2 = gp2)
    }
    noAges = length(ages)
    rr = parallel::mclapply(1:noAges, do.one.a, mc.cores = mc.cores)
    logl = 0
    for (a in 1:noAges) {
        resMat[, a] = rr[[a]]$res
        zModels[[a]] = rr[[a]]$m0
        pModels[[a]] = rr[[a]]$m.pos
        loMat[, a] = rr[[a]]$lo
        upMat[, a] = rr[[a]]$up
        sdMat[, a] = rr[[a]]$sd
        gPreds[[a]] = rr[[a]]$gp
        logl = logl + rr[[a]]$ll
        gPreds2[[a]] = rr[[a]]$gp2
        allobs[[a]] = x$Nage[, a]
    }
    getEdf <- function(m) sum(m$edf)
    totEdf = sum(unlist(lapply(zModels, getEdf))) + sum(unlist(lapply(pModels,
        getEdf)))
    rownames(resMat) <- yearRange
    colnames(resMat) <- ages
    out <- list(idx = resMat, zModels = zModels, pModels = pModels,
        lo = loMat, up = upMat, sd = sdMat, gPreds = gPreds, logLik = logl,
        edfs = totEdf, gPreds2 = gPreds2, family = famVec, cutOff = cutOff,
        dataAges = dataAges, yearNum = yearNum, refGear = myGear,
        predfix = predfix, knotsP = knotsP, knotsZ = knotsZ,
        allobs = allobs, CIlevel = CIlevel)
    class(out) <- "surveyIdx"
    set.seed(314159265)
    for (a in 1:noAges) resid[[a]] = residuals(out, a)
    out$residuals = resid
    out
}



surveyIdxPlots_df <- function (x, dat, alt.idx = NULL, myids, cols = 1:length(x$pModels),
    select = c("index", "map", "residuals", "fitVsRes"), par = list(mfrow = c(3,
        3)), colors = rev(heat.colors(6)), map.cex = 1, plotByAge = TRUE,
    legend = TRUE, predD = NULL, year = NULL, main = NULL, legend.signif = 3,
    legend.pos = "topright", restoreOldPar = FALSE, ...)
{
    if (!plotByAge & !is.null(par)) {
        op <- par(par)
        if (restoreOldPar)
            on.exit(par(op))
    }
    mainwasnull <- is.null(main)
    for (a in cols) {
        if (mainwasnull)
            main <- paste("Age group", colnames(dat$Nage)[a])
        if (plotByAge & !is.null(par)) {
            op <- par(par)
            if (restoreOldPar)
                on.exit(par(op))
        }
        if (any(select == "index")) {
            ys = range(as.numeric(levels(dat$Year)))
            ys = ys[1]:ys[2]
            yl = range(c(x$idx[, a], 0, x$lo[, a], x$up[, a])/mean(x$idx[,
                a]), na.rm = TRUE)
            if (!is.null(alt.idx) && a <= ncol(alt.idx)) {
                yl = range(c(alt.idx[, a]/mean(alt.idx[, a]),
                  yl)) * 1.1
                plot(ys, alt.idx[, a]/mean(alt.idx[, a], na.rm = TRUE),
                  ylim = yl, col = 2, ylab = "Index", xlab = "Year",
                  main = main)
            }
            else {
                plot(ys, rep(NA, length(ys)), ylim = yl, col = 2,
                  ylab = "Index", xlab = "Year", main = main)
            }
            idx = x$idx
            lo = x$lo
            up = x$up
            idx[x$idx <= 0] = NA
            lo[x$idx <= 0] = NA
            up[x$idx <= 0] = NA
            lines(ys, idx[, a]/mean(idx[, a], na.rm = TRUE),
                lwd = 2)
            lines(ys, lo[, a]/mean(idx[, a], na.rm = TRUE), lwd = 2,
                lty = 2)
            lines(ys, up[, a]/mean(idx[, a], na.rm = TRUE), lwd = 2,
                lty = 2)
            if (legend && !is.null(alt.idx))
                legend(legend.pos, pch = c(1, NA), lty = c(NA,
                  1), col = c(2, 1), legend = c("alt.idx", "GAM"))
        }
        if (any(select == "map")) {
            xlims = range(dat$Lon, na.rm = TRUE)
            ylims = range(dat$Lat, na.rm = TRUE)
            mapvals = NULL
            if (is.null(predD)) {
                tmp = subset(dat, haul.id %in% myids)
            }
            else {
                tmp = predD
            }
            if (is.null(year)) {
                concT = surveyIndex:::concTransform(log(x$gPreds[[a]]))
                mapvals = x$gPreds[[a]]
            }
            else {
                y = which(as.numeric(as.character(names(x$gPreds2[[a]]))) ==
                  year)
                if (length(y) == 0)
                  stop(paste("Year", year, "age group", a, "not found."))
                concT = surveyIndex:::concTransform(log(x$gPreds2[[a]][[y]]))
                mapvals = x$gPreds2[[a]][[y]]
            }
            if (length(colors) > 1)
                zFac = cut(concT, 0:length(colors)/length(colors))
            else zFac = 1
            if (length(map.cex) > 1)
                sFac = cut(log(x$gPreds[[a]]), length(map.cex))
            else sFac = 1
            myCols = colors
            plot(tmp$Lon, y = tmp$Lat, col = 1, pch = 1, cex = map.cex[sFac],
                xlim = xlims, ylim = ylims, xlab = "Longitude",
                ylab = "Latitude", main = main, ...)
            points(tmp$Lon, y = tmp$Lat, col = myCols[zFac],
                pch = 16, cex = map.cex[sFac])
            maps::map("worldHires", xlim = xlims, ylim = ylims,
                fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
            if (legend) {
                maxcuts = aggregate(mapvals ~ zFac, FUN = max)
                mincuts = aggregate(mapvals ~ zFac, FUN = min)
                mm = mean(mapvals)
                ml = signif(mincuts[, 2]/mm, legend.signif)
                ml[1] = 0
                leg = paste0("[", ml, ",", signif(maxcuts[, 2]/mm,
                  legend.signif), "]")
                legend(legend.pos, legend = leg, pch = 16, col = colors,
                  bg = "white")
            }
        }
        if (any(select == "absolutemap")) {
            if (is.null(year) || length(year) < 1)
                stop("argument 'year' must be vector of length>=1 for type 'absolutemap'")
            if (!all(year %in% levels(dat$Year)))
                stop("invalid years selected")
            xlims = range(dat$Lon, na.rm = TRUE)
            ylims = range(dat$Lat, na.rm = TRUE)
            if (is.null(predD)) {
                tmp = subset(dat, haul.id %in% myids)
            }
            else {
                tmp = predD
            }
            ally = data.frame(val = x$gPreds2[[a]][[1]], year = as.character(levels(dat$Year)[1]))
            cc = 0
            for (y in levels(dat$Year)) {
                cc = cc + 1
                ally = rbind(ally, data.frame(val = x$gPreds2[[a]][[cc]],
                  year = as.character(levels(dat$Year)[cc])))
            }
            ally$conc = surveyIndex:::concTransform(log(ally$val))
            ally$zFac = cut(ally$conc, 0:length(colors)/length(colors))
            for (yy in year) {
                plot(tmp$Lon, y = tmp$Lat, col = 1, pch = 1,
                  cex = map.cex, xlab = "Longitude", ylab = "Latitude",
                  axes = FALSE)
                box()
                title(yy, line = 1)
                sel = which(ally$year == yy)
                points(tmp$Lon, y = tmp$Lat, col = colors[as.numeric(ally$zFac[sel])],
                  pch = 16, cex = map.cex)
                maps::map("worldHires", xlim = xlims, ylim = ylims,
                  fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
                if (legend && yy == year[1]) {
                  maxcuts = aggregate(val ~ zFac, data = ally,
                    FUN = max)
                  mincuts = aggregate(val ~ zFac, data = ally,
                    FUN = min)
                  mm = mean(ally$val)
                  ml = signif(mincuts[, 2]/mm, legend.signif)
                  ml[1] = 0
                  leg = paste0("[", ml, ",", signif(maxcuts[,
                    2]/mm, legend.signif), "]")
                  legend(legend.pos, legend = leg, pch = 16,
                    col = colors, bg = "white")
                }
            }
        }
        for (k in 1:length(select)) {
            ss = suppressWarnings(as.numeric(select[k]))
            if (!is.na(ss)) {
                plot.gam(x$pModels[[a]], select = ss, main = main,
                  ...)
            }
        }
        if (any(select == "residuals") || any(select == "fitVsRes") ||
            any(select == "resVsYear") || any(select == "resVsShip") ||
            any(select == "spatialResiduals")) {
            resi <- x$residuals[[a]]
        }
        if (any(select == "residuals")) {
            hist(resi, nclass = 30, main = main, xlab = "Residuals",
                ...)
        }
        if (any(select == "fitVsRes")) {
            plot(fitted(x$pModels[[a]]), residuals(x$pModels[[a]]),
                xlab = "Fitted", ylab = "Residuals", main = main,
                ...)
        }
        if (any(select == "resVsYear")) {
            plot(dat$Year, resi, main = main, xlab = "Year",
                ylab = "Residuals", ...)
        }
        if (any(select == "resVsShip")) {
            plot(dat$Ship, resi, main = main, xlab = "Year",
                ylab = "Residuals", ...)
        }
        if (any(select == "spatialResiduals")) {
            scale <- 3 * map.cex
            if (is.null(year) || length(year) > 1)
                stop("a single year must be supplied")
            sel <- which(dat$Year == as.character(year))
            plot(dat$Lon, dat$Lat, type = "n", xlab = "Longitude",
                ylab = "Latitude", main = main, ...)
            maps::map("worldHires", fill = TRUE, plot = TRUE,
                add = TRUE, col = grey(0.5))
            positive = resi[sel] > 0
            points(dat$Lon[sel][positive], dat$Lat[sel][positive],
                pch = 1, cex = scale * sqrt(resi[sel][positive]),
                col = "blue")
            points(dat$Lon[sel][!positive], dat$Lat[sel][!positive],
                pch = 1, cex = scale * sqrt(-resi[sel][!positive]),
                col = "red")
        }
    }
}


#This code is modified for D.L. Miller's dsm package for distance sampling, from
#the rqgam.check function. The code is designed to extract randomized quantile
#residuals from GAMs, using the family definitions in mgcv. Note statmod only
#supports RQ residuals for the following families: Tweedie, Poisson, Gaussian,  Any errors are due to Eric Pedersen
#This has functions for randomized quantile residuals
rqresiduals = function (gam.obj) {
  if(!"gam" %in% attr(gam.obj,"class")){
    stop('"gam.obj has to be of class "gam"')
  }
  if (!grepl("^Tweedie|^Negative Binomial|^poisson|^binomial|^gaussian|^Gamma|^inverse.gaussian",
             gam.obj$family$family)){
    stop(paste("family " , gam.obj$family$family,
                " is not currently supported by the statmod library,
                 and any randomized quantile residuals would be inaccurate.",
               sep=""))
  }
  if (grepl("^Tweedie", gam.obj$family$family)) {
    if (is.null(environment(gam.obj$family$variance)$p)) {
      p.val <- gam.obj$family$getTheta(TRUE)
      environment(gam.obj$family$variance)$p <- p.val
    }
    qres <- qres.tweedie(gam.obj)
  }
  else if (grepl("^Negative Binomial", gam.obj$family$family)) {
    if ("extended.family" %in% class(gam.obj$family)) {
      gam.obj$theta <- gam.obj$family$getTheta(TRUE)
    }
    else {
      gam.obj$theta <- gam.obj$family$getTheta()
    }
    qres <- qres.nbinom(gam.obj)
  }
  else {
    qres <- qresid(gam.obj)
  }
  return(qres)
}
