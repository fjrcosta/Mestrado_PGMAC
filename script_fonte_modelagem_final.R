#As versões mais atualizadas das bibliotecas seguintes deverão ser obtidas junto de seus mantenedores 
#remotes::install_github("DesiQuintans/librarian")
#remotes::install_github("r-tmap/tmaptools") #para a versão mais recente do desenvolvedor
#remotes::install_github("r-tmap/tmap") #para a versão mais recente do desenvolvedor
#install.packages("plotGoogleMaps", repos="http://R-Forge.R-project.org")

library(librarian)

shelf(Hmisc,gamlss,gamlss.add,geobr,readxl,sf,rgdal,spatialEco,
      mapview,RSAGA,plotKML,inlabru,RColorBrewer,rayshader,av,
      rgl,maps,GISTools,geoR,spacetime,zoo,ggplot2,hnp,fields,
      cowplot,rosm,ggmap,spData,tmap,tmaptools,raster, fields,
      raster, spData, ceramic, plotGoogleMaps, dplyr, tibble)


setwd("<...>/dissertacao")


# Arquivos necessários:
# a planilha com os dados: Terrenos_R_GIT.xlsx;

# Criar os diretórios onde se localizam os arquivos necessários
# diretorio para os dados ='<...>dissertacao/dados'

########################################################################################################################
# Função para remover os rótulos dos dados
# Fonte da função:
# https://stackoverflow.com/questions/2394902/remove-variable-labels-attached-with-foreign-hmisc-spss-import-functions
########################################################################################################################

clear.labels <- function(x) {
  if (is.list(x)) {
    for (i in 1:length(x))
      class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled')
    for (i in 1:length(x))
      attr(x[[i]], "label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}

########################################################################################################################
# Função para converter um objeto da classe "ggmap"  "raster" em raster (3 canais de cor / rgb) 
# Fonte da função:
# https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/download-raster.R
########################################################################################################################

ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

########################################################################################################################
# Função wp() modificada para incluir possibilidade de variação no tamanho dos 
# caracteres no título (cex.main), legendas de eixos (cex.labs) e eixos (cex.axis) ...
########################################################################################################################


wp_mod=function (object = NULL, xvar = NULL, resid = NULL, n.inter = 4, 
          xcut.points = NULL, overlap = 0, xlim.all = 4, xlim.worm = 3.5, 
          show.given = TRUE, line = TRUE, ylim.all = 12 * sqrt(1/length(resid)), 
          ylim.worm = 12 * sqrt(n.inter/length(resid)), cex = NULL, cex.lab = NULL, 
          pch = 21, bg = "wheat", col = "red", bar.bg = c(num = "light blue"), main=NULL,
          cex.main=NULL, col.main=NULL, col.lab = NULL,  col.axis = NULL, cex.axis=NULL,...) 
{
  panel.fun <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), 
                        cex = par("cex"), col.smooth = "red", span = 2/3, iter = 3, 
                        ...) {
    qq <- as.data.frame(qqnorm(y, plot = FALSE))
    if (any(is.infinite(qq$y))) 
      line <- FALSE
    qq$y <- qq$y - qq$x
    grid(nx = NA, ny = NA, lwd = 2)
    points(qq$x, qq$y, pch = pch, col = col, bg = bg, cex = cex)
    abline(0, 0, lty = 2, col = col)
    abline(0, 1e+05, lty = 2, col = col)
    yuplim <- 10 * sqrt(1/length(qq$y))
    level <- 0.95
    lz <- -xlim.worm
    hz <- xlim.worm
    dz <- 0.25
    z <- seq(lz, hz, dz)
    p <- pnorm(z)
    se <- (1/dnorm(z)) * (sqrt(p * (1 - p)/length(qq$y)))
    low <- qnorm((1 - level)/2) * se
    high <- -low
    {
      no.points <- length(qq$y)
      total.points <<- total.points + no.points
      no.mis <- sum(abs(qq$y) > ylim.worm)
      cat("number of missing points from plot=", no.mis, 
          " out of ", no.points, "\n")
      if (any(abs(qq$y) > ylim.worm)) 
        warning("Some points are missed out ", "\n", 
                "increase the y limits using ylim.worm")
    }
    if (any(abs(qq$x) > xlim.worm)) {
      warning("Some points are missed out ", "\n", "increase the x limits using xlim.worm")
    }
    lines(z, low, lty = 2, lwd = 0.5)
    lines(z, high, lty = 2, lwd = 0.5)
    if (line == TRUE) {
      fit <- lm(y ~ x + I(x^2) + I(x^3), data = qq)
      s <- spline(qq$x, fitted(fit))
      flags <- s$x > -2.5 & s$x < 2.5
      lines(list(x = s$x[flags], y = s$y[flags]), col = col, 
            lwd = 0.5)
      assign("coef1", coef(fit), envir = parent.frame(n = 3))
      assign("coef", c(coef, coef1), envir = parent.frame(n = 3))
    }
  }
  check.overlap <- function(interval) {
    if (!is.matrix(interval)) {
      stop(paste("The interval specified is not a matrix."))
    }
    if (dim(interval)[2] != 2) {
      stop(paste("The interval specified is not a valid matrix.\nThe number of columns should be equal to 2."))
    }
    crows = dim(interval)[1]
    for (i in 1:(crows - 1)) {
      if (!(abs(interval[i, 2] - interval[i + 1, 1]) < 
            1e-04)) {
        interval[i + 1, 1] = interval[i, 2]
      }
    }
    return(interval)
  }
  get.intervals <- function(xvar, xcut.points) {
    if (!is.vector(xcut.points)) {
      stop(paste("The interval is not a vector."))
    }
    if (any((xcut.points < min(xvar)) | any(xcut.points > 
                                            max(xvar)))) {
      stop(paste("The specified `xcut.points' are not within the range of the x: (", 
                 min(xvar), " , ", max(xvar), ")"))
    }
    extra <- (max(xvar) - min(xvar))/1e+05
    int <- c(min(xvar), xcut.points, (max(xvar) + 2 * extra))
    ii <- 1:(length(int) - 1)
    r <- 2:length(int)
    x1 <- int[ii]
    xr <- int[r] - extra
    if (any(x1 > xr)) {
      stop(paste("The interval is are not in a increasing order."))
    }
    cbind(x1, xr)
  }
  deparen <- function(expr) {
    while (is.language(expr) && !is.name(expr) && deparse(expr[[1L]])[1L] == 
           "(") expr <- expr[[2L]]
    expr
  }
  if (is.null(object) && is.null(resid)) 
    stop(paste("A fitted object with resid() method or the argument resid should be used ", 
               "\n", ""))
  resid <- if (is.null(object)) 
    resid
  else resid(object)
  DataExist <- FALSE
  if (!is.null(object) && any(grepl("data", names(object$call))) && 
      !(object$call["data"] == "sys.parent()()")) {
    DaTa <- eval(object$call[["data"]])
    DataExist <- TRUE
  }
  if (!grepl("$", deparse(substitute(xvar)), fixed = T) && 
      !grepl("~", deparse(substitute(xvar)), fixed = T) && 
      DataExist) {
    xvar <- eval(substitute(xvar), envir = as.environment(DaTa))
  }
  if (is.null(xvar)) {
    qq <- as.data.frame(qqnorm(resid, plot = FALSE))
    qq$y <- qq$y - qq$x
    level <- 0.95
    lz <- -xlim.all
    hz <- xlim.all
    dz <- 0.25
    z <- seq(lz, hz, dz)
    p <- pnorm(z)
    se <- (1/dnorm(z)) * (sqrt(p * (1 - p)/length(qq$y)))
    low <- qnorm((1 - level)/2) * se
    high <- -low
    if (any(abs(qq$y) > ylim.all)) {
      warning("Some points are missed out ", "\n", "increase the y limits using ylim.all")
    }
    if (any(abs(qq$x) > xlim.all)) {
      warning("Some points are missed out ", "\n", "increase the x limits using xlim.all")
    }
    plot(qq$x, qq$y, ylab = "Desvio", xlab = "Unidades de quantis Normais", 
         xlim = c(-xlim.all, xlim.all), ylim = c(-ylim.all,ylim.all), 
         cex = cex, pch = pch, bg = bg, cex.lab = cex.lab, main=main, cex.main=cex.main,
         col.main=col.main, col.lab = col.lab, col.axis = col.axis, cex.axis=cex.axis)
    grid(lty = "solid")
    abline(0, 0, lty = 2, col = col)
    abline(0, 1e+05, lty = 2, col = col)
    lines(z, low, lty = 2)
    lines(z, high, lty = 2)
    if (line == TRUE) {
      fit <- lm(qq$y ~ qq$x + I(qq$x^2) + I(qq$x^3))
      s <- spline(qq$x, fitted(fit))
      flags <- s$x > -xlim.all & s$x < xlim.all
      lines(list(x = s$x[flags], y = s$y[flags]), col = col, 
            lwd = 0.5)
    }
    return(invisible(coef(fit)))
  }
  if (!is(xvar, "formula")) {
    if (is.factor(xvar)) 
      stop("Use formula for factors i.e. xvar=~f")
    w <- if (is.null(object)) 
      rep(1, length(resid))
    else object$weights
    if (all(trunc(w) == w)) 
      xvar <- rep(xvar, w)
    if (is.null(xcut.points)) {
      given.in <- co.intervals(xvar, number = n.inter, 
                               overlap = overlap)
      if (overlap == 0) 
        given.in <- check.overlap(given.in)
    }
    else {
      given.in <- get.intervals(xvar, xcut.points)
    }
    total.points <- 0
    coef <- coef1 <- NULL
    y.y <- resid
    x.x <- resid
    coplot(y.y ~ x.x | xvar, given.values = given.in, panel = panel.fun, 
           ylim = c(-ylim.worm, ylim.worm), xlim = c(-xlim.worm, xlim.worm), 
           ylab = "Desvio", xlab = "Unidades de quantis Normais", 
           show.given = show.given, bg = bg, pch = pch, cex = cex, 
           bar.bg = bar.bg)
    if (overlap == 0) {
      if (total.points != length(resid)) 
        warning("the total number of points in the plot is not equal \n to the number of observations in y \n")
    }
    mcoef <- matrix(coef, ncol = 4, byrow = TRUE)
    out <- list(classes = given.in, coef = mcoef)
    return(invisible(out))
  }
  if (is(xvar, "formula")) {
    w <- if (is.null(object)) 
      rep(1, length(resid))
    else object$weights
    total.points <- 0
    coef <- coef1 <- NULL
    y.y <- resid
    x.x <- resid
    if (DataExist) {
      coplot(as.formula(paste("y.y~x.x|", as.character(xvar), 
                              sep = "")[2]), data = DaTa, panel = panel.fun, 
             overlap = overlap, number = n.inter, ylim = c(-ylim.worm,ylim.worm), 
             xlim = c(-xlim.worm, xlim.worm), 
             ylab = "Desvio", xlab = "Unidades de quantis Normais", 
             show.given = show.given, bg = bg, pch = pch, 
             cex = cex, bar.bg = bar.bg, ...)
      mcoef <- matrix(coef, ncol = 4, byrow = TRUE)
      if ("given.values" %in% names(vars <- list(...))) 
        given.in <- vars$given.values
      else {
        rhs <- deparen(xvar)[[2L]]
        if (length(rhs) == 3 && rhs[[1]] != "$") {
          a <- eval(deparen(rhs[[2L]]), envir = as.environment(DaTa))
          b <- eval(deparen(rhs[[3L]]), envir = as.environment(DaTa))
          if (length(n.inter) == 1L) 
            n.inter <- rep(n.inter, 2)
          if (length(overlap) == 1L) 
            overlap <- rep(overlap, 2)
          Inter1 <- if (is.factor(a)) 
            levels(a)
          else co.intervals(unclass(a), number = n.inter[1L], 
                            overlap = overlap[1L])
          Inter2 <- if (is.factor(b)) 
            levels(b)
          else co.intervals(unclass(b), number = n.inter[2L], 
                            overlap = overlap[2L])
          given.in <- list(Inter1, Inter2)
        }
        else {
          a <- eval(deparen(rhs), envir = as.environment(DaTa))
          if (length(n.inter) == 1L) 
            n.inter <- rep(n.inter, 2)
          if (length(overlap) == 1L) 
            overlap <- rep(overlap, 2)
          given.in <- if (is.factor(a)) 
            levels(a)
          else co.intervals(unclass(a), number = n.inter[1L], 
                            overlap = overlap[1L])
        }
      }
      out <- list(classes = given.in, coef = mcoef)
      return(invisible(out))
    }
    else {
      coplot(as.formula(paste("y.y~x.x|", as.character(xvar), 
                              sep = "")[2]), panel = panel.fun, overlap = overlap, 
             number = n.inter, ylim = c(-ylim.worm, ylim.worm), 
             xlim = c(-xlim.worm, xlim.worm), ylab = "Desvio", 
             xlab = "Unidades de quantis Normais", show.given = show.given, 
             bg = bg, pch = pch, cex = cex, bar.bg = bar.bg, ...)
      mcoef <- matrix(coef, ncol = 4, byrow = TRUE)
      if ("given.values" %in% names(vars <- list(...))) 
        given.in <- vars$given.values
      else {
        rhs <- deparen(xvar)[[2L]]
        if (length(rhs) == 3 && rhs[[1]] != "$") {
          a <- eval(deparen(rhs[[2L]]))
          b <- eval(deparen(rhs[[3L]]))
          if (length(n.inter) == 1L) 
            n.inter <- rep(n.inter, 2)
          if (length(overlap) == 1L) 
            overlap <- rep(overlap, 2)
          Inter1 <- if (is.factor(a)) 
            levels(a)
          else co.intervals(unclass(a), number = n.inter[1L], 
                            overlap = overlap[1L])
          Inter2 <- if (is.factor(b)) 
            levels(b)
          else co.intervals(unclass(b), number = n.inter[2L], 
                            overlap = overlap[2L])
          given.in <- list(Inter1, Inter2)
        }
        else {
          a <- eval(deparen(rhs))
          if (length(n.inter) == 1L) 
            n.inter <- rep(n.inter, 2)
          if (length(overlap) == 1L) 
            overlap <- rep(overlap, 2)
          given.in <- if (is.factor(a)) 
            levels(a)
          else co.intervals(unclass(a), number = n.inter[1L], 
                            overlap = overlap[1L])
        }
      }
      out <- list(classes = given.in, coef = mcoef)
      return(invisible(out))
    }
  }
}

########################################################################################################################
# Função histDist() modificada para incluir possibilidade de variação no tamanho dos 
# caracteres no título (cex.main), legendas de eixos (cex.labs) e eixos (cex.axis) 
########################################################################################################################


histDist_mod=function(y, family = NO, freq = NULL, density = FALSE, nbins = 10, 
                      xlim = NULL, ylim = NULL, main = NULL, xlab = NULL, ylab = NULL, 
                      data = NULL, col.hist = "gray", border.hist = "blue", fg.hist = rainbow(12)[9], 
                      line.wd = 2, line.ty = c(1, 2), line.col = c(2, 3), col.main =NULL, 
                      col.lab = NULL, col.axis = NULL, cex.main=NULL, cex.axis=NULL, cex.lab=NULL)
{
  FA <- as.gamlss.family(family)
  fname <- FA$family[1]
  dfun <- paste("d", fname, sep = "")
  lpar <- length(FA$parameters)
  typeDist <- FA$type
  subsY <- substitute(y)
  if (!is.null(data)) {
    y <- get(deparse(substitute(y)), envir = as.environment(data))
    if (deparse(substitute(freq)) != "NULL") 
      freq <- get(deparse(substitute(freq)), envir = as.environment(data))
  }
  switch(typeDist, Continuous = {
    extra <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))/5
    xmin <- if (is.null(xlim)) min(y, na.rm = TRUE) - extra else xlim[1]
    xmax <- if (is.null(xlim)) max(y, na.rm = TRUE) + extra else xlim[2]
    if (!FA$y.valid(xmin)) xmin <- 0.001
    if (!FA$y.valid(xmax)) xmax <- 0.999
    x1 <- seq(xmin, xmax, length = 101)
  }, Discrete = {
    if (fname %in% .gamlss.bi.list) {
      if (NCOL(y) == 1) {
        x1 <- c(0, 1)
        bd <- rep(1, 2)
      } else {
        if (any(abs(y - round(y)) > 0.001)) {
          warning("non-integer counts in a binomial GAMLSS!")
        }
        bd <- y[, 1] + y[, 2]
        y1 <- y[, 1]
        if (any(y1 < 0 | y1 > bd)) stop("y values must be 0 <= y <= N")
        xmin <- if (is.null(xlim[1])) {
          if (all(bd == bd[1])) min(y1, na.rm = TRUE) else 0
        } else xlim[1]
        xmax <- if (is.null(xlim)) {
          if (all(bd == bd[1])) max(bd, na.rm = TRUE) else 1
        } else xlim[2]
        x1 <- seq(xmin, xmax, by = 1)
      }
    } else {
      xmin <- if (is.null(xlim)) min(y, na.rm = TRUE) else xlim[1]
      xmax <- if (is.null(xlim)) max(y, na.rm = TRUE) else xlim[2]
      x1 <- seq(xmin, xmax, by = 1)
    }
  }, Mixed = {
    stop("Mixed distributions are not implemented yet")
  })
  if (is.null(freq)) {
    {
      mod <- try(gamlssML(y, family = fname), silent = TRUE)
      if (any(class(mod) %in% "try-error")) {
        mod <- try(gamlss(y ~ 1, family = fname, ...))
      }
    }
  }
  else {
    {
      mod <- try(gamlssML(y, weights = freq, family = fname, 
                          ...), silent = TRUE)
      if (any(class(mod) %in% "try-error")) {
        mod <- try(gamlss(y ~ 1, weights = freq, family = fname, 
                          ...))
      }
    }
  }
  mod$call$family <- eval(as.expression(fname))
  if (mod$method == "BFGS" || mod$method == "nlminb") {
    mod$call$formula <- subsY
  }
  else {
    mod$call$formula[[2]] <- subsY
  }
  if (!is.null(data)) 
    mod$call$data <- substitute(data)
  switch(lpar, `1` = {
    newcall <- if ((fname %in% .gamlss.bi.list)) call(dfun, 
                                                      x1, mu = fitted(mod)[1], bd = bd[1]) else call(dfun, 
                                                                                                     x1, mu = fitted(mod)[1])
  }, `2` = {
    newcall <- if ((fname %in% .gamlss.bi.list)) {
      call(dfun, x1, mu = fitted(mod)[1], sigma = fitted(mod, 
                                                         "sigma")[1], bd = bd[1])
    } else {
      call(dfun, x1, mu = fitted(mod)[1], sigma = fitted(mod, 
                                                         "sigma")[1])
    }
  }, `3` = {
    newcall <- if ((fname %in% .gamlss.bi.list)) {
      call(dfun, x1, mu = fitted(mod)[1], sigma = fitted(mod, 
                                                         "sigma")[1], nu = fitted(mod, "nu")[1], bd = bd[1])
    } else {
      call(dfun, x1, mu = fitted(mod)[1], sigma = fitted(mod, 
                                                         "sigma")[1], nu = fitted(mod, "nu")[1])
    }
  }, `4` = {
    newcall <- if ((fname %in% .gamlss.bi.list)) {
      call(dfun, x1, mu = fitted(mod)[1], sigma = fitted(mod, 
                                                         "sigma")[1], nu = fitted(mod, "nu")[1], tau = fitted(mod, 
                                                                                                              "tau")[1], bd = bd[1])
    } else {
      call(dfun, x1, mu = fitted(mod)[1], sigma = fitted(mod, 
                                                         "sigma")[1], nu = fitted(mod, "nu")[1], tau = fitted(mod, 
                                                                                                              "tau")[1])
    }
  })
  switch(typeDist, Continuous = {
    y1 <- eval(newcall)
    xlim <- c(xmin, xmax)
    main <- if (is.null(main)) paste("The ", deparse(subsY), 
                                     " and the fitted ", FA$family[1], " distribution", 
                                     sep = "") else main
    if (!is.null(freq)) y <- rep(y, freq)
    xlab <- if (is.null(xlab)) deparse(subsY) else xlab
    ylab <- if (is.null(ylab)) paste("f()") else ylab
    truehist(y, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
             nbins = nbins, main = main, col = col.hist, lty = 3, 
             border = border.hist, fg = fg.hist, col.main = col.main, 
             col.lab = col.lab, col.axis = col.axis, 
             cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis)
    lines(x1, y1, col = line.col[1], lwd = line.wd, lty = line.ty[1], 
          fg = gray(0.7))
    if (density == TRUE) {
      dens <- density(y)
      lines(dens$x, dens$y, col = line.col[2], lwd = line.wd, 
            lty = line.ty[2])
    }
  }, Discrete = {
    if (fname %in% .gamlss.bi.list) {
      if (all(bd == bd[1])) {
        y1 <- eval(newcall)
        tabley <- if (NCOL(y) == 1) table(y) else table(y[, 
                                                          1])
        dft <- data.frame(tabley)
        if (!is.null(freq)) dft[, 2] <- freq
        r <- barplot(dft[, 2]/sum(dft[, 2]), fg = fg.hist, 
                     col = col.hist, axis.lty = 1, border = border.hist, 
                     col.axis = col.axis, col.main = col.main, col.lab = col.lab, 
                     cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis,
                     ylim = ylim, xlim = xlim, ylab = ylab, xlab = xlab, 
                     main = main)
        yy1 <- y1[x1 %in% dft[[1]]]
        lines(r, yy1, type = "h", col = line.col[1], 
              lwd = line.wd, lty = line.ty[1])
        points(r, yy1, col = line.col[1], pch = 21, lwd = 2)
      } else {
        xlim <- c(xmin, xmax)
        main <- paste("proportions of ", deparse(subsY), 
                      sepp = "")
        main <- if (is.null(main)) paste("proportions of ", 
                                         deparse(subsY), sepp = "") else main
        xlab <- if (is.null(xlab)) deparse(subsY) else xlab
        ylab <- if (is.null(ylab)) paste("f()") else ylab
        truehist(y1/bd, xlim = xlim, xlab = "proportions", 
                 ylab = ylab, ylim = ylim, nbins = nbins, col = "gray", 
                 lty = 3, border = border.hist, col.axis = col.axis, 
                 col.main = col.main, col.lab = col.lab, main = main, 
                 cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis, fg = fg.hist)
        lines(fitted(mod), rep(1, length(fitted(mod))), 
              type = "h", col = line.col, lwd = line.wd, 
              lty = line.ty[1])
        points(fitted(mod), rep(0, length(fitted(mod))), 
               col = line.col, pch = 25)
        points(fitted(mod), rep(1, length(fitted(mod))), 
               col = line.col, pch = 24)
      }
    } else {
      y1 <- eval(newcall)
      fy <- if (is.null(freq)) factor(y, levels = x1) else factor(rep(y, 
                                                                      freq), levels = x1)
      notresy <- if (is.null(freq)) factor(y) else factor(rep(y, 
                                                              freq))
      dft <- data.frame(tabley <- xtabs(~fy))
      main <- if (is.null(main)) paste("Barplot of the ", 
                                       deparse(subsY), " and the fitted ", FA$family[2], 
                                       " distribution", sep = "") else main
      r <- barplot(tabley/sum(xtabs(~notresy)), fg = fg.hist, 
                   col = col.hist, axis.lty = 1, border = border.hist, 
                   col.axis = col.axis, col.main = col.main, col.lab = col.lab, 
                   main = main, ylim = ylim, ylab = ylab, xlab = xlab,
                   cex.main=cex.main, cex.lab=cex.lab, cex.axis=cex.axis)
      yy1 <- y1[x1 %in% dft[[1]]]
      lines(r, yy1, type = "h", col = line.col[1], lwd = line.wd, 
            lty = line.ty[1])
      points(r, yy1, col = line.col[1], pch = 21, lwd = 2)
    }
  }, Mixed = {
    stop("Mixed distributions are not implemented yet")
  })
  mod
}


# Rótulos pré-formatados

lab_at=expression("Área do terreno" ~ (m^{2}))
lab_at_vunit_est=expression("Valores unitários estimados" ~ ("R$"~"/"~m^{2})) #PARA QUANDO O UNITÁRIO FOR USADO
lab_at_vunit_obs=expression("Valores unitários observados" ~ ("R$"~"/"~m^{2})) #PARA QUANDO O UNITÁRIO FOR USADO 


########################################################################################################################
# Entrada dos dados e manipulações preliminares 
########################################################################################################################

# Carregamento dos dados via planilha (tibble)

diretorio.1='<...>/dissertacao/dados/Terrenos_R_GIT.xlsx'

# Mudança para a classe "dataframe"

dados_terrenos=as.data.frame(read_excel(diretorio.1))

# Manipulação dos dados
# Formato numérico para a variável DATA
dados_terrenos$DATA=as.numeric(as.Date(dados_terrenos$DATA,format="%m/%d/%y", tz="UTC", origin="1970-01-01"))  

# Removendo 'labels' 
dados_terrenos=clear.labels(dados_terrenos)

# Extraindo apenas as variáveis que serão trabalhadas
dados_treino_terrenos=select(dados_terrenos, REF, DATA,UTM_X, UTM_Y, PAD01, PAD02, VOC01, VOC02, REG01, REG02,
                             REL01, REL02, AT, NAT, VALOR, PAV, IMP) 

# Manipulação dos dados
as.matrix(dados_treino_terrenos)
row.names(dados_treino_terrenos)=(dados_treino_terrenos$REF)
dados_treino_terrenos=dados_treino_terrenos[order(dados_treino_terrenos$DATA),] 
dados_treino_terrenos$UNIT=dados_treino_terrenos$VALOR/dados_treino_terrenos$AT

# Codificando os fatores 

dados_treino_terrenos$NATUREZA[dados_treino_terrenos$NAT==1]="Transação"
dados_treino_terrenos$NATUREZA[dados_treino_terrenos$NAT==0]="Oferta"
dados_treino_terrenos$NATUREZA=factor(dados_treino_terrenos$NATUREZA,
                                      levels =  c("Oferta", "Transação"))

dados_treino_terrenos$IMPLANTACAO[dados_treino_terrenos$IMP==1]="Condomínio"
dados_treino_terrenos$IMPLANTACAO[dados_treino_terrenos$IMP==0]="Isolado"
dados_treino_terrenos$IMPLANTACAO=factor(dados_treino_terrenos$IMPLANTACAO,
                                         levels =  c("Isolado", "Condomínio"))

dados_treino_terrenos$PAVIMENTACAO[dados_treino_terrenos$PAV==1]="Pav. asfáltica"
dados_treino_terrenos$PAVIMENTACAO[dados_treino_terrenos$PAV==0]="Outra"
dados_treino_terrenos$PAVIMENTACAO=factor(dados_treino_terrenos$PAVIMENTACAO,
                                          levels =  c("Outra", "Pav. asfáltica"))

dados_treino_terrenos$RELEVO[dados_treino_terrenos$REL01==0&dados_treino_terrenos$REL02==0]="Plano"
dados_treino_terrenos$RELEVO[dados_treino_terrenos$REL01==1&dados_treino_terrenos$REL02==0]="Aclive"
dados_treino_terrenos$RELEVO[dados_treino_terrenos$REL01==0&dados_treino_terrenos$REL02==1]="Declive"
dados_treino_terrenos$RELEVO=factor(dados_treino_terrenos$RELEVO,
                                    levels =  c("Plano", "Aclive", "Declive"))

dados_treino_terrenos$REGIAO[dados_treino_terrenos$REG01==0&dados_treino_terrenos$REG02==0]="Zona de expansão"
dados_treino_terrenos$REGIAO[dados_treino_terrenos$REG01==1&dados_treino_terrenos$REG02==0]="Anel central"
dados_treino_terrenos$REGIAO[dados_treino_terrenos$REG01==0&dados_treino_terrenos$REG02==1]="Bairros"
dados_treino_terrenos$REGIAO=factor(dados_treino_terrenos$REGIAO,
                                    levels =  c("Zona de expansão", "Anel central", "Bairros"))

dados_treino_terrenos$PADRAO[dados_treino_terrenos$PAD01==0&dados_treino_terrenos$PAD02==0]="Padrão mínimo"
dados_treino_terrenos$PADRAO[dados_treino_terrenos$PAD01==1&dados_treino_terrenos$PAD02==0]="Padrão baixo"
dados_treino_terrenos$PADRAO[dados_treino_terrenos$PAD01==0&dados_treino_terrenos$PAD02==1]="Padrão médio"
dados_treino_terrenos$PADRAO[dados_treino_terrenos$PAD01==1&dados_treino_terrenos$PAD02==1]="Padrão alto"
dados_treino_terrenos$PADRAO=factor(dados_treino_terrenos$PADRAO,
                                    levels =  c("Padrão mínimo", "Padrão baixo", "Padrão médio", "Padrão alto"))

dados_treino_terrenos$VOCACAO[dados_treino_terrenos$VOC01==0&dados_treino_terrenos$VOC02==0]="Industrial/com. atacado"
dados_treino_terrenos$VOCACAO[dados_treino_terrenos$VOC01==1&dados_treino_terrenos$VOC02==0]="Comercial varejo/serviços"
dados_treino_terrenos$VOCACAO[dados_treino_terrenos$VOC01==0&dados_treino_terrenos$VOC02==1]="Residencial"
dados_treino_terrenos$VOCACAO=factor(dados_treino_terrenos$VOCACAO,
                                     levels =  c("Industrial/com. atacado", "Comercial varejo/serviços","Residencial"))

# Removendo observações discrepantes sob quaisquer modelos

remover=c(498,705,721,714,719,591,532,546) 
dados_treino.san=dados_treino_terrenos
rownames(dados_treino.san,dados_treino.san$REF) 
dados_treino.san = dados_treino.san [!(rownames(dados_treino.san) %in% remover), ] 	
dados_treino_terrenos=dados_treino.san

# Removendo elementos com área superior a 50000m2

corte=50000
dados_treino_terrenos = subset(dados_treino_terrenos , AT < corte) #50000


########################################################################################################################
# Investigando possíveis distribuições teóricas para a variável resposta
########################################################################################################################


# fitDist()
dist_terrenos=fitDist(y=UNIT, data=dados_treino.san, k=2, type = "realplus")
sort(dist_terrenos$fits)

# chooseDist()
# distribuição Normal
m1=gamlss(UNIT~ UTM_X+UTM_Y+DATA+AT+NATUREZA+PAVIMENTACAO+IMPLANTACAO+RELEVO,family=NO, data=dados_treino.san)

# Escolhendo outras distribuições com valores na reta dos reais   
t1=chooseDist(m1, type="realplus")
# ordenando pelo GAIC
sort(t1[,2])  

# Distribuição marginal de Y 
# Extraindo o AIC da distribuição marginal de Y (simples referência incial)
m.GA=gamlss(UNIT ~ 1, data=dados_treino.san, family=GA) 
m.GB2=gamlss(UNIT ~ 1, data=dados_treino.san, family=GB2)
m.BCTo=gamlss(UNIT ~ 1, data=dados_treino.san, family=BCTo) 
m.NO=gamlss(UNIT ~ 1, data=dados_treino.san, family=NO) 
GAIC(m.GB2,  m.BCTo, m.GA, m.NO)

# Distribuição condicional de Y 
# Extraindo o AIC da distribuição marginal de Y (simples referência incial)

m.NO.a=gamlss(UNIT~AT+DATA+NATUREZA+IMPLANTACAO+
                PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
              sigma.formula = ~AT+DATA+NATUREZA+IMPLANTACAO+
                PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
              family=NO,
              data=dados_treino.san,
              k = 2,
              n.cyc=20,
              c.crit=0.01,
              method=RS())


m.GA.a=gamlss(UNIT~AT+DATA+NATUREZA+IMPLANTACAO+
                PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
              sigma.formula = ~AT+DATA+NATUREZA+IMPLANTACAO+
                PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
              family=GA,
              data=dados_treino.san,
              k = 2,
              n.cyc=20,
              c.crit=0.01,
              method=RS())

m.GB2.a=gamlss(UNIT~AT+DATA+NATUREZA+IMPLANTACAO+
                 PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
               sigma.formula = ~AT+DATA+NATUREZA+IMPLANTACAO+
                 PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
               nu.formula =  ~AT+DATA+NATUREZA+IMPLANTACAO+
                 PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
               tau.formula = ~AT+DATA+NATUREZA+IMPLANTACAO+
                 PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
               family=GB2(mu.link = "log"),
               data=dados_treino.san,
               k = 2,
               n.cyc=20,
               c.crit=0.01,
               method=RS())

m.BCTo.a=gamlss(UNIT~AT+DATA+NATUREZA+IMPLANTACAO+
                  PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
                sigma.formula = ~AT+DATA+NATUREZA+IMPLANTACAO+
                  PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
                nu.formula =  ~AT+DATA+NATUREZA+IMPLANTACAO+
                  PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
                tau.formula = ~AT+DATA+NATUREZA+IMPLANTACAO+
                  PAVIMENTACAO+RELEVO+REGIAO+PADRAO+VOCACAO,
                family=BCTo(mu.link = "log"),
                data=dados_treino.san,
                k = 2,
                n.cyc=20,
                c.crit=0.01,
                method=RS())

GAIC(m.GB2.a,  m.BCTo.a, m.NO.a, m.GA.a)


########################################################################################################################
# Ajuste do modelo
########################################################################################################################


cont_1=gamlss.control(c.crit = 0.001, n.cyc = 1000, 
                      mu.step = 0.1, 
                      sigma.step = 0.1, 
                      nu.step = 0.1, 
                      tau.step = 0.1, 
                      gd.tol = Inf, iter = 0, trace = TRUE, autostep=FALSE,save = TRUE)

cont_2=glim.control(cc = 0.001, cyc =500,  glm.trace = FALSE, 
                    bf.cyc =500, bf.tol = 0.001, bf.trace = FALSE)

mod_teste=gamlss(UNIT~ ga(~ti(AT, bs='cr'), method = "REML")+
                                NATUREZA+IMPLANTACAO+RELEVO+PAVIMENTACAO+                   
                                ga(~ti(UTM_X, UTM_Y, DATA, bs=c('tp','cr'), d=c(2,1)), method = "REML")+
                                ga(~ti(UTM_X, UTM_Y, bs='tp', d=2), method = "REML")+
                                ga(~ti(DATA, bs='cr'), method = "REML"),
                              sigma.formula = ~  ga(~ti(AT, bs='cr'), method = "REML")+
                                ga(~ti(UTM_X, UTM_Y,bs='tp'), method = "REML")+
                                ga(~ti(DATA, bs='cr'), method = "REML"),
                              nu.formula = ~ ga(~ti(AT, bs='cr'), method = "REML")+
                                ga(~ti(UTM_X, UTM_Y,bs='tp'), method = "REML")+
                                ga(~ti(DATA, bs='cr'), method = "REML"),
                              tau.formula = ~ ga(~ti(AT, bs='cr'), method = "REML"),
                              family=BCTo,
                              data=dados_treino_terrenos,
                              control = cont_1,
                              i.control=cont_2,
                              method = RS())

# modelo 'mod_teste': GAMLSS-RS iteration 150: Global Deviance = 7618.533     


########################################################################################################################
# Diagnóstico 
########################################################################################################################

# Wormplot geral "modificado"

wp_mod(mod_teste, ylim.all = TRUE, main="Worm-plot do modelo proposto",
       cex.lab = 1.2, col.main = "black", col.lab = "black", col.axis = "black",
       cex.main=1.4, cex.axis=1.1) 

# Wormplot por cada variável

wp_DATA=wp(mod_teste, xvar = ~DATA,
           n.inter = 4, line=TRUE, 
           cex=1, col='red', ylim.all = TRUE) 
mtext(side=1, "Worm plot \n(resíduos quantílicos versus DATA)", line=3)


# Para salvar no diretório os coeficientes dos polinômios ajustados ao wormplot

sink("wp_coef_DATA")
wp_DATA$coef
sink()

#Convertendo as datas numéricas para o formato usual
# as.Date(wp_DATA$classes[,1], origin="1970-01-01")
# as.Date(wp_DATA$classes[,2], origin="1970-01-01")

# AIC
GAIC(mod_teste) 

# Plot's báscos
plot(mod_teste, scheme=1)

# Pseudo R2
Rsq(object=mod_teste, type="both")


# Normalidade dos resíduos quantílicos
# obs: zvals=TRUE para a função retornar os valores da estatística Z)

res.quant=residuals(mod_teste, "z-scores")


Z.stat_mod_teste_DATA=Q.stats(resid=res.quant, 
                              xvar = dados_treino_terrenos$DATA,  
                              n.inter = 10, zvals=TRUE)
mtext(side=1, "Variável: DATA", line=4)

sink("z.stats_DATA")
print(Z.stat_mod_teste_DATA, digits=3)
sink()

#Convertendo as datas numéricas para o formato usual
# d1=c(9278.5,11262.5)  
# d2=c(11262.5,11778.5) 
# d3=c(11778.5,12204.5)  
# d4=c(12204.5,12539.5)  
# d5=c(12539.5,12874.5) 
# d6=c(12874.5,13269.5)  
# d7=c(13269.5,13902.5) 
# d8=c(13902.5,15196.5) 
# d9=c(15196.5,16015.5) 
# d10=c(16015.5,18697.5) 
# as.Date(d10, origin="1970-01-01")



# Half Normal plot
par(oma=c(1,1,1,1))
par(bg = "#f7f7f7")
hnp(res.quant,sim = 100, conf = 0.99, 
    halfnormal = TRUE, scale = FALSE, 
    plot.sim = TRUE, verb.sim = FALSE, 
    warn = TRUE, how.many.out = TRUE, 
    print.on = TRUE, paint.out = TRUE,  
    col.paint.out="red", cex=1,
    main="Half Normal Plot do modelo proposto\n (envoltória de 100 simulações)", 
    xlab="", 
    ylab="", 
    cex.main=1.5, cex.axis=1.3)
mtext(side = 1, text = "Quantis teóricos N(0,1)", line = 5, cex=1.3)
mtext(side = 2, text = "Resíduos quantílicos Normalizados", line = 3, cex=1.3)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")

# QQ plot
par(oma=c(1,1,1,1))
par(bg = "#f7f7f7")
qqnorm(res.quant, 
       main="Gráfico quantil-quantil",  cex.main=1.5,  cex.lab=1.3, cex.axis=1.3,
       ylab="Resíduos quantílicos Normalizados",
       xlab="Quantis teóricos N(0,1)");
qqline(res.quant, col = 2, lwd=2, lty=2)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")

# Owen's plot

# Variável DATA
dtop(mod_teste, xvar = dados_treino_terrenos$DATA , n.inter = 4, type= "Owen", conf.level = '95') 
title("Gráfico de Owen: variável DATA \n(nc=0,95)", line = -38)



########################################################################################################################
# Extraíndo os efeitos das covariáveis incorporadas ao preditor da mediana 
# Reajustar os parâmetros da janela gráfica
########################################################################################################################


# Incorporadas diretamente na função gamlss()

par(oma=c(1, 2, 1, 1) + 0.1)
term.plot(object=mod_teste, what = "mu",
          data = dados_treino_terrenos, partial.resid = TRUE,
          terms = 2, se = TRUE, ylim = "common", scheme = "shaded", 
          xlabs = "Natureza da informação", ylabs = "Efeito no preditor da mediana", 
          main = "Efeito da variável ''NATUREZA''", 
          family="sans",
          font.main=1,font.sub=3,font.axis=3,font.lab=3,
          cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1,
          col.term = "red",
          col.shaded = "gray", col.res = "lightblue",
          lwd.term = 2.5,
          cex.res = 0.8)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")


par(oma=c(1, 2, 1, 1) + 0.1)
term.plot(object=mod_teste, what = "mu",
          data = dados_treino_terrenos, partial.resid = TRUE,
          terms = 3, se = TRUE, ylim = "common", scheme = "shaded", 
          xlabs = "Forma de implantação", ylabs = "Efeito no preditor da mediana", 
          main = "Efeito da variável ''IMPLANTACAO''", 
          family="sans",
          font.main=1,font.sub=3,font.axis=3,font.lab=3,
          cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1,
          col.term = "red",
          col.shaded = "gray", col.res = "lightblue",
          lwd.term = 2.5,
          cex.res = 0.8)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")


par(oma=c(1, 2, 1, 1) + 0.1)
term.plot(object=mod_teste, what = "mu",
          data = dados_treino_terrenos, partial.resid = TRUE,
          terms = 4, se = TRUE, ylim = "common", scheme = "shaded", 
          xlabs = "Relevo", ylabs = "Efeito no preditor da mediana", 
          main = "Efeito da variável ''RELEVO''", 
          family="sans",
          font.main=1,font.sub=3,font.axis=3,font.lab=3,
          cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1,
          col.term = "red",
          col.shaded = "gray", col.res = "lightblue",
          lwd.term = 2.5,
          cex.res = 0.8)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")


par(oma=c(1, 2, 1, 1) + 0.1)
term.plot(object=mod_teste, what = "mu",
          data = dados_treino_terrenos, partial.resid = TRUE,
          terms = 5, se = TRUE, ylim = "common", scheme = "shaded", 
          xlabs = "Tipo de pavimentação", ylabs = "Efeito no preditor da mediana", 
          main = "Efeito da variável ''PAVIMENTACAO''", 
          family="sans",
          font.main=1,font.sub=3,font.axis=3,font.lab=3,
          cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1,
          col.term = "red",
          col.shaded = "gray", col.res = "lightblue",
          lwd.term = 2.5,
          cex.res = 0.8)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")


# Incorporadas indiretamente na função gamlss(), usando a função ga() 


# A variável "AT" foi incorporda ao modelo via função ga()
name_1=expression(sqrt(paste("Área total")))
name_2=expression(log(paste("Área total")))
par(mar=c(4, 5,2, 1) + 0.1)
par(bg = "#f7f7f7")
plot(getSmo(object=mod_teste, what = "mu", which = 1), 
     family="sans",
     main="", 
     sub="",las=1,
     font.main=1,font.sub=3,font.axis=3,font.lab=3,
     col.main="black",col.sub="black",col.axis="black",col.lab="black", fg="black",
     col="darkblue", xlab="Área total", ylab="Efeito no preditor da mediana",
     lwd=1, lty=1, cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1)
mtext(side=1, "Efeito da função suavizadora da variável ''AT'' \n(edf=4)",
      line=-32, cex=2)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")


# Os splines de produto tensor foram incorporados ao modelo via função ga()
par(mar=c(4, 4, 6, 1) + 0.1)
par(oma=c(1,1,6,1))
par(bg = "#f7f7f7")
plot(getSmo(object=mod_teste, what = "mu", which = 2),
     family="sans",
     main="", 
     sub="",
     las=0,
     font.main=1,font.sub=1,font.axis=3,font.lab=3,
     col.main="black",col.sub="black",col.axis="black",col.lab="black", fg="black",
     col="darkblue", xlab="Área", ylab="Efeito no preditor da mediana",
     lwd=1, lty=1, cex.lab=1, cex.axis=0.7, cex.main=1, cex.sub=1)
mtext(side=1, "Efeito espaço temporal dado pelo produto tensor \n(suavizador das variáveis ''UTM_X, UTM_Y'' pelo suavizador da variável ''DATA'') \nno preditor  da mediana \n(edf=50,95)", 
      line=-31, cex=1.5)


# Os thin plate splines de produto tensor foram incorporados ao modelo via função ga()
par(mar=c(4, 5, 6, 1) + 0.1)
par(bg = "#f7f7f7")
plot(getSmo(object=mod_teste, what = "mu", which = 3), 
     family="sans",
     main="", 
     sub="",las=0,
     font.main=1,font.sub=3,font.axis=3,font.lab=3,
     col.main="black",col.sub="black",col.axis="black",col.lab="black", fg="black",
     col="darkblue", xlab="UTM_X", ylab="UTM_Y",
     lwd=1, lty=1, cex.lab=1.8, cex.axis=1.4, cex.main=1.5, cex.sub=1)
mtext(side=1, "Efeito espacial dado pela função suavizadora bidimensional \ndas variáveis ``UTM_X,UTM_Y'' \nno preditor da mediana \n(edf=23,08)",
      line=-28, cex=2)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")



par(mar=c(4, 5, 2, 1) + 0.1)
par(bg = "#f7f7f7")
plot(getSmo(object=mod_teste, what = "mu", which = 4), 
     family="sans",
     main="", 
     sub="",las=1,
     font.main=1,font.sub=3,font.axis=3,font.lab=3,
     col.main="black",col.sub="black",col.axis="black",col.lab="black", fg="black",
     col="darkblue", xlab="Data (1=01/01/1970)", ylab="Efeito no preditor da mediana",
     lwd=1, lty=1, cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1)
mtext(side=1, "Efeito da função suavizadora da variável ''DATA'' \n(edf=3,99)",
      line=-32, cex=2)
grid(7,14, lty=1, lwd = 0.4, col = "lightgray")

# Incorporando a data em formato aaaa-mm-dd no eixo 'x'

    d1=as.Date(10000, origin="1970-01-01")
    d2=as.Date(12000, origin="1970-01-01")
    d3=as.Date(14000, origin="1970-01-01")
    d4=as.Date(16000, origin="1970-01-01")
    d5=as.Date(18000, origin="1970-01-01")
    
    par(mar=c(7.5, 5, 5.5, 1) + 0.1)
    par(bg = "#f7f7f7")
    plot(getSmo(object=mod_teste, what = "mu", which = 4), 
         family="sans",
         main="", 
         sub="",las=1,
         font.main=1,font.sub=3,font.axis=3,font.lab=3,
         col.main="black",col.sub="black",col.axis="black",col.lab="black", fg="black",
         col="darkblue", xlab="Data (aaaa-mm-dd)", ylab="Efeito no preditor da mediana",
         lwd=1, lty=1, cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=1,
         xaxt="n")
    axis(1, at=c(10000, 12000, 14000, 16000, 18000),labels=c(d1, d2, d3, d4, d5), cex.axis=1.5)
    mtext(side=1, "Efeito da função suavizadora da variável ''DATA'' \n(edf=3,99)",
          line=-40, cex=2)
    grid(7,14, lty=1, lwd = 0.4, col = "lightgray")




########################################################################################################################
# Salvando o resultado
########################################################################################################################


save(mod_teste, file="mod_teste")
#load("mod_teste")













