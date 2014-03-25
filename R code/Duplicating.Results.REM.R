# Gauss to R: Poisson
# setwd("~/Dropbox/Internship/Population Estimate/R")
b2009 <- read.table("datasets/bestand2009.dat")
summary(b2009)

    y   <- trunc.3.wide.df[, 1]
    x   <- cbind(1, data.matrix(trunc.3.wide.df[, 4:5]))
    #x   <- matrix(1, ncol = 1, nrow = 125)
    col <- ncol(x)
    n   <- length(y)
    b   <- rep(0, col)


TruncPoisFit <- function(data, covs, y.col = 1, it=25, conv=1e-7) {
    y   <- data[, y.col]
    x   <- cbind(1, data.matrix(data[, covs]))
    #x   <- matrix(1, ncol = 1, nrow = 125)
    col <- ncol(x)
    n   <- length(y)
    b   <- rep(0, col)

    hessian <- matrix(0, ncol=col, nrow=col)
    iter <- 1

    for (i in 1:it) {

        u       <- exp(x%*%b)
        p0      <- exp(-u)
        pCap    <- 1 - p0
        logl    <- sum(y * log(u) - u - log(pCap) - lgamma(y + 1))
        Ey      <- u/pCap
        delta   <- p0 * Ey
        ux      <- c(u)*x
        #hessian <- matrix(0, ncol=col, nrow=col)
        db      <- colSums(c(1/u) * c(y - u - delta) * ux)
        ddb     <- (1 - p0 * c(u + 1))/c(u*pCap^2)
        hessian <- t(ux)%*%(c(ddb)*ux)
        infor   <- solve(hessian)
        bnew    <- b + infor %*% db
        if(all((bnew - b) <= conv)) break
        b    <- bnew
        iter <- iter + 1
    }

        u       <- exp(x%*%b)
        p0      <- exp(-u)
        pCap    <- 1 - p0
        Ey      <- u/pCap
        stde    <- sqrt(diag(infor))
        Nhat    <- colSums(1/pCap)
        var1    <- colSums(-x*c(exp(log(u)-u)/pCap^2))
        var1    <- t(var1)%*%infor%*%var1
        var2    <- colSums(p0/pCap^2)
        stdHT   <- sqrt(var1+var2)

        list("NHat" = Nhat, "coef" = bnew, "iter" = iter, "logl" = logl, "stdHT" = stdHT)
}
summary(b2009)
TruncPoisFit(b2009, 8)
TruncPoisFit(trunc.wide.df, c(0))

REMFit <- function(data, covs, it = 25, conv = 1e-7, b = rep(0, ncol(covs) + 1)){
    # argnames <- names(list(...))
    # if(!("w" %in% argnames)) {w <- data[, 3]}
    # if(!("v" %in% argnames)) {v <- data[, 4]}
    #
    #w <- data[, 3]
    #v <- data[, 4]
    w <- 1
    v <- 1
    y <- data[, 1]
    #x <- data.matrix(data[, 7:ncol(data)])
    #x <- matrix(1, ncol = 1, nrow = 125)
    x   <- cbind(1, data.matrix(data[, covs]))
    iter <- 1

    for (i in 1:it) {
        u    <- exp(x%*%b)
        vu   <- v*u
        wu   <- w*u
        p    <- u^y*exp(-wu)
        p0   <- exp(-vu)
        logl <- sum(log(p) - log(1 - p0))
        db <- c(t(x)%*%(y-wu+vu/(1-(exp(vu)))))
        ddb <- wu - vu * c(exp(vu)*(vu-1)+1)/c((exp(vu) - 1)^2)

        hessian <- t(x)%*%(c(ddb)*x)

        infor <- solve(hessian)
        bnew <- b + infor%*%db

        if(sum((abs(bnew - b)) <= conv)){
            print("Converged")
            break
        }
        bold <- b
        b <- bnew
        iter <- iter+1
    }

    stde <- sqrt(diag(infor))
    tval <- b*stde
    pval <- 2*(qnorm(abs(tval)))
    p0 <- exp(-vu)
    pCap <- 1-p0
    Nhat <- sum(1/pCap)
    #var1 <- c()
    #for j (1,c,1);
    #    var1[j] = sum( -x[, j].*vu.*p0 ./pCap^2 );
    #var1 = var1'*infor*var1;
    #var2 = sumc(p0 ./pCap^2);
    #varHT = var1+var2;
    #stdHT = sqrt(varHT);
    #lowCI = Nhat-1.96*stdHT;
    #uppCI = Nhat+1.96*stdHT;

    list("coef" = bnew, "iter" = iter, "logl" = logl, "Nhat" = Nhat)
}
REMFit(trunc.wide.df, c(), b=c(0))
rem.fit <- REMFit(b2009, 8:ncol(b2009), b = c(rep(0, 14)))

ncol(b2009)
