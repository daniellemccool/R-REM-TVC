# Gauss to R: Poisson
# setwd("~/Dropbox/Internship/Population Estimate/R")
b2009 <- read.table("datasets/bestand2009.dat")

TruncPoisFit <- function(data, covs, y.col = 1, it=2500, conv = 1e-7, const = TRUE) {
    y   <- data[, y.col]

    if(const == TRUE){
        x   <- data.matrix(data[, covs])
    }else{
        x   <- data.matrix(cbind(1, data.matrix(data[, covs])))
    }

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
        db      <- colSums(c(1/u) * c(y - u - delta) * ux)
        ddb     <- (1 - p0 * c(u + 1))/c(u*pCap^2)
        hessian <- t(ux)%*%(c(ddb)*ux)
        infor   <- solve(hessian)
        bnew    <- b + infor %*% db

        if(all((abs(bnew - b)) <= conv)){
            print("Converged")
            b <- bnew
            iter <- iter + 1
            break
        }
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
#summary(b2009)
#TruncPoisFit(b2009, 8)
#TruncPoisFit(trunc.3.wide.df, covs=c(4,5), cons=FALSE)
#TruncPoisFit(t.data, covs = 2)

REMFit <- function(data, covs, y.col = 1, it = 2500, conv = 1e-7,
                w = 1, v = 1, const = TRUE){

    y   <- data[, y.col]
    if(const == TRUE){
        x   <- data.matrix(data[, covs])
    } else{
        x   <- data.matrix(cbind(1, data.matrix(data[, covs])))
    }

    col <- ncol(x)
    b   <- rep(0, col)

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

        if(all((abs(bnew - b)) <= conv)){
            print("Converged")
            b <- bnew
            iter <- iter+1
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

#res <- REMFit(trunc.3.wide.df, covs=c(4, 5), w = trunc.3.wide.df$Tact, v = 365, const=FALSE)

#rem.fit <- REMFit(b2009, 8:ncol(b2009), b = c(rep(0, 14)))
#REMFit(t.data, covs = 2, v = 365, w = 365)

