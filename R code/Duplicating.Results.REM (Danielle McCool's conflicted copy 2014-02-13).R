# Gauss to R: Poisson
setwd("~/Dropbox/Internship/Population Estimate/R")
b2009 <- read.table("datasets/bestand2009.dat")
summary(b2009)

{TruncPoisFit <- function(data, it=25, conv=1e-7) {
    y   <- data[, 1]
    x   <- data.matrix(data[, 7:ncol(data)])
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

TruncPoisFit(b2009)
}
b.val <- c(-7.656, 0.411,  0.377,  0.518,  0.257,
            0.576, -0.947, -0.906, -0.540, -0.049,
            -1.709,  0.066, -0.200,  0.024 )

{REMFit <- function(data, it = 25, conv = 1e-7, b = rep(0, 14), ...){
    argnames <- names(list(...))
    if(!("w" %in% argnames)) {w <- data[, 3]}
    if(!("v" %in% argnames)) {v <- data[, 4]}
    y <- data[, 1]
    x <- data.matrix(data[, 7:ncol(data)])
    iter <- 1

    for (i in 1:it) {
        u    <- exp(x%*%b)
        vu   <- v*u
        wu   <- w*u
        p    <- u^y*exp(-wu)
        p0   <- exp(-vu)
        logl <- colSums(log(p) - log(1 - p0))
        db <- t(x)%*%(y-wu+vu/(1-(exp(vu))))
        ddb <- wu - vu * exp(vu)*(vu-1)+1/(exp(vu) - 1)^2
        hessian <- t(x)%*%(c(ddb)*x)

        infor <- solve(hessian)
        bnew <- b + infor%*%db

        if(all((abs(bnew - b)) <= conv)){
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

REMFit(b2009, b = b.val, it=25000, w = 1, v = 1)
}

b.test <- rep(0.02, 14)

b <- 0
    iter <- 1

proc(4) =  ZTPsurvival(y,x,v,w,covx,datfile);

local c,n,b,iter,u,vu,wu,p,p0,pCap,logl,db,ddb,hessian,
      infor,bnew,stde,tval,pval,Nhat,var1,var2,varHT,
      stdHT,lowCI,uppCI;

c = cols(x);
n = rows(x);
b = zeros(c,1);

iter = 1;
  do while iter <= 25;
    u    = exp(x*b);
    vu   = v.*u;
    wu   = w.*u;
    p    = u^y.*exp(-wu);
    p0   = exp(-vu);
    logl = sumc(ln(p)-ln(1-p0));
/* first derivative */
    db   = x'*(y-wu+vu./(1-exp(vu)));
/* hessian */
    ddb     = wu-vu.*(exp(vu).*(vu-1)+1)./(exp(vu)-1)^2;
    hessian = x'*(ddb.*x);
    infor   = inv(hessian);
    bnew    = b + infor*db;
    if abs(bnew - b) <=  1e-7;
      b   = bnew;
      goto conv;
    else;
      b = bnew;
    endif;
  iter = iter+1;
endo;
conv:

stde = sqrt(diag(infor));
tval = b./stde;
pval = 2*(1-cdfn(abs(tval)));
pCap = 1-p0;
Nhat = sumc(1/pCap);
var1 = zeros(c,1);
for j (1,c,1);
  var1[j] = sumc( -x[.,j].*vu.*p0 ./pCap^2 );
endfor;
var1 = var1'*infor*var1;
var2 = sumc(p0 ./pCap^2);
varHT = var1+var2;
stdHT = sqrt(varHT);
lowCI = Nhat-1.96*stdHT;
uppCI = Nhat+1.96*stdHT;

/* printing results */
format /rd 1,0;
print "===================================================";
print "      ZERO-TRUNCATED RECURRENT POISSON-EVENT MODEL     ";
print "           datafile  : " $datfile;
print "           data      : " datestr(0);
print "           observed  : " n;
print "===================================================";
print "              PARAMETER ESTIMATES";
print "---------------------------------------------------";
print "depvars   parameter   st.error   t-value    p-value";
print "===================================================";
for i (1,c,1);
  format /ld 8,0;
  print  $covx[i];;
  format /rd 10,4;
  print  bnew[i]~stde[i];;
  format /rd 9,2;
  print tval[i];;
  format /rd 10,4;
  print pval[i];
endfor;
format /rd 10,0;
print "===================================================";
print "      Horvitz-Thompson estimates of N";
print "---------------------------------------------------";
print "loglikelihood:                " logl;
print "Nhat:                         " Nhat;
print "Nhat variance term 1:         " var1;
print "Nhat variance term 2:         " var2;
print "Nhat 95% confidence interval: " lowCI~uppCI;
print "===================================================";


retp(varHT,infor,b,Nhat);
endp;