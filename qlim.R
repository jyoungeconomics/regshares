soo <- svydesign(ids=~SDMVPSU,weights=~WTDR2D,strata=~SDMVSTRA,data=master,nest=T)

sls5.1 <- svyglm(cost~H+T3+simpson+H*T3+year0708,
               design=soo,family=gaussian(link = "identity"))
summary(sls5.1)
rSquared(y=master$cost,resid=sls5.1$residuals)

#################################################################################################

bag <- bugg[,-which(colnames(bugg) %in% c("shore","s1","Projection61k"))]
bang <- cbind.data.frame(bugg[,c("shore","Projection61k")],bag)
# colnames(bang)[1:2] <- c("shore","t2010")

xxx <- colnames(bang)[-c(1:3)]
fm <- as.formula(paste("shore ~ ", 
                       paste(xxx, collapse= "+")))

b.qlim <- censReg(fm,data = bang,left = 0,
                             right = 1,method="BFGS")
summary(b.qlim)
# save the betas, but the logSigma is not needed for finding the marginal effect - it is a nuisance parameter
boota <- as.matrix(coef(summary(b.qlim))[-which(rownames(coef(summary(b.qlim)))=="logSigma"),1])
xxx <- bang[,-c(2:3)]
# we don't need Y anymore to calculate Xb, so just make it a column of 1 (an intercept in the model)
xxx$shore <- 1
# this would normally just be a vector of nX1 since we are taking the product Xb 
inner <- mean(as.matrix(as.matrix(xxx)%*%boota),na.rm = T)
phi.left <- pnorm(0-inner)
phi.right <- pnorm(1-inner)
# take the normal CDF of the right-most limit of the Xb values minus the left-most
marge <- phi.right-phi.left
# we want the marginal effect of income, or "inc"
marginaleffects <- marge*coef(summary(b.qlim))["inc",1]

#####################################################

# these are references on how to do ordered logit in R

# https://rdrr.io/cran/erer/man/ocME.html
# https://cran.r-project.org/web/packages/erer/erer.pdf
# http://www.public.asu.edu/~gasweete/crj604/old/lectures/Lecture%2010.pdf
# https://cran.r-project.org/web/packages/margins/vignettes/TechnicalDetails.pdf
# http://r.789695.n4.nabble.com/Survey-weighted-ordered-logistic-regression-td804146.html

bang <- cbind.data.frame(bugg[,c("s1","Projection61k")],bag)
# colnames(bang)[1:2] <- c("s1","Projection61k","t2010")

#WARNING WARNING WARNING ------- The thingy is in opposite order!!!!!!!!!! First, fix it...
bang$s1 <- as.factor(-1*(bang$s1-2))
foo <- as.formula(paste("s1~",paste(xxx,collapse = "+")))

# couldn't actually get the survey design as part of the ordered logit, "surveylogit" in SAS
# but this is more or less the code to do it

#repwtdesign <- as.svrepdesign(svydesign(ids = ~1,
#                                        weights = ~Projection61k,data = bang),
#                              type = "bootstrap")
#ologit <- withReplicates(repwtdesign, 
#                          quote(coef(polr(s1~inc, weights=Projection61k))))

ord.log <- polr(foo,data = bang,Hess = T,method = "logistic")
summary(ord.log)
m1.coef <- data.frame(coef(summary(ord.log)))
m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)
m1.coef
moomoo <- exp(-1*coef(ord.log))
