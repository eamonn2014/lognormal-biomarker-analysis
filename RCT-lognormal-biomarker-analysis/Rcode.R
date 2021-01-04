library(shiny)
library(RColorBrewer)
library(shinythemes)
library(shinyWidgets)
require(MethylCapSig)  # generate log normal correlated vars
#Frank Harrell package 
require(rms)


fig.width <- 900
fig.height <- 950
text1  <- "blah blah"
text2  <- "blah blahx"
set.seed(0878747655)
p3 <- function(x) {formatC(x, format="f", digits=3)}
p4 <- function(x) {formatC(x, format="f", digits=4)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p1 <- function(x) {print(formatC(x, format="f", digits=1),quote=FALSE)}
p1 <- function(x) {formatC(x, format="f", digits=1)}
p0 <- function(x) {formatC(x, format="f", digits=0)}

 
  
 n <- d <-261          # Total patients randomised",
 r=.65          # Correlation between baseline and outcome",
 mu1 =4300      # log-normal mean, baseline",
 sd1=5730       # log-normal standard deviation, baseline",
 mu2 =3300      # log-normal mean, outcome",
 sd2=5400       # "log-normal standard deviation, outcome",
 grp1=.87       # "proportional change Treatment A",
 grp2=.61       # "proportional change Treatment B",
                 
 
 
    
    
    z <- z0 <- mvlognormal(n = n, Mu = c( mu1, mu2),  # learn what this function does
                      Sigma = c( sd1^2,  sd2^2), R = toeplitz( r^(0:1))); # toepliz 
    
 
  
    z <- as.data.frame(z)
    
    
     
    
    # nice plot
    x1 <- stack(z)
    
    z$trt <- 1*(runif(nrow(z))>.5)
    
    prop.change <- grp1/grp2 # true prop change
    
    z$x2 <- ifelse(z$trt %in% 1, z$V2* grp1, z$V2*grp2)  # allow an effect in both arms. 0.7/.875 = .8
    
    z$trt <- ifelse(z$trt %in% 1, "A", "B")    
    
    z$trt <- as.factor(z$trt)
    z<-z[,c(1,3,4)]
    names(z) <- c("BASE","TRTP","AVAL")
    
    logged <- foo2 <- z
    
    
    L <- z %>% gather("BASE", "AVAL", -c(TRTP))
    
    
    require(dplyr)
        
    
    
    df_sum <- L %>% # the names of the new data frame and the data frame to be summarised
      group_by( BASE, TRTP) %>%                      # the grouping variable
      summarise( Ns=length(TRTP),              # unique subjects
                 mean_PL = mean(AVAL, na.rm=TRUE),   # calculates the mean of each group
                 sd_PL = sd(AVAL, na.rm=TRUE),       # calculates the sd of each group
                 #n_PL = length(na.omit(AVAL)),      # calculates the sample size per group
                 SE_PL = sd(AVAL, na.rm=TRUE)/sqrt(length(na.omit(AVAL))), # SE of each group
                 low = mean(AVAL, na.rm=T) - qt(.975, length(na.omit(AVAL))-1)*2*(sd(AVAL, na.rm=T)/ length(na.omit(AVAL))^.5),
                 upp = mean(AVAL, na.rm=T) + qt(.975, length(na.omit(AVAL))-1)*2*(sd(AVAL, na.rm=T)/ length(na.omit(AVAL))^.5),
                 median=median(AVAL, na.rm=T) ,
                 min = min(AVAL, na.rm=T),
                 max = max(AVAL, na.rm=T) ,
                 miss = sum(is.na(AVAL)) 
      )
    
    
    L$AVAL <- log(L$AVAL)
    df_sumstat2 <- L %>% # the names of the new data frame and the data frame to be summarised
      group_by( BASE, TRTP) %>%                      # the grouping variable
      summarise( Ns=length(TRTP),                    
                 mean_PL = mean(AVAL, na.rm=TRUE),   # calculates the mean of each group
                 SE_PL = sd(AVAL, na.rm=TRUE)/sqrt(length(na.omit(AVAL))), # SE of each group
                 sd_PL = sd(AVAL, na.rm=TRUE),       # calculates the sd of each group
                 min = min(AVAL, na.rm=T),
                 median=median(AVAL, na.rm=T) ,
                 max = max(AVAL, na.rm=T) ,
                 miss = sum(is.na(AVAL)) 
      )
    df_sumstat2 <- as.data.frame(df_sumstat2)
    
    names(df_sumstat2) <- c("Timepoint","Trt","N","Mean","SE","SD","Minimum","Median","Maximum","Missing")
    
   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    trt.effx <- Norm.2.logN()$trt.eff.1  
    
    ss <- reshape::melt(ss[c("Timepoint","Trt","Mean")])
    
    A.AVAL <- ss[ss$Timepoint %in% "AVAL" & ss$Trt %in% "A" , "value"]
    B.AVAL <- ss[ss$Timepoint %in% "AVAL" & ss$Trt %in% "B" , "value"]
    A.BASE <- ss[ss$Timepoint %in% "BASE" & ss$Trt %in% "A" , "value"]
    B.BASE <- ss[ss$Timepoint %in% "BASE" & ss$Trt %in% "B" , "value"]
    
    Y.DIFF <- B.AVAL - A.AVAL
    word <- ifelse(Y.DIFF > 0 , "higher","lower")
    
    B.DIFF <- B.BASE - A.BASE
    word2 <- ifelse(B.DIFF > 0 , "higher","lower")
    
    
    cat(paste0("On the log scale, on average, the patients treated in arm B were ", p3(Y.DIFF),  " units ",word, " than the patients under treatment A, \nthat is y= ", 
               p3(B.AVAL)," under treatment B and y= ", p3(A.AVAL), " for treatment A."))
    
    cat(paste0("\nBut the two groups differed in their pre treatment version of the predictor, the patients treated in arm B were ", 
               p3(B.DIFF),  " units ",word2, " \nthan the patients under treatment A, that is y= ", p3(B.BASE)," under treatment B and y= ", p3(A.BASE), " for treatment A."))
    
    cat(paste0("\nAfter adjusting for this difference we obtained an estimated treatment effect B-A of ", p3(trt.effx[1] [[1]])," units."))
    
 
    
   ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    
    
    # log the values
    foo2$AVAL <- log(foo2$AVAL)
    foo2$BASE <- log(foo2$BASE)
    
    # endpoints
    foo2$CHG  <- foo2$AVAL-foo2$BASE                    # change from baseline not used
    foo2$PCHG <- ((foo2$AVAL-foo2$BASE)/foo2$BASE)*100  # % change from baseline not used
    
    
 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # use Normal mean and sd to get back the lognormal mean and lognormal SD!
  
 
    xx =  foo2
    
    #summary(lm(CHG ~ TRTP + BASE, xx))
    
    #summary(lm(AVAL ~ TRTP + BASE, xx))
    
    #t.test(AVAL~TRTP, x, paired =FALSE) # this is what we powered for
    
    #table(xx$TRTP)
    
    ddz <<- datadist(xx) ;    options(datadist='ddz')
    
    #means: analysis on logged values. note the response is chg from baseline (1-new/old)100 == 100(old-new)/old
    f <- (ols(AVAL ~ TRTP + BASE, xx)) 
    
    # mean.B <- rms::Predict(f, TRTP = 'B', BASE=mean(foo2$BASE))
    # mean.A <- rms::Predict(f, TRTP = 'A', BASE=mean(foo2$BASE))
    
    resid= f$stats["Sigma"][[1]]
    
    rms::contrast(f, list( TRTP = 'B'), list( TRTP = 'A'))
    
    #means: analysis on logged values.
    trt.eff.2 <- rms::Predict(f, TRTP = 'B', BASE=mean(xx$BASE)) # matches sas #
    trt.mu.B <- exp(unlist(trt.eff.2[,3:5]))
    
    #rms::Predict(f, TRTP = 'B', BASE) 
    trt.eff.3 <- rms::Predict(f, TRTP = 'A', BASE=mean(xx$BASE))
    trt.mu.A <- exp(unlist(trt.eff.3[,3:5]))
    
    #t.test(AVAL~TRTP, xx, paired =FALSE) # this is what we powered for
    
    #trt effect, difference on the log scale 
    trt.eff.1 <- rms::contrast(f, list(TRTP='B',BASE=mean(xx$BASE)), 
                               list(TRTP='A',BASE=mean(xx$BASE)), type='average')
    
    #plot(Predict(f))
    #p <- Predict(f )
    #ggplot(p )
    
    #exponentiate the mean values
    #rms::Predict(f, TRTP = 'B', BASE=mean(xx$BASE), fun=exp)  
    
    est <- (rms::contrast(f, list(TRTP='B',BASE=mean(xx$BASE)), 
                          list(TRTP='A',BASE=mean(xx$BASE))))
    
    res <- c(est$Contrast, est$Lower, est$Upper) 
    
    names(res) <- c("Estimate", "Lower 95%CI", "Upper 95%CI")
    
    #exponentiate
    res2 <- exp(res)
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    ss <- reshape::melt(df_sumstat2[c("Timepoint","Trt","Mean")])
    
    A.AVAL <- ss[ss$Timepoint %in% "AVAL" & ss$Trt %in% "A" , "value"]
    B.AVAL <- ss[ss$Timepoint %in% "AVAL" & ss$Trt %in% "B" , "value"]
    A.BASE <- ss[ss$Timepoint %in% "BASE" & ss$Trt %in% "A" , "value"]
    B.BASE <- ss[ss$Timepoint %in% "BASE" & ss$Trt %in% "B" , "value"]
    
    Y.DIFF <- B.AVAL - A.AVAL
    word <- ifelse(Y.DIFF > 0 , "higher","lower")
    
    B.DIFF <- B.AVAL - A.AVAL
    word2 <- ifelse(B.DIFF > 0 , "higher","lower")
    
    
    s1<-    paste("On the log scale, on average, the patients treated in arm B were ", p2(Y.DIFF),  " units ",word, " 
    than the patients under treatment A, that is y= ", p2(B.AVAL)," under treatment B and y= ", p2(A.AVAL), " for treatment A.")
    
    s2<-   paste("But the two groups differed in their pre treatment version of the predictor
    , the patients treated in arm B were", p2(B.DIFF),  " units ",word2, " 
    than the patients under treatment A, that is y= ", p2(B.BASE)," under treatment B and y= ", p2(A.BASE), " for treatment A.")
    
    s3 <- paste("Afer adjusting for this difference we obtained an estimated treatment effect B-A of ", 
                p3(trt.eff.1[1] [[1]])," units.")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  

  
  
  #---------------------------------------------------------------------------
  # Plot a scatter plot of the data, joining patient data

    dd = foo2
    
    dd$ID<- 1:nrow(dd)
    dd$CHG <- NULL
    dd$PCHG <- NULL
    
    require(tidyverse)
    #reshape to wide 
    
    L <- dd %>% gather("TIME", "AVAL", -c(ID,TRTP))
    
    
    linecolors <- c("#714C02", "#01587A", "#024E37")
    fillcolors <- c("#9D6C06", "#077DAA", "#026D4E")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#nice  plot  
    
    #http://stulp.gmw.rug.nl/ggplotworkshop/comparinggroupstatistics.html
    
    L$GRP <- paste( L$TRTP,L$TIME, sep=".")
    
    L$GRP <- gsub( "AVAL","FVAL" , L$GRP) ## helps with ordering of factor on x axis
    
    L$GRP2 <- as.numeric(as.factor(L$GRP))
    
    b <- runif(nrow(L), -0.35 , 0.35 )
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# v nice plot , no means tho , reusing transparent colours
    
    linecolors <- c("#714C02", "#01587A", "#024E37")
    fillcolors <- c("#9D6C06", "#077DAA", "#026D4E")
    namez <- c("Treatment A\n BASELINE", "Treatment A\n FOLLOW UP" ,"Treatment B\n BASELINE", "Treatment B\n FOLLOW UP")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GREAT START
    
    require(dplyr)
    
    df_sum <- L %>% # the names of the new data frame and the data frame to be summarised
      group_by( TIME, TRTP) %>%                      # the grouping variable
      summarise( Ns=length(unique(ID)),              # unique subjects
                 mean_PL = mean(AVAL, na.rm=TRUE),   # calculates the mean of each group
                 sd_PL = sd(AVAL, na.rm=TRUE),       # calculates the sd of each group
                 #n_PL = length(na.omit(AVAL)),      # calculates the sample size per group
                 SE_PL = sd(AVAL, na.rm=TRUE)/sqrt(length(na.omit(AVAL))), # SE of each group
                 low = mean(AVAL, na.rm=T) - qt(.975, length(na.omit(AVAL))-1)*2*(sd(AVAL, na.rm=T)/ length(na.omit(AVAL))^.5),
                 upp = mean(AVAL, na.rm=T) + qt(.975, length(na.omit(AVAL))-1)*2*(sd(AVAL, na.rm=T)/ length(na.omit(AVAL))^.5),
                 median=median(AVAL, na.rm=T) ,
                 min = min(AVAL, na.rm=T),
                 max = max(AVAL, na.rm=T) ,
                 miss = sum(is.na(AVAL)) 
      )
    
    L2 <- merge(L, df_sum)  # merge data and summary stats
    
    linecolors <- c("#714C02", "#01587A", "#024E37")
    fillcolors <- c("#9D6C06", "#077DAA", "#026D4E")
    namez <- c("Treatment A\n BASELINE", "Treatment A\n FOLLOW UP" ,"Treatment B\n BASELINE", "Treatment B\n FOLLOW UP")
    
    pl1 <-ggplot(L2, aes(x=GRP2 + b, y=(AVAL), fill = factor(TRTP))) + #+ #colour = factor(TRTP), fill = factor(TRTP))) +
      geom_point( shape = 21, alpha = 0.3, size = 2) +
      labs(x="", y="Log of response") +
      scale_color_manual(values=linecolors) +
      scale_fill_manual(values=fillcolors) +
      geom_line( aes(group = ID, x = GRP2 + b  ) , linetype="solid" , alpha=0.05)   + 
      geom_errorbar(data = L2 ,
                    mapping = aes(x = GRP2, y = mean_PL, 
                                  ymin = mean_PL - 2*SE_PL, 
                                  ymax = mean_PL + 2*SE_PL), 
                    size=1, color="black", width=.1) +
      geom_point(data=L2, mapping = aes(x = GRP2, y = mean_PL), size=1, color="red") +
      theme(axis.text.x=element_text(angle=0, hjust=.5 ,vjust = 1)) +
      scale_x_discrete(
        breaks=c(namez ) ,
        limits=c(namez ) 
      ) + 
      guides(fill = guide_legend(title = "Treatment", title.position = "top")) +
      theme_bw() +
      theme(legend.position="none") +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GREAT END
    print(pl1)
    print(prop.change)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
  #~~~~~~~~~~~~~~~~~~~~~~~~~ANCOVA PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
 
    
    fit <-  lm(AVAL ~  BASE + TRTP, foo2)
    
    # lm(AVAL ~  BASE, foo2)  # simple regression just checking correlation 
    # cor(foo2$BASE,foo2$AVAL)* sd((foo2$AVAL)/ sd(foo2$BASE))  # slope
    # cor(foo2$BASE,foo2$AVAL)
    
    
    #fit
    #require(ggplot2)
    digitz=3
    p <- ggplot(data = foo2,  aes(y=AVAL, x=BASE, color= TRTP)) + 
      geom_point() +
      # theme(legend.position="none") +
      geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1], col='red') +
      geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1]+ fit$coefficients[3], col='blue2') +
      xlab("Log baseline measurement of endpoint") +
      ylab("Log endpoint")  +
 
      labs(title = paste("Treatment effect, true proportional change ",grp2, "(B) /",grp1, "(A) = ",p4(1/prop.change), 
                         ". On the log scale (B-A) ",p4(log(grp2))," - ",   p4(log(grp1))," = ", p4(-log(prop.change)))) +
      theme_bw()  
    p1 <- p + theme(axis.line=element_blank(),
                    
                    panel.background=element_blank(),
                    
                    plot.margin = unit(c(1,1,1,1), "cm")
                    
                    
    )
    
    p1 <- p1+labs(col="treatment")
    
    
    # add the equation to the plot
    lm_eqn <- function(df){
      m <- lm(AVAL ~ BASE + TRTP, df);
      eq <- substitute(italic(y) == a + b %.% italic(baseline)* + g %.% italic(treatmentB)*"" , 
                       list(a = format(unname(coef(m)[1]), digits = 3),
                            b = format(unname(coef(m)[2]), digits = 3),
                            g = format(unname(coef(m)[3]), digits = 3)))
      
      if (coef(m)[3] >= 0)  {
        eq <- substitute(italic(y) == a + b %.% italic(baseline)* + g %.% italic(treatmentB)*"",
                         list(a = format(unname(coef(m)[1]), digits = 3),
                              b = format(unname(coef(m)[2]), digits = 3),
                              g = format(unname(coef(m)[3]), digits = 3)))
      } else {
        eq <- substitute(italic(y) == a + b %.% italic(baseline)*  g %.% italic(treatmentB)*"",
                         list(a = format(unname(coef(m)[1]), digits = 3),
                              b = format(unname(coef(m)[2]), digits = 3),
                              g = format(unname(coef(m)[3]), digits = 3)))
      }
      
      as.character(as.expression(eq))
    }
    
    
    p1 + annotate('text', x = 4.5, y = 11.5, 
                  label = lm_eqn(foo2) ,parse = TRUE, size=5) + 
      
      xlim(3,12) +ylim(3,12)  + geom_abline(slope=1, intercept=0, linetype="dotted")
    
     
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    g <- f$residuals
    
    summary(g)
    
    par(mfrow=c(1,2))
    qqnorm(g)
    qqline(g, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    h <- hist(g, density = 10, breaks=100, col = "lightgray",
              xlab = " ", main = "Residuals",
              xlim=c(-4,4))
    
    xfit <- seq(min(g)-.5, max(g)+1, length = 40)
    
    yfit <- dnorm(xfit, mean = mean(g), sd = sd(g))
    yfit <- yfit * diff(h$mids[1:2]) * length(g)
    
    yfit2 <- dnorm(xfit, mean = 0, sd = f$stats["Sigma"][[1]])
    yfit2 <- yfit2 * diff(h$mids[1:2]) * length(g)
    
    lines(xfit, yfit, col = "black", lwd = 2)
    lines(xfit, yfit2, col = "blue", lwd = 2)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    par(mfrow=c(1,1))
    
     resid= f$stats["Sigma"][[1]]
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
    
    # see my previous app https://raw.githubusercontent.com/eamonn2014/sample-size-for-RCT-endpoint-of-log-normally-distributed-biomarker-/master/power/app.R
    # convert lognormal <-> normal
    v <- sd1^2
    m <- mu1
    
    phi = sqrt(v + m^2);
    mu    = log(m^2/phi)           # mean of log(Y)      
    sigma = sqrt(log(phi^2/m^2))   # std dev of log(Y)  
    
    
    
    sd <- sigma
    
    prop.change <- grp1/grp2 # true prop change
    
    Po <- power.t.test(n= d/2, delta =log(prop.change), sd=sd, sig.level=0.05,
                       power=NULL, type="two.sample", alternative=c("two.sided"))
    
    
     Po
     Po$power
    
    
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # quick check on statement I make
  # moving from median on transformed and untransformed scales
     set.seed(123)
     n <- 9
     x<-rlnorm(n)
     x<- sort(x)
     lx <- log(x)
     
     
     x
     lx
     summary(x)
     summary(lx)
     