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


ui <- fluidPage(
    
  setBackgroundColor(
    color = c( "#2171B5", "#F7FBFF"), 
    gradient = "linear",
    direction = "top"
  ),
    # App title ----
    titlePanel("The analysis of an RCT based on a continuous endpoint that is log-normal distributed"),
    
    # Sidebar layout with input and output definitions ----
    div(p("Imagine you are using the result of a laboratory test as the primary endpoint in your RCT. Laboratory tests are typically skewed and therefore a natural log transformation is often applied so that the distribution is more normal looking. 
                  This means the original distribution is assumed log-normal. In many situations we carry out inferences on logarithims of quantites and then transform the results back to an interpretable scale. Thus often using normal theory, distribution of odds ratios and hazard ratios are in fact log-normal distributions.
        The objective is to perform an analysis of a log-normally distributed laboratory test result that is the primary endpoint. We transform to the normal distribution. An ANCOVA analysis is used on the transformed data with baseline test result as a covariate and treatment as the covariate of interest. It is best to model the actual raw response (the approach here). Percent change from baseline is not recommended.
          It can easily be shown that change from baseline analysis and analysis of the raw data are equivalent analyses. 
          EMA Guideline on adjustment for baseline covariates states : 'Note that when the baseline is included as a covariate in a standard linear model, the estimated treatment effects are identical for both ‘change from baseline’ and the ‘raw outcome’ analysis.\n
          The data is simulated log-normal data using the MethylCapSig package. Use the slider inputs below to investigate population parameters and sample size of interest. Click the 'Simulate new sample' button to generate another dataset with the same parameters. Click on the tabs to explore the analysis. There is also a tab to investigate power.")),
    
    
    
    br(),
  
    actionButton(inputId='ab1', label="RShiny code",   icon = icon("th"), 
               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/lognormal-biomarker-analysis/master/RCT-lognormal-biomarker-analysis/app.R', '_blank')"),   

    actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                 onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/lognormal-biomarker-analysis/master/RCT-lognormal-biomarker-analysis/Rcode.R', '_blank')"),   
    
    actionButton("resample", "Hit to simulate a new sample"),
    br(), br(),
    tags$a(href = "https://en.wikipedia.org/wiki/Log-normal_distribution", "Read more about the log-normal distribution."),
    
  
  #######################################################################
 # br(),
  # actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
  #              onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/sample-size-for-RCT-endpoint-of-log-normally-distributed-biomarker-/master/power/app.R', '_blank')"),   
  # actionButton("resample", "Simulate a new sample"),
  # br(),
  
  
  tags$style(type="text/css", ".span8 .well { background-color: #00BFFF; }"),
  
  
  # actionButton(inputId='ab1', label="Rshiny code",   icon = icon("th"),   
  #              onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/sample-size-for-RCT-endpoint-of-log-normally-distributed-biomarker-/master/power/app.R', '_blank')"),   
  # actionButton("resample", "Hit to simulate a new sample"),
  # br(),  
  tags$style(".well {background-color:#A9A9A9 ;}"), ##ABB0B4AF
  
  tags$head(
    tags$style(HTML('#ab1{background-color:orange}'))
  ),
  
  tags$head(
    tags$style(HTML('#resample{background-color:orange}'))
  ),
  
  #######################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  #  br(),
    br(),
    #p(strong("Generate true population parameters:")),
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
          p(strong("Generate true population parameters:")),
          div(p("Select the number of patients randomised 1:1 to each treatment, the correlation between the outcome measure and baseline version, log-normal means and the log-normal SDs, the treatment effect is the ratio of the changes in the two treatment arms.")),
         
            br(),
          sliderInput("d",
                      "Total patients randomised",
                      min=5, max=2000, step=1, value=261, ticks=FALSE),
            sliderInput("r",
                        "Correlation between baseline and outcome",
                        min=0, max=1, step=.01, value=.65, ticks=FALSE),  
          
          
          sliderInput("mu1",
                      "log-normal mean, baseline",
                      min=1, max=20000, step=1, value=4300, ticks=FALSE), 
            sliderInput("sd1",
                        "log-normal standard deviation, baseline",
                        min=1, max=20000, step=1, value=5730, ticks=FALSE), 
          
          sliderInput("mu2",
                      "log-normal mean, outcome",
                      min=1, max=20000, step=1, value=3300, ticks=FALSE),
            sliderInput("sd2",
                        "log-normal standard deviation, outcome",
                        min=1, max=20000, step=1, value=5400, ticks=FALSE), 
          
          
          
          
            sliderInput("grp1",
                        "proportional change Treatment A",
                        min=0, max=1, step=.01, value=0.87, ticks=FALSE),
            sliderInput("grp2",
                        "proportional change Treatment B",
                        min=0, max=1, step=.01, value=0.61, ticks=FALSE)#,  # 0.8714286 0.7/.875 = .8
       
                      ),

        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        
                        tabPanel( div(h4(tags$span(style="color:black", "Distributions of outcome measure"))),
                                 value=4, h3("Before and after log transformation, showing baseline and outcome data, by treatment group."),
                                 plotOutput("plot4"),
                                 h5("The top two plots show the untransformed values. Here we are looking for a proportional change, this is hard to see. The bottom two plots show the same data log transformed. The difference is now easier to see and the data more normally distributed. Note with regard to the 2nd panel from the top, the y-axis may change considerably compared to the top panel, and so the same distribution can look radically different, but it is just the scale.")
                              , #, br(),
                              verbatimTextOutput("summary2")
                        ),
                        
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel( div(h4(tags$span(style="color:black", "Scatter plot"))),
                           h3("A plot of the log transformed data"),
                                 p('A plot of the individual data points, by treatment group with pre-post datapoints for each patient joined by a faint line. Random uniform noise (jitter) added horizontally to aid visualisation.'
                                   ,'On the left, the biege points, are the patients randomised to treatment A, On the right, the blue points, patients randomised to treatment B.
                          We show the crude means plus minus 2 SEs. The treatment effect estimate will be the difference of the within arm differences adjusting for the baseline. As we are analysing on the log scale the estimates may be exponentiated to present a proportional change.
                              
                              The geometric mean is simply exponentiating the models estimates.'),
                                 br(),plotOutput("plot"),#,     
                                 #h5("True  ....."),
                                 verbatimTextOutput("summary3")
                        ),
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel( div(h4(tags$span(style="color:black", "Treatment effect"))),
                          h3("Estimate the treatment effect using ANCOVA model"),
                                 p('This is the result of interest, the difference on the log scale with 95% confidence.'), 
                                 verbatimTextOutput("summary") ,
                                 p('The log scale is not very intuitive so we back transform and present below. This is the result of interest, on the more relevant original scale, the proportional change adjusted for baseline with 95% confidence (this is the ratio of adjusted geometric means (B/A))'), 
                                 verbatimTextOutput("summary99") ,
                                 p('A reminder of the true population parameters for comparison'), 
                                 verbatimTextOutput("summary999") 
                        ),
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                         tabPanel( div(h4(tags$span(style="color:black", "Adjusted means"))),
                        h3("Estimate the adjusted means from ANCOVA model"),
                                 #p('The model we fit is an ', strong('ANCOVA') ), 
                                 #br(),
                                 tags$a(href = "https://www.ncbi.nlm.nih.gov/pubmed/16921578", "Further information on ANCOVA."),
                                 br(),
                                 br(),
                                
                                 p('yhat is the adjusted mean with 95% confidence in group A, adjusted to the stated BASE value on log scale'),
                                 verbatimTextOutput("summaryx3"),
                                 p('yhat is the adjusted mean with 95% confidence in group B, adjusted to the stated BASE value on log scale'),
                                 verbatimTextOutput("summaryx2"),
                                 p('Geometric Mean of outcome measure treatment A'), 
                                 verbatimTextOutput("muA"),
                                 p('Geometric Mean of outcome measure treatment B'), 
                                 verbatimTextOutput("muB")
                        ),
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel( div(h4(tags$span(style="color:black", "ANCOVA"))),
                       
                                 h3("Analysis of covariance"),
                                 p('Perform analysis of covariance. The difference between the parallel lines is a measure of the treatment effect.'),
                                 plotOutput("plot2"),
                                p("True model output presents the treatment effect in terms of B-A"),br(),
                                 verbatimTextOutput("summaryx4")
                                
                                 ),   
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel( div(h4(tags$span(style="color:black", "Assumption check"))),
                                  
                          h3("Assess normality of the residuals"),
                                 p('Look left at the distribution of the residuals and assess normality assumption.'),
                                 br(), plotOutput("plot3"),  h5("The model residual estimate is printed below (on the log transformed scale)..... useful to power a follow up study."),
                                 # ),
                                 tableOutput("table")),
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        tabPanel( div(h4(tags$span(style="color:black", "Power t-test"))),
                        h3("Sample Size for a two-sample t-test"),
                                 p('The goal of this analysis is to estimate the power for a test intended to
                      determine if there is a difference in the outcome of interest. The computations are based on a ',strong('t-test') ,' for two independent 
                      countinuous populations and are performed for a two-sided hypothesis test. This is a suboptimal way of powering this tudy design as we are ignoring baseline information and correlation, only focussing on follow up.'),
                               #  br(),
                                # tags$a(href = "http://en.wikipedia.org/wiki/Student's_t-test", "More detail about t-test."),
                                 # br(),
                                 # br(),
                                 
                                 p('\nLook left at the true population parameters, in this example ',strong('power'),'is determined,
                                   the ',strong('sd') ,'below is the normal distribution sd and follows 
                                   from the ',strong('log-normal standard deviation, baseline,'), 'slider, the', strong('log-normal mean, baseline') ,'is 
                                   actually used in the calculation transforming to the normal distribution, ', strong('delta') ,
                                   'is the hypothesised difference of importance and is the log of ratio of the two proportional change sliders, 
                                   the sample size ', strong('n') ,' is half the', strong('Total patients randomised') ,' selected on the left. 
                                   The canned power calculation function in R is used to determine the',strong('power.')),
                                   
                                 br(),
                                 
                               #  p('Interesting to compare the symmetry around 1 (no change), select sliders to get',strong('delta 0.8'),'and then', strong('delta 1.25'), ', also try 0.7/0.87 ~ 0.8'), 
                               verbatimTextOutput("summary22"),  
                               verbatimTextOutput("power"))
                        
                          
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                      
                        # 
                        # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        # 
                        # tabPanel("Notes", value=3, #h3("Some notes and a further reference"),
                        #          p('Formulae to swap back and forth between the log-normal and the normal distribution'),
                        #          br(),
                        # 
                        #          withMathJax(
                        #              helpText('We have
                        #        $${{Y}\\sim{LN}}{\\left({\\mu,\\thinspace\\sigma^2}\\right)}\\!$$')),
                        # 
                        #          withMathJax(
                        #              helpText('then,
                        #        $${{log(Y)}\\sim{N}}{\\left({\\mu,\\thinspace\\sigma^2}\\right)}\\!$$')),
                        # 
                        #          withMathJax(
                        #              helpText('We wish to know,
                        #        $${ \\sim{N} \\left({\\mu,\\thinspace\\sigma^2}\\right)}\\!$$')),
                        # 
                        #          helpText('Let\'s calculate the normal distribution parameters,$${ \\phi =sqrt({\\sigma^2}+{\\mu^2}  )}\\!$$') ,
                        # 
                        #          helpText('$${ \\mu =log({\\mu^2}/{\\phi}  )}\\!$$') ,
                        # 
                        #          helpText('$${ \\sigma = sqrt(log({\\phi^2}/{\\mu^2}  ))}\\!$$',
                        # 
                        #                   'Now let\'s revert back to log-normal distribution parameters,
                        #                         $${ \\mu = \\exp({\\mu}+{0.5\\sigma^2}  )}\\!$$'),
                        # 
                        #          helpText('
                        #                         $${ \\sigma = {\\mu}({sqrt(\\exp(\\sigma^2)-1)}  )}\\!$$'),
                        # 
                        #          helpText('Alternative equivalent parameterisation of sigma...
                        #                         $${ \\sigma = sqrt(\\exp(2\\mu+\\sigma^2) (\\exp(\\sigma^2)-1)  )}\\!$$'),
                        # 
                        # 
                        #          br(),
                        #          tags$a(href = "https://blogs.sas.com/content/iml/2014/06/04/simulate-lognormal-data-with-specified-mean-and-variance.html", "Blog on simulating lognormal data"),
                        #          br(),
                        #          br(),
                        #          br()
                        # 
                        # 
                        # 
                        # )
                        )
            
        )
    )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






server <- shinyServer(function(input, output) {

  
  random.sample <- reactive({
    # Dummy line to trigger off button-press
    foo <- input$resample
    
    n  <-  input$d
    mu1 <- input$mu1
    mu2 <- input$mu2
    sd1 <- input$sd1
    sd2 <- input$sd2
    r <-   input$r
    
    z0 <- mvlognormal(n = n, Mu = c( mu1, mu2),  # learn what this function does
                     Sigma = c( sd1^2,  sd2^2), R = toeplitz( r^(0:1))); # toepliz 
    
    return(list(z=z0))
  })
  
   
      
    logN.2.Norm <- reactive({
        
      
      sample <- random.sample()
        # Given mean (Mu), variances (Sigma) and correlation structure (R) of the distribution, mvlognormal
        # generates multivariate lognormal random variables.
        z <- mvlognormal(n = input$d, Mu = c(input$mu1,input$mu2),  # learn what this function does
                         Sigma = c(input$sd1^2, input$sd2^2), R = toeplitz(input$r^(0:1))); # toepliz 
        
        
        
        
        
        z <- as.data.frame(z)
        
        # nice plot
        x1 <- stack(z)
       
        z$trt <- 1*(runif(nrow(z))>.5)
        
        prop.change <- input$grp1/input$grp2 # true prop change
        
        z$x2 <- ifelse(z$trt %in% 1, z$V2*input$grp1, z$V2*input$grp2)  # allow an effect in both arms. 0.7/.875 = .8
        
        z$trt <- ifelse(z$trt %in% 1, "A", "B")    
 
        z$trt <- as.factor(z$trt)
        z<-z[,c(1,3,4)]
        names(z) <- c("BASE","TRTP","AVAL")

        logged <- foo2 <- z
        
        # log the values
        foo2$AVAL <- log(foo2$AVAL)
        foo2$BASE <- log(foo2$BASE)
        
        # endpoints
        foo2$CHG  <- foo2$AVAL-foo2$BASE                    # change from baseline not used
        foo2$PCHG <- ((foo2$AVAL-foo2$BASE)/foo2$BASE)*100  # % change from baseline not used
         
        
        return(list(z = z, foo2 = foo2 , prop.change = prop.change, x1=x1, logged=logged , grp1= input$grp1 , grp2= input$grp2))
        
    }) 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # use Normal mean and sd to get back the lognormal mean and lognormal SD!
    
    Norm.2.logN <- reactive({
         
        
        prop.change = logN.2.Norm()$prop.change
        
        xx = logN.2.Norm()$foo2
   
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
         
        return(list(res = res, f =f,  trt.eff.1= trt.eff.1,     trt.eff.2= trt.eff.2,   trt.eff.3= trt.eff.3    ,
                    resid=resid , trt.mu.B = trt.mu.B , trt.mu.A = trt.mu.A   , res2=res2  ))
        
    }) 
    
   
    txt<-reactive({
        
        zzz<-"something"
        
    })
    
    # --------------------------------------------------------------------------
    
    
    # Dummy line to trigger off button-press
    
    simulate <- reactive({
        
       sample <- random.sample()
        
      
    }) 
    
   
    
    #---------------------------------------------------------------------------
    # Plot a scatter plot of the data, joining patient data
    output$plot <- renderPlot({         
         
        prop.change = logN.2.Norm()$prop.change
        
        dd = logN.2.Norm()$foo2
         
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
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~ANCOVA PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$plot2 <- renderPlot({         
        
        sample <- random.sample()
        
        prop.change = logN.2.Norm()$prop.change
        
        foo2 <- logN.2.Norm()$foo2
   
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
          # labs(title = paste(
          #   "Intercep.trt1 =",signif(fit$coef[[1]],3 ),
          #   "Int.trt2 =",signif(fit$coef[[3]]+fit$coef[[1]],3),
          #   "Trt effect =",signif(fit$coef[[3]] ,3),
          #   " Slope =",signif(fit$coef[[2]], 3),
          #   " P =",signif(summary(fit)$coef[3,4], 4))) +
          labs(title = paste("Treatment effect, true proportional change ", input$grp2, "(B) /",input$grp1, "(A) = ",p4(1/prop.change), 
                             ". On the log scale (B-A) ",p4(log(input$grp2))," - ",   p4(log(input$grp1))," = ", p4(-log(prop.change)))) +
        theme_bw()  
        p1 <- p + theme(axis.line=element_blank(),
                        #axis.text.x=element_blank(),
                        #axis.text.y=element_blank(),
                        #axis.ticks=element_blank(),
                        #axis.title.x=element_blank(),
                        #axis.title.y=element_blank(),
                        # legend.position="none",
                        panel.background=element_blank(),
                        #panel.border=element_blank(),
                        #panel.grid.major=element_blank(),
                        #panel.grid.minor=element_blank(),
                        # plot.background=element_blank())
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
      
    })
     
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Plot residuals 
    output$plot3 <- renderPlot({         
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      f =  Norm.2.logN()$f 
      
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
      
      return(list(resid= f$stats["Sigma"][[1]]))
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    power <- reactive({
       
        # see my previous app https://raw.githubusercontent.com/eamonn2014/sample-size-for-RCT-endpoint-of-log-normally-distributed-biomarker-/master/power/app.R
        # convert lognormal <-> normal
        v <- input$sd1^2
        m <- input$mu1
        
        phi = sqrt(v + m^2);
        mu    = log(m^2/phi)           # mean of log(Y)      
        sigma = sqrt(log(phi^2/m^2))   # std dev of log(Y)  
        
    
      
      sd <- sigma
      
      prop.change <- input$grp1/input$grp2 # true prop change
      
      Po <- power.t.test(n= input$d/2, delta =log(prop.change), sd=sd, sig.level=0.05,
                        power=NULL, type="two.sample", alternative=c("two.sided"))
      
      
      return(list(P=Po, power=Po$power))
      
      
      # sd=0.83, delta=log(0.75) 80% power requires total N=132 completers
      # sd=0.85, delta=log(0.80) 85% power requires total N=524 completors
      # sd=0.85, delta=log(0.82) 85% power requires total N=662 completors
      
      
    }) 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    
    # Plot data  
    output$plot4 <- renderPlot({         
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      logged =  logN.2.Norm()$logged
      
      x1 <- xlab("Untransformed baseline measurement")
      x2 <- xlab("Untransformed outcome measurement")
      x3 <- xlab("Log transformed baseline measurement")
      x4 <- xlab("Log transformed outcome measurement")
      
      p1 <- ggplot(logged, aes(BASE, fill = TRTP)) + geom_density(alpha = 0.2) + xlim(0,20000) + x1
      p2 <- ggplot(logged, aes(AVAL, fill = TRTP)) + geom_density(alpha = 0.2) + xlim(0,20000) + x2
      
      p3 <- ggplot((logged), aes(log(BASE), fill = TRTP)) + geom_density(alpha = 0.2) + xlim(0,15) + x3
      p4 <- ggplot((logged), aes(log(AVAL), fill = TRTP)) + geom_density(alpha = 0.2) + xlim(0,15) + x4

      gridExtra::grid.arrange(p1,  p2, p3, p4, nrow=4) 
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$summary22 <- renderPrint({
      
      cat("True proportional change from baseline treatment A, ")
      cat((logN.2.Norm()$grp1))
      cat("\n")
      cat("True proportional change from baseline treatment B, ")
      cat((logN.2.Norm()$grp2))
      cat("\n")
      cat("True treatment effect, proportional change treatment B / treatment A, ")
      cat((1/logN.2.Norm()$prop.change))
      cat("\nTrue treatment effect, a shift on the transformed scale B-A, ")
      cat(-(log(logN.2.Norm()$prop.change))) 
      
      
    })
    output$summary2 <- renderPrint({
        
      cat("True proportional change from baseline treatment A, ")
      cat((logN.2.Norm()$grp1))
      cat("\n")
      cat("True proportional change from baseline treatment B, ")
      cat((logN.2.Norm()$grp2))
      # cat("\n")
      # cat("True treatment effect, proportional change treatment A / treatment B, ")
      # cat((logN.2.Norm()$prop.change))
      # cat("\nTrue treatment effect, a shift on the transformed scale A-B, ")
      # cat((log(logN.2.Norm()$prop.change)))
      cat("\n")
      cat("True treatment effect, proportional change treatment B / treatment A, ")
      cat((1/logN.2.Norm()$prop.change))
      cat("\nTrue treatment effect, a shift on the transformed scale B-A, ")
      cat(-(log(logN.2.Norm()$prop.change))) 
      
       
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$summary3 <- renderPrint({
      
      cat("True proportional change from baseline treatment A, ")
      cat((logN.2.Norm()$grp1))
      cat("\n")
      cat("True proportional change from baseline treatment B, ")
      cat((logN.2.Norm()$grp2))
      # cat("\n")
      # cat("True treatment effect, proportional change treatment A / treatment B, ")
      # cat((logN.2.Norm()$prop.change))
      # cat("\nTrue treatment effect, a shift on the transformed scale A-B, ")
      # cat((log(logN.2.Norm()$prop.change)))
       cat("\n")
      cat("True treatment effect, proportional change treatment B / treatment A, ")
      cat((1/logN.2.Norm()$prop.change))
      cat("\nTrue treatment effect, a shift on the transformed scale B-A, ")
      cat(-(log(logN.2.Norm()$prop.change))) 
      
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    output$summary999 <-   renderPrint({
      cat("True treatment effect, a shift on the log transformed scale B-A, ")
      cat(-(log(logN.2.Norm()$prop.change))) 
      cat("\n\nTrue treatment effect, proportional change treatment B / treatment A, ")
      cat(input$grp2, "/", input$grp1," = ",1/logN.2.Norm()$prop.change)
      
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$summary <- renderPrint({
        (Norm.2.logN()$res)
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$summary99 <- renderPrint({
      (Norm.2.logN()$res2)
      
    })
    
    output$summaryx <- renderPrint({
        (Norm.2.logN()$trt.eff.1)
    })
    
    output$summaryx2 <- renderPrint({
        (Norm.2.logN()$trt.eff.2)
    })
    
    output$summaryx3 <- renderPrint({
        (Norm.2.logN()$trt.eff.3)
    })
    
    output$muA <- renderPrint({
      (Norm.2.logN()$trt.mu.A)
    })
    
    
    output$muB <- renderPrint({
      (Norm.2.logN()$trt.mu.B)
    })
    
    
    output$summaryx4 <- renderPrint({  
      (Norm.2.logN()$f)
    })
    
    # Generate an HTML table view of the data
    output$table <- renderTable({
        print(simulate()$d)
    })
    
    
     output$table <- renderTable({
         Norm.2.logN()$resid
     })
     
     output$power <- renderPrint({
       power()$P
     })
})

 
# Create Shiny app ----


# Run the application 
shinyApp(ui = ui, server = server)