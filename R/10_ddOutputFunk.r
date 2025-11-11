#
#suppressWarnings(suppressMessages( library(GGally, quietly=TRUE) ))
#suppressWarnings(suppressMessages( library(mvtnorm, quietly=TRUE) ))

#
#

#' A function primarily for internal use that make colors transparent.
#'
#' @param  someColor a color given however R handles a color
#' @param  alpha     the alpha channel value that wil be added
#'
#' @return a version of someColor with the alpha channel added
makeTransparent = function(someColor, alpha=100){
        newColor = col2rgb(someColor)
        apply(newColor, 2, function(curcoldata){
                rgb(red=curcoldata[1], green=curcoldata[2], blue=curcoldata[3], alpha=alpha, maxColorValue=255)
        })
}

#
#

#' A function for internal use to print
#'
#' @param ins  ins 
#' @param outs outs
printSelf = function(ins, outs=c()){
        #
        n = 5
        #
        nome = names(self)
	extraNames = c(outs, ".__enclos_env__", "initialize", "printSelf", "clone")#, "iterate", "optimize", "clone", "model", "prior", "plotMean", "plotBand", "plotRS", "N0Funk", "B0Funk", "save", "load", "like")
        display = nome[!nome%in%extraNames | nome%in%ins]
	display = display[order(nchar(display))]
	if(length(ins)>0){ display=display[display%in%ins] } 
        #
        for(d in display){
                #
                text = sprintf("self$%s", d)
                if( typeof(eval(parse(text=text)))=='list' ){
                        #
                        cat( sprintf('%s\t:\n', d) )
                        print( eval(parse(text=text)) )
                } else{
                        #       
                        if(length(eval(parse( text=text )))>n){
                                cat( sprintf('%s\t:', d), eval(parse( text=text ))[1:n], '...\n' )
                        } else{
                                cat( sprintf('%s\t:', d), eval(parse( text=text )), '\n' )
                        }
                }
        }
}

#' A function for internal use to plot an abitratry transformation of model paratmeters thru time
#'
#' @param quan  a quantity to plot given as a function
#' @param col   col 
#' @param alpha alpha
#' @param lwd   lwd
#' @param add   add
#' @param ...   ...
plotQuan = function(quan=function(B){B}, col='black', alpha=100, lwd=3, add=F, ...){
        #arguments passed directly to plotting functions
	
	#
	private$N0_classify(quan)

        #       
        if(!add){
                plot(self$time, self$quan(),
                        type='l',
                        lwd=lwd,
                        col=col,
			...
                )
        } else{
                lines(self$time, self$quan(),
                        lwd=lwd,
                        col=col,
			...
                )
        }
}

#' A function for internal use to plot the mean of the dynamics
#'
#' @param col   col
#' @param alpha alpha
#' @param lwd   lwd
#' @param add   add
plotMean = function(col='black', alpha=100, lwd=3, add=F){
        #arguments passed directly to plotting functions

        #       
        if(!add){
                plot(self$time, exp(self$lq+log(self$B)),
                        type='l',
                        lwd=lwd,
                        col=col
                )
        } else{
                lines(self$time, exp(self$lq+log(self$B)),
                        lwd=lwd,
                        col=col
                )
        }
}

#' A function for internal use to plot uncertainty bands 
#'
#' @param prob  Size of the uncertainty bands as defined as posterior probability
#' @param col   col
#' @param alpha alpha
plotBand = function(prob=0.95, col='black', alpha=100){
        #arguments passed directly to plotting functions

        #
        left = (1-prob)/2
        right = prob+left
        #
        polygon( c(self$time, rev(self$time)),
                c(
                        private$qLikes[[self$model$observation]](self, left),
                        rev(private$qLikes[[self$model$observation]](self, right))
                ),
                col = makeTransparent(col, alpha=alpha),
                border = NA
        )
}

#' A function for internal use to plot repeated sampling posterior-like parameter distributions
#'
#' @param m      the number of samples from the repeated sampling distribution
#' @param sample a boolean to indicate if samples should be returned
#' @param save   a boolean to indicate if samples should be saved 
plotRS = function(m=10^4, sample=F, save=F){
        #m      : how many samples
        #save   : FALSE or a filename

        #
        parNames = colnames(self$rsCov)
        sam = mvtnorm::rmvnorm(m, private$selfToPar(parNames), self$rsCov)
        #
        if(save==F){
                GGally::print_if_interactive(GGally::ggpairs(as.data.frame(sam)))
        } else{
		ggplot2::ggsave(filename=save, plot=GGally::ggpairs(as.data.frame(sam)))
        }
        #
        if(sample){ return(sam) }
}

#
#SHINY
#

##
#ui = shinydashboard::dashboardPage(
#        shinydashboard::dashboardHeader(),
#        shinydashboard::dashboardSidebar(
#                #Base Model NOTE: need to figure out how to update sliders to the new selected model
#                #varSelectInput("who", "Base Model", whoMight, selected=startWho),
#
#                ##Contrast
#                #sliderInput("sliderChi", "chi", min=0, max=1, step=0.05, value=1),
#                #Growth & Maturity
#                shiny::sliderInput("sliderAS","aS", min=0.1, max=10., step=0.1, value=self$aS),
#                shiny::sliderInput("sliderKappa","kappa", min=0.1, max=5, step=0.1, value=self$kappa),
#                #Recruitment
#                shiny::sliderInput("sliderAlpha", "alpha", min=self$M+0.1, max=exp(self$lalpha)*3, step=0.05, value=exp(self$lalpha)),
#                shiny::sliderInput("sliderBeta" , "beta" , min=0, max=exp(self$lbeta)*1.5, step=10^(floor(log10(exp(self$lbeta)))-1), value=exp(self$lbeta)),
#                shiny::sliderInput("sliderGamma", "gamma", min=-2, max=2, step=0.10001, value=self$gamma)
#
#        ),
#        dashboardBody(
#                shiny::fluidRow(column(12, shiny::plotOutput('rowOne'))),
#                shiny::fluidRow(column(12, shiny::plotOutput('rowTwo'))),
#                shiny::fluidRow(column(12, shiny::plotOutput('rowThree'))),
#                shiny::fluidRow(column(12, shiny::plotOutput('rowFour'))),
#                shiny::fluidRow(column(12, shiny::plotOutput('rowFive')))
#        )
#)
#
##
#server = function(input, output, session){
#        #
#        reactiveDat = shiny::reactive({
#		
#                ##
#                #if(input$who!=dat$whoAmI){
#                #       whoNew = sprintf("%s/%s", path, as.character(input$who))
#                #       dat = readRDS(whoNew)
#                #       dat$whoAmI = whoNew 
#                #}
#		
#                ##Contrast
#                #con = input$sliderChi
#                #Growth
#                self$aS = input$sliderAS
#                self$kappa = input$sliderKappa
#                #Recruitment
#                self$alpha = input$sliderAlpha
#                self$lalpha= log(input$sliderAlpha)
#                self$beta  = input$sliderBeta
#                self$lbeta = log(input$sliderBeta)
#                self$gamma = input$sliderGamma
#		
#                #
#                ww = vbGrow(self$aS, self$kappa, self$WW, self$a0) #WW*(1-exp(-kappa*a0))
#                self$FMsy = FMsy(self$M, self$kappa, ww, self$WW, exp(self$lalpha), exp(self$lbeta), self$gamma)
#                self$xi   = rbind(self$xi, self$FMsy/self$M)
#                self$zeta = rbind(self$zeta, getZeta(self$FMsy, self$M, self$kappa, ww, self$WW, exp(self$lalpha), exp(self$lbeta), self$gamma))
#                #dat$catch = fContrast(con)
#                #
#                self$iterate()
#		
#                #return
#                self
#        })
#        #
#        output$rowOne = shiny::renderPlot({
#                #       
#                dat = reactiveDat()
#                layout(t(1:2))
#                dat$plotQuan( function(B){B}, main="Biomass", ylim=c(0,max(dat$B)), xlab="Time", ylab="Biomass")
#                dat$plotQuan( function(N){N}, main="Numbers", ylim=c(0,max(dat$N)), xlab="Time", ylab="Numbers")
#        })
#        #
#        output$rowTwo = shiny::renderPlot({
#                #
#                dat = reactiveDat()
#                #
#                layout(t(1:2))
#                ##
#                #curve(SRR(x, dat), 0, 3*dat$B0, lwd=3, xlab="Biomass", ylab="Recruitment", main="Stock-Recruitment", n=1000)
#                #abline(0, dat$M, col='red')
#                #
#                ww = vbGrow(dat$aS, dat$kappa, dat$WW, dat$a0)
#                BMsy = BBar(dat$FMsy, dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma)
#                g = function(x){BBar(x, dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma)}
#                g = Vectorize(g, 'x')
#                maxF = uniroot(g, c(dat$FMsy, exp(dat$lalpha)))$root
#                #print(maxF)
#                FFs = seq(0, maxF, length.out=1000)
#                #f = function(x){surplus(x, dat)/surplus(BMsy, dat)*BMsy*dat$FMsy}
#                #curve(f(x), 0, dat$B0, lwd=3, xlab="Biomass", ylab="Equilibrium Surplus Biomass", main="Yield Curve", n=1000)
#                plot(g(FFs), FFs*g(FFs), type='l', lwd=3, xlab="Biomass", ylab="Equilibrium Surplus Biomass", main="Yield Curve")
#                segments(BMsy, 0, BMsy, BMsy*dat$FMsy)
#                points(BMsy, BMsy*dat$FMsy, pch=19)
#                #abline(0, dat$FMsy)
#                #curve(BBar(dat$FMsy, dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma))
#                #rug( BBar(dat$FMsy, dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma), lwd=3 )
#                #rug( BBar(0, dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma), lwd=3 )
#                #abline(h=BBar(dat$FMsy, dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma)*dat$FMsy)
#                #
#                curve(vbGrow(x, dat$kappa, dat$WW, dat$a0), 0, 15, lwd=3, xlab="Age", ylab="Biomass", main="VB Growth", ylim=c(0, dat$WW))
#                segments(dat$aS, 0, dat$aS, ww)
#                segments(0, ww, dat$aS, ww)
#                points(dat$aS, ww, pch=19)
#        })
#        #
#        output$rowThree = shiny::renderPlot({
#                #       
#                dat = reactiveDat()
#                #layout(cbind(c(1,2), 3))
#                #
#                #ww = vbGrow(dat$aS, dat$kappa, dat$WW, dat$a0) #WW*(1-exp(-kappa*a0))
#                #dat$FMsy = FMsy(dat$M, dat$kappa, ww, dat$WW, exp(dat$lalpha), exp(dat$lbeta), dat$gamma)
#                #
#                #par(mar=c(4,5,2,3))
#                #dat$plotQuan( function(catch, FMsy){catch*FMsy}, main="Fishing", ylim=c(0,1.5), xlab="Time", ylab="F")
#                #dat$plotQuan( function(B, catch, FMsy){B*catch*FMsy}, ylim=c(0,max(dat$B*dat$catch*dat$FMsy)*1.1), xlab="Time", ylab="Catch") 
#                #par(mar=c(5,5,5,3))
#                #
#                layout(t(1:2))
#                #
#                ww = vbGrow(dat$aS, dat$kappa, dat$WW, dat$a0)
#                #
#                curve(SRR(x, dat), 0, 3*dat$B0, lwd=3, xlab="Biomass", ylab="Recruitment #s", main="Stock-Recruitment", n=1000)
#                abline(0, dat$M*(dat$M+dat$kappa)/dat$kappa/dat$WW/(1+dat$M*ww/dat$kappa/dat$WW), col='red')
#                #abline(v=dat$B0)
#                segments(dat$B0, 0, dat$B0, SRR(dat$B0, dat))
#                #segments(0, SRR(dat$B0, dat), dat$B0, SRR(dat$B0, dat))
#                points(dat$B0, SRR(dat$B0, dat), pch=19)
#                #
#                dat$plotQuan( function(B, N){B/N}, main="Average Size", ylim=c(0,max(dat$B/dat$N)), xlab="Time", ylab="Biomass Per Individual")
#        })
#        #
#        output$rowFour = shiny::renderPlot({
#                #
#                dat = reactiveDat()
#                #
#                layout(t(1:2))
#                dat$plotQuan( function(catch, FMsy){catch*FMsy}, main="Fishing", ylim=c(0,max(dat$catch*dat$FMsy)), xlab="Time", ylab="F")
#                dat$plotQuan( function(B, catch, FMsy){B*catch*FMsy}, ylim=c(0,max(dat$B*dat$catch*dat$FMsy)*1.1), xlab="Time", ylab="Biomass", main="Catch")
#        })
#        #
#        output$rowFive = shiny::renderPlot({
#                #
#                dat = reactiveDat()
#                #
#                howManyRP = length(dat$xi)
#                howManyGrey = min(howManyRP, 50)
#                greys = rev(81-round(logseq(80, 1, howManyGrey))) #seq(60, 2, -1)
#                nWhite = max(howManyRP-howManyGrey+1, 0)
#                #
#                plot(dat$xi, dat$zeta, pch=19, col=c(rep("white", nWhite), sprintf("grey%d",greys), "black"), xlab="Fmsy/M", ylab="Bmsy/B0", main="Reference Points")
#                points(dat$xi[howManyRP], dat$zeta[howManyRP], pch=19, col='black')
#                points(dat$xi[howManyRP], dat$zeta[howManyRP], col='red')
#        })
#}

#' A function for internal use to launch a shiny app from self
#'
#' @param host An optional argument specifying the ip address, as a sting, where to host the app.
#' @param port An optional argument specifying the port, as an integer, fopr accessing the host.
launchShinyApp = function(host="0.0.0.0", port=5050){
	#
	if( is.function(self$UIFunk) ){ self$UI=self$UIFunk() }
	#
	runApp(shinyApp(self$UI, server), host=host, port=port)	
}


