library(shiny)

means <- NULL
means_offset <- 0

shinyServer(function(input, output) {
    # Reactive expression is called whenever the input(s) changed.
    #1 Draw n samples
    data <- reactive({
        dist <- switch(input$dist, norm = rnorm, unif = runif,lnorm = rlnorm, exp = rexp, rnorm)
                input$resample
                input$checkbox
                input$reps
        dist(input$n) 
    })
    
    #2 Nullify the list of means when input(s) is reset.
    doReset <- reactive({
        dist <- switch(input$dist, norm = rnorm, unif = runif, lnorm = rlnorm, exp = rexp, rnorm)
                input$checkbox
                input$n
                input$reps
        means<<-NULL
    })
    
    # Output plots
    output$plot <- renderPlot({
        # Set parameters
        tcol="slateblue1" 
        acol="slateblue4" 
        tscale=2;  
        dist <- input$dist
        n <- input$n
        reps <- input$reps
        x<-data()
        doReset()
        
        # Add a sample means to list when Draw New Sample is requested 
        if (input$checkbox) {
            if (length(means)==0) {means_offset<<-input$resample}
            means[input$resample-means_offset+1]<<-mean(x)
        }
        # Draw n samples repeatedly the numner of times as requested
        else {
            means<<-1:reps
            for (i in 1:reps) {
                means[i] <<-mean(switch(dist,
                                        norm = rnorm(n),
                                        unif = runif(n),
                                        lnorm = rlnorm(n),
                                        exp = rexp(n),
                                        rnorm(n)))
            }
        }
        
        # Plot range
        xmin = switch(dist, norm = -3, unif = 0, lnorm = 0, exp = 0, -3)
        xmax = switch(dist, norm =  3, unif = 1, lnorm = 4, exp = 4,  3)
 
        xsample<-x
        xsample[x>xmax]<-NA
        xsample[x<xmin]<-NA
        means[means>xmax]<-NA
        means[means<xmin]<-NA
        
        par(mfrow=c(3,1),mar=c(6,6,5,2), font.lab=1, font.main=2) 
        
        # Plot population distribution
        x0 = seq(xmin,xmax,length.out=1000);
        y0 = switch(dist,
                    norm = dnorm(x0),
                    unif = dunif(x0),
                    lnorm = dlnorm(x0),
                    exp = dexp(x0),
                    dnorm(x0))
        y0=y0/sum(y0) 
        plot(x0,y0,type="l",lwd=0,col=NULL,main="Population",xlab="",ylab="Probability",frame=F,cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale) 
        polygon(c(xmin,x0,xmax),c(0,y0,0),col=tcol,border=NA)
        
        # Plot sample in histogram
        hist(xsample, 
             breaks=seq(xmin,xmax,length.out=50),
             main="Sample", warn.unused = FALSE,
             col=tcol, border=tcol, xlab="",
             cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale)
        if (any(x<xmin)) {
            points(rep(xmin-0.1,sum(x<xmin)),rbeta(sum(x<xmin),2,2),lwd=2,col=tcol,cex=tscale)
        }
        if (any(x>xmax)) {
            points(rep(xmax+0.1,sum(x>xmax)),rbeta(sum(x>xmax),2,2),lwd=2,col=tcol,cex=tscale)
        }
        
        # Plot sample mean in histogram
        nh<-hist(means, breaks=x0, main="Sample Means", warn.unused = FALSE,
                 col=tcol, border=tcol, xlab="",
                 cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale)
        if (mean(x)>xmin && mean(x)<xmax) {
            hist(mean(x), breaks=x0, col=acol, border=acol, add=TRUE, ylim=c(0,max(nh$counts)))
        }
        print(input$resample)
    }, width=600, height=700)
})

