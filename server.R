shinyServer(function(input, output) {
  output$MFAPlot <- renderPlot({
    data <- read.csv(text=getURL('https://raw.githubusercontent.com/ucb-stat243/stat243-fall-2016/master/problem-sets/final-project/data/wines.csv'))
    mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
      # Stop condition
      if (class(data)!="matrix" && class(data)!="data.frame") { 
        stop('data must be of class "matrix" or "data.frame"')
      }
      
      # remove NA's
      data = na.omit(data)
      
      indices = c()
      for (i in 1:length(sets)) {
        indices = c(indices, sets[[i]])
      }
      dat = data[,indices]
      
      # center and scale if requested
      if (scale) {
        if (center) {
          dat = scale(dat, center = TRUE, scale = FALSE)
          dat = apply(dat, 2 , function(x){x/sqrt(sum(x^2))})
        }
        else {
          dat = scale(dat, center = FALSE, scale = apply(dat, 2, sd, na.rm = TRUE))
        }
      }
      if (center) {
        dat = scale(dat, center = TRUE, scale = FALSE)
      }
      
      
      # Step 1: PCA of Each Data Table
      F_partial = list()
      a = c()
      K = length(sets)
      J = c()
      indx = 1
      for (i in 1:length(sets)) {
        
        # break up data into each assessor
        Xi = dat[,indx:(indx+length(sets[[i]])-1)]
        J = c(J, length(sets[[i]]))
        
        # compute SVD
        SVD = svd(Xi)
        U = SVD$u
        D = diag(SVD$d)
        V = SVD$v
        
        # alfa weights
        alfa_1 = D[1,1]^-2
        a = c(a, rep(alfa_1,length(sets[[i]])))
        
        # partial factor scores (step 1)
        F_partial[[i]] = K*alfa_1*Xi
        
        indx = indx + length(sets[[i]])
      }
      
      
      # Step 2: Generalized SVD of X
      m = rep(1/dim(dat)[1], dim(dat)[1])
      
      # compute GSVD
      GSVD = svd(diag(m^(1/2)) %*% dat %*% diag(a^(1/2)))
      Q = t(GSVD$v) %*% diag(a^(-1/2))  # factor loadings
      
      # eigenvalues
      eigenvalues = GSVD$d^2
      
      # common factor scores
      F_common = dat %*% diag(a) %*% t(Q)
      
      # partial factor scores (step 2)
      indx = 1
      for (i in 1:length(sets)) {
        F_partial[[i]] = F_partial[[i]] %*% t(Q)[indx:(indx+length(sets[[i]])-1),]
        indx = indx + length(sets[[i]])
      }
      
      # in order to extract number of requested components
      if (is.null(ncomps)) {
        ncomps = length(eigenvalues)
      }
      
      for (i in 1:length(F_partial)) {
        F_partial[[i]] = F_partial[[i]][,1:ncomps]
      }
      
      # placing results into a list and setting the class of the list as "mfa"
      res <- list(
        alfa_weights = a,  # ask, maybe remove
        Jk = J,  # ask, maybe remove
        eigenvalues = eigenvalues[1:ncomps],
        common_factor_scores = F_common[,1:ncomps],
        partial_factor_scores = F_partial,
        factor_loadings = t(Q)[,1:ncomps]
      )
      class(res) <- "mfa"
      
      return(res)
    }
    
    print.alpha.weights <- function(mfa,...) {
      print(mfa$alfa_weights)
    }
    
    print.jk <- function(mfa,...) {
      print(mfa$Jk)
    }
    
    print.eigenvalues <- function(mfa,...) {
      print(mfa$eigenvalues)
    }
    
    print.mfa.compromise <- function(mfa,...) {
      print(mfa$common_factor_scores)
    }
    
    plot.mfa.compromise <- function(compromise, dimone, dimtwo, ...) {
      plot(compromise[,dimone],compromise[,dimtwo])
    }
    
    print.mfa.fpart <- function(mfa,...) {
      print(data.frame(mfa$partial_factor_scores))
    }
    
    plot.mfa.fpart <- function(fpart, dimone, dimtwo,...) {
      plot(fpart[,dimone], fpart[,dimtwo])
    }
    
    print.mfa.factor.load <- function(mfa,...) {
      print(mfa$factor_loadings)
    }
    
    plot.mfa.factor.load <- function(factorload,...) {
      plot(factorload[,dimone], factorload[,dimtwo])
    }
    
    arrays <- list(c(2:7), c(8:13), c(14:19), c(20:24), c(25:30), c(31:35), c(36:39), c(40:45), c(46:50), c(51:54))
    winescores <- mfa(data, arrays)
    
    x <- as.numeric(input$col1)
    y <- as.numeric(input$col2)
    
    if (input$plot == "eigenvalue") {
    hist(winescores$eigenvalues, breaks=100, main="Histogram of Eigenvalues", xlab="Eigenvalue", ylab="Frequency")
  }
    if (input$plot == "common factor scores") {
      plot(winescores$common_factor_scores[,x], winescores$common_factor_scores[,y], main="Common Factor Scores", xlab="Critic 1", ylab="Critic 2")
    }
    if (input$plot == "partial factor scores") {
      X <- print.mfa.fpart(winescores)
      plot.mfa.fpart(X, x, y)
    }
    if (input$plot == "loadings factor scores") {
      plot(winescores$factor_loadings[,x], winescores$factor_loadings[,y])
    }
  }
  )
})
  
