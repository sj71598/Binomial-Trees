#CCR EUROPEAN PUT AND CALL
build_stock_tree_CRREuropean= function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

q_prob = function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp(r*delta_t) - d)/(u-d))
}

value_binomial_option_CRREuropean = function(tree, sigma, delta_t, r, X, type){
  q = q_prob(r, delta_t, sigma)
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}

binomial_option_CRREuropean <- function(type, sigma, T, r, X, S, N) {
  q <- q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree <- build_stock_tree_CRREuropean(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- value_binomial_option_CRREuropean(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
  
}
#UBER-CALL-ATM-5:3 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.25,r=.04,X=54.58,S=55,N=5)
#UBER-CALL-ITM-5:3 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.25,r=.04,X=54.58,S=50,N=5)

#UBER-PUT-ATM-5:3 month periods 
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.25,r=.04,X=54.58,S=53,N=5)
#UBER-PUT-ITM-5:3 months periods
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.25,r=.04,X=54.58,S=55,N=5)

#UBER-CALL-ATM-30:1 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)
#UBER-CALL-ITM-30:1 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=50,N=30)

#UBER-PUT-ATM-30:1 month periods 
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=53,N=30)
#UBER-PUT-ITM-30:1 months periods
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)

################################################################################

#TATA-CALL-ATM-5:3 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.25,r=.04,X=296.75,S=295,N=5)
#UBER-CALL-ITM-5:3 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.25,r=.04,X=296.75,S=250,N=5)

#TATA-PUT-ATM-5:3 month periods 
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.25,r=.04,X=296.75,S=295,N=5)
#UBER-PUT-ITM-5:3 months periods
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.25,r=.04,X=296.75,S=350,N=5)

#TATA-CALL-ATM-30:1 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#UBER-CALL-ITM-30:1 month periods 
#binomial_option_CRREuropean(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=250,N=30)

#TATA-PUT-ATM-30:1 month periods 
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#UBER-PUT-ITM-30:1 months periods
#binomial_option_CRREuropean(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=350,N=30)

# CCR AMERICAN PUT AND CALL
build_stock_tree_CRRAmerican= function(S, sigma, delta_t, N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
}

q_prob = function(r, delta_t, sigma) {
  u = exp(sigma*sqrt(delta_t))
  d = exp(-sigma*sqrt(delta_t))
  
  return((exp((r)*(delta_t)) - d)/(u-d))
}

value_binomial_option_CRRAmerican = function(tree, sigma, delta_t, r, X, type){
  
  q = q_prob(r, delta_t, sigma)
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      
      if(type == 'put' && option_tree[i,j]<(X-tree[i,j])){
        option_tree[i,j]=(X-tree[i,j])
      } 
      else if(type == 'put' && option_tree[i,j]>(X-tree[i,j])){
        option_tree[i, j] =((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      } 
      else if(type == 'call' && option_tree[i,j]>(tree[i,j]-X)){
        option_tree[i, j] =((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      } 
      else {
        option_tree[i,j]=(tree[i,j]-X)
      }
    }
  }
  return(option_tree)
}

binomial_option_CRRAmerican <- function(type, sigma, T, r, X, S, N) {
  q <- q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree <- build_stock_tree_CRRAmerican(S=S, sigma=sigma, delta_t=T/N, N=N)
  option <- value_binomial_option_CRRAmerican(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}
#UBER-CALL-ATM-5:3 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.25,r=.04,X=54.58,S=55,N=5)
#UBER-CALL-ITM-5:3 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.25,r=.04,X=54.58,S=50,N=5)

#UBER-PUT-ATM-5:3 month periods 
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.25,r=.04,X=54.58,S=53,N=5)
#UBER-PUT-ITM-5:3 months periods
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.25,r=.04,X=54.58,S=55,N=5)

#UBER-CALL-ATM-30:1 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)
#UBER-CALL-ITM-30:1 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=50,N=30)

#UBER-PUT-ATM-30:1 month periods 
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=53,N=30)
#UBER-PUT-ITM-30:1 months periods
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)

################################################################################

#TATA-CALL-ATM-5:3 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.25,r=.04,X=296.75,S=295,N=5)
#TATA-CALL-ITM-5:3 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.25,r=.04,X=296.75,S=250,N=5)

#TATA-PUT-ATM-5:3 month periods 
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.25,r=.04,X=296.75,S=295,N=5)
#TATA-PUT-ITM-5:3 months periods
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.25,r=.04,X=296.75,S=350,N=5)

#TATA-CALL-ATM-30:1 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#TATA-CALL-ITM-30:1 month periods 
#binomial_option_CRRAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=250,N=30)

#TATA-PUT-ATM-30:1 month periods 
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#TATA-PUT-ITM-30:1 months periods
#binomial_option_CRRAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=350,N=30)

# HULL/WHITE AMERICAN PUT AND CALL
build_stock_tree_HWAmerican= function(S, sigma, delta_t, r,N) {
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp((r*(delta_t))+(sigma*((delta_t)^(.5))))
  d = exp((r*(delta_t))-(sigma*((delta_t)^(.5))))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
  
}

q_prob = function(r, delta_t, sigma) {
  u = exp((r*(delta_t))+(sigma*((delta_t)^(.5))))
  d = exp((r*(delta_t))-(sigma*((delta_t)^(.5))))
  
  return((exp((r)*(delta_t)) - d)/(u-d))
}


value_binomial_option_HWAmerican = function(tree, sigma, delta_t, r, X, type){
  
  q = q_prob(r, delta_t, sigma)
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      
      if(type == 'put' && option_tree[i,j]<(X-tree[i,j])){
        option_tree[i,j]=(X-tree[i,j])
      } 
      else if(type == 'put' && option_tree[i,j]>(X-tree[i,j])){
        option_tree[i, j] =((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
      } 
      else if(type == 'call' && option_tree[i,j]>(tree[i,j]-X)){
        option_tree[i, j] =((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
        
      } 
      else {
        option_tree[i,j]=(tree[i,j]-X)
      }
    }
  }
  return(option_tree)
}

binomial_option_HWAmerican <- function(type, sigma, T, r, X, S, N) {
  q <- q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree <- build_stock_tree_HWAmerican(S=S, sigma=sigma, delta_t=T/N, r=r,N=N)
  option <- value_binomial_option_HWAmerican(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
}
#UBER-CALL-ATM-30:1 month periods 
#binomial_option_HWAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)
#UBER-CALL-ITM-30:1 month periods 
#binomial_option_HWAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=50,N=30)

#UBER-PUT-ATM-30:1 month periods 
#binomial_option_HWAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=53,N=30)
#UBER-PUT-ITM-30:1 months periods
#binomial_option_HWAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)

################################################################################

#TATA-CALL-ATM-30:1 month periods 
#binomial_option_HWAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#TATA-CALL-ITM-30:1 month periods 
#binomial_option_HWAmerican(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=250,N=30)

#TATA-PUT-ATM-30:1 month periods 
#binomial_option_HWAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#TATA-PUT-ITM-30:1 months periods
#binomial_option_HWAmerican(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=350,N=30)

# LogNormal European Put and Call
build_stock_tree_LogNormEuropean= function(S, sigma, delta_t, r, N){
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  u = exp(((r-(sigma^2)*.5)*delta_t)+(sigma*(delta_t)^(1/2)))
  d = exp(((r-(sigma^2)*.5)*delta_t)-(sigma*(delta_t)^(1/2)))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  return(tree)
  
}

q_prob = function(r, delta_t, sigma) {
  
  u = exp(((r-(sigma^2)*.5)*delta_t)+(sigma*(delta_t)^(1/2)))
  d = exp(((r-(sigma^2)*.5)*delta_t)-(sigma*(delta_t)^(1/2)))
  
  return((exp(r*delta_t) - d)/(u-d))
}

value_binomial_option_LogNormEuropean = function(tree, sigma, delta_t, r, X, type){
  
  q = q_prob(r, delta_t, sigma)
  
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] = pmax(X - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] = pmax(tree[nrow(tree),] - X, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] = ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta_t)
    }
  }
  return(option_tree)
}

binomial_option_LogNormEuropean <- function(type, sigma, T, r, X, S, N) {
  q <- q_prob(r=r, delta_t=T/N, sigma=sigma)
  tree <- build_stock_tree_LogNormEuropean(S=S, sigma=sigma, delta_t=T/N,r=r, N=N)
  option <- value_binomial_option_LogNormEuropean(tree, sigma=sigma, delta_t=T/N, r=r, X=X, type=type)
  return(list(q=q, stock=tree, option=option, price=option[1,1]))
  
}
#UBER-CALL-ATM-30:1 month periods 
#binomial_option_LogNormEuropean(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)
#UBER-CALL-ITM-30:1 month periods 
#binomial_option_LogNormEuropean(type='call',sigma = .3,T=0.08333,r=.04,X=54.58,S=50,N=30)

#UBER-PUT-ATM-30:1 month periods 
#binomial_option_LogNormEuropean(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=53,N=30)
#UBER-PUT-ITM-30:1 months periods
#binomial_option_LogNormEuropean(type='put',sigma = .3,T=0.08333,r=.04,X=54.58,S=55,N=30)

################################################################################

#TATA-CALL-ATM-30:1 month periods 
#binomial_option_LogNormEuropean(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#TATA-CALL-ITM-30:1 month periods 
#binomial_option_LogNormEuropean(type='call',sigma = .3,T=0.08333,r=.04,X=296.75,S=250,N=30)

#TATA-PUT-ATM-30:1 month periods 
#binomial_option_LogNormEuropean(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=295,N=30)
#TATA-PUT-ITM-30:1 months periods
#binomial_option_LogNormEuropean(type='put',sigma = .3,T=0.08333,r=.04,X=296.75,S=350,N=30)

################################################################################

#What is a binomial tree?
# An options pricing model that is typically used in finance and provides a way 
# to numerically calculate the valuation of stock options over time. In other
# words,it can also be shown as a graphical representation of possible values 
# that a stock option may take at different nodes or time periods.

# What did we do for the project? 
# We took stock options data from Uber, Beyond Meat and Tata motors and used
# various methods such as binomial and LogNormal pricing, with European and 
# American put and calls in order to see if it matched with the current option 
# data (at expiry).  

#Conclusion
# A few noticeable patterns were that the calls for the prices are exactly the same 
# if only the parameter (European or American) changes. Also, the put prices are 
# always different if that is the only parameter that changes. This is likely due 
# to the fact that for the given r and sigma values, early exercise is only beneficial 
# in puts. Another pattern is that there are greater increases in the put price
# between the European and American options when the total time to maturity of the
# option is increased. The most obvious pattern is that the options with the ITM
# strike prices cost more than those with the ATM strike prices. 