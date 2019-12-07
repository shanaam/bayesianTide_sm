#' ---
#' title:     "Model Carpentry with Stan"
#' subtitle:  "SCS/ISR: Getting Ready for the Bayesian Tide"
#' date:      "November 2019"
#' author:    "Georges Monette"
#' header-includes:
#'   - \usepackage{bm}
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: false
#'     theme: readable
#'     fig.width: 12
#'     fig.height: 10
#' bibliography: Bayes.bib
#' link-citations: yes
#' ---
#' (Updated: `r format(Sys.time(), '%B %d %Y %H:%M')`)
#'
#+ setup, include=FALSE
# This code does not show in the output but does get executed
verbose <- 0
ifstan <- FALSE
k <- knitr::knit_exit
undergraduate_course <- FALSE
knitr::opts_chunk$set(comment="  ", error = TRUE)
if(.Platform$OS.type == 'windows') windowsFonts(Arial=windowsFont("TT Arial")) 
interactive <- TRUE  # run interactive code
interactive <- FALSE  # do not run interactive code
#' 
#' # A first example
#'  
#' This is a very simple small artificial data set that purports to show
#' some kind of relationship between __Coffee__ consumption and 
#' __Heart__ damage.  As it turns out, we have also measured another
#' variable __Stress__ so we can explore the consequences of fitting
#' different models to this data.  
#' 
#' Standard regression models will take us part of the way.
#' Bayesian models with Stan will allow us to easily
#' explore more deeply.
#' 
#+ include=verbose
library(spida2)    # to install use: devtools::install_github('gmonette/spida2')
library(p3d)     # to install use:  devtools::install_github('gmonette/p3d')
# If necessary, the following packages can be installed 
# from CRAN with: install_packages('name_of_package')
library(car)
library(magrittr)
library(lattice)
library(latticeExtra)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
library(BayesFactor)

data(coffee)
#'
#' Here's the data set:
#'
#+ 
print(coffee[,c('Coffee','Stress','Heart')])
#'
#' If we look at __Heart__ versus __Coffee__
#' we see a strong relationship that suggests
#' that coffee might be quite harmful.
#' 
xyplot(Heart ~ Coffee, coffee, pch = 16, cex = 2)
#'
#' In fact, a linear regression:
#' 
fit <- lm(Heart ~ Coffee, coffee)
S(fit)
#'
#' Shows very strong evidence of a relationship
#' that even survives a Bayesian analysis.
#' 
fit_B <- lmBF(Heart ~ Coffee, coffee)
fit_B
#'
#' What happens if we take __Stress__ into
#' account?
#'
#+ plot1, message = FALSE, warning = FALSE, echo=verbose
Init3d( family='serif',cex = 1.2)
par3d(mouseMode = c('polar','zoom','fov','none') )
Plot3d( Heart ~ Stress + Coffee, coffee,
        ylim = c(0,150),  xlim = c(0,180), zlim = c(0,180),
        col = 'blue', theta=-90, phi=0, fov= 0)
rglwidget(elementId = "plot1")
#'
#' What happens if we regress __Heart__ on both __Coffee__ and __Stress__?
#'
fit2 <- lm(Heart ~ Coffee + Stress, coffee)
S(fit2)
compareCoefs(fit, fit2)
#'
#' With a Bayesian analysis:
#'
fit2_B <- lmBF(Heart ~ Coffee + Stress, coffee)
fit2_B
#' 
#' Compare the __Coffee__-alone model with the multiple regression model:
#' 
fit2_B / fit_B
#' 
#' Note how the structure of R made it possible for the authors of the 'BayesFactor'
#' package to define '/' to produce a Bayes factor when it's applied to an object created
#' by 'lmBF'.
#' 
#' Lets' fit __Stress__ alone:
#' 
fit_Stress_B <- lmBF(Heart ~ Stress, coffee)
fit_Stress_B
fit2_B / fit_Stress_B
#'
#' This adds weight to the null hypothesis!
#'
#' ## What's happening?
#'
#' Consider what the first model using __Coffee__ alone looks like in 3D
#'
#'
#+ plot2, echo = verbose
Fit3d(fit, col = 'blue', lwd = 2)
rglwidget(elementId = "plot2")
#' 
#' Now we add __Stress__:
#'
#+ plot3, echo=verbose 
Fit3d(fit2, col = 'yellow3', lwd = 2)
rglwidget(elementId = "plot3")
#' 
#' Both models get close to the data but the model with 
#' both __Coffee__ and __Stress__ produces a dramatically 
#' different effect for __Coffee__ -- with the opposite sign.
#' 
#' One way to understand how these models fit together is to
#' consider the confidence ellipse and confidence intervals
#' for the multiple regression model and the simple regression on
#' __Coffee__.  But we won't dwell on that.
#'
#' __Joint confidence ellipsoid for beta_coffee and beta_stress__
#+ lattice, echo=verbose
par(mar=c(5,5,0,1))
plot( 0,0, xlim = c(-2,2),ylim= c(-1,3), type = 'n', asp = 1,
      xlab = list( ~beta[Coffee], cex =2), ylab=list(~beta[Stress], cex = 2))
abline( h = 0 )
abline( v = 0 )
points( rbind(coef(fit2)[c('Coffee','Stress')]), pch = 16 , col ='red')
lines( cell(fit2), col = 'black', lwd=2)
lines( cem <- cell(fit2, df=1 ), col = 'green', lwd=2)
#'
#' The 95% confidence intervals for the multiple regression are
#' perpendicular projections of the 
#' 
#' __95% 1-dimensional projection ellipse__ (in red)
#' 
#' which is a slightly shrunken version of the
#' 
#' __95% joint 2-dimensional confidence ellipse__ (in black)
#' 
#' whose projections are __ScheffÃ©__ 95% confidence intervals with
#' protection for hypothesis-fishing in 2 dimensions.
#' 
#+ echo=verbose 
plot( 0,0, xlim = c(-2,2),ylim= c(-1,3), type = 'n', asp = 1,
      xlab = list( ~beta[Coffee], cex =2), ylab=list(~beta[Stress], cex = 2))
abline( h = 0 )
abline( v = 0 )
points( rbind(coef(fit2)[c('Coffee','Stress')]), pch = 16 , col ='red')
lines( cell(fit2), col = 'black', lwd=2)
lines( cem <- cell(fit2, df=1 ), col = 'red', lwd=2)

points( x = coef(fit2)["Coffee"], y=0, pch = 16, col = 'red')
lines( x = rep( coef(fit2)["Coffee"],2), y = c(0,  coef(fit2)["Stress"]), col = 'red')
lines( x = confint(fit2)["Coffee",], y = c(0,0), lwd =3, col = 'red')

points( x = 0, y=coef(fit2)["Stress"], pch = 16, col = 'red')
lines( x = c(0,0) , y = confint(fit2)["Stress",], lwd =3, col = 'red')
#'
#' But where does the confidence interval for the effect of coffee not controlling for streess come from?
#'
#+ echo=verbose 
plot( 0,0, xlim = c(-2,2),ylim= c(-1,3), type = 'n', asp = 1,
      xlab = list( ~beta[Coffee], cex =2), ylab=list(~beta[Stress], cex = 2))
abline( h = 0 )
abline( v = 0 )
points( rbind(coef(fit2)[c('Coffee','Stress')]), pch = 16 , col ='red')
lines( cell(fit2), col = 'black', lwd=2)
lines( cem <- cell(fit2, df=1 ), col = 'red', lwd=2)

points( x =  coef(fit)["Coffee"], y =0, pch = 16, col = 'blue')
lines( x = confint(fit)["Coffee",], y = c(0,0), lwd =3, col = 'blue')
points( ellptc( cem , dir= c(1,0), radius = -1), col = 'black', pch = 16)
lines( elltan( cem, dir = c(1,0), radius = -1, len = 2))
lines( elltanc( cem, dir = c(1,0), radius = 0, len = 20))
lines( elltanc( cem, dir = c(1,0), radius = 1, len = 20))
lines( elltanc( cem, dir = c(1,0), radius = -1, len = 20))
# The 'shape' of the CE is deternined by Sigma(X)
# It is scaled by  d x RSE / sqrt(n)
# where d is a quantile from an appropriate distribution
# Its center is at Beta.hat
#'
#' # Doing this in Stan:
#'
#' The code in Stan in easier to understand if we first think of the 
#' vector or matrix form of the linear model.
#' 
#' The __case-by-case__ model is:
#' 
#' $$Heart_i = \beta_0 + \beta_1 Coffee_i + \beta_2 Stress_i + \epsilon_i$$
#' 
#' where the index $i = 1, ..., 20$ ranges over the individuals in the sample and
#' the $\epsilon_i$'s are assumed to be independently normally distributed
#' with a common unknown variance $\sigma^2$.
#' 
#' The __vector__ model is:
#' 
#' $$\mathbf{Heart} = \beta_0 + \beta_1 + \mathbf{Coffee} + \beta_2 \mathbf{Stress} + \boldsymbol\epsilon$$
#' 
#' where __Heart__, __Coffee__ and __Stress__ are vectors containing the observations on those variables and
#' $\boldsymbol\epsilon$ is a random vector with a multivariate normal distribution, $\textrm{N}(\mathbf{0},\sigma^2 I)$
#' where $I$ is the 20 $\times$ 20 identity matrix.
#' 
#' The __matrix__ model is:
#' 
#' $$\mathbf{Y} = X \boldsymbol\beta + \boldsymbol\epsilon$$
#' 
#' using a generic symbol __Y__ for the response vector and the matrix $X$ consisting of a column of 1's followed by
#' the two predictor vectors __Coffee__ and __Stress__. The $\boldsymbol\beta$ vector is:
#' 
#' $$\boldsymbol\beta = \begin{pmatrix} \beta_0 \\ \beta_1 \\ \beta_2 \end{pmatrix}$$
#' 
#' ## Step 0: Prepare your data
#' 
#' We're largely skipping this step but it can take 98% of the effort in statistical analysis.
#' 
#' Particularly for Bayesian analysis with proper priors, you want to consider rescaling variables so
#' the uncertainty in their coefficients is comparable. 
#' 
#' ## Step 1: Postulate and code a model
#' 
#' Using vectors in R can be quite forgiving compared with the rules in mathematics that are
#' largely followed in Stan.
#' 
#' In R, 
#' 
#' - multiplying two vectors produces 'element-wise' multiplication, if the vectors are not of the same
#'   length, the shorter one is recycled,
#' - multiplying a matrix by a vector also produces pointwise multiplication with the vector __recycled__ 
#'   if it isn't long enough,
#' - to get _proper_ muliplication of matrices and vectors, you need to use the 
#'   operator <tt>`r '%*%'`</tt>, e.g. <tt>`r 'A %*% x'`</tt> in R
#' - objects are automatically 'declared' (i.e. their size and mode (floating point, integer, charater) are determined)
#'   when they created. No formal declarations are needed.
#'   
#' In Stan,
#' 
#' - vector and matrix multiplication obey the same rules as in mathematics:
#'     - To multiply two matrices, <tt>A * B</tt>, the matrices must be conformable which
#'       means that the number of columns of <tt>A</tt> must be equal to the number of rows
#'       of <tt>B</tt>.
#'     - To multiply a matrix by a vector, <tt>A * x</tt>, the length of the vector must
#'       be equal to the number of columns of the matrix. Strictly, to multiply a vector
#'       by a matrix, the vector would need to be a row vector but Stan will silently
#'       turn a column vector into a row vector for you, so you can use <tt>x * A</tt>
#'       if the length of <tt>x</tt> is equal to the number of rows of <tt>A</tt>.
#' - a __scalar__ can multiply a vector or a matrix and the result will be, as in mathematics,
#'   the product of the scalar by each of the elements of the vector or the matrix. You can't
#'   however multiply a vector of length 1 by a matrix or other vector if they are not conformable.
#' - in Stan, as in most computer languages, almost all objects must be explicitly declared. The exception
#'   is loop indices.
#' - Element-wise multiplication of vectors and matrices is possible with the <tt>.*</tt> operator.
#' - [See operators in Stan](https://mc-stan.org/docs/2_18/reference-manual/arithmetic-expressions-section.html)  
#'
#' Here is the Stan code to fit our regression.
#' 
#' Note:
#' 
#' - that we are using three __code blocks__: 
#'    - __data__ to declare the data objects
#'    - __parameters__ for the parameters, and 
#'    - __model__ to define the model
#' - objects are declared as 
#'    - scalars: __int__ and __real__, or as
#'    - vectors: __vector[n]__ where __n__ is the length of the vector
#'    - data declarations can use data declared earlier in the data block
#' - the model uses the __normal__ distribution with two arguments:
#'    - the vector mean of the response, and
#'    - the standard deviation -- not the variance
#'    - this looks like a command that could generate random normal variates, but it isn't. It
#'      uses the -log-likelihood of the normal to define the shape of the 
#'      'skateboard bowl' that HMC will ride in to sample from the posterior.
#' - we will use more blocks later.
#' - the model is defined as a string that is saved to a text file. We could just edit
#'   a text file but it would be harder to incorporate the file in this presentation.
#'   The preferred way of creating a Stan program is to edit a '.stan' file directly. 
#'   Current versions of RStudio know about '.stan' files and provide highlighting, etc.
#'   to help with editing.        
#' - Anything after '//' is a comment but the more familiar '#' is also allowed now
#' - Every command line ends with a semi-colon ';'
#'
cat( c("
  data {
    int N;   // number of observations
    vector[N] Coffee;   
    vector[N] Stress;   
    vector[N] Heart;
  }
  parameters {
    real b_0;      // intercept parameter
    real b_Coffee; 
    real b_Stress;
    real<lower=0> sigma_y; // non-negative standard deviation
  }
  model {
    Heart ~ normal(
      b_0 + b_Coffee * Coffee + b_Stress * Stress, // scalar *or+ vector = vector
      sigma_y);   // model of form y ~ normal(mean, sd) 
  }
"), file = 'fit2B.stan')
#'
#' Notes:
#' 
#' - <tt>cat( c("something"), file = 'some.txt')</tt> writes "something" (without the quotes) into the file named 'some.txt'.
#'   So this is a way to create an external file from an R script.
#' - Every command must end with a semicolon
#' - Every variable is declared (except loop indices but there were none)
#' - We didn't specify any priors in the 'model' statement
#'   so the default priors are uniform, in this case improper from $-\infty$ to $+\infty$ 
#'   for each $\beta$ and from 0 to $+\infty$ for $\sigma_y$.
#' - We can specify lower and upper bounds for parameters
#'   as we did for the standard deviation
#' 
##'    
##' ## Step 2: Compile the Model
##'
#' Compile the Stan program to create an object module 
#' (Dynamic Shared Object) with C++.
#' 
#' Stan translates the Stan program into C++ and compiles the code
#' as a dynamic shared object which can then be executed to generate samples
#' from the posterior distribution.
#' 
#+ fit2B.dso, cache=TRUE 
system.time(
  fit2B.dso <- 
    stan_model('fit2B.stan', model_name = 'Coffee and Stress')
)
#'
#' The parser is amazingly generous and informative with
#' its error messages.
#' 
#' Compiling takes a while because it produces optimized code that
#' will be used to:
#' 
#' 1. compute the height of the bowl as the skateboards moves around
#' 2. the gradient of the bowl
#' 3. in leapfrog steps
#'   - the new momentum of each skateboard
#'   - the new position of each skateboard
#' which all needs to be done very fast to minimize sampling time.
#' 
#' The new versions of Stan take advantage of the <tt>gpuR</tt>
#' package using your graphics card for parallel processing.
#' See [this link](https://github.com/stan-dev/math/wiki/OpenCL-GPU-Routines) for more information.
##'
##' ## Step 3: Prepare a Data List for Stan ----
##'                
#' Every variable declared in the 'data' step needs to be
#' fed to Stan through a list in R
#' 
dat <- list(
  N = nrow(coffee),
  Coffee = coffee$Coffee,
  Stress = coffee$Stress,
  Heart = coffee$Heart
)
#' Here is what it looks like:
dat
##' 
##' ## Step 4: Sample From the Posterior
##'
#' We give 4 skateboards (chains) a random shove and let 
#' them sample using HMC (Hamiltonian Monte Carlo) with NUTS (No U-turn Sampler).
#' 
#' As mentioned earlier, the sampler seems considerably faster than it was just a few years ago. 
#' 
#+ cache=TRUE
fit2B.stanfit <- sampling(fit2B.dso, dat)
#'
#' Each of 4 (by default) skateboards (formally 'chains') ran for 2000 steps (default).    
#' 
#' - the first 1000 are 'warmup' and discarded. The warm-up period is used: 
#'     - to lose the arbitrary influence of the starting point and let the chains reach a state
#'       in which they are intermingling well
#'     - to fine-tune parameters, particularly the stepsize, $\epsilon$.
#' - the last 1000 are the steps used to generate the sample. So with 1000 draws from 4 chains
#'   you get 4000 randon observations for each parameter. It's recommended that you run the
#'   chains much longer for your final run before publishing. 
#' - this was fast. For some challenging problems it can run overnight
#' - the message to 'adjust your expectations accordingly' is helpful for long-running jobs
#'                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               -                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
##'
##' ## Step 5: Check whether HMC worked
##' 
#'  These are diagnostics to check whether Stan worked, 
#'  no so much whether the model is good, although problems 
#'  with Stan are often a consequence of a poor model.
#'  
#' - Did some chains (skateboards) get stuck far from the others
#' - Is there low correlation within each chain, or do they look
#'   like slow local random walks?
#'
#' Initial diagnostics by printing the fit:
#' 
fit2B.stanfit
#'
#' Notes:
#'
#' - This is just the ouput from printing. The actual object is HUGE. It has all
#'   the samples (of 2000) from each chain and each parameter as well as a lot of
#'   other information
#' - The output shows you the estimated posterior mean for each parameter and the log of
#'   the posterior. These are estimated because you are working with a 
#'   sample from the __posterior__. This does not refer to error of estimation
#'   from having a random sample.
#' - The __se_mean__ tells you how well the mean 'estimates' the value you would
#'   get if you were to get an infinite number of iterations, i.e. how well
#'   the reported mean estimate the mean of the posterior distribution. 
#' - The __sd__ does refer to the variability in __Y__ and tells you how 
#'   well the sample allows to you estimate the population you sampled from.
#' - You can use the columns with percentages to create __credibility intervals__
#' - But BEWARE: With an effective n around 1000, you only have 25 observations in the
#'   tail of 95% credibility intervals. Those quantiles are not well estimated.
#'   Get a much larger sample or use a lower credibility: e.g. 90%.
#' - __MCMC diagnostics__
#'     - __n_eff_ is the effective number of draws (from a total of 4000) taking
#'       into account the lack of independence between draws. Positive association
#'       between draws means that some of the information is redundant and you
#'       have a smaller effective number of iterations.  
#'        - With default 4 chains of
#'          2000 (1000 post-warmup), the total sample has 
#'          size 4,000. So 1,000 is pretty good. If it isn't greater than 1,000, run MCMC for
#'          more iterations, especially if you are reporting results. 
#'        - [Stan's best practices page](https://github.com/stan-dev/stan/wiki/Stan-Best-Practices)
#'          recommends that if `N_eff / N < 0.001` you should be suspicious of the calculation
#'          of `N_eff`.
#'        - In our example, the two predictors are stongly correlated 
#'          which results in a slightly lower `N_eff/N` than with uncorrelated predictors with 
#'          equal variance.
#'        - If n_eff is too small, consider:
#'            - increasing the number of iterations
#'            - reparametrizing the model
#'            - reconsidering the model
#'     - __Rhat__: Total variability / Variability within Chains. 
#'       If the chains have converged this should be very close to 1. 
#'         - values much greater 1 show that chains are not in agreement and not exploring 
#'           the same regions of parameter space
#'         - have a look at the traceplot (below) to see whether they are diverging or 
#'           converging but not fully converged. In the later case, more iterations with a longer
#'           warm-up period might work. In the former, reparametrization and/or remodelling.
#'         - I have seen a suggested requirement that Rhat < 1.01 for all parameter. I get 1.02 or 
#'           a bit larger on problems that I think are okay. 
#'        
#' Print the traceplot of the chains for each parameter:
#'          
rstan::traceplot(fit2B.stanfit, alpha = .4) # alpha produces some transparency
#'
#' Notes:
#'
#' - Sometime functions with the same name will exist in more than one loaded 
#'   package and you need to clarify which one you want to use
#' - The traceplot lets you check whether the chains have mixed, are converging but
#'   not yet well mixed at the beginning of the chains, or are diverging
#' - Using alpha = .4 makes them transparent so you can see behind the 4th chain
#' - Note how the distribution of <tt>sigma_y</tt> is skewed.
#' 
#' The __pairs plot__
#' 
pairs(fit2B.stanfit)
# library(shinystan)
# launch_shinystan(fit2B.stanfit)
#' 
#' This is one of major tools for the diagnosis of the HMC sample. To understand it we
#' need to understand more details about [how the sample is generated.](https://mc-stan.org/docs/2_20/reference-manual/hamiltonian-monte-carlo.html)
#' Each chain follows an independent path:
#' 
#' - The chain starts with the particle (skateboard) at some point in parameter space where it 
#'   has a potential energy 
#'   determined by the height of its position in the bowl (the negative of the log posterior). 
#'   Left untouched it would start rolling down the bowl.
#' - However, it also gets a random initial push (a random normal vector with variance $\Sigma$) 
#'   so its velocity vector is determined over time
#'   by this initial push (kinetic energy) and the shape of the bowl (potential energy). 
#' - If it were a real frictionless particle, it would proceed to move
#'   in a way that conserves its total energy: the sum of its momentum energy and its 
#'   potential energy. This means that, as it moves lower it must move faster and vice versa. It would
#' - Since the computer can't calculate
#'   this path in infinitesimal time, it approximates it using discrete steps using a   
#'   'leapfrog integrator'. Using a small time interval $\epsilon$ (parameter world time,
#'   not real world time), the integrator works out the change in position assuming constant velocity,
#'   then the integrator works out the change in the velocity vector at the new position. 
#'   This produces an approximation
#'   to the 'true' path because it ignores the fact that the velocity vector changes continuously 
#'   as the particle moves. 
#' - The leapfrog integrator repeats this for $L$ step for a total elapsed time of $L \epsilon$.
#'   At the end, the particle has new position and velocity. Because of the approximations, its
#'   new total energy may be higher or lower than the original. If the bowl had a smooth almost quadratic
#'   shape the new energy should be quite close to the original. The new position of the particle is
#'   now subject to a 'Metropolis' accept step. Refer to the first lecture for a discussion of this. The idea is that
#'   if the energy has gone up we accept the new position and add it to the sample. If the energy has gone down,
#'   we generate a random number to decide whether to accept the new position or go back to the starting position
#'   and add that point (again) to the sample. 
#' - This segment moving from the original state to the new state is called a __transition.__
#' - A new random push is applied and the process starts again to produce the next transition.
#' 
#' Tuning HMC:
#' 
#' The behaviour of sampling depends on the shape of the bowl, on the length of the discrete time
#' interval, $\epsilon$, the number of steps, $L$, and the variance of the random push $\Sigma$.
#' 
#' If $\epsilon$ is too small, the particle will move very slowly (in real world time). The 
#' path will be accurate so little change in energy and high probability that new proposals will
#' be accepted. On the other hand the particle won't move very far and successive samples will be 
#' close to each other and it will take longer to cover the distribution. We say that the 
#' chain becomes like a random walk, disparagingly.
#' 
#' If $\epsilon$ is too large, the approximation will be poor, the energy will change a lot and
#' many proposals will be rejected. 
#' 
#' If $L$ is too small the trajectory will be like a random walk, if it's too large, sampling will
#' be slow in real time.
#' 
#' Ideally, the variance matrix should have a shape that conforms with the shape of the bowl. Since
#' the bowl can have a different shape in different places a variance matrix might not work well everywhere. 
#' 
#' The role of warm-up sampling is to:
#' 
#' 1. remove the effect of the starting position,
#' 2. fine tune $\Sigma$, $L$ and $\epsilon$ and
#' 3. allow the chains to have found common ground
#'
#' There is one parameter you should consider changing, <tt>adapt_delta</tt> which is the target
#' Metropolis acceptance rate. The default delta is 0.8 and it must be less than 1. In some cases you can
#' set it to a higher value, e.g. 0.98, with the control argument in sampling:
#' 
#' <tt>sampling(fit2B.dso, dat, control = list(adapt_delta = 0.98))</tt>
#' 
#' Another parameter that can help get around parts of the bowl that are irregular allows
#' 'jittering' $\epsilon$ so it varies randomly (which is okay as long as it's random)
#' from step to step. e.g.
#' 
#' <tt>sampling(fit2B.dso, dat, control = list(stepsize_jitter = 0.5))</tt>
#' 
#' will randomly vary $\epsilon$ from 0.5 to 1.5 times its typical size.
#' 
#' NUTS instead of pure HMC introduces a few variations:
#' The algorithm will vary $L$ on the fly using the NUTS algorithm.
#' When the trajectory starts turning back on itself before the target number of steps, it stops
#' and chooses randomly one of the points on the path to the U-turn as a proposal.
#' 
#' For more control variables, see R help for <tt>stan</tt>, i.e. use the command: <tt>?stan</tt>.
#' 
#' __Divergent transitions__
#' 
#' A transition is divergent if the change in energy is considered so large that the sample loses its
#' validity. This is generally a sign that the bowl is not smooth with highly varying curvature, for 
#' example a funnel shape.  
#' 
#' Starting points that lead to divergent transitions are shown in red in the pairs plot. Their location
#' can help diagnose the problem. You can also use __shinystan__ to view the sample in 3D projections
#' potentially revealing structure that is hard to see in the 2D projections of the pairs plot.
#' 
#' For an extended example see 
#' [Betancourt (2017) Diagnosing Biased Inference with Divergences](https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html)
#' 
#' The two halves of the pairs plot: The top right half shows points for which 'accept_stat__', the acceptance
#' probability for the proposal was above the median. The lower half shows points for which it was below the
#' median. The authors of Stan say that the presence of divergent transitions in the upper half is more
#' likely to be cured by an increase in 'adapt_delta__' than divergent transitions in the lower half.
#' Divergent transitions in the lower half are more likely the signal the need for reparametrization,
#' rethinking the model, getting more data given the complexity of the model or using more informative
#' priors!!  
#'  
#' See 
#' 
#' - [chapter 14.2 of the reference manual for more information](https://mc-stan.org/docs/2_20/reference-manual/hmc-algorithm-parameters.html).
#' - [Fox and Monette (2016) Some Notes on MCMC](http://blackwell.math.yorku.ca/ICPSR/MCMC-examples.pdf)
#' - [Divergent transitions after warmup](https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup)
#' - [Reference Manual Divergent transitions](https://mc-stan.org/docs/2_19/reference-manual/divergent-transitions.html)
#' - [How sampling works](https://mc-stan.org/docs/2_20/reference-manual/hamiltonian-monte-carlo.html)
#' - [Michael Betancourt: A Conceptual Introduction to Hamiltonian Monte Carlo](https://arxiv.org/abs/1701.02434v2)
#'
#' # Mediation analysis: non-linear function of parameters 
#'
#' This example shows how you can estimate more than one model at a time.
#' Suppose we assume that Stress is a confounder and we 
#' want to estimate the indirect effect of Stress through Coffee.
#' 
#' Baron-Kenny approaches estimate the product of 
#' 
#' - the regression coefficient of Coffee regressed on Stress times
#' - the regression coefficient of Coffee in the regression of Heart on Coffee and Stress
#'  
cat( c("
  data {
    int N;  
    vector[N] Coffee;   
    vector[N] Stress;   
    vector[N] Heart;
  }
  parameters {
    real b_0;      
    real b_Coffee; 
    real b_Stress;
    real<lower=0> sigma_y;
    real b_Coffee_Stress;
    real b0_Coffee_Stress;
    real<lower=0> sigma_Coffee_Stress;
  }
  model { 
    Heart ~ normal(
      b_0 + b_Coffee * Coffee + b_Stress * Stress, 
      sigma_y);
    Coffee ~normal(
      b0_Coffee_Stress + b_Coffee_Stress * Stress,
      sigma_Coffee_Stress);
  }
  generated quantities {
    real indirect_effect;
    indirect_effect = b_Coffee * b_Coffee_Stress;
  }
"), file = 'fit_mediation.stan')

system.time(
  fit_mediation.dso <- 
    stan_model('fit_mediation.stan', model_name = 'Mediation')
)
fit_mediation.stanfit <- sampling(fit_mediation.dso, dat)
fit_mediation.stanfit 
plot(fit_mediation.stanfit)
rstan::traceplot(fit_mediation.stanfit, alpha = .4)
pairs(fit_mediation.stanfit)
#'
#' # Measurement error
#' 
#' What happens if Stress is measured with error? 
#' 
#' The use of Stress as a confounding factor will undercorrect for the 'real' effect of Stress.
#' 
#' Suppose we have a hypothetical range of values for the measurement error and believe nevertheless that
#' the measurement error is __independent__ of 'true' Stress.
#' 
cat( c("
  data {
    real s_ME;  // standard error of measurement error in Stress
    int N;  
    vector[N] Coffee;   
    vector[N] Stress;   
    vector[N] Heart;
  }
  parameters {
    real b_0; 
    real b_Coffee; 
    real b_Stress;
    real<lower=0> sigma_y; 
    vector[N] true_Stress;
  }
  model {
    Stress ~ normal(true_Stress,s_ME);
    Heart ~ normal(
      b_0 + b_Coffee * Coffee + b_Stress * true_Stress, 
      sigma_y);   
  }
"), file = 'fit_measurement_error.stan')
#' 
#'
set.seed(123)
system.time(
  fit_measurement_error.dso <- 
    stan_model('fit_measurement_error.stan', model_name = 'Measurement Error')
)
fit_measurement_error.stanfit <- sampling(fit_measurement_error.dso, c(dat,s_ME = 5))
fit_measurement_error.stanfit 
traceplot(fit_measurement_error.stanfit)
#' Get names for pairs plot
(nams <- names(fit_measurement_error.stanfit))
pairs(fit_measurement_error.stanfit,
      pars = grep('true', nams, invert = T, value = T))
#' 
#' - Divergent transitions seem to concentrate in regions of low 'sigma_y'
#' - That makes sense because weird 'true_Stress' could be overfitting the response
#' - What happens if we experiment by putting a lower bound on sigma_y before
#'   trying something less Procrustean
#'   
#'   
cat( c("
  data {
    real s_ME;  // standard error of measurement error in Stress
    int N;  
    vector[N] Coffee;   
    vector[N] Stress;   
    vector[N] Heart;
  }
  parameters {
    real b_0; 
    real b_Coffee; 
    real b_Stress;
    real<lower=5> sigma_y;   //Procrustean measure
    vector[N] true_Stress;
  }
  model {
    Stress ~ normal(true_Stress,s_ME);
    Heart ~ normal(
      b_0 + b_Coffee * Coffee + b_Stress * true_Stress, 
      sigma_y);   
  }
  generated quantities {
    vector[N] ME;
    ME = Stress - true_Stress;
  }

"), file = 'fit_measurement_error_big_sigma.stan')
#' 
#'
set.seed(123)
system.time(
  fit_measurement_error_big_sigma.dso <- 
    stan_model('fit_measurement_error_big_sigma.stan', model_name = 'Measurement Error with big sigma')
)
fit_measurement_error_big_sigma.stanfit <- sampling(fit_measurement_error_big_sigma.dso, c(dat,s_ME = 5))
fit_measurement_error_big_sigma.stanfit 
traceplot(fit_measurement_error_big_sigma.stanfit)
#' Get names for pairs plot
(nams <- names(fit_measurement_error_big_sigma.stanfit))
pairs(fit_measurement_error_big_sigma.stanfit,
      pars = grep('true|ME', nams, invert = T, value = T))
