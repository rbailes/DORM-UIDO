#computes all pairwise means of adjacent numbers of the array. Then, get the sd of the means, the DORM, which we want to minimize.

#If you set the "correct" option to TRUE, then the function adds a correction to the dorm equal to the probability that the uniquely most uniform distribution of 
#probabilities in the vector (the uido) could be arrived at by chance, i.e. the probability of arriving at 1 order out of all possible unique orders for the probabilities.
#This both penalizes shorter utterances, since it is more likely for shorter utterances that a speaker might have achieved UIDO by chance.
#(Also, shorter utterances will be less resistant to noise anyway, as per Shannon's theorem and Plotkin and Nowak.) E.g., a one-word utterance will have perfect UIDO
#by definition, because there's no alteernative order. It also penalizes repetitions of a single word, as in ASD stereotyped language.
#Uses: library(combinat), permn(), unique()

#library(zoo)

dorm <- function(logvec,correct=FALSE)
{
  
  #In the case of a single-item vector or two item, there is no rolling mean with window 2, and no standard deviation of means, so we just set the dorm to 0 in that case.
  if (length(logvec) <= 2)
  {
    logvecdorm = 0
  }

  #The usual case, where the vector has more than 1 element...
  else
  {
    #takes rolling means for pairs of numbers, which we will use to compute the deviation of rolling means (dorm)
    means <- rollmean(logvec,2)
    logvecdorm <- var(means) #I have changed this to sample variance as per Ferrer i Cancho's advice, as this is an unbiased estimator; Old code: sd(means) #use sqrt(mad())?
      
  }
  
  if (correct==TRUE)
    {
    #What follows is the olf brute-force way of doing this, which I've commented out and replaced with a simple formula for finding the number of unique permutations:
    #library(combinat)
    
    #generates all possible permutations of logvec, then applies unique() which returns a list of only the unique ones (in case there are doubles)
    #uniquePerms <- unique(permn(logvec))
    #numberUniquePerms <- length(uniquePerms)
    
    #What follows is the formula for finding the number of unique perms, once you know how many repeats there are in the vector (what the next line does), and which items are repeating how many times
    itemCount = as.data.frame(table(logvec))
    
    #extract from data frame how many repetitions the repeating items have, in a vector
    reps <- itemCount[itemCount$Freq > 1,]$Freq
    
    denominator <- 1
    
    if (length(reps) != 0)
    {denominator <- prod(factorial(reps))}
    
    numberUniquePerms = factorial(length(logvec))/denominator
    
    penalty=(1/numberUniquePerms)
    
    logvecdorm <- logvecdorm+penalty
    }
  
  
  return(logvecdorm)
}

#dormoldschool; I could put in the dorm function I hand-coded instead of using rollmean(), but do we need it? It might be slightly faster, but whatever.
#Maybe this is also useful for Alice to see what the rolling mean actually is.

dormoldschool <- function(logvec)
{
kk = 1
pairmeanlist = array()
#save all means of adjacent numbers into a list of means
while ((kk+1) <= length(logvec)) #remember, lists in R perversely start at offset 1, not 0
{
  pairmean = (logvec[kk]+logvec[kk+1])/2
  pairmeanlist[kk] <- pairmean
  kk = kk+1
}
return(sd(pairmeanlist))
}


#The uido function permutes the order of a vector of probabilites till it finds the one that most uniformly distributes the information contents derived from
#the probabilities. You can then use the dorm() function defined above to get a heuristic for uniformity of the most uniform distribution, and compare it to 
#some observed distribution.

#The input should be a vector of probabilities. It won't be normally dist, but the function converts it to info content, log2(1/p) , which
#should make it normally dist enough to compute standard deviations as usual, especially after the rolling mean is taken (Central Limit Theorem).
uido <- function(infovec)
{
  #if infovec is 1 or 2 items, there is no way to improve the dorm, so we skip that case
  if (length(infovec) <= 2)
  {
    return(infovec)
    break
  }
  
  #remember, lists in R perversely start at offset 1, not 0
  #midpoint <- (length(infovec))/2 #remember, lists in R perversely start at offset 1, not 0
  
  #I've modified this to do decreasing because it migth be better for head-initial langs
  newlist <- sort(infovec,decreasing=T)
  mm = 1 #remember, lists in R perversely start at offset 1, not 0
  jj = length(newlist)
  
  while (jj > mm) #jj has to stay greater than midpoint because otherwise there'll be an extra swap for even-numbered arrays
  {
    #starting from both ends of the array, swaps every other pair of numbers, which gets us very close to optimized
    cup <- newlist[mm]
    newlist[mm] <- newlist[jj]
    newlist[jj] <- cup
    mm = mm+2
    jj = jj-2
  }  
  
  #Check to make sure the dorm of the swapped list is actually lower than that of the original vector...if it isn't, start the process below with the original vector instead of newlist 
  if (dorm(infovec) < dorm(newlist))
    {swaplist <- infovec}
  else
    {swaplist <- newlist}
  
  #Gets the sd of the rolling pairwise means, the DORM, which we want to minimize, using the dorm function.
  prevsd <- dorm(swaplist)
  currentsd <- prevsd #initialize currentsd
  
  #Now, see if swapping any pair of numbers gets us a lower sd for pairmeanlist. If any swap does, then do it, otherwise don't. Repeat till no swap helps.
  ll = 1
  while((ll+1) <= length(swaplist))
  {
    #do the swap
    if ((ll-1) == 0)
    {
      hyplist <- array(c(swaplist[ll+1], swaplist[ll], swaplist[(ll+2):length(swaplist)]))
    }
    else if ((ll+1) == length(swaplist))
    {
      hyplist <- array(c(swaplist[1:(ll-1)], swaplist[ll+1], swaplist[ll]))
    }
    else
    {
      hyplist <- array(c(swaplist[1:(ll-1)],swaplist[ll+1],swaplist[ll],swaplist[(ll+2):length(swaplist)]))
    }
    
    currentsd <- dorm(hyplist)
    
    
    #if the swap helped, then save that version of the list, and start the process over again, starting the counter at 1 again
    if (currentsd < prevsd)
    { 
      #print(swaplist) #debug
      #print((prevsd-currentsd)) #debug
      #print(sort(infovec)) #debug
      swaplist <- hyplist
      prevsd <- currentsd
      ll = 1
      print("we improved") #debug
    }
    else
    {ll = ll+1}
    
  }
  
  #print("and here is new dorm") #debug
  #print(prevsd)#debug
  
  return(swaplist)
  
}

uidoAutoCov <- function(infovec)
{
  newlist <- sort(infovec,decreasing=T)
  mm = 1 #remember, lists in R perversely start at offset 1, not 0
  jj = length(newlist)
  
  while (jj > mm) #jj has to stay greater than midpoint because otherwise there'll be an extra swap for even-numbered arrays
  {
    #starting from both ends of the array, swaps every other pair of numbers, which gets us very close to optimized
    cup <- newlist[mm]
    newlist[mm] <- newlist[jj]
    newlist[jj] <- cup
    mm = mm+2
    jj = jj-2
  }  
  
  #computes autocovariance array for the vector, and returns the absolute value of the last autocovariance value.
  infovec_autoCov <- abs(acf(infovec, type = "covariance", plot = FALSE)[[1]][length(infovec)])
  newlist_autoCov <- abs(acf(newlist, type = "covariance", plot = FALSE)[[1]][length(newlist)])
  
  #Check to make sure the autocov of the swapped list is actually lower than that of the original vector...if it isn't, start the process below with the original vector instead of newlist 
  if (infovec_autoCov < newlist_autoCov)
  {swaplist <- infovec}
  else
  {swaplist <- newlist}
  
  #Gets the sd of the rolling pairwise means, the DORM, which we want to minimize, using the dorm function.
  prevsd <- abs(acf(swaplist, type = "covariance", plot = FALSE)[[1]][length(swaplist)])
  currentsd <- prevsd #initialize currentsd
  
  #Now, see if swapping any pair of numbers gets us a lower sd for pairmeanlist. If any swap does, then do it, otherwise don't. Repeat till no swap helps.
  ll = 1
  while((ll+1) <= length(swaplist))
  {
    #do the swap
    if ((ll-1) == 0)
    {
      hyplist <- array(c(swaplist[ll+1], swaplist[ll], swaplist[(ll+2):length(swaplist)]))
    }
    else if ((ll+1) == length(swaplist))
    {
      hyplist <- array(c(swaplist[1:(ll-1)], swaplist[ll+1], swaplist[ll]))
    }
    else
    {
      hyplist <- array(c(swaplist[1:(ll-1)],swaplist[ll+1],swaplist[ll],swaplist[(ll+2):length(swaplist)]))
    }
    
    currentsd <- abs(acf(hyplist, type = "covariance", plot = FALSE)[[1]][length(hyplist)])
    
    
    #if the swap helped, then save that version of the list, and start the process over again, starting the counter at 1 again
    if (currentsd < prevsd)
    { 
      #print(swaplist) #debug
      #print((prevsd-currentsd)) #debug
      #print(sort(infovec)) #debug
      swaplist <- hyplist
      prevsd <- currentsd
      ll = 1
      print("we improved by autoCov") #debug
    }
    else
    {ll = ll+1}
    
  }
  
  #print("and here is new dorm") #debug
  #print(prevsd)#debug
  
  return(swaplist)
  
}
