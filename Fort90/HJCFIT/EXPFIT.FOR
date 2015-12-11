	subroutine EXPFIT()
c To fit sums of exponentials, with some tau estimates common between
c different data setsc  Called in HJCFIT, rather than EKDIST, because 
c HJCFIT already has stuff for dealing with multiple data sets at diferent
c conc
c First get guesses, then fit.  Leave this subroutine for display
