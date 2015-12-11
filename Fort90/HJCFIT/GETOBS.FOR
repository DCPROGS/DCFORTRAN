	subroutine GETOBS()
c To get observations in MECHFIT
c Get nset different sorts of obs; for each read time constants and areas/amps
c as in INWIND, as in CVFIT? -or as for input of param in CVFIT/EKDIST?
c                                                Additional data needed:
c  idtype=1  Pulse response (macroscopic): conc before,during,after + length
c  idtype=2  Step response (macroscopic)         conc before, after
c  idtype=3  Shut time pdf                       conc
c  idtype=4  Open time pdf                       conc
c  idtype=5  Burst length pdf                    conc
c
===	real*4 Yval(ndv1,ndimd)
	real*4 tau(nc,np),amp(nc,np)		!nc=component #; np=idtype
c
c How many data types (sets) to be fitted (nset)?
====
c Go through each set and get (a) idtype (b) tau & area/amp (c) values
c for additional variables (conc, pulse length etc)
====
c Decide on set of time points to be used for each set
====
c Calculate Yval(i,j) at each Xval(i,j)=time(i) for set j -the 'observations'
======
