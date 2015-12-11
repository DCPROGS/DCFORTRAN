	subroutine READREL(irec,nsamp,iav,sampv,calfac,calvolt,
     & ipatch,ivhclamp,Yval,jc,jv,ndv1,ndimd,ioffset,newform)
c
c To read data from CJUMP.DAT in CJFIT. At each call reads (a) current
c array, and, if VSAMP, (b) voltage array.
c NB calfac=pA per ADC unit, so values are read into Yval(i,j) in pA
c
c Modif 12/24/94 10:05am to adjust voltages for ivhclamp,ipatch as they
c are read into Yval.  NB voltage is that MEASURED so it includes
c holding potential whether it was set on Axopatch or by 1401. So
c no need to add ivhclamp here -just reverse sign when necessary.
c
c Modified 08/30/94 04:27pm for CJUMP4 data; dimensions of arrays in call
c NB: iADC must be in main prog/call so it can be allocated -NO? this
c version has iADC allocated locally and deallocated before leaving.
c
c No longer need ngr etc: just allocate Yval() to have 1st dimension
c of ndv1=nsamp, so can fit any size sample into Yval(i,1), i=1,2,..,nsamp.
c
c (a) If one data set only read (#nj1, nj2=0) then
c	Yval(i,1)=AVCUR (if 'drug') or AVCURCON (if control) for #nj1
c			(or float(iADC) for single sweeps)
c and, if V recorded,
c	Yval(i,2)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj1
c			(or float(iADC1) for single sweeps)
c (b) If two data sets read (#nj1, #nj2) then
c	Yval(i,1)=AVCUR (if 'drug') or AVCURCON (if control) for #nj1
c			(or float(iADC) for single sweeps)
c	Yval(i,2)=AVCUR (if 'drug') or AVCURCON (if control) for #nj2
c			(or float(iADC) for single sweeps)
c and, if V recorded,
c	Yval(i,3)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj1
c			(or float(iADC1) for single sweeps)
c	Yval(i,4)=AVOLT (if 'drug') or AVOLTCON (if control) for #nj2
c			(or float(iADC1) for single sweeps)
c
c Older versions:
c 07/15/91 04:48pm Yval made (2048,15) for compat with CJVDU (not needed here)
c Modif 07/11/91 11:29am to cope with nsamp>2048 by using several columns
c of Yval(i,j)
c	NB if more than 2048 points are written to Yval(i,2) say, then the
c	extra ones (when i>2048) do not cause error but just continue into
c	Yval(i,3) i.e. Yval(2049,2) is interpreted as Yval(1,3) and so on
c	-no problem as long as we do not go beyond end of Yval, ie Yval(2048,10);
c	(see TEQUIV.FOR and TVPR1.FOR).  This simplifies things greatly! Readrel
c	needs no changes as long as it is called with appropriate jc,jv values.
c     IF NSAMP > 2048 then can still plot with VPLOTR if more than 1 array
c used per graph, as along as icurvd() specifies the column (j value) of
c Yval(i,j) in which data starts
c Need ngr=1+(nsamp-1)/2048 arrays of 2048k each. Adopt
c convention that jc1,jv1 are indices for the FIRST array used (containing
c the 1st 2048 points).
c (a) If one data set only read (#nj1, nj2=0) then jc=1,jv=1+ngr so need
c	2*ngr arrays altogether (OK up to ngr=10, nsamp=20480 if voltage
c	not recorded, or ngr=5, nsamp=10240 if voltage recorded)
c (b) If two data sets read (#nj1, #nj2) then for #nj1 jc=1, and
c	for #nj2 jc=1+ngr: 2*ngr arrays altogether (OK up to ngr=5, nsamp=10240
c	if voltage not recorded. If voltage recorded then for #nj1 jv=2*ngr+1
c	and for #nj2 jv=3*ngr+1; need 4*ngr arrays altogether (OK up to ngr=2,
c	 nsamp=4096)
c
c
c No need to distinguish 'drug' and 'control' here -known in main prog
c which is which.
c On input specify jc,jv=values of j in Yval(i,j) to put current
c and voltage into.
c
c
c=	real*4 YVAL(2048,15)
c	real*4 avcur(2048),avcurcon(2048)	!average current (drug,control)
c	real*4 avolt(2048),avoltcon(2048)	!average voltage (drug,control)
c=	integer*2 iADC(20480)
c==	integer*2 iADC(ndv1)
	ALLOCATABLE::iADC
	integer*2 iADC(:)
	integer*2 ivhclamp,ipatch
	real*4 YVAL(ndv1,ndimd)
	logical sampv,allocated,revsign,newform
c
c Note: Yval has been allocated to dimension of exactly nsamp, so must
c not read whole of last record if it goes above nsamp.
c Now allocate iADC locally
	if(allocated(iADC)) DEALLOCATE(iADC)
	ALLOCATE(iADC(ndv1))
c
	acal=6553.6		!adc units/volt
	vcal=1000./(acal*calvolt)	!*ADC1 units=mV
c reverse pipette pot for i/o or c/a
	revsign=ipatch.eq.2.or.ipatch.eq.3
	vhclamp=float(ivhclamp)
c
	if(newform) goto 100	!to read cjump5 files onwards
c
	if(iav.eq.0) then
	   nrec=1 + (nsamp-1)/256		!for iADC(i)  =int*2 so 1 rec=256 values
	   lastn=nsamp - (nrec-1)*256	!number to read from last record
	else
	   nrec=1 + (nsamp-1)/128		!for real*4 1 rec=128 values
	   lastn=nsamp - (nrec-1)*128	!number to read from last record
	endif
	if(iav.eq.0) then
	   n=1
	   do j=1,nrec
		m=n+255
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
		read(14,rec=irec) (iADC(i),i=n,m)
		n=n+256
	   enddo
c	Float the iADC values here
	   do i=1,nsamp
c		avcur(i)=float(iADC(i))
		Yval(i,jc)=calfac*float(iADC(i))
		if(revsign) Yval(i,jc)=-Yval(i,jc)
	   enddo
	else
	   n=1
	   do j=1,nrec
		m=n+127
		if(j.eq.nrec) m=n+lastn-1
		irec=irec+1
c		read(14,rec=irec) (AVCUR(i),i=n,m)
		read(14,rec=irec) (Yval(i,jc),i=n,m)
		n=n+128
	   enddo
	   do i=1,nsamp
	      Yval(i,jc)=calfac*Yval(i,jc)
		if(revsign) Yval(i,jc)=-Yval(i,jc)
	   enddo
	endif
c If voltage sampled, read it too
	if(.not.sampv) goto 91
	if(iav.eq.0) then
	   n=1
	   do 503 j=1,nrec
	   m=n+255
	   if(j.eq.nrec) m=n+lastn-1
	   irec=irec+1
c	   read(14,rec=irec) (iADC1(i),i=n,m)
	   read(14,rec=irec) (iADC(i),i=n,m)
	   n=n+256
503	   continue
	   do i=1,nsamp
c603	      avolt(i)=float(iADC1(i))
C=	      Yval(i,jv)=vhclamp + vcal*float(iADC(i))	!NO!
	      Yval(i,jv)=vcal*float(iADC(i))
		if(revsign) Yval(i,jv)=-Yval(i,jv)
	   enddo
	else
	   n=1
	   do 504 j=1,nrec
	   m=n+127
	   if(j.eq.nrec) m=n+lastn-1
	   irec=irec+1
c	   read(14,rec=irec) (AVOLT(i),i=n,m)
	   read(14,rec=irec) (Yval(i,jv),i=n,m)
	   n=n+128
504	   continue
	   do i=1,nsamp
		Yval(i,jv)=vcal*Yval(i,jv)
		if(revsign) Yval(i,jv)=-Yval(i,jv)
	   enddo
	endif
c
91	continue		!data set read in
	goto 999		!return
c
c Now code for new format cjump.dat
100	continue
	irec=irec+ioffset		!points to start of data
	if(iav.eq.0) then
	   read(14,rec=irec) (iADC(i),i=1,nsamp)
         irec=nsamp*2+irec
c	Float the iADC values here
	   do i=1,nsamp
c		avcur(i)=float(iADC(i))
		Yval(i,jc)=calfac*float(iADC(i))
		if(revsign) Yval(i,jc)=-Yval(i,jc)
	   enddo
	else
	   read(14,rec=irec) (Yval(i,jc),i=1,nsamp)
         irec=irec+4*nsamp
	   do i=1,nsamp
	      Yval(i,jc)=calfac*Yval(i,jc)
		if(revsign) Yval(i,jc)=-Yval(i,jc)
	   enddo
	endif
c If voltage sampled, read ADC1 too
	if(sampv) then
	   if(iav.eq.0) then
		read(14,rec=irec) (iADC(i),i=1,nsamp)
      	irec=irec+2*nsamp
		do i=1,nsamp
	         Yval(i,jv)=vcal*float(iADC(i))
		   if(revsign) Yval(i,jv)=-Yval(i,jv)
		enddo
	   else
		read(14,rec=irec) (Yval(i,jv),i=1,nsamp)
		do i=1,nsamp
		   Yval(i,jv)=vcal*Yval(i,jv)
		   if(revsign) Yval(i,jv)=-Yval(i,jv)
		enddo
		irec=irec+4*nsamp
	   endif
	endif
c
999	continue
	DEALLOCATE(iADC)
	RETURN
	end


