	subroutine MPERM1(QD,IQ,inew,samenum,km)
C Version of MPERM (for HJCFIT) that does not have Peq as param; samenum added.
C TO REASSIGN KA ETC AND/OR RENUMBER STATES AND PERMUTE Q
C AND PEQ ACCORDINGLY
	real*8 QD(km,km)
	INTEGER INEW(km),IQ(km,km)
	character*1 UC,ans
	logical caplock,debug
	logical discprt,reads,samenum
c
	common/dp/discprt
	common/KBLK/KA,KB,KC,KD
C
	debug()=caplock()
C
101	format(a1)
	K=KA+KB+KC+KD
	do 804 i=1,k
804	INEW(i)=i	!IN CASE NOT ALTERED
C FOR SOME SCGEN'S KNOW IN ADVANCE WHAT RENUMBERING NEEDED
C
	reads=.false.
c	resub=.false.
c	renum=.false.
	print 18,kA,kB,kC,kD
18	format(' At present kA,kB,kC,kD = ',4i4,/,
     & ' Change this allocation to subsets, and/or renumber',
     & ' states [N] ? ')
	read 101,ans
	if(UC(ans).ne.'Y') goto 25	!straight out
c
c	print 19
c19 	format(' Renumber the states [N] ? ')
c	read 101,ans
c	if(UC(ans).eq.'N') goto 21	!allocation as from QGEN
c	renum=.true.
	print 20
20	format(
     & ' Read last allocation of states to subsets, and last',/,
     & ' renumbering of states, from disc [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 21	!allocation as from QGEN
	reads=.true.
c	resub=.true.
c	renum=.true.
      OPEN(unit=14,file='D:\SCLUST.DAT',status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=64)
	read(14,rec=1) kA,kB,kC,kD,inew
      close(unit=14)
21	continue
c	kF=kB+kC+kD
	print 2011,kA
2011	FORMAT('  number of open states, kA = ',i5,/)
	print 2012,kB,kC,kD
2012	format(' kB,kC,kD = ',3i5,'  O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).ne.'N') goto 801
155	print 202	!kB=
202	FORMAT('&  number of short shut states, kB = ')
c	read 10,kB
c10	format(i8)
	call INPUTi(kB)
	print 203   !kC=
203	FORMAT('&  number of long-lived shut states C, kC = ')
c	read 10,kC
	call INPUTi(kC)
	print 204   !kD=
204	FORMAT(
     & '&  number very long (between cluster) shut states, kD = ')
c	read 10,kD
	call INPUTi(kD)
	if((kA+kB+kC+kD).ne.k) goto 155
801	continue
	if(reads) then
	   print 50
50	   format('&Use state numbering as from disc [Y] ? ')
	else
	   print 103
103	   format('&Renumber the states [N] ? ')
	endif
	read 101,ans
	if(UC(ans).ne.'Y') goto 90
	DO 104,I=1,K
	print 105,I
105	FORMAT('&New state #',i2,' is old state #= ')
104	read 106,INEW(I)
106	FORMAT(I8)
90	continue
c Swap also the elements of IQ (needed to get Q from theta and vice versa
c in fitting progs)
	call MSWAP1(QD,IQ,k,inew,km)	!SWAP ROWS AND COLS
c
	do 91 i=1,k
      print 92,i,inew(i)
      if(discprt) write(8,92) i,inew(i)
92	format(' New state #',i3,' is old state #',i3)
91	continue
	print 22
22	format(' O.K. [Y] ? ')
	read 101,ans
	if(UC(ans).eq.'N') goto 21	!start allocation again
	print 23
23	format(
     & '&Save this allocation of states to subsets on disc [N] ? ')
	read 101,ans
	if(UC(ans).eq.'Y') then
         OPEN(unit=14,file='D:\SCLUST.DAT',status='UNKNOWN',
     &    access='DIRECT',form='UNFORMATTED',recl=64)
	   write(14,rec=1) kA,kB,kC,kD,inew
         close(unit=14)
	endif
c
25	continue
c Test if numbering altered; if not inew=1,2,3,...,k still
	samenum=.true.
	do 24 i=1,k
24	if(inew(i).ne.i) samenum=.false.
	RETURN
	END


