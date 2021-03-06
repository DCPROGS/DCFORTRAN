	subroutine SETFBLK(npar,titlep,nlig,ligname)
c To set up rates (out if open state(S)) that are slowed by fast channel block
c This is special case of pooling a subset of connected states tranistions
c between which are in rapid equilibrium.  The rates OUT of the subset are
c replaced by the effective rates=true rates out multiplied by the fraction
c of the subset that is in the state from which exit is made.
c If the subset is AR*===AR*B then fraction in AR* is 1/(1+cB)=fo say
c and fraction on AR*B is cB/(1+cB)=fb=1-fo so all rates OUT OF THE SUBSET
c that start from AR* are multiplied by fo and all rates out of the subset
c that start from AR*B are multiplied by 1-fo,  This preserves MR if the
c states are part of a cycle.
c Modif 06/14/06 10:24am for case of very fast open channel block, with known
c value of KB (could later make it estimate KB), Number of rate constants that
c are slowed by block=nfblock (all with same KB) ifb(m),jfb(m)=i,j such that all q(i,j) are divided
c by (1.0+B/KB) (m=1,nfblock). This is the factor by which block extends the
c apparent opening if NO blockages are detected
c NB all rates that lead out of an open state need to be multiplied by this
c factor, because the factor is the fraction of time that the open/blocked
c state is actually open, and therefore available for the transition.
c NB the blocker correction appears ONLY in QD(), which has, eg,
c alpha2/(1.0+B/KB) as actual transition rate.  As with aother connc dep
c transition rates, the conc is not present in theta0() or QT()
	character*1 ans
	character*10 titlep(200)
	character*20 ligname(10)

c
	logical*4 fastblk
	real*4 aKB
	integer*4 ifb(20),jfb(20),mfb(20),iflig
	common/fblck/fastblk,aKB,nfblock,iflig,ifb,jfb ,mfb

	integer irate(200),jrate(200)
	common/q0/irate,jrate		!for modec50
	common/KBLK/kA,kB,kC,kD
	logical discprt
	common/dp/discprt
c
c Number of states to be pooled (2 for now, but easily generalised)
c	npool=2
cc
c	nsfast(1)=1
c	do i=1,npool
c	   print 20,nsfast(i)
c	enddo
c Identify automatically all routes that lead out of open states

	nfblock=0
	n=0
	do m=1,npar
	   i=irate(m)
	   j=jrate(m)
	   if(i.le.kA) then
		print 1,titlep(m),m,i,j
1		format(1x,a10,
     & ' rate ',i2,' = q(',i2,',',i2,') leads from open state ',/,
     & ' Assume this rate is slowed by very fast open channel block'
     & ' [Y] ? ')
		ans='Y'
		call INPUTa(ans)
		if(ans.eq.'Y') then
		   n=n+1
		   ifb(n)=i
		   jfb(n)=j
		   mfb(n)=m
		endif
	   endif
	enddo
	nfblock=n
c
	if(n.gt.0) then
	   print 2
2	   format(/,
     & ' Equilibrium constant for channel block (micromolar) = ')
	   call INPUTr(aKB)
	   aKB=1.e-6*aKB	!molar
	else if(n.eq.0) then
	   fastblk=.false.
	endif
c
c Print the values
	if(n.gt.0) then
	   print 3
	   if(discprt) write(8,3)
3	   format(/,
     &     ' FAST OPEN CHANNEL BLOCK ASSUMED',/,
     & ' The following rates are divided by (1+xB/KB)')
	   do n1=1,n
		m=mfb(n1)
		print 4,titlep(m),m,ifb(n1),jfb(n1)
		if(discprt) write(8,4) titlep(m),m,ifb(n1),jfb(n1)
4		format(/,1x,a10,
     &    ': rate ',i2,' = q(',i2,',',i2,')')
	   enddo
c
	   if(nlig.gt.1) then
		do i=1,nlig
		   print 10,i,ligname(i)
10		   format(1x,i3,': ',a10)
		enddo
		iflig=nlig	!default
		print 11,nlig
11		format(
     &	' Ligand number for blocker (1,..,',i2,') [',i2,'] = ')
		call INPUTi(iflig)
	   else
		iflig=1
	   endif
c
	   print 5,1.e6*aKB,ligname(iflig)
	   if(discprt) write(8,5) 1.e6*aKB,ligname(iflig)
5	   format(
     &  ' Equilibrium constant for open channel block (micromolar) = ',
     &   g13.6,/,' Blocker = ',a10)
	else
	   print 6
	   if(discprt) write(8,6)
6	   format(/,' Fast open channel block NOT assumed')
	endif
	RETURN
	end

