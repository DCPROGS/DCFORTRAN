	subroutine PRNTEC50(QT,conc,k,nlig,nset,npar,ncdep)
c To print Pmax and EC50 values in HJCFIT (replaces misc printouts
c in HJCFIT, QSET_HJC and HJCLIK, which were getting messy).
c This prints only ideal (tres=0) values (Popen plots in HJCDISP
c calculate also HJC values from EC50_HJ2)
c
c NB EC50_HJC used to use only conc(i,1) -should really use conc(i,jset)
c and have jset as parameter (when more than one ligand, so
c conc of other ligand matters) -this now done.
c
c (1) If nlig=1
c	Prints P(0),P(max) and EC50 for the one ligand.
c	Printed only once, however many sets, because in this case
c	there are no 'other ligands' that could affect the EC50
c
c (2) If nlig>1
c	Prints P(0),P(max) and EC50 for each ligand, and this
c    is repeated for each set, because each set will have diff conc
c    of ligands other that that for which Popen curve is calculated.
c
c    If either one or two EC50 constraints used, then prints also
c	P(0),P(max) and EC50 for each ligand, (using the specified
c     conc of other ligand, rather than conc used for data), and
c     repeats this for each constraint.  This serves as check
c     that constraints have worked, when called with final rates after

c     fit.
c
c No call to SETMR is used here -that should have been done already for
c both initial and final rates
c
	real*4 conc(10,10)
	real*8 QT(100,100)
c For ec50 constraint
	logical fixec50,prtec50,dcmodel
	real*8 ec50,pmax,ec501,xqlo,xqhi,ec5
	real*4 curmax,curinfs,concmax,cur0,pop0
	common/ec/fixec50,imod0,ec501,i50,j50,m50,prtec50,
     &	xqlo,xqhi,dcmodel
c=	real*8 qval,qval2,ec50out,ec5012,ec50out2,
c=     &	x1s2,x2s2,fx12,fx22
c define common/ec1/ for printing in simphjc (in DSPOUT)
c Modif 10/09/03 05:24pm for case where two ec50s are constrained
c=	common/ec1/nfixec51,fixec51,qval,ec501,ec50out,i501,j501,x1s,x2s,
c=     &fx1,fx2,qval2,ec5012,ec50out2,i5012,j5012,x1s2,x2s2,fx12,fx22  !for print in SIMPHJC
	logical monot
	real*8 cfacd,dgamma(100)
	character*20 ligname(10)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	common/ec3/ligname		!for qset_hjc
c Added 10/09/03 11:01am for constraining a 2nd ec50
	real*8 ec502,xqlo2,xqhi2
	real*4 conc_ec1(10),conc_ec2(10)
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
	allocatable::conc1
	real*4 conc1(:,:)
	logical discprt
	common/dp/discprt
c
	if(ncdep.eq.0) RETURN
c
	kdim=100
c NB with one ligand, EC50 should be same regardless of nset, because there
c are no other ligands for which conc can vary between sets 9so call with jset=1)
	if(nlig.eq.1) then
	   nlvar=1
	   if(dcmodel.and.nlig.eq.1) then
		call MODEC50(imod0,QT,npar,kdim,pmax,EC50)
	   else
		jset=1
		call EC50_HJC(EC50,curinfs,curmax,concmax,cur0,pop0,
     &	    pmax,nlvar,QT,jset,conc,k,kdim,nerr)
		if(nerr.ne.0) then
		   call BELL(1)
		   print 590,nerr
		   if(discprt) write(8,590) nerr
590		   format('  Error in bisection for EC50: nerr = ',i2)
		endif
	   endif
	   print 140,nlvar,ligname(nlvar)
         if(discprt) write(8,140) nlvar,ligname(nlvar)
140	   format(/,
     &  ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
c
	   print 593,ligname(nlvar),pop0
	   if(discprt) write(8,593) ligname(nlvar),pop0
593	   format(
     & ' At zero concentration of ',a10,',',' P(open) = ',g12.5)
	   if(monot) then
		print 591,pmax,ligname(nlvar)(1:10),ec50*1.e6
      	if(discprt) write(8,591) pmax,ligname(nlvar)(1:10),
     &		ec50*1.e6
591		format(
     & ' Equilibrium response-concentration curve is monotonic',/,
     & ' Maximum Popen = ',f9.6,/,
     & '    EC50 = Conc of ',a10,' for 50% of this maximum (muM) = ',
     &     g12.5)
		print 108
		if(discprt) write(8,108)
108		format(/)
	   else	!not monotonic (never true for binding curve)
      	print 592,curmax,concmax*1.e6,pmax,
     &		ligname(nlvar)(1:10),ec50*1.e6
      	if(discprt) write(8,592) curmax,concmax*1.e6,pmax,
     &		ligname(nlvar)(1:10),ec50*1.e6
592		format(
     &    ' Equilibrium response-concentration curve has maximum.',/,
     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
     &    ' Maximum Popen = ',f9.6,/,
     &    '   Conc of ',a10,
     &    ' for 50% of this max. current (muM) (left of max) = ',
     &        g12.5)
	   endif
c
c Now case where nlig>1
	else if(nlig.gt.1) then
c Print pmax and ec50 for each ligand,  at the conc of other ligands
c used in each data set
	   do jset=1,nset
		print 1,jset
		if(discprt) write(8,1) jset
1		format(
     & ' SET ',i3,':',/,
     & '_________________ ')
		do nlv=1,nlig
		   print 140,nlv,ligname(nlv)
      	   if(discprt) write(8,140) nlv,ligname(nlv)
c140		   format(/,
c     &	  ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
		   call EC50_HJC(EC50,curinfs,curmax,concmax,cur0,pop0,
     &	    pmax,nlv,QT,jset,conc,k,kdim,nerr)
		   if(nerr.ne.0) then
			call BELL(1)
			print 590,nerr
			if(discprt) write(8,590) nerr
c590			format('  Error in bisection for EC50: nerr = ',i2)
		   endif
		   if(monot) then
			print 591,pmax,ligname(nlv)(1:10),ec50*1.e6
      		if(discprt) write(8,591) pmax,ligname(nlv)(1:10),
     &		ec50*1.e6
		   else	!not monotonic (never true for binding curve)
      		print 592,curmax,concmax*1.e6,pmax,
     &	  	  ligname(nlv)(1:10),ec50*1.e6
	      	if(discprt) write(8,592) curmax,concmax*1.e6,pmax,
     &		   ligname(nlv)(1:10),ec50*1.e6
		   endif
c and print conc of other ligands
		   do j=1,nlig
			if(j.ne.nlv) then
			   print 143,j,ligname(j),conc(j,jset)*1.e6
			   if(discprt) write(8,143)j,
     &			ligname(j),conc(j,jset)*1.e6
143			format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,
     &			' micromolar)')
			endif
		   enddo
		   print 108
		   if(discprt) write(8,108)
		enddo
	   enddo
c
c If one (or 2) ec50s are fixed, print then at the conc of other ligands
c at the conc of other ligands at which the specified ec50s were determined
	   if(fixec50) then
		print 4
      	if(discprt) write(8,4)
4	   	format(' Check constrained EC50s')
		allocate(conc1(10,10))
c Copy conc(i,j) to conc1(i,j) so former stays unchanged as conc used
c for set #j
 		do i=1,10
		   do j=1,10
			conc1(i,j)=conc(i,j)
		   enddo
		enddo
		do ifix=1,nfixec50
		   if(ifix.eq.1) then
			nlv=nlvar
			i5=i50
			j5=j50
c			m5=m50
			ec5=ec501
			do i=1,nlig
			   conc1(i,1)=conc_ec1(i)	!conc of other ligands at which ec50 determined
			enddo
		   else if(ifix.eq.2) then
			nlv=nlvar2
			i5=i502
			j5=j502
c			m5=m502
			ec5=ec502
			do i=1,nlig
			   conc1(i,1)=conc_ec2(i)	!conc of other ligands at which ec50 determined
			enddo
		   endif
		   print 2,ec5*1.d6,ligname(nlv)(1:10),i5,j5
		   if(discprt) write(8,2) ec5*1.d6,ligname(nlv)(1:10),i5,j5
2		   format(/,
     & ' EC50 = ',f11.3,' for ',a10,' used to constrain q(',
     & i2,',',i2,')',/,' so print actual EC50 attained')
c NB jset is now a parameter, but conc_ec1 etc have been
c copied into j=1 of array conc1, so call with jset1=1 and conc1()
		   jset1=1
		   call EC50_HJC(ec50,curinfs,curmax,concmax,cur0,pop0,
     &	    pmax,nlv,QT,jset1,conc1,k,kdim,nerr)
		   if(nerr.ne.0) then
			call BELL(1)
			print 590,nerr
			if(discprt) write(8,590) nerr
c590			format('  Error in bisection for EC50: nerr = ',i2)
		   endif
		   if(monot) then
			print 591,pmax,ligname(nlv)(1:10),ec50*1.e6
      		if(discprt) write(8,591) pmax,ligname(nlv)(1:10),
     &		ec50*1.e6
		   else	!not monotonic (never true for binding curve)
      		print 592,curmax,concmax*1.e6,pmax,
     &	  	  ligname(nlv)(1:10),ec50*1.e6
	      	if(discprt) write(8,592) curmax,concmax*1.e6,pmax,
     &		   ligname(nlv)(1:10),ec50*1.e6
		   endif
c and print conc of other ligands
		   do j=1,nlig
			if(j.ne.nlv) then
			   print 143,j,ligname(j),conc1(j,1)*1.e6
			   if(discprt) write(8,143)j,ligname(j),conc1(j,1)*1.e6
c143			format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,
c     &			' micromolar)')
			endif
		   enddo
		enddo		!ifix-1,nfixec50
		deallocate(conc1)
	   endif		!if(fixec50)
	endif			!nlig.gt.1
c

	print 3
      if(discprt) write(8,3)
3	   format(/,
     & ' _____________________________________________________ ',/)
c
	RETURN
	end
