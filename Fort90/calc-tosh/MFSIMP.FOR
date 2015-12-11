	subroutine MFSIMP(QD,theta,titlep,titype,idtype,npar,readini,cA1,
     & cB1,auto,loguess,higuess,repeat,tau,area,ncomp)
c To get guesses and call simplex in MECHFIT
c params/common
c Modif 06/29/99 04:42pm.  Now define MR and constraints earlier, before
c allocating QD to theta, because rate constants used for initial guesses
c may well be different from those shown in blue window in GETQD if
c constraints are imposed here.
c Also remove concentration from QD at start
c
	real*8 QD(10,10)
	real*4 theta(50)
	character*10 titlep(50)
	character*50 titype(20)		!data types
	integer idtype(20)
	integer IQ(10,10)
	logical CONCDEP	!function
	logical discprt
	logical readini,auto,repeat,prtcor,endrun
	real*4 loguess(50),higuess(50)
	real*4 tau(10,20),area(10,20)		!nc=component #; np=idtype
	integer*4 ncomp(20)
	common/qblk/IQ		!from getqd
	common/data2/nset,nsfit,juse(20),jmiss(20)
	common/dp/discprt
	common/KBLK/kA,kB,kC,kD
	COMMON/CPAR/NCDEP,IX(10),JX(10),X
	COMMON/EBLK/NEQ,IE(10),JE(10),IF(10),JF(10),EFAC(10)
	COMMON/MPAR/NCYC,NSC(8),IM(8,15),JM(8,15)
	COMMON/LIG/nlig,IL(10)
	real*4 lowval(50),highval(50),pertval(50)		!parameter
	real*4 lowsav(50),highsav(50)
	common/parlim/lowval,highval,pertval
c  internal:
	integer jfix(50)
	common/fix/jfix		!so fixed param not changed in SSDMF
	character ans*1
	character string*74,pre*2
	LOGICAL pon,slock,skipfit
	integer*2 ktype
	logical KBHIT		!Utility lib
	character ch,getch
	real*4 sminval(1000),ssdj(1000,20),ssdjmin(20)
	integer nrmj(20)
c	logical debug,caplock
c from SIMPLEX
	logical disp
	common/prt/idisp,disp
c from SSDMF
	real*4 SSD(20)
	common/ss/SSD
c from simplex/ssdmf/mfout
	logical bad,abort
	common/materr/ifail,ibad	!from QMAT5
	common/fails/nfail,nfailmax	!to count failures in QMAT5 etc in SSDMF
	common/abt/abort,ikey		!from simplex4
c
	external SSDMF
c
	pon()=slock()
c
	k=kA+kB+kC+kD
	km=10
	kQ=k
	nrun=1
	abort=.false.
c
c Specify constraints first
	if(readini) then
	   print 21,neq
21	   format(' Number of constraints set previously = ',i3)
	   do L=1,neq
      	print 219,IE(L),JE(L),EFAC(L),IF(L),JF(L)
219		format(
     &    ' Constraint: q(',i2,i2,') = ',g11.4,' times q(',i2,i2,')')
	   enddo
	   ans='Y'
	   call DCASK('Use these constraints again',ans,ans)
	   if(ans.eq.'Y') then
c		if(neq.gt.0) goto 211	!fix them
		goto 211
	   else
		goto 2121
	   endif
	endif
	NEQ=0
2121	continue
	print 108
      if(discprt) write(8,108)
	print 216
216	format(' Number of q(i,j) constrained to be equal to a fixed ',/,
     & '    multiple of another (-1 for old) [0] = ')
	L=0
	call INPUTi(L)
	if(L.lt.0) goto 212
	neq=L
	if(neq.eq.0) goto 212
	do L=1,neq
	   print 218,L
218	   format(/,' #',i2,' . Element i,j= factor times element i1,j1',
     &     /,'    specify: i,j = ')
	   call INPUT2i(ie(L),je(L))
	   print 2182
2182	   format('&   specify i1, j1 = ')
	   call INPUT2i(if(L),jf(L))
	   print 2181,efac(L)
2181	   format('&   specify factor [',f8.2,'] = ')
	   call INPUTr(efac(L))
	enddo		!end of do L=1,neq
c
c
c Now check and print the constraints
c Check that constrained parameter, q(ie,je), is not one of the params
c calc by micro rev
211	continue
	bad=.false.
	do n=1,neq
	   if(ncyc.gt.0) then
		do m=1,ncyc
		   if(ie(n).eq.im(m,1).and.je(n).eq.jm(m,1)) then
			call BELL(2)
			print 221,ie(n),je(n),m
			bad=.true.
221			format(/,' ********ERROR*********',/,
     & ' Element ',i2,',',i2,' is fixed by microscopic reversibility',/,
     & ' (cycle #',i3,'). Do not constrain it!')
	 	   endif
		   if(if(n).eq.im(m,1).and.jf(n).eq.jm(m,1)) then
			call BELL(2)
			print 222,if(n),jf(n),m
			bad=.true.
222			format(/,' ********ERROR*********',/,
     & ' Element ',i2,',',i2,' is fixed by microscopic reversibility',/,
     & ' (cycle #',i3,'). Do not use it constrain another rate!')
	 	   endif
		enddo
		if(bad) then
		   bad=.false.
		   print 223
223		   format(/,
     & ' Options:',/,
     & '  (1) Redefine which rates to fixed by microscopic rev.',/,
     & '  (2) Redefine constrained parameters',/,
     & ' Option number [1] = ')
		   iopt=1
		   call INPUTi(iopt)
		   if(iopt.eq.1) then
			call GETREV()
		   else
			goto 2121
		   endif
		endif
	   endif
c
	enddo		!end of do n=1,neq
c now print
	do n=1,neq
         print 220,IE(n),JE(n),EFAC(n),IF(n),JF(n)
         if(pon()) write(7,220) IE(n),JE(n),EFAC(n),IF(n),JF(n)
         if(discprt) write(8,220) IE(n),JE(n),EFAC(n),IF(n),JF(n)
c220     format('&',
220	   format(
     & ' Constraint: q(',i2,i2,') = ',g11.4,' times q(',i2,i2,')')
	enddo		!end of do n=1,neq
c print out constrained values here too, AND set them as fixed
c   In SIMPLEX constrained values should surely be treated as fixed, rather
c than variable: QD(ie,je)=efac*QD(if,jf) so only RHS value need be adjusted
c (no point in adjusting LHS as value is overwritten anyway in QSETD, in PDCALC)
	if(neq.gt.0) then
	   do L=1,neq
c Thus Q(ie,je) is, as far as simplex is concerned, fixed
		m=IQ(ie(l),je(l))
		JFIX(m)=1	!fixed
	   enddo
	endif
c
212	continue
c
c
c Remove concentration for QD here
c
	call QNEWC(QD,cA1,cB1,QD,1.0,1.0,
     &		ncdep,nlig,IL,IX,JX,k,km)		!Get Q(xA0) in Q0
c
c Allocate Q elements to theta(), using IQ
	string=
     & ' * = association rate; '//char(244)//' = calc by micro rev'
	print 13,string
      if(pon()) write(7,13) string
      if(discprt) write(8,13) string
13	format(/,
     &' Initial values of rate constants (with constraints and mic rev)'
     &,/,a74)
	do m=1,npar
	   pre='  '
	   call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
	   theta(m)=sngl(QD(i,j))           !define theta()
	   r=theta(m)
	   if(ncdep.gt.0) then
		do n=1,ncdep
		   if(ix(n).eq.i.and.jx(n).eq.j) then
			pre='* '		!asterisk indicates rate to be mult by conc
c=			r=r/cA1		!conc now removed from QD above
		   endif
		enddo
	   endif
c check if either if i,j or j,i is a micro rev route
	   if(ncyc.gt.0) then
		do n=1,ncyc
		   if(im(n,1).eq.i.and.jm(n,1).eq.j) then
			pre(2:2)=char(244)	!dagger sign '=q(1,2)' indicates micro rev route
		   endif
		enddo
	   endif
         print 15,m,pre,i,j,titlep(m),r
         if(pon()) write(7,15)m,pre,i,j,titlep(m),r
	   if(discprt) write(8,15)m,pre,i,j,titlep(m),r
15	   format(i3,2x,a2,' q(',i2,',',i2,')=',3x,a10,2x,g13.6)
	enddo
c
c Add parameters for number of channels in patch (assume 1 for latencies)
c NB can't keep these in theta(npar+1), +2,+3 etc because (a) might
c end up with theta(9) and theta(11) but no theta(10); anyway would
c not work if there was more than one data set with, say, idtype=1, e.g.
c pulse to different conc.  Therefore just increment kt each time an
c Nchan parameter is needed -since sets always processed in same order,
c a similar incrementing process in SSDMF, and when final results are
c printed, should access the Nchan parameters correctly.
	print 108
      if(pon()) write(7,108)
      if(discprt) write(8,108)
108	format(/)
	kt=npar		!count # of parameters to be estimated
	do j1=1,nsfit
	   j=juse(j1)
	   enchan=1.0
	   if(idtype(j).eq.1) then	!macro pulse
		kt=kt+1
		print 1,kt,j
1		format(' (',i2,') Set ',i3,
     &     ': Guess for number of channels in macroscopic pulse [1] = ')
 	      call INPUTr(enchan)
		theta(kt)=enchan
            if(pon()) write(7,4) j,titype(idtype(j)),enchan
            if(discprt) write(8,4) j,titype(idtype(j)),enchan
	   else if(idtype(j).eq.2) then	!macro step
		kt=kt+1
		print 2,kt,j
2		format('&(',i2,') Set ',i3,
     &     ': Guess for number of channels in macroscopic step [1] = ')
	      call INPUTr(enchan)
		theta(kt)=enchan
            if(pon()) write(7,4) j,titype(idtype(j)),enchan
            if(discprt) write(8,4) j,titype(idtype(j)),enchan
	   else if(idtype(j).eq.5) then	!shut time pdf
		kt=kt+1
		print 3,kt,j
3		format('&(',i2,') Set ',i3,
     &     ': Guess for number of channels in shut time data [1] = ')
		call INPUTr(enchan)
		theta(kt)=enchan
            if(pon()) write(7,4) j,titype(idtype(j)),enchan
            if(discprt) write(8,4) j,titype(idtype(j)),enchan
	   endif
4	   format(' Set ',i3,': ',a50,/,'   Guess for no of channels = ',
     &	g9.2)
	enddo
c
	call DCASK('Fit with Simplex','y',ans)
c     if fit skipped, reset lowval, highval so they can't interfere (in
c	SSDMF) with	rate constants that have been set (but save values
c	so these not written back to .ini)
	if(ans.eq.'N') then
	   skipfit=.true.
	   neqsav=neq
	   neq=0
	   do i=1,kt
		lowsav(i)=lowval(i)
		highsav(i)=highval(i)
		lowval(i)=0.01
		highval(i)=1.e20
	   enddo
c	   GOTO 98	!for debug test calcs using init guesses
	else
	   skipfit=.false.
	endif
c NB Even if Simplex not used, it may still be necessary to fix parameters
c to prevent call to SSDMF changing a micro rev parameter that may have been
c fixed ar zero -better solution is probably not to make any calls to
c SSDMF if SImplex not used -going to label 99 now skips the call
c	if(skipfit) goto 98
	if(skipfit) goto 99
c
	nfix=neq
c==	nfix=0
	if(nfix.gt.0) then	!may have fixed some at zero above
	   print 1161,nfix
1161	   format(/,'  ',i2,
     & ' parameters already fixed: number of others to be fixed [0] = ')
	   i1=0
	   call INPUTi(i1)
	   if(i1.gt.0) nfix=nfix+i1
	   n=i1		!number to get now
	else
	   i1=0
	   print 116
116	   FORMAT(' Number of parameters to be fixed [0] = ')
	   call INPUTi(i1)
	   if(i1.gt.0) nfix=nfix+i1
	   n=nfix		!number to get now
	endif
	kfit=kt-NFIX
	if(i1.eq.0) goto 117    !no more to fix
	do i=1,n
	   print 118,i
118	   format('&(',i2,'):   parameter # = ')
	   call INPUTi(itemp)
	   JFIX(itemp)=1		!ELEMENT OF JFIX=1 IF PARAM FIXED
	enddo
117	continue


c ===Insert warning if any of the param to be fixed are constrained or micro rev
c
c For MECHFIT, all q(i,j), and all Nchan, must be positive, so
c always constrain (see SSDMF)
c
	auto=.false.
	call DCASK(
     & 'Do repeated runs with random guesses (F2 to end)','n',ans)
	if(ans.eq.'Y') then
	   auto=.true.
	   skipfit=.false.
	   imode=0		!set limits and generate theta()
	   call SETGUESS(kt,npar,theta,titlep,loguess,higuess,QD,cA1,
     &	 repeat,pon(),idprt,jfix,imode)
	endif
c
c Set limits on fitted values
c Define limits for parameters, and perturbation value for anti-flattening
c stuff in simplex4.
c NB lowval(50),highval(50) are stored WITH conc (for c-dep rates)  (unlike
c loguess, higuess), because must have conc incorporated when used
c in simplex and ssdmf (standard conc, cA1).  Before storing in .INI
c the conc factor is removed
	if(auto) then
	   call DCASK('Use same limits for fitted values','y',ans)
	   if(ans.eq.'Y') then
		do m=1,kt
		   lowval(m)=loguess(m)		!make these the defaults
		   highval(m)=higuess(m)	!make these the defaults
		   pertval(m)=0.1*theta(m)	!plausible value
c=		   if(m.le.npar) then
c=			call GETIJ(IQ,k,i,j,m)		!get i,j for rate constant #m
c=			if(CONCDEP(i,j)) then
c=			   lowval(m)=lowval(m)*cA1
c=			   highval(m)=highval(m)*cA1
c=			   pertval(m)=pertval(m)*cA1
c=			endif
c=		   endif
		enddo
	   else
		call SETLIMIT(kt,npar,lowval,highval,pertval,theta,
     &	jfix,cA1,pon())
	   endif
	else
	   call SETLIMIT(kt,npar,lowval,highval,pertval,theta,
     &	jfix,cA1,pon())
	endif
c
	irestrt=3
	print 312,irestrt
312	FORMAT(' Limit number of restarts to [',i2,']: n = ')
	call INPUTi(irestrt)
c
	errfac=1.e-5
	print 525,errfac
525	format('&Relative error [',e10.4,']= ')
	call INPUTr(errfac)
	ndisp=100
	print 523,ndisp
523	format(
     & ' Print every Nth estimate [',i5,'] :  N= ')
	call INPUTi(ndisp)
c
524	continue
	kfit=kt-nfix    !in case jump to 524
c	stpfac=0.1        !'normal' value
c	confac=0.5        !'normal' value
c	resfac=1.0        !'normal' value
	confac=0.8		!parameter for simplex3 -for bad guesses
	stpfac=0.5		!better for bad guesses?
	resfac=4.0		!better for bad guesses?
	print 14,stpfac
14	format(' Initial step size factor [',f5.1,'] = ')
	call INPUTr(stpfac)
	print 16,confac
16	format(' Simplex contraction factor (0-1) [',f5.1,'] = ')
	call INPUTr(confac)
	print 17,resfac
17	format(' Restart step size=resfac*critstep: resfac [',f5.1,'] = ')
	call INPUTr(resfac)
	n=25000
	print 27,n
27	format(' Maximum number of simplex iterations [',i7,'] = ')
	call INPUTi(n)
	nevmax=n
	nevmax=-nevmax		!so iterations not printed to disc
	delmin=-1.		!do not use delmin for convergence
c	delmin=1.e-7	!use delmin for convergence
	print 12,stpfac,confac,resfac
      if(pon()) write(7,12) stpfac,confac,resfac
      if(discprt) write(8,12) stpfac,confac,resfac
12	format(/,
     &  ' stpfac = ',f6.2,'    confac = ',f6.2,' resfac = ',f6.2,/)
c
	print 108
      if(discprt) write(8,108)
	call DCASK(
     & 'Print correlation matrix for parameters','n',ans)
	prtcor=ans.eq.'Y'
	print 108
      if(discprt) write(8,108)
c
	nfail=0
	nfailmax=25
c
c
98	continue		!jump here if Simplex skipped
97	continue		!return here for next auto-fit
c
c==	RETURN	!========debug -write .ini before crash!
	smin=SSDMF(kt,THETA)		!for init guesses
	if(abort.and.(.not.auto)) RETURN
	nfail=0
	print 20,smin
      if(discprt) write(8,20) smin
20	format(' Total SSD for initial values = ',g13.6)
	do j1=1,nsfit
	   j=juse(j1)
         print 200,j,ssd(j)
         if(discprt) write(8,200) j,ssd(j)
200	   format(' SSD for each data set, SSD(',i2,') = ',g13.6)
	enddo
      print 201
      if(discprt) write(8,201)
201	format('  theta = ',/)
	if(kt.le.6) then
	   print 11,(theta(m),m=1,kt)
	   if(discprt) write(8,11) (theta(m),m=1,kt)
11	   format(6g13.6)
	else
	   print 10,(theta(m),m=1,kt)
	   if(discprt) write(8,10) (theta(m),m=1,kt)
10	   format(4(5g13.6,/))
	endif
	if(skipfit) goto 99
c
	idisp=1	!so get extra print of SSD(j) in SSDMF
c
c	call SIMPLEX3(kt,THETA,stpfac,errfac,neval,nevmax,
c     & smin,SSDMF,Ndisp,jfix,delmin,confac,irestrt,iconv)
	call SIMPLEX4(kt,THETA,stpfac,errfac,neval,nevmax,
     & smin,SSDMF,Ndisp,jfix,delmin,confac,irestrt,resfac,iconv,
     & lowval,highval,pertval)
c
      print 5233,neval
      if(pon()) write(7,5233) neval
      if(discprt) write(8,5233) neval
5233	FORMAT(/,' End of fitting: ',i8,' evaluations')
c nfit not defined at present
c	SRES=SQRT(SMIN/FLOAT(NFIT-kfit))	!ERROR S.D.
c	IF(npar.GE.7) print 5231,NEV,SMIN,(THETA(I),I=1,npar)
c	IF(npar.LT.7) print 5232,NEV,SMIN,(THETA(I),I=1,npar)
c	if(pon()) write(7,5234)NFIT,SRES
c      if(discprt) write(8,5234)NFIT,SRES
c5234	FORMAT( ' Number of points= ',I8,/,' Error S.D.= ',G13.6)
c
c Print results; theta + observed and fitted pdfs
c
c      print 23,neval,smin
c      if(pon()) write(7,23) neval,smin
c      if(discprt) write(8,23) neval,smin
c23	format(' Number of evaluations = ',i8,' Smin = ',g13.6)
	print 23,smin
      if(discprt) write(8,23) smin
23	format(' Total SSD for fitted values = ',g13.6)
	do j1=1,nsfit
	   j=juse(j1)
         print 200,j,ssd(j)
         if(discprt) write(8,200) j,ssd(j)
c200	   format(' SSD for each data set, SSD(',i2,') = ',g13.6)
	enddo
c      print 201
c      if(discprt) write(8,201)
cc201	format('  theta = ',/)
c
99	continue	!jump here to skip fit
	call MFOUT(kt,theta,titype,titlep,jfix,pon(),
     &  smin,kfit,auto,idprt,prtcor,tau,area,ncomp)
	if(abort.and.(.not.auto)) RETURN
c===========display graphs -nicest!
c If fit, was skipped restore neq etc (for .ini)
	if(skipfit) then
	   neq=neqsav
	   do i=1,kt
		lowval(i)=lowsav(i)
		highval(i)=highsav(i)
	   enddo
	endif
c
	if(nfail.gt.nfailmax) then
	   print 30, nrun,nfailmax
	   if(discprt) write(8,30) nrun,nfailmax
30	   format(' RUN # ',i3,' ABORTED because nfail exceeded ',i3,/)
	endif
	if(auto) then
	   if(nrun.le.1000) then
		sminval(nrun)=smin
		do j1=1,nsfit
		   j=juse(j1)
		   ssdj(nrun,j1)=SSD(j)
		enddo
	   endif
c
	   endrun=nrun.ge.1000.or.ikey.eq.2
	   if(.not.endrun) then
		if(KBHIT()) then
		   ch=GETCH(ktype)
		   endrun=ktype.eq.0.and.ichar(ch).eq.60	 !F2 to end
		endif
	   endif
	   if(endrun) then
c=		auto=.false.
		ssdmin=1.e37
		do j=1,nsfit
		   ssdjmin(j)=1.e37
		enddo
		do i=1,nrun
		   if(sminval(i).lt.ssdmin) then
			ssdmin=sminval(i)
			nrm=i
		   endif
		   do j=1,nsfit
			if(ssdj(i,j).lt.ssdjmin(j)) then
			   ssdjmin(j)=ssdj(i,j)
			   nrmj(j)=i
			endif
		   enddo
c
	         print 25,i,sminval(i),(ssdj(i,j),j=1,nsfit)
      	   if(pon()) write(7,25) i,sminval(i),
     &		   (ssdj(i,j),j=1,nsfit)
	         if(discprt) write(8,25)i,sminval(i),
     &		   (ssdj(i,j),j=1,nsfit)
25             format(' Run ',i3,' Smin = ',g13.6,/,
     &		 ' SSD for each data set = ',4(5g13.6,/))
		enddo
            print 26,ssdmin,nrm,
     &	 (ssdjmin(j),nrmj(j),j=1,nsfit)
            if(pon()) write(7,26) ssdmin,nrm,
     &	 (ssdjmin(j),nrmj(j),j=1,nsfit)
            if(discprt) write(8,26) ssdmin,nrm,
     &	 (ssdjmin(j),nrmj(j),j=1,nsfit)
26          format(/,' Best overall SSD = ',g13.6,' run # ',i4,/,
     &	 ' Best for each data set = ',20(g13.6,'(run ',i4,')',/))
		RETURN
	   endif
	   nrun=nrun+1
	   nfail=0
         print 24,nrun
         if(pon()) write(7,24) nrun
         if(discprt) write(8,24) nrun
24	   format(/,/,' RUN # ',i2,/)
	   imode=1		!generate theta() and print it
	   call SETGUESS(kt,npar,theta,titlep,loguess,higuess,QD,cA1,
     &	 repeat,pon(),idprt,jfix,imode)
	   nevmax=-iabs(nevmax)		!NB call to simplex resets nevmax positive
	   goto 97
	endif
	RETURN
	end

