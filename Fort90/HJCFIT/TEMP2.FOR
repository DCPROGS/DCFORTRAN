c NUMBER OF PARAMS TO ESTIMATE=kfit, kept in theta(1),...,theta(kfit)
c Now put parameters to be estimated, kfit in number, into theta(), and
c record the corresponding values of q(i,j) in IQf(i,j), and in irate1(m),jrate1(m)
c exclude fixed, constrained params, and those calc by micro rev
c Also define here icdep(i)=0 if theta(i) is not an assoc rate, =1 if it is
c an assoc rate (in common/amax so max assoc rate can be constrained in hjclik)
c=	kfit=npar-nfix-neq-ncyc
c Next bit for debug
	if(neq.ne.neqsav) then
	   print 77,neq,neqsav
77	   format(' Problem: neq has been reset -please tell DC.',/,
     &   '	neq = ',i3,';  neqsav = ',i3)
	   neq=neqsav
	   pause
	endif
c
	kfit=npar-nfix-neq-nmr	!nmr=# of cycles constrained by mr (.le.ncyc)
	if(fixec50) kfit=kfit-nfixec50
	ik=0		!use to check vs kfit
	do m=1,npar
	 if(jfix(m).eq.0.and.jcon(m).eq.0.and.jmic(m).eq.0) then
	   if((.not.(fixec50.and.m.eq.m50)).and.
     &	  (.not.(nfixec50.eq.2.and.m.eq.m502))) then
		ik=ik+1
		i=irate(m)
		j=jrate(m)
		irate1(ik)=i
		jrate1(ik)=j
		icdep(ik)=0		!numbering for FITTED params
		do n=1,ncdep
		   if((ix(n).eq.i.and.jx(n).eq.j)) then
			icdep(ik)=1
		   endif
		enddo
		IQf(i,j)=m
		theta(ik)=QT(i,j)
	   endif
	 endif
	enddo
c
c	call BELL(2)
	print 135,npar,nfix,neq,nmr,nfixec50,kfit
	if(discprt) write(8,135) npar,nfix,neq,nmr,nfixec50,kfit
135	format(
     & '  Total number of rates = ',i6,/,
     & '    Number that are fixed       = ',i6,/,
     & '    Number that are constrained = ',i6,/,
     & '    Number set by micro rev     = ',i6,/,
     & '    Number set by fixed EC50    = ',i6,/,
     & '  Number of free rates to be estimated = ',i6,/)
	if(kfit.ne.ik) then
	   call BELL(2)
	   print 122,ik
	   if(discprt) write(8,122) ik
122	   format(' Number of free parameters = ',i6,/,
     & ' ERROR: too many parameters constrained or fixed? ',/)
	endif
c
991	continue	!jump here if nodata
	call DCASK('Write details to hjcfit.ini before fitting','N',ans)
