	   call HMAT_HJC(theta,var,elmax,kfit,badpar,kgood,
     &    covar,covlog,varlog,npar,tint,ampl,iprops,nd1,nd2,
     & 	Z00A,Z10A,Z11A,Z00F,Z10F,Z11F,
     & 	XAF,XFA,QexpQA,QexpQF,
     & 	alpha2,beta2,aloglik,kab,
     & 	kAm,kFm,km)
c  restore ML values of QT etc
	   do i=1,k
		theta0(i)=thsav1(i)
		do j=1,k
		    QT(i,j)=QTsav(i,j)
		    QD(i,j)=QDsav(i,j)
		enddo
	   enddo
c	   DEALLOCATE(QTsav,QDsav,thsav1)
c
c Define jbad(i), i=1, npar
c Also define jcov(i), i=1,npar, such that jcov(i)=n where n=row/col number
c in covar matrix that corresponds to parameter #i, and jcov(n)=-1 if
c parameter #i does not appear in the covar matrix
	   i1=0
	   i2=0
	   do m=1,npar
		jcov(m)=-1
		jbad(m)=0
		sd(m)=-1.0		!unless good value defined
		sdlog(m)=-1.0		!unless good value defined
		if(jcon(m).eq.0.and.jmic(m).eq.0.and.jfix(m).eq.0) then	!free
		   if(.not.(fixec50.and.m.eq.m50)) then		!free
			i1=i1+1
	      	if(badpar(i1)) jbad(m)=1
			if(.not.badpar(i1)) then
			   i2=i2+1
			   if(var(i2).gt.0.0d0) then
				sd(m)=sngl(dsqrt(var(i2)))
				sdlog(m)=sngl(dsqrt(varlog(i2)))
				jcov(m)=i2
			   endif
			endif
		   endif
		endif
	   enddo
c
c Now print covar() here rather than in hmat, so parameter names can be
c put at rows and of cols more easily
c If can print titlep + 10 values per row then each row of matrix
c will occupy 1+kgood/9 rows of printout
	   print 3711
	   if(discprt) write(8,3711)
3711	   FORMAT(/,' Correlation matrix= ')
	   npr=10
c	   nr=1 + kgood/(npr-1)
c  col titles
	   ir=0
	   do j=1,npar
		if(jbad(j).eq.0.and.sd(j).gt.0.0) then
		   ir=ir+1
		   if(ir.eq.1) then
		      print 50,titlep(j)(1:6)
			if(discprt) write(8,50) titlep(j)(1:6)
50			format(12x,a6)		!space for row titles
		   else
			print 51,titlep(j)(1:6)
			if(discprt) write(8,51) titlep(j)(1:6)
51			format('& ',a6)
		   endif
		   if(mod(ir,npr).eq.0) then
			ir=0
		   endif
		endif
	   enddo
c now print matrix
c
	   ii=0
	   do i=1,npar
	      if(jbad(i).eq.0.and.sd(i).gt.0.0) then
		   print 53,titlep(i)		!row label
		   if(discprt) write(8,53) titlep(i)
53		   format(1x,a10)
		   ii=ii+1
		   jj=0
		   ir=0	!count number on each row
		   do j=1,npar	!print row i
			if(jbad(j).eq.0.and.sd(j).gt.0.0) then
			   ir=ir+1
			   jj=jj+1
			   ci=covar(ii,ii)
			   cj=covar(jj,jj)
			   if(ci.le.1.d300/cj.and.ci.gt.1.d-300/cj) then
			 	den=dsqrt(covar(ii,ii)*covar(jj,jj))
				if(den.gt.1.d-30) then
				   cor=covar(ii,jj)/den
				   print 54,cor
				   if(discprt) write(8,54) cor
54				   format('& ',f6.3)
				else
				   print 55
				   if(discprt) write(8,55)
55				   format('&  n.d. ')
				endif
			   else
				print 55
				if(discprt) write(8,55)
			   endif
			   if(mod(ir,npr).eq.0) then
				ir=0
				print 52
				if(discprt) write(8,52)
52				format('             ')	!start new row
			   endif
			endif
		   enddo
		endif
	   enddo
