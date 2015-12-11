	subroutine ACGAU(Acrit,x1,sd1,a1,x2,sd2,a2)
c Calc Acrit for min total number misclassified in discrimination
c between 2 gaussians with means x1,x2.
c Modif 03/15/95 03:16pm -acoef is found by difference so must treat
c   it as zero if less than, say 1.e-5 (1.e-4 safer) or else all
c   sig figs lost in acoeff.
c
c Solve the quadratic
	var1=sd1*sd1
	var2=sd2*sd2
	acoef=(1./var2) - (1./var1)
	bcoef=-2.*((x2/var2) - (x1/var1))
	ccoef=(x2*x2/var2) - (x1*x1/var1) - 2.*alog((a2*sd1)/(a1*sd2))
	if(abs(acoef).lt.1.e-5) goto 3	!treat as zero
c==	if(abs(acoef).lt.1.e-20) goto 3
c	print 2,acoef,bcoef,ccoef
c2	format(' a,b,c= ',3g13.6)
c
	disc=sqrt(bcoef*bcoef - 4.*acoef*ccoef)
	root1=(-bcoef-disc)/(2.*acoef)
	root2=(-bcoef+disc)/(2.*acoef)
cd	print 1,root1,root2
cd1	format(' root1,root2= ',2g13.6)
	if((root1.gt.0.).and.(root2.lt.0.)) acrit=root1
	if((root2.gt.0.).and.(root1.lt.0.)) acrit=root2
c some numbers give max as well as min- both roots pos
	if((root1.gt.0.).and.(root2.gt.0.)) goto 10
	RETURN
c check values if both roots pos
10	call check(root1,x1,sd1,a1,x2,sd2,a2,en1)
	call check(root2,x1,sd1,a1,x2,sd2,a2,en2)
	acrit=root1
	if(en2.lt.en1) acrit=root2
	RETURN
c
3	acrit=-ccoef/bcoef
cd	print 5,acrit
cd5	format(' -c/b= ',g13.6)
	RETURN
	end

	subroutine check(acrit,x1,sd1,a1,x2,sd2,a2,en)
c
	u1=(acrit-x1)/sd1
	u2=(acrit-x2)/sd2
	y1=erfs(u1/sqrt(2.))
	y2=erfs(u2/sqrt(2.))
	en1=1000.*a1*0.5*(1.-y1)
	en2=1000.*a2*0.5*(1.+y2)
	en=en1+en2
cd	print 7,acrit,en1,acrit,en2,en
cd7	format( ' No (per 1000) of pop 1 above ',g10.2,' = ',g13.6,/,
cd     & ' No (per 1000) of pop 2 below ',g10.2,' = ',g13.6,/,
cd     & ' Total no misclassified= ',g13.6)
	RETURN
	end


