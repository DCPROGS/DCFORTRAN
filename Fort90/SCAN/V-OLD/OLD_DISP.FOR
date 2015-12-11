===old code for display of fitted values in fitsub/fitsub6
	i=0
	k1=kt+ntrans		!new kt (after accepted)
	nst=0				!number of sets of values concat into string
	do k=kt,k1
	 i=i+1	!=1,2,..,ntrans+1
	 if(k.gt.0) then
         call INTCONV(k,cnum1(i))	!transition # (if accepted)
	   if(k.lt.k1) then
		t=delt(i-1)*1.e-3		!duration (ms)
		if(t.lt.1000.) then
	         call DCFORMAT(t,7,3,cnum2(i))		!duration
		else if((t.ge.1000.).and.(t.lt.10000.)) then
	         call DCFORMAT(t,7,2,cnum2(i))		!duration
		else
	         call DCFORMAT(t,7,1,cnum2(i))		!duration
		endif
	   else		!k=kt+ntrans
	      cnum2(i)=' ?        '
	   endif
c===	   call DCFORMAT(amp(i)*calfac2,6,2,cnum3(i))	!amplitude
	   if(iampl(k).eq.0) then
		cnum3(i)=' 0         '              !amplitude
	   else
	      a=calfac2*float(iampl(k))
	      call DCFORMAT(a,6,2,cnum3(i))		!amplitude
	   endif
	   n0=NBLANK(string)
	   n1=NBLANK(cnum1(i))
	   n2=NBLANK(cnum2(i))
	   n3=NBLANK(cnum3(i))
	   if(n0.eq.0) then
	      string=cnum1(i)(1:n1)//' '//cnum2(i)(1:n2)//'ms'//
     &	  ' '//cnum3(i)(1:n3)//'pA'
		nst=1		!number of sets of values concat into string
	   else
	      string=string(1:n0)//'; '//cnum1(i)(1:n1)//' '//
     &	  cnum2(i)(1:n2)//'ms'//' '//cnum3(i)(1:n3)//'pA'
		nst=nst+1	!number of sets of values concat into string
	   endif
c	   dubious=iprops(k).eq.1
	   dubious=BTEST(iprops(k),0)		!bit 0 set (='1')
	   if(dubious) then
	      n0=NBLANK(string)
		string=string(1:n0)//'*'	!asterisk for dubious amplitude
	   endif
c Indicate fixed values (kfix(0) part applies to where 1st level is an
c opening that is fixed at level of last open level in prev fit; iprops not
c set in this case)
c	   if(iprops(k).eq.2.or.
	   if(BTEST(iprops(k),1).or.			!bit 1 set ='2'
     &	(k.eq.kt.and.kfix(0).ne.0)) then
	      n0=NBLANK(string)
		string=string(1:n0)//char(240)   !equiv sign for fixed amplitude
	   endif
	   if(k.eq.k1.or.nst.eq.3) then	!write line to dialog box now
	      call WDIALOG(1,string,icw)
	      do j=1,70
	   	   string(j:j)=char(32)	!blank ready for next
		enddo
	   endif
	 endif
	enddo
