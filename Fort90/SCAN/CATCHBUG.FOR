	subroutine CATCHBUG(ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax)
c Subroutine for SCAN to try to catch the mysterious appearence of crazy
c values in ifreq(), ifmin(), ifmax()
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
	integer jmin(3),jmax(3)
	character cnum1*11,cnum2*11,cnum3*11
c
c Check ifmin, ifmax
	do j=1,3
	   if(ifmin(j).lt.imin.or.ifmin(j).gt.imax) then
		call INTCONV(j,cnum1)
		call INTCONV(imin,cnum2)
		call INTCONV(imax,cnum3)
		call BELL(6)
		call WDIALOG(1,'ERROR -CALL DC NOW!!!',12)
		call WDIALOG(1,
     & 'ifmin('//charnb(cnum1)//' c.f. imin = '//charnb(cnum2)//
     & ' imax = '//charnb(cnum3),12)
	   endif
c
	   if(ifmax(j).lt.imin.or.ifmax(j).gt.imax) then
		call INTCONV(j,cnum1)
		call INTCONV(imin,cnum2)
		call INTCONV(imax,cnum3)
		call BELL(6)
		call WDIALOG(1,'CATCHBUG ERROR -CALL DC NOW!!!',12)
		call WDIALOG(1,
     & 'ifmax('//charnb(cnum1)//' c.f. imin = '//charnb(cnum2)//
     & ' imax = '//charnb(cnum3),12)
	   endif
	enddo
c
c Check ifreq()
	jmax(1)=-2147483647
	jmin(1)=2147483647
	do j=ifmin(1),ifmax(1)
	   if(ifreq1(j).lt.jmin(1)) jmin(1)=ifreq1(j)
	   if(ifreq1(j).gt.jmax(1)) jmax(1)=ifreq1(j)
	enddo
c
	jmax(2)=-2147483647
	jmin(2)=2147483647
	do j=ifmin(2),ifmax(2)
	   if(ifreq2(j).lt.jmin(2)) jmin(2)=ifreq2(j)
	   if(ifreq2(j).gt.jmax(2)) jmax(2)=ifreq2(j)
	enddo
c
	jmax(3)=-2147483647
	jmin(3)=2147483647
	do j=ifmin(3),ifmax(3)
	   if(ifreq3(j).lt.jmin(3)) jmin(3)=ifreq3(j)
	   if(ifreq3(j).gt.jmax(3)) jmax(3)=ifreq3(j)
	enddo
c
	do i=1,3
	   if(iabs(jmin(i)).gt.30000.or.iabs(jmax(i)).gt.30000) then
		call INTCONV(i,cnum1)
		call INTCONV(jmin(i),cnum2)
		call INTCONV(jmax(i),cnum3)
		call BELL(6)
		call WDIALOG(1,'CATCHBUG ERROR -CALL DC NOW!!!',12)
		call WDIALOG(1,
     & 'Min, max in ifreq('//charnb(cnum1)//') = '//charnb(cnum2)//
     & ',  '//charnb(cnum3),12)
	   endif
	enddo
C
	RETURN
	end

