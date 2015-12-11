	subroutine WRITQNUM(iplot,x,y,icol,idev)
c To write queue number on screen/plot in AUTPLOT
c ix,iy=Hgraph devise units of position to start writing
	character cnum*11
c	character qfile*33
	character qfile*40
	common/queue/qfile	!changed 08/30/02 03:20pm
c
	call setfnt(1)
c	call SETSIZE(2.0)	!in call to justifystring
	call LINWID(0.15)
	call INTCONV(iplot,cnum)
	call lincols(icol,idev)
c      call JUSTIFYSTRING(x,y,' queue # '//charnb(cnum),0,3.,-1)
c write queue file name (but not path)
c Define first and last non-blank characters in qfile
	ndim=40
	n1=1		!in case filnam is all blank
	n2=1		!in case filnam is all blank
	do 10 i=1,ndim
	   ic=ichar(qfile(i:i))
	   if(ic.eq.0.or.ic.eq.32) goto 10
	   n1=i
	   goto 11		!first non-blank character found
10	continue
11	do 12 i=ndim,1,-1
	   ic=ichar(qfile(i:i))
	   if(ic.eq.0.or.ic.eq.32) goto 12
	   n2=i
	   goto 13		!last non-blank character found
12	continue
13	continue
c Look for last '\' character
	do i=n2,n1,-1
	   i1=i
	   if(qfile(i:i).eq.'\') goto 5
1	enddo
c If reach here no \ found so no path given
	n1=1
	goto 6
5	continue
	n1=i1+1		!position after last \
6	continue
      call JUSTIFYSTRING(x,y,qfile(n1:n2)//': #'//charnb(cnum),0,2.2,-1)

c
	RETURN
	end



