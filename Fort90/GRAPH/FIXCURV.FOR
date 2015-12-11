	subroutine FIXCURV(ikey,ncurvd,icurvd,isym,syms,ijoin,logx,
     & x1calc,x2calc,ncurvc,icurvc,iline,icol,rlth,ndimd,ndimc)
c========================================================================
c For fixing data or calc curves -posh options with ikey=6,7,8,9
c ask which data set/calc curve (j value) if more than one
c Fixed 10/18/97 09:01pm because x1calc, x2calc are always non-log now
c	text(6)=' 6: SYMBOL TYPE'
c	text(7)=' 7: SYMBOL SIZE'
c	text(8)=' 8: FIX DATA LINE'
c	text(9)=' 9: FIX CALC LINE'
c
	real*4 rlth(100) 		!for line thickness
	integer*4 icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	real*4 syms(ndimd)		!symbol size
	integer*4 icurvc(ndimc),iline(ndimc)
	real*4 x1calc(ndimc),x2calc(ndimc)	!start and end points for calc lines
	integer icol(100)
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	logical logx
	logical help
	common/hlp/help		!for QDIALOG
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
c========================================================================
c
c
	ict=11
	if(ikey.eq.6.or.ikey.eq.7.or.ikey.eq.8) then	!data
	   if(ncurvd.eq.0) goto 99
	   if(ncurvd.gt.1) then
		call SELDATA(ncurvd,j1,icol,0)		!choose which data line
		if(j1.eq.-1) goto 99		!cancel
	   else
		j1=1
	   endif
	   if(j1.gt.0) then
		j2=icurvd(j1)		!selected set
	   else if(j1.eq.0) then	!all sets
		j2=0
c		j2=icurvd(1)	!use 1st set plotted for 'present value'
	   endif
c
	   if(ikey.eq.6) then
1161		if(j2.gt.0) then
		   i=isym(j2)		!default
		   call DEFOLTi(i,defolt)
		   call QDIALOG(1,'Symbol type (F1=help)',defolt,ict,cans)
		else
		   i=-10			!default -for 'as now'
		   call QDIALOG(1,'Symbol type (F1=help) [as now]',
     &	   ' ',ict,cans)
		endif
		if(help) then
		   call VHELP(6)
		   goto 1161
		else
		   call GETINPi(cans,i)
		endif
		if(j1.gt.0) then
		   isym(j2)=i
		else if(i.ne.-10) then	!skip if 'as now' chosen
		   do n=1,ncurvd
		      j=icurvd(n)
			isym(j)=i
		   enddo
		endif
	   else if(ikey.eq.7) then
		if(j2.gt.0) then
	         s=syms(j2)
		   call DEFOLTr(s,defolt)
		   call QDIALOG(1,'Symbol size (0.5 to 18)',defolt,ict,cans)
		else
		   s=-1.			!default -for 'as now'
		   call QDIALOG(1,'Symbol size (0.5-18) [as now]',
     &	   ' ',ict,cans)
		endif
		call GETINPr(cans,s)
		if(j1.gt.0) then
		   syms(j2)=s
		else if(s.ge.0.) then   !skip for 'as now'
		   do n=1,ncurvd
			j=icurvd(n)
			syms(j)=s
		   enddo
	      endif
	   else if(ikey.eq.8) then
1162		continue
		if(j2.gt.0) then
		   i=ijoin(j2)
		   call DEFOLTi(i,defolt)
		   call QDIALOG(1,'Line type (0-15, -1=omit: F1=help)',
     &		defolt,ict,cans)
		else
		   i=-2			!default -for 'as now'
		   call QDIALOG(1,
     &	   'Line type (0-15, -1=omit: F1=help) [as now]',
     &	   ' ',ict,cans)
		endif
		if(help) then
		   call VHELP(7)
		   goto 1162
		else
		   call GETINPi(cans,i)
		endif
		if(j1.gt.0) then
		   ijoin(j2)=i
		else if(i.ge.-1) then   !skip for 'as now'
		   do n=1,ncurvd
			j=icurvd(n)
	     		ijoin(j)=i
		   enddo
		endif
	   endif
	   ioff=0		!offset for colour/line thickness arrays
	else if(ikey.eq.9) then		!calc curve
	   if(ncurvc.eq.0) goto 99
	   if(ncurvc.gt.1) then
		call SELDATA(ncurvc,j1,icol,1)		!choose which calc line
		if(j1.eq.-1) goto 99		!cancel
	   else
		j1=1
	   endif
	   if(j1.gt.0) then
		j2=icurvc(j1)	!selected set
	   else if(j1.eq.0) then	!all sets
		j2=0
c		j2=icurvc(1)	!use 1st set plotted for 'present value'
	   endif
1163	  continue
	  if(j2.gt.0) then
		i=iline(j2)
		call DEFOLTi(i,defolt)
		call QDIALOG(1,'Line type (0-5, -1=omit; F1=help)',defolt,
     &	   ict,cans)
	   else
		i=-2			!default -for 'as now'
		call QDIALOG(1,'Line type (0-5, -1=omit: F1=help) [as now]',
     &	   ' ',ict,cans)
	   endif
	   if (help) then
		call VHELP(8) ! (7) ?
		goto 1163
	   else
		call GETINPi(cans,i)
	   endif
	   if(j1.gt.0) then
		iline(j2)=i
	   else if(i.ge.-1) then      !skip if 'as now'
		do n=1,ncurvc
		   j=icurvc(n)
		   iline(j)=i
		enddo
	   endif
c Also ask for start and end X value for calculated line
	   if(j2.gt.0) then
		j3=j2
	   else
		j3=1
	   endif
c	   if(logx) then
c		x1=10.0**x1calc(j3)
c		x2=10.0**x2calc(j3)
c	   else
		x1=x1calc(j3)
		x2=x2calc(j3)
c	   endif
	   call DEFOLT2r(x1,x2,defolt)
	   call QDIALOG(1,'Plot calc line between X values ',
     &       defolt,ict,cans)
         call GETINP2r(cans,x1,x2)
	   if(j2.eq.0) then
		i1=1
		i2=ncurvc
	   else
		i1=j1
		i2=j1
	   endif
	   do i=i1,i2
		j=icurvc(i)
c		if(logx) then
c		   x1calc(j)=alog10(x1)
c		   x2calc(j)=alog10(x2)
c		else
		   x1calc(j)=x1
		   x2calc(j)=x2
c		endif
	   enddo
c Finally set offset for last questions
	   ioff=10		!offset for colour/line thickness arrays
	endif
c Ask also for colour/thickness for data or calc lines
	if(ikey.eq.8.or.ikey.eq.9) then
	   if(j1.gt.0) then
		call SETBIGJ(j1,j0)	!in case j1>10
	   else
		j0=1
	   endif
c       First colour
1164	   if(j2.gt.0) then
		ic=icol(j0+ioff)
 		call DEFOLTi(ic,defolt)
		call QDIALOG(1,
     &	   'Colour (F1=help); at present',defolt,ict,cans)
	   else
		ic=-2			!default -for 'as now'
		call QDIALOG(1,
     &	   'Colour (F1=help); [as now]',' ',ict,cans)
	   endif
	   if(help) then
c		ifont2=2
c		call COLBAR(1,0,1200,ixlo,ixhi,iylo,iyhi,ifont2)
		call colbar(1)
		goto 1164
	   else
		call GETINPi(cans,ic)
	   endif
	   if(j1.gt.0) then
		icol(j0+ioff)=ic
	   else if(ic.ge.0) then      !skip for 'as now'
		if(ikey.eq.8) then
		do n=1,ncurvd
		   j=icurvd(n)
c		   call SETBIGJ(j,j0)	!in case j1>10
		   call SETBIGJ(n,j0)	!in case j1>10
		   icol(j0+ioff)=ic
		enddo
		else if(ikey.eq.9) then
		   do n=1,ncurvc
			j=icurvc(n)
c			call SETBIGJ(j,j0)	!in case j1>10
			call SETBIGJ(n,j0)	!in case j1>10
			icol(j0+ioff)=ic
		   enddo
		endif
	   endif
c======================================================================
c	   and thickness
	   t4=rlth(j0+ioff)
	   if(j2.gt.0) then
 		call DEFOLTr(t4,defolt)
		call QDIALOG(1,
     &	'Line thickness (real); at present',defolt,ict,cans)
	   else
		t4=-1.	!default -for 'as now'
		call QDIALOG(1,
     &	'Line thickness (real); [as now]',' ',ict,cans)
	   endif
	   call GETINPr(cans,t4)
	   if(j1.gt.0) then
		rlth(j0+ioff)=t4
	   else if(t4.ge.0.) then	!skip for 'as now'
		if(ikey.eq.8) then
		do n=1,ncurvd
		   j=icurvd(n)
c		   call SETBIGJ(j,j0)	!in case j1>10
		   call SETBIGJ(n,j0)	!in case j1>10
		   rlth(j0+ioff)=t4
		enddo
		else if(ikey.eq.9) then
		   do n=1,ncurvc
			j=icurvc(n)
c			call SETBIGJ(j,j0)	!in case j1>10
			call SETBIGJ(n,j0)	!in case j1>10
			rlth(j0+ioff)=t4
		   enddo
		endif
	   endif
	endif
c
99	continue
	RETURN
	end
