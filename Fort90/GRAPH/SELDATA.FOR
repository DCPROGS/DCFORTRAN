	subroutine SELDATA(ncurv,id,icol,imode)
c To choose data set (imode=0) or calc curve (imode=1) on basis of
c the colour of the lines/symbols
c ncurv=ncurvd or ncurvc
	character title*20,charout*1,cnum*11	!for popmenu
	character*20 strings(30)
      character*55 helps(1)	!for popmenu
	integer icol(100)
	allocatable::icol1
	integer icol1(:)
c
	allocate(icol1(101))
c
	nxlo=50	!pixels
	nylo=-1
	nyhi=370    !pixels
	if(ncurv.gt.22) nyhi=480
	ibkm=8		!background grey
	ibkm=7		!background white
	icfm=6		!frame brown
	icupm=12		!upper case red
	icol1(1)=0		!black for 'all curves' -first line
	if(imode.eq.0) then
	   TITLE='SELECT SET'
	   helps(1)=' Choose which data set to alter, by colour'
	   strings(1)='All data sets   '
	else
	   TITLE='SELECT CURVE'
	   helps(1)=' Choose which curve to alter, by colour'
	   strings(1)='All calc. curves'
	endif
	do i=1,100
	   icol1(i+1)=icol(i)	!so line numbers correspond with colours
	enddo
c NB does not erase properly if string() is shorter than title
	do i=1,ncurv
	   call INTCONV(i,cnum)
	   i1=i+1
	   if(imode.eq.0) then
		strings(i1)='  data set # '//charnb(cnum)//' '
		call SETBIGJ(i,i0)
		icol1(i1)=icol(i0)
	   else
		strings(i1)='calc. curve # '//charnb(cnum)//' '
		call SETBIGJ(i,i0)
		icol1(i1)=icol(i0+10)
	   endif
	enddo
	iline=id+1	!start with set #id highlighted
	nval=ncurv+1
	nhelp=1
	call POPMENUc(nxlo,nylo,nyhi,strings,nval,icol1,icfm,icupm,
     & ibkm,title,helps,nhelp,iline,charout,ival)
	id=iline-1		!data set #
c Return data set number as 'id'
c id=0 for 'all sets'
c id=-1 if menu cancelled
c
	deallocate(icol1)
	RETURN
	end

