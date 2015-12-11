	subroutine SELDATA(k1,kt,tint,iampl,calfac2,kval,ntmax,imode)
c Similar to LISTDATA but puts recent fittings only into POPMENU
c (intervals k1 to kt=last)
c and allows value to be chosen by highlighting it for
c (a) to SET DURATION UNUSABLE (imode=0)
c (a) to SET AMPLITUDE DUBIOUS (imode=1)
c
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
c	integer*1 iprops(ntmax)	!properties of ith interval (see above)
	allocatable	:: datasc
	character*29 datasc(:)	!for ascii version of data for display
c	character*29 header
	character*29 title,string(30)
	character*55 helpst(10)		!help strings
	character*11 cnum1,cnum2,cnum3
	character charout
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
c
	if(k1.lt.1) k1=1
	ndat=kt-k1+1
	if(ndat.le.0) then
	   kval=-1		!signal 'cancel'
	   RETURN
	endif
	ALLOCATE(datasc(ndat))
	do i=k1,kt
     	   call INTCONV(i,cnum1)	!transition #
	   t=tint(i)
	   if(i.eq.kt) then
	      cnum2='  ?'
	   else
	    if(t.lt.1.) then
	      call DCFORMAT(t,7,3,cnum2)		!duration
	    else if((t.ge.1.).and.(t.lt.10.)) then
            call DCFORMAT(t,7,2,cnum2)		!duration
	    else
            call DCFORMAT(t,7,1,cnum2)		!duration
   	    endif
   	   endif
	   if(iampl(i).eq.0) then
		cnum3='  0'                       !amplitude
	   else
	      a=calfac2*float(iampl(i))
	      call DCFORMAT(a,6,2,cnum3)		!amplitude
	   endif
c	   ip=iprops(i)
c	   call INTCONV(ip,cnum4)
c	  datasc(i)=cnum1(1:5)//cnum2(1:11)//cnum3(1:10)//cnum4(1:3)
	  datasc(i-k1+1)=cnum1(1:5)//cnum2(1:11)//cnum3(1:10)
	enddo
c
c	header='#  time(ms)  amp(pA)  props'
	if(imode.eq.0) then
	   title='Unusable shut time'
	else
	   title='Dubious amplitude'
	endif
	string(1)='Cancel'
	string(2)='#   time(ms)   amp(pA) '
	do i=1,ndat
	   string(i+2)=datasc(i)
	enddo
c
	nval=ndat+2
c
	if(imode.eq.0) then
	   helpst(1)='CHOOSE UNUSABLE SHUT TIME   '
	   helpst(2)='(Shows transition no (#) and'
	   helpst(3)=' length and amp of event    '
	   helpst(4)=' following this transition.)'
	   helpst(5)='Values shown only for fits '
	   helpst(6)='  that have been accepted. '
	   helpst(7)='The last value (length=?) is'
	   helpst(8)='  for event that precedes   '
	   helpst(9)='  first transition of the   '
	   helpst(10)='  current fit.             '
	   nhelp=10
	else
	   helpst(1)='SET AMPLITUDE AS DUBIOUS    '
	   helpst(2)='(Shows transition no (#) and'
	   helpst(3)=' length and amp of event    '
	   helpst(4)=' following this transition.)'
	   helpst(5)='Values shown only for fits '
	   helpst(6)='  that have been accepted. '
	   helpst(7)='Opening with an ill-defined'
	   helpst(8)='  amplitude (rather than    '
	   helpst(9)='  just brief) should be     '
	   helpst(10)='  marked as DUBIOUS.       '
	   nhelp=10
	endif
c
c	call POPTABLE(100,-1,470,datasc,header,helpst,nhelp,
c     &	20,kt,1,15,1,12)
c	subroutine POPMENU(ixlo,iylo,iyhi,strings,n,ic,icup,ibk,title,
c     & helps,nhelp,iret,charout,ival)
c	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	iline=kt	!default=last
	call POPMENU(nxlo,nylo,nyhi,string,nval,ictm,icfm,icupm,ibkm,
     & title,helpst,nhelp,iline,charout,ival)
	deallocate(datasc)
c
c NB POPMENU returns iline=0 when ESC hit
	if(iline.eq.0.or.iline.eq.1.or.iline.eq.2) then
	   kval=-1		!signals CANCEL
	else
c Calculate index of the marked tint()
	   kval=k1+iline-1-2		!-2 for 1st 2 lines
	endif
c
	RETURN
	end

