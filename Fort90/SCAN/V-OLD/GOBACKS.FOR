	subroutine GOBACKS(indfst,indlast,ndisp,ibdpp,SHUT,
     & ixcr,iycr,iccr,ixframe,ibk,icase,
     & nfits,timsav,ifits,ibaselin,dfinter,xtrig,ndisp1,
     & kt,base,infit,tlast,iampl,tint,iprops,npfilt,ntmax,nfmax)
c Subroutine to service all 'GO TO' options in SCAN.  Uses POPMENU to show
c the various options, and does them.
c Returns icase=1 for red-cursor
c Returns icase=2 for 'go to'
c Returns icase=3 for 'redo'
	integer*1 array1(:)	!for 'popup' notices
	allocatable :: array1
	character*30 title,string(5),cans
	character*60 string1(1)
	character*30 helpst(13)		!help strings
	character*11 cnum
	character charout
	logical shut
c temp data files
	real*8 timsav(nfmax),dfinter,tlast,tt1
	integer*2 ifits(nfmax),ibaselin(nfmax)
c	real*8 timsav(1024),dfinter,tlast,tt1
c	integer*2 ifits(1024),ibaselin(1024)
c
	real*4 tint(ntmax)
	integer*2 iampl(ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
c	real*4 tint(1024)
c	integer*2 iampl(1024)
c	integer*1 iprops(1024)	!properties of ith interval (see SCAN)
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
c
	ix1=70		!ixlo for popup boxes
	iy2=200		!iyhi for popup boxes
	title='GO TO options'
	string(1)='go to red Cursor'
	string(2)='go back to Last fitting'
	string(3)='go back N fittings'
	string(4)='Redo last fitting'
	string(5)='redo last n Fittings'
	nval=5
	helpst(1)='(1) Go to position in sample '
	helpst(2)='  indicated by red cursor on'
	helpst(3)='  progress line at top      '
	helpst(4)='(2) Go back n fittings or to '
	helpst(5)='  last (n=1): redisplay data'
	helpst(6)='  for this fitting but don''t'
	helpst(7)='  refit anything.           '
	helpst(8)='(3) Redo last n fittings or  '
	helpst(9)='  last (n=1): display data for'
	helpst(10)='  the preceding fit (now the'
	helpst(11)='  last one fitted) and carry'
	helpst(12)='  on fitting from there.    '
	helpst(13)='NB all subsequent fits lost.'
	nhelp=13
c	ic=15
c	ibk=8		!don't overwrite global value
c	icup=12
c	subroutine POPMENU(ixlo,iylo,iyhi,strings,n,ic,icf,icup,ibk,title,
c     & helps,nhelp,iret,charout,ival)
	call POPMENU(nxlo,nylo,nyhi,string,nval,ictm,icfm,icupm,ibkm,
     & title,helpst,nhelp,iline,charout,ival)
	if(iline.eq.3.or.iline.eq.5) then
         call INTCONV(nfits,cnum)
	   n=NBLANK(cnum)
10	   call QDIALOG(1,
     &	'Value of n (up to '//cnum(1:n)//')',' ',12,cans)
	   call GETINPi(cans,n)
	   if(n.gt.nfits) then
		call BELL(1)
		goto 10
	   endif
	else if(iline.eq.2) then
	   n=1
	   iline=3
	else if(iline.eq.4) then
	   n=1
	   iline=5
	endif
c
	SELECT CASE(iline)
	   case(1)		!goto red cursor
		icase=1
		string1(1)=
     &      'Move red cursor with ->, <-, home, end; <Enter>=continue'
		call WINDIM(ix1,iy1,ix2,iy2,idimen,string1,1,ixc0,iyc0)
		ALLOCATE(array1(idimen))
		call RDGWIN1(ix1,iy1,ix2,iy2,array1)  !Store orig in array1
		call FRAMEFILL(ix1,iy1,ix2,iy2,1,14)
		call WRSTRING18(ixc0,iyc0,string1,1,14)
		call MOVE_RED_CURSOR(ixcr,iycr,iccr,ixframe,ibk)
c==		ivl=indfst/ibdpp
		indfst=float(ixcr)*ibdpp
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
c Remove POPUP and redraw orig screen
		call WRGWIN1(ix1,iy1,ix2,iy2,array1)
		deallocate(array1)

	   case(3)		!goback N
		icase=2
		nf1=nfits-n+1	!=nfits (latest fitting) if n=1
		in=1 + timsav(nf1)/dfinter	!abs index of 1st trans in fitting
		indfst=in-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
c		base1=float(ibaselin(nf1))

	   case(5)		!redo N
		icase=3
		if(n.eq.nfits) then	!restart from beginning!
		   indfst=1
		   indlast=indfst+ndisp-1
		   ktsav=kt
		   kt=0
		   nfits=0
		   infit=0
		   goto 11
		endif
		nf1=nfits-n+1	!=nfits (latest fitting) if n=1
		nf2=nfits-n		!# of fitting to be displayed
		in=1 + timsav(nf2)/dfinter	!abs index of 1st trans in fitting
		indfst=in-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
c         Now reset nfits,iampl,tint,iprops,base,kt,etc
c     -note that if 'redo last fitting' then display fitting BEFORE last
c     (one earlier than for 'go back' option) with infit marker at end
c	of it, and carry on fitting from there. If nfits=1 (new nfits=0)
c	then display from start of data.
c    NB number of transitions fitted in fit #i is ifits(i+1)-ifits(i)
		ktsav=kt
		kt=ifits(nf1)-1	!=last transition of last fit to be retained
		nfits=nfits-n
		base=float(ibaselin(nfits))	!reset ibasevga etc?
		tt1=timsav(nf1)			!time to start 1st overwritten fit
		tlast=tt1-1.d3*dble(tint(kt)) !time of trans before this
c want revised infit at this point
		infit=1 + tlast/dfinter	!abs index of 1st trans in fitting
		infit=infit+npfilt
11		continue
c RESET SHUT so it gives correctly the state after the new 'last fitting'
		if(kt.eq.0) then
		   SHUT=.true.
		else
		   SHUT=iampl(kt).eq.0
		endif
c zero relevant elements of iampl,tint,iprops
		do i=kt+1,ktsav
		   iampl(i)=0
		   tint(i)=0.0
		   iprops(i)=0
		enddo

	end select
c
	RETURN
	end

