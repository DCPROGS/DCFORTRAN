	subroutine GOBACKS(indfst,indlast,ndisp,bdpp,SHUT,goback,
     & shutsav,ixcr,iycr,iccr,ixframe,ibk,icase,prevlevel,
     & nfits,timsav,ifits,ibaselin,dfinter,xtrig,ktlast,
     & kt,base,infit,tlast,iampl,tint,iprops,npfilt,ntmax,nfmax,
     & cjump,ijump,kjumps,njdim,ndat,indfst0,ktjump,readnew,
     & njump,njfit,intzero,tzerod,tsample,jumps,ijumpsav,
     & iyoff,ioff1,ibasevga,basevga,sfac3,seqdisp,inseq1,inseq2)
c
c Subroutine to service all 'GO TO' options in SCAN.  Uses POPMENU to show
c the various options, and does them.
c goback=true for the REDO option -prevents SHUT being reset in FITSUB
c & FITSUB6 (correct value is set here)
c Now returns icase=iline
cc Returns icase=1 for red-cursor
cc Returns icase=2 for 'go to'
cc Returns icase=3 for 'redo'
cc Returns icase=4 for 'goto CONSAM(i)'
c Modified 11/20/96 05:29pm to fix errors in 'redo' with jumps
c
	integer*1 array1(:)	!for 'popup' notices
	allocatable :: array1
	character*30 title,string(10)
	character string1(1)*60,string2*30
	character*44 helpst(20)		!help strings
	character*11 cnum,cnum1,cnum2
	character charout,name*12
	logical shut,shutsav,goback,present,ekfile,allocated
	character*33 ekpath	!path for ekdist.tmp
c for data from ekdist.tmp
	allocatable::kcombs,ifreq,intvals
	integer*4 kcombs(:,:),ifreq(:),intvals(:,:,:)
	integer kfile(50)
	real*4 ymid(10)
	character defolt*30,cans*30	!to hold text & result of QDIALOG
c For cjumps
c	integer*2 kjumps(njdim),jumps(njdim)
	integer*4 kjumps(njdim)
	integer*2 jumps(njdim)
	logical cjump,readnew,btest
	real*8 tzerod,tsample
c temp data files
	real*8 timsav(nfmax),dfinter,tlast,tt1
	integer*2 ibaselin(nfmax)
	integer*4 ifits(nfmax)
c
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
c	real*4 tint(1024)
c	integer*2 iampl(1024)
c	integer*1 iprops(1024)	!properties of ith interval (see SCAN)
	logical seqdisp	!true while showing sequence from ekdist.tmp
	logical discprt
	common/dp/discprt
	common/popvalm/nxlo,nylo,nyhi,ictm,ibkm,icfm,icupm	!values for popmenu calls
	SAVE icom,jseq
c
	goback=.false.
	seqdisp=.false.
	ix1=70		!ixlo for popup boxes
	iy2=200		!iyhi for popup boxes
	title='GO TO options'
c
c Check for presence of EKDIST.TMP: if present it contains indices (in tint())
c of transitions which can be shown on screen
c=	INQUIRE(file='\fortran\ekdist.tmp',exist=present,flen=ilen)
	ekpath='c:\ekdist.tmp'
	INQUIRE(file=ekpath,exist=present,flen=ilen)
	ekfile=present.and.ilen.gt.0.and.(.not.cjump)
cc  If not in current directory, then ask if it is elsewhere
c	if(.not.ekfile) then
c	   if(ipath.eq.0) then
c		ans='N'		!1st time
c	   else
c		ans='Y'
c	   endif
c96 	   call DEFOLTa(ans,defolt)
c	   call QDIALOG(1,
c     &'Inspect sublevel transitions listed in last run of EKDIST',
c     &       defolt,11,cans)
c	   call GETINPa(cans,ans)
c	   if(ans.eq.'Y') then
c	      call WDIALOG(1,'Specify location of EKDIST.TMP:',11)
c		call DEFOLTi(ipath,defolt)
c10		call QDIALOG(1,
c     &     '(1) Current directory (2) \FORTRAN\SCAN\ (3) other',
c     &	defolt,11,cans)
c		call GETINPi(cans,ipath)
c		ekfile=.true.
c		if(ipath.eq.0) then
c		   ekfile=.false.
c		else if(ipath.eq.1) then
c		   ekpath='EKDIST.TMP'
c		else if(ipath.eq.2) then
c		   ekpath='\FORTRAN\SCAN\EKDIST.TMP'
c		else if(ipath.eq.3) then
c		   call TITENT0('Enter entire path',ekpath,33,.false.)
c		else
c	   	   goto 96
c		endif
c	   endif
c	endif
c
	string(1)='go to red Cursor'
	string(2)='Go to most recent fit'
	string(3)='go back N fittings'
	string(4)='Redo most recent fit'
	string(5)='redo last n Fittings'
	if(cjump) then
	   string(6)='Start of c-jump #'
	   string(7)='start of ith Jump  '
	   nval=7
	   if(ekfile) then
		string(8)='show Ekdist data     '
		nval=8
	   endif
	else
	   string(6)='go to Transition #  '
	   string(7)='go to point in Data  '
	   nval=7
	   if(ekfile) then
		string(8)='show Ekdist data     '
		nval=8
	   endif
	endif
c==========original
c	string(1)='go to red Cursor'
c	string(2)='go back to Last fitting'
c	string(3)='go back N fittings'
c	string(4)='Redo last fitting'
c	string(5)='redo last n Fittings'
c	string(6)='go to point in Data  '
c	string(7)='go to Transition #   '
c	nval=7
c	if(ekfile) then
c	   string(8)='show EKDIST data     '
c	   nval=8
c	endif
c===========end of original
	helpst(1)='(1) Go to position in sample shown by  '
	helpst(2)='  red cursor on progress line (at top) '
	if(cjump) then
         helpst(1)='(1) Go to position in current jump shown'
	   helpst(2)='  by red cursor on progress line at top'
	endif
	helpst(3)='(2,3) Go back n fittings or to last (n=1):'
	helpst(4)='  redisplay data for this fitting but '
	helpst(5)='  don''t refit anything.'
	helpst(6)='(4,5) Redo last n fittings or last (n=1):'
	helpst(7)='  display data for the preceding fit (now'
	helpst(8)=' = last one fitted) and carry on fitting'
	helpst(9)=' from there. N.B. all later fits lost.  '
	if(cjump) then
	   helpst(10)='Start of c-jump #: go to start of jump'
	   helpst(11)='   (jump numbering as in CJUMP.DAT) '
	   helpst(12)='start of ith Jump: go to start of jump'
	   helpst(13)='   (i=1,2,...,njump).               '
	   helpst(14)='(9) Show transitions defined in EKDIST.'
	   nhelp=13
	   if(ekfile) nhelp=14
	else
	   helpst(10)='(6) Display the specified transition    '
	   helpst(11)='(7) Go to point #i in the digitized    '
	   helpst(12)='  file, CONSAM.DAT.                    '
	   helpst(13)='(9) Show transitions defined in EKDIST.'
	   nhelp=12
	   if(ekfile) nhelp=13
	endif
c	ic=15
c	ibk=8		!don't overwrite global value
c	icup=12
c	subroutine POPMENU(ixlo,iylo,iyhi,strings,n,ic,icf,icup,ibk,title,
c     & helps,nhelp,iret,charout,ival)
	if(icase.ge.1.and.icase.le.10) iline=icase	!default
	call POPMENU(nxlo,nylo,nyhi,string,nval,ictm,icfm,icupm,ibkm,
     & title,helpst,nhelp,iline,charout,ival)
	icase=iline
	if(iline.eq.3.or.iline.eq.5) then
         call INTCONV(nfits,cnum)
	   n1=NBLANK(cnum)
	   n=1		!default
	   call DEFOLTi(n,defolt)
10	   call QDIALOG(1,
     &	'Value of n (up to '//cnum(1:n1)//')',' ',12,cans)
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
	   CASE(1)		!goto red cursor
		string1(1)=
     &      'Move red cursor with ->, <-, home, end; <Enter>=continue'
		call WINDIM(ix1,iy1,ix2,iy2,idimen,string1,1,ixc0,iyc0)
		ALLOCATE(array1(idimen))
		call RDGWIN1(ix1,iy1,ix2,iy2,array1)  !Store orig in array1
		call FRAMEFILL(ix1,iy1,ix2,iy2,1,14)
		call WRSTRING18(ixc0,iyc0,string1,1,14)
		call MOVE_RED_CURSOR(ixcr,iycr,iccr,ixframe,ibk)
		indfst=ifixr(float(ixcr)*bdpp)
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
		if(indlast.gt.ndat) then	!prevent display past end of data
		   indlast=ndat
		   indfst=indlast-ndisp+1
		endif
c Remove POPUP and redraw orig screen
		call WRGWIN1(ix1,iy1,ix2,iy2,array1)
		deallocate(array1)

	   CASE(3)		!goback N fittings
		nf1=nfits-n+1	!=nfits (latest fitting) if n=1
		in=1 + ifixd(timsav(nf1)/dfinter)	!abs index of 1st trans in fitting
c For cjumps, timsav(0 is defined from 1st point of 1st jump, but indfst is
c defined 'within jump' -so must now identify which jump is needed, and correct
c indfst. There are ndat (=nsamp) points in each jump.
c Note that posn of marker is determined by infit, but for jumps infit is
c defined 'within jump' so must suppress 'up to here' marker if jump # changed
c to one that does not contain the most recent opening(set infit=0 in this case)
c NB only 'redo' alters ijump
		if(cjump) then
		   ijump1=1 + in/ndat	   !ith jump, j=1,2,.. -don't change ijump
		   nj1=int4(jumps(ijump1))		!new jump # to go to
		   readnew=nj1.ne.njump		!diff jump to be read in SCAN
		   njump=nj1
		   in=in - (ijump1-1)*ndat  !index within jump of 1st trans in fitting
		endif
		indfst=in-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
c		base1=float(ibaselin(nf1))

	   CASE(5)		!redo N
		goback=.true.
		ktsav=kt
		if(n.eq.nfits) then	!restart from beginning!
		   indfst=1
		   indlast=indfst+ndisp-1
		   kt=0
		   nfits=0
		   infit=0
		   if(cjump) then
			indfst=indfst0
			ktjump=0
c			nfjump=0	!not used for anything!
			tlast=0.0d0
			ijump=1		!return to first jump
			njump=int4(jumps(1))
		      ijumpsav=ijump
		      njfit=0			!no fit yet
			readnew=.true.	!read new jump in SCAN
			shut=.true.
			shutsav=.true.
		   endif
		   goto 11
		endif
		nf1=nfits-n+1	!=nfits (latest fitting) if n=1
		nf2=nfits-n		!# of fitting to be displayed -new nfits
		in=1 + ifixd(timsav(nf2)/dfinter)	!abs index of 1st trans in fitting
c reset nfits and base here -but if returning to start of a jump then
c do not want to set base to value for prev fit (which is an earlier jump,
c but set base to value for first fit after that=1st fit in jump just being
c started
		nfits=nfits-n
		base=float(ibaselin(nfits))	!reset ibasevga etc?
		if(cjump) then
c If 'redo' takes us back to start of a jump, then, rather than displaying
c last fit in previous jump, just go back to start of present jump
c to begin fitting present jump from the beginning -in this case, last
c retained transition will be the last in the prev jump, and will have
c iprops=64 set -bit 6)
c Correction to value of IN (3 lines down) added 11/20/96 05:29pm
		   ijump1=ijump
		   ijump=1 + in/ndat	   !ith jump, j=1,2,... -new value
		   kt=ifits(nf1)-1	!=last transition of last fit to be retained
		   in=in - (ijump-1)*ndat  !index within jump of 1st trans in fitting
		endif
		indfst=in-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
c         Now reset nfits,iampl,tint,iprops,base,kt,etc
c     -note that if 'redo last fitting' then display fitting BEFORE last
c     (one earlier than for 'go back' option) with infit marker at end
c	of it, and carry on fitting from there. If nfits=1 (new nfits=0)
c	then display from start of data.
c    NB number of transitions fitted in fit #i is ifits(i+1)-ifits(i)
		kt=ifits(nf1)-1		!=last transition of last fit to be retained
c		nfits=nfits-n		!now done above
c		base=float(ibaselin(nfits))	!reset ibasevga etc?
c Want revised infit now -new position for 'up to here' marker is at
c end of fit #nf2=nfits-n. In steady-state case this point can be found
c by subtracting tint(kt) from timsav(nf1)=time for start of the next
c fit, but in cjump case this could be a first latency, so need to begin
c at the start of the last retained fit, at timsav(nf2), and add tint() for
c all the intervals in this fit to find position of the end of this fit.
c But which tint() is the last in the fit?
		if(.not.cjump) then
		   tt1=timsav(nf1)			!time to start 1st overwritten fit
		   tlast=tt1-1.d3*dble(tint(kt)) !time of trans before this
		   infit=1 + tlast/dfinter	!abs index of 1st trans in fitting
		   infit=infit+npfilt
c RESET SHUT so it gives correctly the state after the new 'last fitting'
c Redefine prevlevel, if SHUT=false, since new fitting must carry on
c at the same open level as the previous one ended in this case
		   if(kt.eq.0) then
			SHUT=.true.
		   else
			SHUT=iampl(kt).eq.0
		   endif
		else if(cjump) then
c		   New value of kt set above will be wrong if there are
c		   blank sweeps between present position and final position
c		   (blank sweeps are not registered as fits -nfits not incremented)
c		   So if the new kt is a blank sweep (iprops=32) decrement kt
c		   further until a non-blank sweep, containing the required fit,
c		   is located (?need to check if tint(kt) is blank, or just
c		   whether iprops=32 (in which case could be latency for non-blank
c		   sweep?
		   do while(BTEST(iprops(kt),5))
			kt=kt-1
		   enddo
		   ktjump=kt - kjumps(ijump) + 1    !index of kt within jump
c===
		   njump=int4(jumps(ijump))		!new value
		   njfit=njump	!new value for jump containing current fit
		   ijumpsav=ijump
		   readnew=ijump1.ne.ijump	!diff jump to be read in SCAN
		   in=in - (ijump-1)*ndat  !index within jump of 1st trans in fitting
c===
c Case where we start fitting at t=0
		   if(BTEST(iprops(kt),6)) then	!64 set
	   		base=float(ibaselin(nfits+1))	!reset base as above
			ijump=ijump+1		!show start of next jump
			readnew=ijump1.ne.ijump	!diff jump to be read in SCAN
c===next bit sets values as for 'next sweep'
			ijumpsav=ijump
			njump=int4(jumps(ijump))		!new value
			jumps(ijump)=int2(njump)	 !record jump #
c               Reset values for next sweep
			shut=.true.		!in case prev sweep ended in open state
			SHUTSAV=.true.	!ditto
c define in so that indfst set to indfst0 below
			in=indfst0+ifix(xtrig*float(ndisp))	!abs index of 1st point to display
			indfst=indfst0
			indlast=indfst+ndisp-1
c==?			isec=isec0
c==?			isdfst=indfst - (isec-1)*nbuf	!index WITHIN section of point #indfst
c==?			isdlast=isdfst+ndisp-1
c==?			ixfprev=-1	!x value where previous trigger point was drawn
c==?			ix0prev=-1	!x value where previous t=0 point was drawn (cjump)
			ktjump=0
c==			nfjump=0
			tlast=0.0d0
			ktlast=kt	!needed to detect jumps with no openings
		   	call OPENDIALOG(1,7,.true.)	!shadow border (blue=1) (7=white/grey)
	            call WDIALOG(1,'START FITTING FROM t=0',12)
		   else	!normal case
			tt1=timsav(nf2)		!start of jump to be retained
			tt1=tt1-tsample*float(ijump-1)
c Note -timsav() is counted from 1st point of first jump and increments
c continuously throughout all jumps, but tlast, ttrans are 'within jump'
c and are now measured from first point of current jump.
c Next bit seens OK except for case where the last retained fit is the
c first fit in a jump; in this case tt1 is time from start of jump to
c first trans, i.e. already includes first latency, which is then added
c again as tint(kt0), so must increment kt0 by one in this case
			kt0=ifits(nf2)			!kt for 1st trans in this
		 	if(BTEST(iprops(kt0),5)) kt0=kt0+1
			do i=kt0,kt-1
			   tt1=tt1 + 1.d3*dble(tint(i))	!time at end of this fit
			enddo
			infit=1 + tt1/dfinter	!abs index of 1st trans in fitting
			infit=infit+npfilt
			tlast=tt1
c RESET SHUT so it gives correctly the state after the new 'last fitting'
c Redefine prevlevel, if SHUT=false, since new fitting must carry on
c at the same open level as the previous one ended in this case
			if(kt.eq.0) then
			   SHUT=.true.
			else
			   SHUT=iampl(kt).eq.0
			endif
		   endif
c		note that ifits(nf2)=kt for 1st trans of last fit to be retained
c		-but no need to reset nfjump as it is not used!!
		endif
11		continue
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
		if(.not.shut) then
		    prevlevel=float(iampl(kt))+base
		endif
c zero relevant elements of iampl,tint,iprops
		do i=kt+1,ktsav
		   iampl(i)=0
		   tint(i)=0.0
		   iprops(i)=0
c    		   if(deb.and.BTEST(iprops(i),6)) then
    		   if(BTEST(iprops(i),6)) then
	            call INTCONV(i,cnum)
		      call WDIALOG(1,
     &           'iprops('//charnb(cnum)//') =64 reset to 0',11)
		   endif
		enddo
c Recalculate iyoff,ioff1,ibasevga that corespond to 'base' set above
c (see 'adjust baseline')
		i=iyoff	!save
		iyoff=ifixr(basevga - base*sfac3)
		ioff1=iyoff - i
		ibasevga=ifixr(basevga - float(ioff1))

	   CASE(6)		!go to transition #/jump #
	    if(.not.cjump) then
		call QDIALOG(1,
     &	 'Go to transition #',' ',12,cans)
		call GETINPi(cans,k)
		iflast=1
		call GETINDEX(k,k0,if1,iflast,ifits,nfits,ival,nfmax)
c returns if1=fit # preceding tint(k), i0=index in tint() of first interval
c in this fit; and ival=1 if fit #if1 starts
c with tint(k), ival=2 if tint(k) is 2nd interval of fit #if1 etc
		index=ifixd(timsav(if1)/dfinter)	!index for 1st trans of fit
		indfst=index-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
     		call INTCONV(index,cnum)
		n=NBLANK(cnum)
	      call WDIALOG(1,'Index in CONSAM of 1st transition in fit = '
     &	   //cnum(1:n),12)
	    else if(cjump) then		!start of cjump # (numbered as in CJUMP.DAT)
            call INTCONV(int4(jumps(1)),cnum)
	      n1=NBLANK(cnum)
		nj2=nj1		!default
	   	call DEFOLTi(nj2,defolt)
	   	call QDIALOG(1,
     &	'Jump number ('//char(242)//cnum(1:n1)//')',defolt,11,cans)
c	   	call QDIALOG(1,
c     &	'Jump number',' ',11,cans)
	   	call GETINPi(cans,nj1)
		readnew=nj1.ne.njump	!diff jump to be read in SCAN
		njump=nj1		!jump to be read in (but ijump, njfit unchanged)
	      indfst=intzero-ifix(xtrig*float(ndisp))	!show t=0 in standard posn
	      if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
	    endif

	   CASE(7)		!go to CONSAM(index)/ith jump
	    if(.not.cjump) then
		call QDIALOG(1,
     &	 'Value of index in CONSAM.DAT',' ',12,cans)
		call GETINPi(cans,index)
		indfst=index-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
	    else if(cjump) then		!ith jump
            call INTCONV(int4(jumps(ijump)),cnum)
	      n1=NBLANK(cnum)
		ijump1=ijump		!default
	   	call DEFOLTi(ijump1,defolt)
3	   	call QDIALOG(1,
     &	'Jump number (1 to '//cnum(1:n1)//')',defolt,11,cans)
	   	call GETINPi(cans,ijump1)
		if(ijump1.gt.ijump) then	!jumps(ijump1) not defined!
		   call BELL(1)
		   goto 3
		endif
		readnew=.true.
		njump=int4(jumps(ijump1))		!for read after return to SCAN
	      indfst=intzero-ifix(xtrig*float(ndisp))	!show t=0 in standard posn
	      if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
	    endif

	   case(8)		!show EKDIST transitions
c=		OPEN(unit=19,file='\fortran\EKDIST.TMP',
		OPEN(unit=19,file=EKPATH,
     &	status='UNKNOWN',access='TRANSPARENT')
c		read(unit=19,rec=1) k1,ksym,kval,
c     &	 ((kcombs(i,j),ymid(kcombs(i,j)),j=1,kval),i=1,k1),
c     & 	 (ifreq(i),i=1,k1),
c     & 	 ((intvals(i,j),j=1,ifreq(i)),i=1,k1)
cc     & 	 ((intvals(i,j),i=1,k1),j=1,ifreq(i))
c		CLOSE(unit=19)
		read(unit=19,rec=1) k1,k2
		ALLOCATE(kcombs(100,10),ifreq(100),intvals(k1,k2,2))
		read(unit=19,rec=1) k1,k2,ksym,kval,nfile,name,idiscn,
     & 	 ioffset,(kfile(i),i=1,nfile),
     &	 ((kcombs(i,j),ymid(kcombs(i,j)),j=1,kval),i=1,k1),
     &	 (ifreq(i),i=1,k1)
 		irec=ioffset+1
		do i=1,k1
		   do j=1,ifreq(i)
			do k=1,2
			   read(unit=19,rec=irec) intvals(i,j,k)
			   irec=irec+4
			enddo
		   enddo
		enddo
c		read(unit=19,rec=ioffset+1) ((intvals(i,j),i=1,k1),
c     &	  j=1,ifreq(i))
		CLOSE(unit=19)
c Now ask which to display
     		call INTCONV(kfile(1),cnum)
     		call INTCONV(kfile(nfile),cnum1)
     		call INTCONV(idiscn,cnum2)
		n=NBLANK(cnum)
		n1=NBLANK(cnum1)
		n2=NBLANK(cnum2)
		n3=NBLANK(name)
	      call WDIALOG(1,'Data from files '//cnum(1:n)//'-'//
     &	cnum1(1:n1)//': '//name(1:n3)//'''s disk #'//cnum2(1:n2),11)
		if(discprt) write(8,245)name,idiscn,(kfile(i),i=1,nfile)
245		format(' Direct transition data from EKDIST: ',
	&	a12,' disk # ',i2,/,'    files: ',10i5)
     		call INTCONV(k1,cnum)
     		call INTCONV(kval,cnum1)
		n=NBLANK(cnum)
		n1=NBLANK(cnum1)
	      call WDIALOG(1,'Sequences of '//cnum1(1:n1)//' levels: '//
     &	 cnum(1:n)//' combinations',11)
		if(icom.le.0.or.icom.gt.k1) then
		   icom=1	!default
		endif
c set default combo and sequence numbers
		if(jseq.lt.1.or.jseq.gt.ifreq(icom)) then
		   jseq=1
		else
		   if(jseq.lt.ifreq(icom)) then
			jseq=jseq+1
		   else
			jseq=1
			if(icom.lt.k1) then
			   icom=icom+1
			else
			   icom=1
			endif
		   endif
		endif
		call DEFOLTi(icom,defolt)
		call QDIALOG(1,
     &' Inspect sequence number',defolt,11,cans)
		call GETINPi(cans,icom)
		string2='                             '
		do j=1,kval
     		   call INTCONV(kcombs(icom,j),cnum)
		   n=nblank(cnum)
		   if(j.eq.1) then
		      string2=cnum(1:n)
		   else
			n1=nblank(string2)
			string2=string2(1:n1)//'-'//cnum(1:n)
		   endif
		enddo
		n2=nblank(string2)
     		call INTCONV(ifreq(icom),cnum)
		n=NBLANK(cnum)
		call DEFOLTi(jseq,defolt)
		call QDIALOG(1,
     &   ' Sequence = levels '//string2(1:n2)//': '//
     &	cnum(1:n)//' sequences: show # ',defolt,11,cans)
		call GETINPi(cans,jseq)
		int0=intvals(icom,jseq,1) !index in tint() of 1st interval of sequence
		itotlen=intvals(icom,jseq,2) !time from int0 to last trans (musec)
c must now convert int0 into index in CONSAM (see LISTDATA)
		fint=sngl(dfinter/1.d3)		!ms between points
	      iflast=1		!start at first fit
		call GETINDEX(int0,i0,if1,iflast,ifits,nfits,ival,nfmax)
c returns if1=fit # preceding tint(k), i0=index in tint() of first interval
c in this fit; and ival=1 if fit #if1 starts
c with tint(k), ival=2 if tint(k) is 2nd interval of fit #if1 etc
		tt1=timsav(if1)
		index=ifixd(tt1)/dfinter	!index for 1st trans of fit
		if(ival.gt.1) then      !add intervals that follow first in fit
		   dt=0.0
		   do j=i0,i0+ival-2	!adds one interval if ival=2 etc
			dt=dt+tint(j)
			jlast=j
		   enddo
		   index=index+ifixr(dt/fint)
		   tt1=tt1 + 1.d3*dble(dt)	!microsec
		endif
c Define positions of arrows to mark first and last transition in the sequence
c Use itotlen to define position of last transition in the sequence
c (if kval>2) (inseq1, inseq2 are like infit -argument for MARK)
		inseq1=1 + ifixd(tt1/dfinter)	!abs index of first transition
		inseq1=inseq1 + npfilt
		if(kval.gt.2) then
		   inseq2=inseq1 + ifixr(float(itotlen)*1.e-3/fint)
		   call WDIALOG(1,
     &	'First and last transitions marked with red arrows',11)
		else
		   inseq2=0
		   call WDIALOG(1,
     &       	'Transition marked with red arrow',11)
		endif

c
		iflast=if1		!start searching at fit #iflast next cycle
		indfst=index-ifix(xtrig*float(ndisp))	!abs index of 1st point to display
		if(indfst.lt.1) indfst=1
		indlast=indfst+ndisp-1
		seqdisp=.true.

	END SELECT

	if(ekfile.and.allocated(kcombs)) then
	   DEALLOCATE(kcombs,ifreq,intvals)	!no longer needed
	endif
c
	RETURN
	end

