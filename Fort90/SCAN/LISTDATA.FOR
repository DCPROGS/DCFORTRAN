	subroutine LISTDATA(kt,tint,iampl,iprops,calfac2,ntmax,
     & timsav,ifits,nfits,nfmax,dfinter,cjump,
     & jumps,kjumps,njdim,ijump,imode)
c To list/print data in SCAN
c Problem with jump data -when first fit done on each jump
c the data (tint etc) are all moved up one place to allow insertion
c of the interval between t=0 and first transition; ifits(i) is not changed
c so, for 1st sweep, ifits(1)=1 still and index calculated correctly from
c timsav(1) (index and timsav start at t=0, not 1st point in sample) -but
c for subsequent transitions in first fit index is calculated by adding
c tint() values to timsav(1), and these tint values must start at tint(2)
c i.e. not include 1st latency.  But cannot use cjumpfst to judge when
c this is needed -LISTDATA may be called at any time -fits for which
c this is a problem will start with first latency and will therefore have
c bit 5 ='32' set in iprops for the first interval in the fit.
c
c Modif 06/16/95 09:07am to get jump number for given transition via
c kjumps() [call GETJUMP, as in ekdist].  Original method of counting
c start of jumps does not work for imode=1 since it requires that
c we go through all transitions from 1 to kt.  Also, in case of imode=0
c a check is inserted that both methods give same answer!
c
c Modif 06/15/95 06:20pm by adding imode=0 to list all results, or
c imode=1 to list only the last 22 (no scrolling back allowed) for speed
c
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)	!properties of ith interval (see above)
	real*8 timsav(nfmax),dfinter
	integer*2 jumps(njdim)
	integer*4 ifits(nfmax),kjumps(njdim)
	logical discprt,pon,slock,cjump,btest,firstfit,notdef,bad
	allocatable	:: datasc
	character*46 datasc(:)	!for ascii version of data for display
	character*45 header
	character*55 helpst(20)		!help strings
	character*11 cnum1,cnum2,cnum3,cnum4,cnum5,cnum6,cnum7,cnum8
	character defolt*30,cans*30,ans*1		!to hold text & result of QDIALOG
c
	common/popvalt/lxlo,lylo,lyhi,ictt,ibkt,icft,icht	!values for poptable calls
	common/dp/discprt
c
	pon()=slock()
c
	notdef=.true.		!for normal (not jump) case
c
	if(kt.lt.1) then
	   RETURN
	endif
c
	fint=sngl(dfinter/1.d3)		!ms between points
	firstfit=.false.
	bad=.false.
c
	if(imode.eq.0) then
	   call WDIALOG(1,' -converting to ASCII . . . ',-11)
	endif
c
c Display results first -then ask whether to print some of them
	header='#   time(ms)   amp(pA)  props  INDEX'
	if(cjump) then
	   header='#   time(ms)   amp(pA)  props  INDEX  jump #'
	endif
	ALLOCATE(datasc(kt+1))
	iflast=1		!start at first fit
	ijmp=0		!count jump #
	lenmax=NBLANK(header)
	if(imode.eq.0) then
	   kt1=1
	   nval=kt
	else if(imode.eq.1) then
	   kt1=kt-21
	   if(kt1.lt.1) kt1=1
	   nval=kt-kt1+1
	endif
	do i=kt1,kt
	   i1=i-kt1+1			!starts at 1 -index for datasc()
     	   call INTCONV(i,cnum1)	!transition #
	   t=tint(i)
	   if(cjump) then	!check if final interval defined yet
		notdef=.not.(BTEST(iprops(i),6).and.tint(i).gt.0.0)
	   endif
	   if(i.eq.kt.and.notdef) then
	      cnum2='  ?'
	   else
		call TFORMAT(t,cnum2)
   	   endif
	   if(iampl(i).eq.0) then
		cnum3='  0'                       !amplitude
	   else
	      a=calfac2*float(iampl(i))
	      call DCFORMAT(a,6,2,cnum3)		!amplitude
	   endif
	   ip=0
	   ip=iprops(i)
	   call INTCONV(ip,cnum4)
c
	   call GETINDEX(i,i0,if1,iflast,ifits,nfits,ival,nfmax)
	   if(cjump) firstfit=BTEST(iprops(i0),5)
c returns if1=fit # preceding tint(k), i0=index in tint() of first interval
c in this fit; and ival=1 if fit #if1 starts
c with tint(k), ival=2 if tint(k) is 2nd interval of fit #if1 etc
	   index=ifixd(timsav(if1)/dfinter)	!index for 1st trans of fit
	   if(ival.gt.1) then      !add intervals that follow first in fit
		j1=0 		!correction for index of tint()
		if(firstfit) j1=1     !first fit of jump
		dt=0.0
		do j=i0,i0+ival-2	!adds one interval if ival=2 etc
		   dt=dt+tint(j+j1)
		enddo
		index=index+ifixr(dt/fint)
	   endif
	   iflast=if1		!start searching at fit #iflast next cycle
	   call INTCONV(index,cnum5)
c Next bit corrects printout for first fit of jump
	   if(firstfit) then     !first fit of jump
	      if(ival.eq.1) then		!1st latency
		   ijmp=ijmp+1			!count jumps
		   cnum5='latency    '
		else
		   call INTCONV(lastindex,cnum5)
		endif
		lastindex=index			!for cjump
	   endif
	   if(i.lt.kt) then
	 	if(BTEST(iprops(i),5).and.BTEST(iprops(i+1),5)) then   !bit 5='32'
		   cnum5='null sweep '
		   ijmp=ijmp+1			!count in null jumps
		endif
	   else if(i.eq.kt) then  !for case where last sweep is null
		if(BTEST(iprops(i),5)) then
		   cnum5='null sweep '
		   ijmp=ijmp+1			!count in null jumps
		endif
	   endif
c
	   if(.not.cjump) then
	      datasc(i1)=
     &        cnum1(1:5)//cnum2(1:11)//cnum3(1:10)//cnum4(1:4)//
     &        cnum5(1:10)
	   else if(cjump) then
		ns1=1
		call GETJUMP(i,ijmp1,njp1,ns1,jumps,kjumps,ijump)
		if(imode.eq.0) then
		   njp=int4(jumps(ijmp))
		   if(.not.bad) then
			njpsav=njp
			njp1sav=njp1
		   endif
		   if(njp.ne.njp1) then
			bad=.true.
c			call BELL(1)
c			call INTCONV(njp,cnum7)
c			call INTCONV(njp1,cnum8)
c			call WDIALOG(1,
c     &	   'ERROR: transition '//cnum1(1:5)//' in jump '/
c     &	   /cnum7(1:4)//' or '//cnum8(1:4)//' ?',12)
		   endif
		endif
		call INTCONV(njp1,cnum6)
	      datasc(i1)=
     &        cnum1(1:5)//cnum2(1:11)//cnum3(1:10)//cnum4(1:4)//
     &        cnum5(1:11)//cnum6(1:4)
	   endif
	   nl=NBLANK(datasc(i1))
	   if(nl.gt.lenmax) lenmax=nl
	enddo
c
	if(bad) then
	   call BELL(1)
	   call INTCONV(njpsav,cnum7)
	   call INTCONV(njp1sav,cnum8)
	   call WDIALOG(1,
     &	'ERROR: from transition '//cnum1(1:5)//' in jump '/
     &	 /cnum7(1:4)//' or '//cnum8(1:4)//' ?',12)

	endif
c
	helpst(1)='Shows transition no (#) and'
	helpst(2)='length and amp of event    '
	helpst(3)='following this transition. '
	helpst(4)='Properties of event:       '
	helpst(5)='  0=normal estimate        '
	helpst(6)='  1=dubious amplitude      '
	helpst(7)='  2=fixed amplitude        '
	helpst(8)='  4=constrained amplitude  '
	helpst(9)='  8=duration unusable      '
	helpst(10)='  16=amp fit with cursor   '
	if(cjump) then
	   helpst(11)='  32=value before 1st trans'
	   helpst(12)='  64=value after last trans'
	   helpst(13)='INDEX=index in CJUMP.DAT of the'
	   helpst(14)=' transition preceding specified'
	   helpst(15)=' event (starting from t=0).'
	   nhelp=15
	else
	   helpst(11)='INDEX=index in CONSAM.DAT of the'
	   helpst(12)=' transition preceding specified'
	   helpst(13)=' event (starting from 1st point.'
	   nhelp=13
	endif
c
	nlist=22		!number to list
	if(kt.lt.22) nlist=kt
	nprt=12
	if(kt.lt.12) nprt=kt
c
	if(imode.eq.0) then
	   nfst=kt-nlist+1	!first value to show in table
	   if(nfst.lt.1) nfst=1
	else
	   nfst=1
	endif
	call POPTABLE(lxlo,lylo,lyhi,datasc,header,helpst,nhelp,
     &	nlist,nfst,lenmax,nval,ictt,ibkt,icft,icht)
c
	ans='N'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,'Print some of these values',
     &  defolt,13,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'Y') then
         if(pon()) write(7,1) header
         if(discprt) write(8,1) header
1	   format(/,1x,a45)
	   call BELL(1)
	   n1=kt-nprt+1
	   if(n1.lt.1) n1=1
	   n2=kt
	   call DEFOLT2i(n1,n2,defolt)
	   call QDIALOG(1,
     &	'Print events number n1 to n2: n1,n2'
     &	,defolt,13,cans)
	   call GETINP2i(cans,n1,n2)
	   iflast=1		!start search at first fit in GETINDEX
	   do i=n1,n2
		if(imode.eq.0) then
		   i1=i
		else if(imode.eq.1) then
c==		   i1=i-n1+1			!starts at 1 -index for datasc()
		   i1=i+nlist-kt
		   if(i1.lt.1) i1=1
		endif
      	if(pon()) write(7,3) datasc(i1)
		if(discprt) write(8,3) datasc(i1)
3  		format(1x,a46)
	   enddo
	   call WDIALOG(1,'Printing completed',-11)
	endif
	DEALLOCATE(datasc)
	RETURN
	end


