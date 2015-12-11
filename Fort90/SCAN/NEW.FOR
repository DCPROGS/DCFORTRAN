	integer*1 isavbase(320),isavy1(320),isavy2(320)
	integer*1 isavb1(320),isavb2(320),isavb3(320)!store pixels for fit range bar
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*1 iprop1sav
	integer*2 ibaselin(nfmax)
	integer*2 idata1(2048)	!for data to be fitted (and as temp file)
	integer*2 idata0(2048)	!unscaled version of idata1 for AIVPIX
	integer*2 ix(2048),ioff2
c	integer*2 istat(20)	!for search of dy/dt
	integer*2 iampl(0:ntmax)
	integer*2 ixcalc(5120),iycalc0(5120),iycalc1(5120)
	integer*4 ifits(nfmax)
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer iact(0:15)
	real*4 filt1(1024)	!from INSCAN/NORMFILT
	real*4 tint(ntmax)
	real*4 data(2048)
	real*4 ylevel(50)		!temp storage for transition search results
	real*4 dtrigm(50)		!needed for getall; not used here
	real*4 timamax(49)
	real*4 delt(0:50)		!temp storage for transition search results
	real*4 ycalc(5120)
c    up to 5120 points for convol trace
	real*4 ydcalc(2048)		!calc values from SCANFUNC (in common/ycal/)
c arrays to hold graphics overwritten by line
	real*4 stepamp(50),dt(49),dtsav(49),t0sav
	real*8 dfinter,ttrans,tlast,tltemp,tzerod,tsample
	real*8 timsav(nfmax),tcheck
	logical ampdone	!true when prelim amp measurement has been done
	logical opendown,mono,open,shut,shutprev,shutsav
	logical opening(50),disptran,dispderiv,dispguess
	logical dubious	!amplitude ill-defined (not merely short!) (='assume' in PDP)
	logical refit,sublev,newguess,btest,accepted
	logical shut1,flat,shutrun(50)	!for point amp
	logical setshut(0:50)	!not used here -needed for setsubl/modsubl calls
	integer i1r(50),i2r(50)			!ditto
	logical help			!for QDIALOG etc
	logical abort,reject
	logical goback
	logical debug,caplock,discprt,queued
	character*55 helpst(17)		!help strings
	logical fixamp,shutp		!for fixamp
	real*4 ypeak(50)			!for fixamp
	integer mfix(50)			!for fixamp
	character*11 cnum1(50),cnum2(50),cnum3(50),cnum0,cnum
	character*11 cnum11,cnum21,cnum31	!for cjump
	character*14 clong1,clong2	!for double precision numbers in ascii
	character*70 string		!to put result in dialog box
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*12 tstring
	character*3 microsec
	character ans,ch
c For cjump
c=	integer*2 jumps(njdim),kjumps(njdim)
	integer*2 jumps(njdim)
	integer*4 kjumps(njdim)
	logical cjump,cjumpfst,fitpast
c	character getch,UC
c	integer*2 ktype
c for simplex
	real*4 theta(50)
	integer jfix(50),kfix(0:50)
c For plot queue
	character ndev*2,cdate*11,adctime*8
C For DCMENU
	integer Lb(18)
	character*78 text(18)
	logical mouse_on,mous_set
c
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/dialval1/ictd,icfd,icbd  !colours for dialog box #1 (text,frame,back)
	common/fitblk/data,if1,if2,ntrans,neval,nevfix,itype,stepamp,y0
	common/ycal/ydcalc	!calc values from SCANFUNC
	common/fixblk/fixamp,tshort,tlong,ypeak,opening,shutp,mfix
c NB keep normal step response func in filt1()
	common/filtblk/filt1,nfilt1,dtf1,npfilt,finter,filt(640),filtf
	common/hlp/help		!for QDIALOG etc
	common/ampmark/nampmark,iamark(10)
	common/dp/discprt
	common/abt/abort
	common/jump/cjumpfst
	common/steps/step(50),crtstp(50)
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi
	common/DMENU/ifonb,csizem,ifont2,nboxlast,nblast		!for DCMENU
	common/mousval/mouse_on,nbutton
	EXTERNAL SCNFUNC2

==================
	integer*1 isavbase(320),isavopen(320),isavy1(320),isavy2(320)
	integer*1 isavy3(320)
	integer*1 iprops(ntmax)	!properties of ith interval (see SCAN)
	integer*2 iampl(0:ntmax)
	integer*2 idata1(2048)	!for data to be fitted (to redraw after baseline moved)
	integer*2 ix(2048)
	integer*2 ixcalc(5120),iycalc(5120)	!up to 5120 points for convol trace
	integer*2 iyvga,iyv,iyv1
	integer*2    ioff2
	integer   iact(0:15),ixlast(50)
	real*4    data(2048)
	real*4    dt(49),stepamp(50)
	real*4    ylevel(50)		!temp storage for transition search results
	real*4    delt(0:50)
	real*4    ycalc(5120)
	real*4    filt(640)
	real*4    timamax(49)
	character*55 helpst(15)		!help strings
	character*1  ch,getch
	character*11 cnum
	character*12 tstring
	character*3  microsec
	logical bigstep,usekey,mouse_on,mous_set,kbhit,first,mono,reject
	logical opening(50),opendown,redraw,shut,shutprev,disptran,noredo
C For DCMENU
	integer 	Lb(18)
	character*78 text(18)
c
	logical cjumpfst
	common/jump/cjumpfst
	common/mousval/mouse_on,nbutton
	common/popvalx/mxlo,mylo,myhi,ictx,ibkx,icfx	!values for poptext calls
	common/datpos/yd1,yd2,iyd1,iyd2	!position of data area
	common/DMENU/ifonb,csizem,ifont2,nboxlast,nblast		!for DCMENU
      common/ginof/wxmin,wxmax,wymin,wymax,vxlo,vxhi,vylo,vyhi,ymenu,
     & xlo,xhi,ylo,yhi

