	subroutine GETOMIT(x11,x21,nomit,x1,ymin,ymax,logx,pprt,idest)
c To define points to be omitted in CJFIT (called from CJVDU)
	logical logx
	character*1 ans
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	real ax(10),ay(10)
	character*1 ch
	integer icol(100)
	character cnum*11,cnum1*11
	logical pprt,discprt
	common/dp/discprt
	COMMON/cols/icol,mono
c
	jx=320
	jy=240
	ibk=icol(71)
	icf=2
	itype=0
	line=1
	idraw=2
	islope=0
	jxlo=1
	jylo=56
	jxhi=639
	jyhi=386
	jy0=300
	eps=1.
	idest=0
c Add option to omit the first part of the jump. starting at t=0
	ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     & 'Omit the initial bit, starting t=0 (defined above)'
     &	,defolt,11,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'Y') then
	   x11=x1
	else
	   call WDIALOG(1,
     &  'Mark first point to be omitted with cursor; hit enter',12)
	   idraw=-1
	   line=0
	   islope=0
	   call crossh(jx,jy0,jxlo,jylo,jxhi,jyhi,ibk,icf,itype,
     &	line,idraw,islope,ax,ay,6,70,6,1.)
	   x11=ax(1)
	   call BELL(1)
	endif
	call lincols(12)
	iltype=1		!dotted
	   call broken(iltype)
	   call graMOV(x11,ymin)
	   call gralin(x11,ymax)
	call WDIALOG(1,
     &  'Mark last point to be omitted with cursor; hit enter',12)
	call BELL(1)
	   idraw=-1
	   line=0
	   call crossh(jx+60,jy0,jxlo,jylo,jxhi,jyhi,ibk,icf,itype,
     &	line,idraw,islope,ax,ay,6,70,6,1.)
	   x21=ax(1)
	   call graMOV(x21,ymin)
	   call gralin(x21,ymax)
	call lincols(15)
	if(logx) then
	   x11=10.0**x11			!take antilog here
	   x21=10.0**x21			!take antilog here
	endif
	call DCFORMAT(x11,8,1,cnum)
	call DCFORMAT(x21,8,1,cnum1)
	ans='Y'
 	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     & 'Omit from X = '//charnb(cnum)//' to '//charnb(cnum1)//' O.K.'
     &	,defolt,11,cans)
	call GETINPa(cans,ans)
	if(ans.eq.'N') then
 	   call VIDEOMOD(3)	 !utility lib- this makes graph go for good!
	   idest=230
c	   goto 230			!another display?
	endif
	nomit=1		!signals that a bit omitted
	if(pprt) write(7,3032) x11,x21
	if(discprt) write(8,3032) x11,x21
3032	format(/,
     & ' Omit points between X = ',g13.6,' and ',g13.6)
c
	RETURN
	end

