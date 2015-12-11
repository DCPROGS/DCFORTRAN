	character cresult*30,defolt*30,title*20,ans*1,ndev*2,cnum*15
	character cres*30
c
	icf=4
	ibk=0
	ict=11
	call VIDEOMOD(18)
c	subroutine DEFDIALOG(ibox,irhi,iclo,nrow,ncol,icb)
	call DEFDIALOG(1,1,2,4,56,ibk)	!define
	call DEFDIALOG(2,1,65,4,14,0)	!define dialog box #2
c	subroutine OPENDIALOG(ibox,icf)
	call OPENDIALOG(1,icf,.true.)			!draw
	iylo=0
	iyhi=400
	call HLINE(0,639,iylo,icf)
	call VLINE(639,iylo,iyhi,icf)
	call HLINE(0,639,iyhi,icf)
	call VLINE(0,iylo,iyhi,icf)
	call WDIALOG(1,
     &	'GET BIT OF EMPTY BASELINE ON SCREEN FOR SD',-ict)
	rms=0.234
	call DCFORMAT(rms,8,3,cnum)
	call WDIALOG(1,'RMS baseline noise (pA) ='
     & 	//charnb(cnum)//
     &	': pA',ict)
	call WDIALOG(1,' O.K. [Y] ?',-ict)
	call ANYKEY
	call WDIALOG(1,
     &	'Next line #1',ict)
	call ANYKEY
	call WDIALOG(1,
     &	'Next line #2',-ict)
	call ANYKEY
c
	call CLRDIALOG(1,ibk)
c
	xval=3.45
	call DEFOLTr(xval,defolt)
	call QDIALOG(1,
     &	'Now input real',defolt,ict,cresult)
	call GETINPr(cresult,xval)
c	call ANYKEY
	x1=1.45
	x2=-4.5
	call DEFOLT2r(x1,x2,defolt)
	call QDIALOG(1,
     &	'Now two real',defolt,ict,cresult)
	call GETINP2r(cresult,x1,x2)
c	call ANYKEY
	ival=-9
	call DEFOLTi(ival,defolt)
	call QDIALOG(1,
     &	'Now input integer',defolt,ict,cresult)
	call GETINPr(cresult,ival)
c
	ans='Y'
	call DEFOLTa(ans,defolt)
	call QDIALOG(1,
     &	'Answer Y or N',defolt,ict,cresult)
c	ans=cresult(1:1)
	call GETINPa(cresult,ans)
c
	title=' orig title'
	call TDIALOG(1,'Give title',title,20,.false.,ict)
c	call ANYKEY
c
c	subroutine DISCDIALOG(ibox,query,idisc,ndev,iask,idef,ict)
	iask=-1
	idef=1
c	ndev='G:'
c	idef=0
	ndev='  '
	call DISCDIALOG(1,'Specify disc',idisc,ndev,iask,idef,ict)
	call WDIALOG(1,' Next enter SCROLDIAL',ict)
	call SCROLDIAL(1)
	call WDIALOG(1,' Left SCROLDIAL -hit any key',-ict)
	call ANYKEY
c
	call CLRDIALOG(1,ibk)
	call ANYKEY
	call OPENDIALOG(2,12,.true.)
	call WDIALOG(2,'Test the line wrapping',-12)
	call ANYKEY
	call WDIALOG(2,'Short #1',14)
	call WDIALOG(2,'Short #2',-14)
	call ANYKEY
	call WDIALOG(2,'Short #3',14)
	call QDIALOG(2,'Scroll OK?',' ',12,cres)
	call ANYKEY
c
	call ENDDIALOG(1,ibk)
	call ENDDIALOG(2,ibk)
	call ANYKEY
	call VIDEOMOD(3)
	print 10,cresult,xval,x1,x2,ival,title,ans,ndev
10	format(1x,a12,3x,3g13.6,i8,/,1x,a12,/,1x,a1,/,1x,a2)
	call ANYKEY
	end
