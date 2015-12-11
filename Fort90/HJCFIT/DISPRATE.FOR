	subroutine DISPRATE(QT,irate,jrate,chardef,indmod,npar,titlep,
     &  jcon,wtitle,ligname,icolour,
     &  imod0,charmod,jlast,ilast,mtitle,
     &  ncon,IC,ncdep,IX,JX,IQ,nlig,IL,imode)
c To use spindrift windows to show/alter rate constants
c Used in GETQD2/HJCFIT
c icolour=0 for usual blue window
c icolour=1 for green window (for true rates used for sim)
c wtitle=title for window
c
c Modif 01/29/02 10:46pm to make IQ,IC, IL all parameters, rather than
c in common, so disprate can be called with arrays approriate for mech
c used for simulation. Also imode for qwindhjc (=0 normally; =1 when
c constraints and m-r not to be used, e.g for simulation model when it
c differs from fitted model (NB there is already an 'imode'
c in getqd2). Also things in common/model
c
	real*8 QT(100,100)
	integer irate(200),jrate(200),jcon(200)
	logical chardef,indmod
	character*20 ligname(10)
	character*79 wtitle,heading,title
	character*10 titlep(200)
	character*2 charmod(25,40)	!to print model
	character*2 cmodnew(25,40)	!for NEWMOD
c	character*1 ans
c
	integer SCRNBUF0(1024)
	character*11 cdata(50,3),cnum
c
c=	common/model/imod0,charmod,jlast,ilast,mtitle,imodold	 !now param
	common/nmodel/cmodnew
	integer IC(2,200)			!now parameter
c	COMMON/QPAR/NCON,IC(2,200)
	integer IX(100) ,JX(100)	!now parameter
c	COMMON/CPAR/NCDEP,IX(100),JX(100),X
	COMMON/MPAR/NCYC,NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer IQ(100,100)			!now parameter
c	COMMON/qblk/IQ(100,100)
	integer IL(100)			!now parameter
c	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc

c
	call CAPTSCN(scrnbuf0)	!before putting up model
c check the rate constants
	if(chardef) then
	   call MODWIND(cmodnew,ilast+2,jlast+2,2,ilast,jlast,icolour)	!display model
	endif
	if(.not.indmod) then
		ni=npar		!number of rows (OK for independent models too)
		nidisp=ni
		if(nidisp.gt.20) nidisp=20
		nr1=0		!posn of QWIND on screen
		if(chardef) then
		   nc1=2*jlast+11
		else
		   nc1=5
		endif
		iflag=3		!so numerical values shown in QWIND2
		call QWINDHJC(QT,ni,nidisp,ncon,IC,ncyc,im,jm,
     & 	 nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,irate,jrate,
     & 	 icolour,jcon,wtitle,npar,nlig,IL,itry,imode)
	else if(indmod) then
		do m=1,npar
		   cdata(m,1)=titlep(m)
		   xs=sngl(QT(irate(m),jrate(m)))
		   call REALTOCH(xs,CDATA(m,2),11)
		   if(ncdep.gt.0) then
			do n=1,ncdep
			   i=ix(n)
			   j=jx(n)
			   if(i.eq.irate(m).and.j.eq.jrate(m)) then
				il1=IL(n)		!ligand number
				cdata(m,3)=ligname(il1)(1:11)
			   endif
			enddo
		   endif
		enddo
		ni0=npar		!rows
		nj0=3		!columns
		nidisp=npar
		if(npar.gt.20) nidisp=20
c		nr1=-1		!so window initially in default position
		nr1=0		!posn of QWIND on screen
		nc1=2*jlast+11
		iconst=0	!no constraint
		iflagc=nj0		!show all cols
		nd1=50		!declared dimensions of cdata
		nd2=3
		call INTCONV(npar,cnum)
		title='     '//charnb(cnum)//' rate constants'
		heading='    name         value        ligand '
		call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     & 	 nr1,nc1,iflagc,iconst,nd1,nd2)
		do i=1,npar
		   titlep(i)=cdata(i,1)(1:10)
		   call CHTOREAL(cdata(i,2),xs)
		   QT(irate(i),jrate(i))=dble(xs)
		enddo
         endif
	   call DISPSCN(scrnbuf0)  !restore orig screen
c
c	   if(itry.eq.1) then
c		ans='N'
c		idest=59	!signal to return to getqd1
c		goto 999
cc		goto 59	!reread rates (from another disc?)
cc	   endif
cc	else		!when model too big/complex for chessboard
cc==	   call MODRATES()	TO BE DONE
c	endif		!end ofif(chardef)
c
	RETURN
	end

