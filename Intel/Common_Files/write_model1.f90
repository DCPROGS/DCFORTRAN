!     Last change:  DC 16-Mar-07
subroutine write_model(main,imodel,models,indrat,ratcons,irecm0,efile,pfilem,&
	jgraph,irecq,theta0,nvdep,ifit,ncyc0,imodmf)
use menu_f90
TYPE rate_constant 
		integer irec
		integer imod
		character*74 title
		character*10 titlep(200)
		real*8 value(200)
		character*15 qij(200)
		integer iconc(200)
		character*20 ligant(200)
		integer nsetq
		integer ieq(200)
		integer jeq(200)
		integer ifq(200)
		integer jfq(200)
		real efacq(200)
		real*8 qt(100,100)
		character*20 micro(200)
		integer ncyc
		integer nsc(50)
		integer im(50,50)
		integer jm(50,50)
	END TYPE rate_constant
	
	PARAMETER  N_STATES=200
	TYPE MODEL
		integer irecm
	    INTEGER N
		integer ka
		integer kb
		integer kstat
		integer kstat0
		integer nsub
		integer kcon
		integer ncon
		integer npar
		logical indmod
		logical chardef
		integer ix
		integer iy
		integer model
		character*80 title_model
		REAL X(N_STATES)
		REAL Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		CHARACTER*15 STATNAME(N_STATES)
		character*10 con_states(20)
		character*10 sub_states(10,20)
		character*10 open_states(10,20)
		character*10 start_states(10,20)
		integer	num_states(10,20)
		integer link(n_states,n_states)
		integer inter_link(n_states)
		character*40 name_link(n_states)
		character*20 ligname(10)
		integer nlig
		integer nbound(n_states,10)
		integer nchan
		real vref
		real*8 dgamma(n_states)
		integer nwidth
		integer nheight
		character*2 charmod(25,40)
		integer ilast
		integer jlast
		integer ic(2,200)
		integer index(n_states)
    END TYPE MODEL
    
	TYPE (MODEL) models(25)
	type(rate_constant) ratcons(500)
	
    REAL*8 dgamma(100)
	real*8 theta0(200)
	character*2 charmod(25,40)	!to print model
	character*2 cmodnew(25,40)	!for NEWMOD
	real*4 pstar(4)
	character cnum*11,cnum1*11
	character efile*60,pfilem*40
	character*10 titlep(200)
	integer IC(2,200)
	integer iradio_toggle(10)
	real*8 QT(100,100)
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100)
	character*74 rtitle,rtitles(500),radio_text(10),rtitle1
	CHARACTER*80 titlem	!title to describe rate constants
	integer irate(200),jrate(200)
	logical boundef,chardef,indmod,altmod
	logical kmfast,present
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	integer ijmod(500)
	logical discprt
	character*11 cnum2
	common/dp/discprt
	!Addition for getbound
	integer nbound(100,10)
	character*74 mtitle1
	common/sm/nsmax		!for getqd/charq
    integer KMCON(9)
	integer IV(100),JV(100)
	real HPAR(100)
	integer IX(100),JX(100)
	integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer ieq(200),jeq(200),ifq(200),jfq(200)
	real efacq(200)
	integer IL(100)
	common/nmodel/cmodnew
	
	INQUIRE(file=efile,exist=present)
	
	if(.not.present) then
	   iver=200
	   nrecs=0
	   do i=1,500
			jstart(i)=0
	   enddo
	   jstart(1)=20000
	   irec=20000
	   nextrec=20000
	   ireclast=irec
	   rtitles(1)=rtitle
	   OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
	   write(17,rec=1) iver,nrecs,nextrec,ireclast,jstart
	   close(unit=17)
	   
	else
         OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
        form='BINARY',RECL=1)
		read(17,REC=1,IOSTAT=I_VAR) iver,nrecs,nextrec,ireclast,jstart
        altmod=.true.
		IF(I_VAR.NE.0) THEN
			CLOSE(unit=17)
			Imes=gmDisplayMessageBox('Stop ','Not a proper data file',&
     			GEXCLAMATION,GOK)
			goto 99
		else
			if(iver.ne.102.and.iver.ne.200.and.iver.ne.201) then
			Imes=gmDisplayMessageBox('Stop ','Not a proper data file',&
     			GEXCLAMATION,GOK)
			CLOSE(unit=17)
			goto 99
			endif
		endif
	endif
	imodalt=imodel
	

	kD=0	!kB,kC,kD not used in hjcfit but define to prevent warning
	kmfast=.false.	!not used but define to prevent warning
OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
read(unit=17,rec=1) iver,nrecs,nextrec,ireclast,jstart
	k=models(jgraph)%n
	ka=models(jgraph)%ka
	kb=models(jgraph)%kb
	indmod=models(jgraph)%indmod
	if(indmod) boundef=.true.
	kstat=k
	kf=k-ka
	kc=kf-kb
	mtitle=models(jgraph)%title_model
	rtitle=ratcons(indrat)%title
if(altmod) then
	mtitle=models(jgraph)%title_model
	rtitle=ratcons(indrat)%title
	do ir=1,nrecs
		irec=jstart(ir)	!previous nextrec
		read(unit=17,rec=irec) iver1,imod1,mtitle1,k1,kA1,kB1,kC1,kD1,rtitle1
	    ijmod(ir)=imod1
		mtits(imod1)=mtitle1	!so mtits(i)=title of model #i
		rtitles(ir)=rtitle1
	enddo
	
!   Look for lowest unused model number in qfilout
	im2=1
81	do i=1,nrecs
		if(im2.eq.ijmod(i)) then
		   im2=im2+1
		   goto 81	!repeat
		endif
	enddo
	imod=im2
else
		imod=1
		im2=1
endif		!end of if(.not.samefil)
close(unit=17)
if(imodel.lt.300) then
		!	imod=imodel 
		imod=im2
else
		!	imod=ratcons(indk)%imod
			if(imod.le.0.and.imod.ge.300) imod=im2
endif
indk=indrat
idest=0
	
	isum=0
	do i=1,ncyc
	   isum=isum + nsc(i)
	enddo
	


	

!!!!!!!
!imod=imodel
	k=models(jgraph)%n
	ka=models(jgraph)%ka
	kb=models(jgraph)%kb
	indmod=models(jgraph)%indmod
	if(indmod) boundef=.true.
	kstat=k
	kf=k-ka
	kc=kf-kb
	ncon=models(jgraph)%ncon
	kcon=models(jgraph)%kcon
	nsub=models(jgraph)%nsub
	kstat0=models(jgraph)%kstat0
	kstat=models(jgraph)%kstat
	npar0=2*kstat0
	npar1=2*kcon
	if(indmod) then
	npar=models(jgraph)%npar
	else
	npar=2*ncon
	endif
	nrateq=2*ncon
	nlig=models(jgraph)%nlig
	chardef=models(jgraph)%chardef
	mtitle=models(jgraph)%title_model
	rtitle=ratcons(indk)%title
	vref=models(jgraph)%vref
	nchan=models(jgraph)%nchan

	j=1
do k=1,models(jgraph)%n
	dgamma(k)=1.0d-12*(models(jgraph)%dgamma(k))

	statname(k)=models(jgraph)%statname(k)

enddo
if(indmod) then
	do j=1,kcon
		statname(j)=models(jgraph)%con_states(j)
	enddo
	do i=1,nsub
	
	do j=1,kstat0
	statname(kcon+j+kstat0*(i-1))=models(jgraph)%sub_states(i,j)
	enddo
	enddo
	if(nsub.eq.0)  then
	i=1
	do j=1,kstat-kcon
	statname(kcon+j)=models(jgraph)%sub_states(i,j)
	enddo
	endif
endif

	!	IQ(i,j)=m
	!	irate(m)=i
	!	jrate(m)=j
	      
k=1	


do m=1,ncon
	ic(1,m)=models(jgraph)%ic(1,m)
	ic(2,m)=models(jgraph)%ic(2,m)
	i=ic(1,m)
	j=ic(2,m)
	irate(k)=i
	jrate(k)=j
	irate(k+1)=j
	jrate(k+1)=i
	k=k+2
enddo

do k=1,npar
	titlep(k)=ratcons(indk)%titlep(k)

	qt(irate(k),jrate(k))=ratcons(indk)%value(k)

enddo
if(indmod.and.imodel.gt.300) then
m1=npar/2+1
l=npar+1
do m=m1,ncon
	ic(1,m)=models(jgraph)%ic(1,m)
	ic(2,m)=models(jgraph)%ic(2,m)

	irate(l)=ic(1,m)
	jrate(l)=ic(2,m)
	irate(l+1)=ic(2,m)
	jrate(l+1)=ic(1,m)
	
	l=l+2
enddo
do k=1,nrateq-npar
	ieq(k)=ratcons(indk)%ieq(k)
	jeq(k)=ratcons(indk)%jeq(k)
	ifq(k)=ratcons(indk)%ifq(k)
	jfq(k)=ratcons(indk)%jfq(k)
	efacq(k)=1.

	 
	qt(ieq(k),jeq(k))=qt(ifq(k),jfq(k))
enddo
else
do i=2,models(jgraph)%n
			 do j=1,i	!look at lower trangular part of QT only
				if(QT(i,j).gt.1.d-30) then
				
				    qt(i,j)=ratcons(indk)%qt(i,j)
				endif
			enddo
			enddo
do i=1,nrateq
			qt(irate(i),jrate(i))=ratcons(indk)%value(i)
enddo
endif
k=1
l=1
do i=1,nlig
	ligname(i)=models(jgraph)%ligname(i)
enddo
do m=1,ncon
	
	i=IC(1,m)
	j=IC(2,m)
	if(ratcons(indk)%iconc(k).ne.0) then
		
		ix(l)=i
		jx(l)=j
		il(l)=ratcons(indk)%iconc(k)
	!	ligname(il(l))=	ratcons(indk)%ligant(k)
		l=l+1
	endif
	if(ratcons(indk)%iconc(k+1).ne.0) then
		
		ix(l)=j
		jx(l)=i
		il(l)=ratcons(indk)%iconc(k+1)
	!	ligname(il(l))=	ratcons(indk)%ligant(k+1)
		l=l+1
	endif
	k=k+2
enddo
	ncdep=l-1
do k=1,npar
	titlep(k)=ratcons(indk)%titlep(k)
	qt(irate(k),jrate(k))=ratcons(indk)%value(k)
enddo		
do i=1,nlig
		
			do j=1,models(jgraph)%n
				nbound(j,i)=models(jgraph)%nbound(j,i)

			enddo
enddo
	if(ratcons(indk)%nsetq.gt.0) then
				nsetq=ratcons(indk)%nsetq
				do i=1,nsetq
					ieq(i)=ratcons(indk)%ieq(i)
					jeq(i)=ratcons(indk)%jeq(i)
					ifq(i)=ratcons(indk)%ifq(i)
					jfq(i)=ratcons(indk)%jfq(i)
					efacq(i)=ratcons(indk)%efacq(i)
				enddo
	endif
	kstat1=kstat
	nbytes=4+4+74+20+74+8+40+12+4*ilast*jlast+4*nrateq+4*nrateq+&
         8*nrateq+10*npar+20*nlig+4*k*nlig+4*ncdep+4*ncdep+&
         4*ncdep+8*kA+4*ncyc+4*ncyc*isum+4*ncyc*isum+&
         4*nvdep+4*nvdep+4*nvdep+16+36+5*4*nsetq+10*kstat1&
         +6*4+4*(2*ncon)+30*kstat1
		 
iverold=200	
nradio=1	 	
call intconv(nrecs+1,cnum)
radio_text(1)='Save at next vacant record:'//cnum
if(irecm0.eq.-1) then
	iy0=3
else
nradio=4
call intconv(nrecs,cnum)
radio_text(2)='Overwrite the last record:'//cnum
call intconv(irecm0,cnum)
radio_text(3)='Replace existing record'
radio_text(4)='Overwrite an earlier record'
endif
if(ifit.eq.-1) imod=imodel
iradio = gmCreateComplexDialogueBox(Main,18 ,8 ,16 , 13-iy0, GALL, 'Save model in:'//efile, &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
	
iwrite_Panel1=gmCreatePanel(iradio, 0, 0,16 , 13-iy0, gmHpos=Gtop, gmVpos=GMIDDLE, gmExpand=GON, gmType=GNOBOUNDARY, &
      gmLineBorder=GOUTEREDGE,gmfillcol=142, gmFillBorder=GOUTEREDGE)
      
if(altmod) then
call intconv(imodalt,cnum)
initText = gmCreateTextEntry(iwrite_Panel1, 0, 0, 16, 1,'Number of model modified:'//cnum(1:3)//&
' Enter this to add only a new rates set',255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)	
endif
call intconv(imod,cnum)
	initText = gmCreateTextEntry(iwrite_Panel1, 1, 1, 4, 1,'Model number:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
	ivalmod=gmCreateValueEntry(iwrite_Panel1,5,1,3,1,imod,3,0,&
              	gedit, gmType=GDECIMAL,gmTextCol=0, &
              	gmVpos=Gtop, gmHpos=Gleft)

	initText = gmCreateTextEntry(iwrite_Panel1, 1, 2, 3, 1,'Title model:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)

	isavetitle = gmCreateTextEntry(iwrite_Panel1, 4, 2, 11, 1,mtitle, 255, gedit, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
	initText = gmCreateTextEntry(iwrite_Panel1, 1, 3, 3, 1,'Rate title:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)
	isaverate = gmCreateTextEntry(iwrite_Panel1, 4, 3, 11, 1,rtitle, 255, gedit, gmBack1Col=0, gmBack2Col=12, &
      gmTextCol=1 , gmVpos=GTOP)


iwrite_Panel2=gmCreatePanel(iwrite_panel1, 1, 5,14 , 6-iy0, gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmType=Gprojected, &
      gmLineBorder=GOUTEREDGE,gmfillcol=141, gmFillBorder=GOUTEREDGE)
iradio_Radio1 = gmCreateRadioBox(iwrite_Panel2, 0, 1, 10, 4-iy0, gmType=GFREEFORM, gmBorderType=Gnoboundary, &
      gmFillCol=141,gmLineBorder=GOUTEREDGE, gmFillBorder=GOUTEREDGE, gmTitle='', gmVpos=GTOP)




do i=1,nradio
jk=0
if(i.eq.1) jk=1
   iradio_Toggle(i) = gmCreateToggleButton(iradio_Radio1, 1, i-1 , 10, 1, radio_text(i), jk, &
   gmType=G3DRADIO, gmHpos=GCENTRE, gmVpos=GTOP,gmcallback=20+i)
enddo
nprevrec=nrecs-2
if(nprevrec.lt.1) nprevrec=1
!initText = gmCreateTextEntry(iwrite_Panel2, 5, 5, 4, 1,'Overwrite record= title:', 255, gdisplay, gmBack1Col=0, gmBack2Col=12, &
!      gmTextCol=1 , gmVpos=GTOP)
if(irecm0.ne.-1) then
ivalrec=gmCreateValueEntry(iwrite_panel2,10,4,3,1,nprevrec,3,0,&
              	gedit, gmType=GDECIMAL,gmTextCol=0, &
              	gmVpos=gtop, gmHpos=Gleft)
endif
iradio_Button1 = gmCreatePushButton(iwrite_Panel1,0,0 , 16/2, 1, 'Cancel', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=20)
iradio_Button1 = gmCreatePushButton(iwrite_Panel1,16/2,0 , 16/2, 1, 'OK', &
   gmhpos=gleft,gmVpos=Gbottom,&
   gmcallback=10)
icall=0
call gmdrawwindow(iradio)
if(irecm0.ne.-1) then
if(nrecs.eq.0) then
	do i=2,4
		call gmsetwidgetstatus(iradio_Toggle(i),gunselectable)
	enddo
endif
if(irecm0.gt.nrecs) then
	call gmsetwidgetstatus(iradio_Toggle(3),gunselectable)
endif
endif
iopt=1

1	icall=gmAction(icall)
2    continue
if(icall.eq.21) then
	iopt=1
	goto 1
else if(icall.eq.22) then
	iopt=2
	goto 1
else if(icall.eq.23) then
	iopt=3
	goto 1
else if(icall.eq.24) then
	iopt=4
	goto 1
else if(icall.eq.10) then
	if(irecm0.eq.-1) then
		iopt=1
    else
	do i=1,4
	istatus=gmEnqToggleSwitch(iradio_Toggle(i))
	if(istatus.eq.1) iopt=i
	enddo
	endif	
	if(iopt.eq.1) then
		irecq=nrecs+1
		iverold=200
	else if(iopt.eq.2) then
		irecq=nrecs
		iverold=200
	else if (iopt.eq.3) then
		irecq=irecm0
	
	    irec=jstart(irecq)		!where to write the record
	   
	    rtitles(irecq)=rtitle
	    nextrec0=irec + nbytes + 1024 	
		if(irecq.lt.nrecs) then
		if(nextrec0.gt.jstart(irecq+1)) then
				imes=gmdisplaymessagebox('',&
			'Not enough space to overwrite:Save as DOS model(Y);Choose other option(N)',&
			gquestion,gyesno)
			if(imes.eq.gyesbutton) then
			    OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
				read(unit=17,rec=irec) iverold
				close(unit=17)
			else
			iverold=200
				goto 1
			endif
		endif
		endif
	else if(iopt.eq.4) then
		irecq=gmenqvaluesetting(ivalrec)
		irecm0=irecq
	    irec=jstart(irecq)		!where to write the record
	   
	    rtitles(irecq)=rtitle
	    nextrec0=irec + nbytes + 1024 
		if(irecq.lt.nrecs) then
		if(nextrec0.gt.jstart(irecq+1)) then
			imes=gmdisplaymessagebox('',&
			'Not enough space to overwrite:Save as DOS model(Y);Choose other option(N)',&
			gquestion,gyesno)
			if(imes.eq.gyesbutton) then
			OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
				read(unit=17,rec=irec) iverold
				close(unit=17)
			else
				iverold=200
				goto 1
			endif
		endif
		endif	
	endif
	imod=gmenqvaluesetting(ivalmod)
	call gmEnqTextSetting(isavetitle,mtitle)
	call gmEnqTextSetting(isaverate,rtitle)
	OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
	do ir=1,nrecs
		irec=jstart(ir)	!previous nextrec
		read(unit=17,rec=irec) iver1,imod1,mtitle1,k1,kA1,kB1,kC1
		kF=kB+kC
		kF1=kB1+kC1
		if(imod.eq.imod1.and.(kA1.ne.kA.or.kF1.ne.kF)) then
			call intconv(imod,cnum1)
			call intconv(im2,cnum2)
			Imes=gmDisplayMessageBox('Model'//cnum1(1:3)//mtitle1,&
			'Already exists but denotes a different mechanism;change to the lowest unused number:'//cnum2(1:3),&
     		Gquestion,gyesno)
			ans='Y'
			if(imes.eq.gYESbutton) then
			   imod=im2
			   call gmsetvaluesetting(ivalrec,im2)
			   OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
			   goto 1
			endif
			goto 98
		endif
	enddo
98  continue
    close(unit=17)
	call gmremovewindow(iradio)
	goto 100
else if(icall.eq.20) then
	
	call gmremovewindow(iradio)
	goto 99
else if(icall.gt.100) then
 !  icall=10
 !  goto 2
endif

	
100 continue 
 !   call gmremovewindow(iradio)
if(iopt.eq.1) then
	   nrecs=nrecs+1
	   irecm0=nrecs
	   irec=nextrec		!where to write the record
	   ireclast=irec
	   jstart(nrecs)=irec
	   rtitles(nrecs)=rtitle
	   nextrec=irec + nbytes + 1024 	
else if(iopt.eq.2) then
       irecm0=nrecs
	   irec=jstart(nrecs)		!where to write the record
	   ireclast=irec
	   rtitles(nrecs)=rtitle
	   nextrec=irec + nbytes + 1024 
else if(iopt.eq.3) then
       
	   irec=jstart(irecm0)		!where to write the record
	   
	   rtitles(irecm0)=rtitle
	   nextrec0=irec + nbytes + 1024 
else if(iopt.eq.4) then
       irecm0=irecq
	   irec=jstart(irecq)		!where to write the record
	   
	   rtitles(irecq)=rtitle
	    nextrec0=irec + nbytes + 1024 	  
endif		
	ncyc=ratcons(indrat)%ncyc
	do i=1,ncyc	
		nsc(i)=ratcons(indrat)%nsc(i)
	enddo

	do l=1,ncyc
		i1=ratcons(indrat)%im(l,1)
		j1=ratcons(indrat)%jm(l,1)
	
		do m=1,nsc(l)
			im(l,m)=ratcons(indrat)%im(l,m)
			jm(l,m)=ratcons(indrat)%jm(l,m)
		enddo
	
17		continue	
			
	
	enddo
    nchan=1
	iver1=iverold
	k=models(jgraph)%n
	if(ifit.eq.-1) then
		do i=1,nrateq
		QT(irate(i),jrate(i))=theta0(i)
		enddo
	endif
	OPEN(unit=17,file=efile,status='UNKNOWN',access='DIRECT',&
       form='BINARY',RECL=1)
	write(17,rec=1) iver,nrecs,nextrec,ireclast,jstart
	ratcons(indk)%imod=imod
	if(iver1.eq.200) then
	    chardef=.false.
		if(imodel.gt.300) then
			chardef=.false.
		!	call generate_charmod(imodel,models,ilast,jlast,0)
		else
			ilast=models(jgraph)%ilast
			jlast=models(jgraph)%jlast
		endif
		do in=1,ilast
		do jn=1,jlast
			charmod(in,jn)=models(jgraph)%charmod(in,jn)
		enddo
		enddo
	
		write(unit=17,rec=irec) iver1,&
     	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar,nsetq,kstat,((charmod(i,j),i=1,ilast),j=1,jlast),&
     	(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),&
		(QT(irate(i),jrate(i)),i=1,nrateq),(titlep(i),i=1,npar),&
        (ligname(i),i=1,nlig),((nbound(i,j),i=1,k),j=1,nlig),&
     	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),&
     	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),((im(i,j),j=1,nsc(i)),i=1,ncyc),&
     	((jm(i,j),j=1,nsc(i)),i=1,ncyc),(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),&
     	(hpar(i),i=1,nvdep),(pstar(i),i=1,4),(kmcon(i),i=1,9),&
	    (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),&
     	(efacq(i),i=1,nsetq),(statname(i),i=1,kstat),&
		nsub,kstat0,npar0,kcon,npar1,ncyc0,&
		((ic(i,j),i=1,2),j=1,ncon),(models(jgraph)%name(i),i=1,k),&
		(models(jgraph)%colour(i),i=1,k),(models(jgraph)%x(i),i=1,k),&
		(models(jgraph)%y(i),i=1,k),models(jgraph)%nwidth,models(jgraph)%nheight
	else if(iver1.eq.201) then
		if(imodel.gt.300) then
		!call generate_charmod(imodel,models,ilast,jlast,0)
		else
		ilast=models(jgraph)%ilast
		jlast=models(jgraph)%jlast
		endif
		do in=1,ilast
		do jn=1,jlast
			charmod(in,jn)=models(jgraph)%charmod(in,jn)
		enddo
		enddo
		chardef=.false.
		write(unit=17,rec=irec) iver1,&
     	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar,nsetq,kstat,((charmod(i,j),i=1,ilast),j=1,jlast),&
		(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),&
		(QT(irate(i),jrate(i)),i=1,nrateq),(titlep(i),i=1,npar),&
        (ligname(i),i=1,nlig),((nbound(i,j),i=1,k),j=1,nlig),&
     	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),&
     	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),((im(i,j),j=1,nsc(i)),i=1,ncyc),&
     	((jm(i,j),j=1,nsc(i)),i=1,ncyc),(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),&
     	(hpar(i),i=1,nvdep),(pstar(i),i=1,4),(kmcon(i),i=1,9),&
	    (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),&
     	(efacq(i),i=1,nsetq),(statname(i),i=1,kstat),&
      	nsub,kstat0,npar0,kcon,npar1,ncyc0,&
		((ic(i,j),i=1,2),j=1,ncon)
	else
			if(imodel.lt.300) then
		ilast=models(jgraph)%ilast
		jlast=models(jgraph)%jlast
		
		do in=1,ilast
		do jn=1,jlast
			charmod(in,jn)=models(jgraph)%charmod(in,jn)
		enddo
		enddo
		endif
		write(unit=17,rec=irec) iver1,&
     	imod,mtitle,k, kA, kB, kC, kD,rtitle,ilast,jlast,nrateq,&
     	ncon,ncdep,nlig,chardef,boundef,ncyc,vref,nvdep,kmfast,&
     	indmod,npar,nsetq,kstat,&
     	((charmod(i,j),i=1,ilast),j=1,jlast),&
		(irate(i),i=1,nrateq),(jrate(i),i=1,nrateq),&
		(QT(irate(i),jrate(i)),i=1,nrateq),(titlep(i),i=1,npar),&
        (ligname(i),i=1,nlig),((nbound(i,j),i=1,k),j=1,nlig),&
     	(IX(i),i=1,ncdep),(JX(i),i=1,ncdep),(IL(i),i=1,ncdep),&
     	(dgamma(i),i=1,kA),(nsc(i),i=1,ncyc),((im(i,j),j=1,nsc(i)),i=1,ncyc),&
     	((jm(i,j),j=1,nsc(i)),i=1,ncyc),(iv(i),i=1,nvdep),(jv(i),i=1,nvdep),&
     	(hpar(i),i=1,nvdep),(pstar(i),i=1,4),(kmcon(i),i=1,9),&
	    (ieq(i),i=1,nsetq),(jeq(i),i=1,nsetq),(ifq(i),i=1,nsetq),(jfq(i),i=1,nsetq),&
     	(efacq(i),i=1,nsetq),(statname(i),i=1,kstat),&
      	nsub,kstat0,npar0,kcon,npar1,ncyc0
	endif
irecq=irec
	close(unit=17)
	imodmf=imod
	write(7,210) efile,nrecs
210		format(' Mechanisms file: ',a40,/,&
        ' contains ',i4,' records of rate constants +model')
		write(7,211) nrecs,irec
211		format(/,/,' Record number ',i4,' (starts at byte #',i6,')')
		write(7,214)mtitle(1:74),imod
214   	format(1x,a74,/,' Model number = ',i3,/)
		write(7,73) nchan,kA
73		format(/,' Number of channels = ',i8,/,' Number of open states = ',i3)
		if(indmod) then
			write(7,74) kcon,nsub,kstat0
74			format(/,' Number of concerted states= ',i8,/,' Number of subunits = ',i3,&
			/,' Number of states in each subunit =',i3)
		endif
		if(nsetq.gt.0) then
			write(7,228)
228  		format(' Fixed q(i,j), set for independent models:')
			do L=1,nsetq
				write(7,229) ieq(L),jeq(L),efacq(L),ifq(L),jfq(L)
229			format(1x,2i3,'= ',f9.3,' times ',2i3)
			enddo
		endif
		do i=1,kstat
		write(7,321) i,statname(i),1.0d12*dgamma(i)
321	   format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
	   enddo
		if(ncdep.eq.0) then
			write(7,132)
132			format(' No concentration-dependent rates')
		else
			write(7,133) nlig
133			format(/,' Number of ligands = ',i3,/,&
     		' Concentration-dependent elements:',/,&
     		'   i   j     ligand #   Ligand name')
			do L=1,ncdep
				write(7,4) IX(L),JX(L),IL(L),ligname(IL(L))
4			format(1x,2i3,5x,i3,2x,a20)
			enddo
		endif
	   if(indmod) then	!don't print state names!
			write(7,4011) (ligname(i)(1:10),i=1,nlig)
4011		format(/,' Number of ligands bound',/,10x,9(2x,a10))
			do i=1,k
				write(7,4012) i,(nbound(i,n),n=1,nlig)
4012			format(' ',i3,': ',9(5x,i5))
			enddo
		else
			write(7,401) (ligname(i)(1:10),i=1,nlig)
401			format(/,' Number of ligands bound',/,' State   ',10x,9(2x,a10))
			do i=1,k
				write(7,40) i,statname(i),(nbound(i,n),n=1,nlig)
40				format(' ',i3,': ',a10,9(5x,i5))
			enddo
		endif
	 
	
		do i=1,ilast
			write(7,671) (charmod(i,j),j=1,jlast)
671			format(4x,35a2)
		enddo
			write(7,800)
800			format( ' Values of rate constants')
			do i=1,npar
				write(7,101) i,ratcons(indk)%qij(i),titlep(i),ratcons(indk)%value(i),&
				ratcons(indk)%ligant(i)
101	   format(i3,':',a10,'= ',a10,g13.6,'*',a10)
enddo
99 continue
!call gmflushcallbackqueue()
	icall=0
	RETURN

end
