!     Last change:  D    15 Jan 104    2:21 pm
	subroutine create_model(ixm,iym,jgraph,imodel,main,form1,form1_txtarray1,&
	graph1_2,GraphMainPanel1_2,graphics1_2,nxmodel,nymodel,mtype,models,&
	mod_create,irecm0,qmec,titlem,dxsm,dysm,ijmod,ka,kf,ic,indk,pfilem,open7,apfile,&
		ipbar_Progress2,indwin,ncdep,iformText,autosim,ksim,&
		initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
!	indk=indrat index of record in file	
	use gino_f90
	use menu_f90
	PARAMETER  N_STATES=200
	integer njlink(100),jfix(200)
	character*20 namelink(100)                                                                                                                                                                                                                                                                                                              
	integer :: Form1_TxtArray1(25,4),form1(25,4),iformText(25,4)
	integer :: Main
	integer :: Graph1_2(100)
	integer :: GraphMainPanel1_2(100)
	integer :: Graphics1_2(100)
	!allocatable text
	!character*10 text(:,:)
	REAL*8 dgamma(100),theta0(200)
	character*2 charmod(25,40)	!to print model
	character*2 cmodnew(25,40)	!for NEWMOD
	real*4 pstar(4)
	character*8 titles(100)
	logical nopath
	character qpath*30,qname*8,suffix*3,ndev*2,cnum*11,conex*30,xtext*100,cnum1*11
	character*5 cnum5,cnum51
	character rfile*40,qmec*60,qfilmsav*40,pfilem*40
	character*10 titlep(200),titlep1(200)
	character*20 statext
	integer IQ(100,100),imatrix(25,40),IC(2,200),ic2(2,200)
	integer inew(100)
	real*8 QT(100,100),temp(100)
	integer jstart(500)
	character*20 ligname(10)
	character*10 statname(100),qij(100)
	character*74 rtitle,rtitles(500)
	CHARACTER*80 titlem	!title to describe rate constants
	integer IQQ(100,100)
	integer irate(200),jrate(200)
	logical boundef,chardef,indmod,repeat,samefil,altmod,apfile
	logical readp
	logical kmfast,readini,useini,present,renum,alter,newrate
	character*1 ans,UC,ans1,ch1,ch2
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	integer ijmod(500),iwid(2),iwcell(10)
	logical discprt,open7,obeymr(50),automr(50),allmr
	common/dp/discprt
	!Addition for getbound
	integer nbound(100,10)
	integer isubpanel(20),isub(10,20),icons(20)
	character*1 ch
	common/nbnd/nbound
	common/mr/obeymr,automr
!	common/model/imod0,charmod,jlast,ilast,mtitle,imodold
	character*11 cnum0
	!For 'true' model used for simulation -used in getqd2
	logical autosim,first
	integer irates(200),jrates(200)
	logical statdef	!state names defined

	real*8 dgammas(100)
	character*60 :: qdir='.'
		character*120 :: qfilt='*.mec'//char(124)//'Mechanism Files(MEC)'//&
     	char(124)//'*.dat'//char(124)//'Old Files (DAT)'//&
     	char(124)//'*.*'//char(124)//'All Files'

	integer::Button7(10),RECORDS
	character*10 label7(2)
    character*74 mtitles	!title for sim model
	character*2 charmods(25,40)	!to print model

	!character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT

	common/sm/nsmax		!for getqd/charq
	common/QDBLK2/npar,IQ,irate1(200),jrate1(200),nlig 
	integer KMCON(9)
	integer IV(100),JV(100)
	real HPAR(100)

	integer IX(100),JX(100)
	integer NSC(50),IM(50,100),JM(50,100)	!up to 50 cycles
	integer IE(200),JE(200),IF(200),JF(200)
	real EFAC(200)
	integer ieq(200),jeq(200),ifq(200),jfq(200)
	real efacq(200)
	integer IL(100)
	
	
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
		real X(N_STATES)
		real Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		character*15 statname(n_states)
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

	TYPE (MODEL) MODELS(25)
	
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
	
	type(rate_constant) ratcons(500)
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/switch/iswicon,igraphText(100),ibutton1,icongmod
nlink=0
do ind_m=1,100
	
	
		models(jgraph)%name_link(ind_m)=' '        
		do k=1,100
			models(jgraph)%link(k,ind_m)=0
		enddo
enddo
call gDefineRGB(48,0.,0.,0.)
	dxs0=dxs
	dys0=dys
	dxs=0.6
	dys=0.8
	nlp=len_trim(qmec)
	call gDefineRGB(50,1.,0.80,0.8)	!pink
    call gDefineRGB(51,0.8,1.0,0.8)	!green
	ired=50
	igreen=51
!	pfilem='hjcfit.txt'
!	if(.not.apfile) REWIND(unit=7)
!	DISCPRT=.TRUE.
!	open7=.true.
!	apfile=.true.
	if(mod_create.lt.0) then
		irecm=-mod_create
		mod_create=1
		i12=1
		imod=imodel
		mtype=1
	else
		i12=0
	endif    
	IF(MOD_CREATE.EQ.1.or.mod_create.eq.2) THEN ! old mechanism
		OPEN(unit=15,file=qmec,status='UNKNOWN',access='DIRECT', form='BINARY', RECL=1)
		read(unit=15,rec=1) iver,nrecs,nextrec,ireclast,jstart
		if(i12.eq.1) goto 12
		if(kf.eq.-1) then
		irecm=irecm0
		goto 12
		endif
		cnum=titlem(1:3)
		call chtoint(cnum,imod)
		
		
		do i=1,nrecs
			if(ijmod(i).eq.imod) then
				irecm=i
				goto 12
			endif
		enddo
12		irecq=irecm
		INDK=Irecm
		irec=jstart(irecq)	!start byte for data in record #irecq
		irecsav=irec
		irecqsav=irecq
		do i=1,25
		do j=1,40
			charmod(i,j)='  '
		enddo
		enddo
		do i=1,100
			models(jgraph)%x(i)=0.
			models(jgraph)%y(i)=0.
			models(jgraph)%name(i)=' '
			models(jgraph)%colour(i)=0
			models(jgraph)%statname(i)=' '
			models(jgraph)%dgamma(i)=0.0
			dgamma(i)=0.0
			irate(i)=0
			jrate(i)=0
			
		enddo
		read(unit=15,rec=irec) iver1
		if(iver1.eq.200) then
			read(unit=15,rec=irec) iver1,&
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
		(models(jgraph)%y(i),i=1,k),nxmodel,nymodel
		models(jgraph)%n=k
		imodel=imod
		kf=k-ka
		!call CQLAST(charmod,ilast2,jlast2)
		
		!call CHARQ(charmod,ilast2,jlast2,kA2,kF2,ncon2,ic2)
		do mc=1,k
			if(models(jgraph)%colour(mc).eq.12) models(jgraph)%colour(mc)=ired
			if(models(jgraph)%colour(mc).eq.2) models(jgraph)%colour(mc)=igreen
		enddo
		else if (iver1.eq.201) then
			read(unit=15,rec=irec) iver,&
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
      	nsub,kstat0,npar0,kcon,npar1,ncyc0,((ic(i,j),i=1,2),j=1,ncon)
     
		models(jgraph)%n=k
		imodel=imod
		kf=k-ka
		else
			read(unit=15,rec=irec) iver1,&
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
		models(jgraph)%n=k
		imodel=imod
		if(chardef) then
			call CQLAST(charmod,ilast,jlast)
		
			call CHARQ(charmod,ilast,jlast,kA,kF,ncon,ic)
		endif
		endif
		! exist some models -saved wrongly 102 with chardef =false
		if(iver1.ne.200.and.chardef) then
		nrow=ilast*2+4
		ncol=jlast*2+4
		nxmodel=ncol
		nymodel=nrow
		do jn=1,jlast
		do in=1,ilast
			ch1=charmod(in,jn)(1:1)
			if(ch1.eq.'O'.or.ch1.eq.'o'.or.ch1.eq.'C'.or.ch1.eq.'c'.or.&
			ch1.eq.'1'.or.ch1.eq.'2'.or.ch1.eq.'3'.or.ch1.eq.'4')then
				ch2=charmod(in,jn)(2:2)
				if(ch1.eq.'O'.or.ch1.eq.'o'.or.ch1.eq.'C'.or.ch1.eq.'c') then
					cnum=ch2
				else
					cnum=charmod(in,jn)
				endif
				call chtoint(cnum,istate)
				if(istate.eq.0) istate=10
				if(ch1.eq.'O'.or.ch1.eq.'o') then
				models(jgraph)%colour(ISTATE)=ired
				models(jgraph)%name(ISTATE)=ch2
				else if(ch1.eq.'C'.or.ch1.eq.'c') then
				models(jgraph)%colour(ISTATE)=igreen
				models(jgraph)%name(ISTATE)=ch2 
				else
					models(jgraph)%name(ISTATE)=charmod(in,jn)
					if(istate.le.ka) then
						models(jgraph)%colour(ISTATE)=ired
				
					else
						models(jgraph)%colour(ISTATE)=igreen
					endif
				endif
				if(ch2.eq.'0') models(jgraph)%name(ISTATE)='10'
        		
				
				models(jgraph)%X(ISTATE)=float(jn*2)
				models(jgraph)%Y(ISTATE)=nrow-float(in*2)
				
			endif
			imatrix(in,jn)=IVAL(charmod,in,jn)
			models(jgraph)%charmod(in,jn)=charmod(in,jn)
		enddo
		enddo
	
		nxmodel=ncol
		nymodel=nrow
		
	
	    
		endif
		models(jgraph)%ilast=ilast
		models(jgraph)%jlast=jlast
		if(ncyc.gt.0) then
			allmr=.true.
			do i=1,ncyc
				if(nsc(i).gt.0) obeymr=.true.
				if(.not.obeymr(i)) allmr=.false.
			enddo
		endif
		kl=1
		do j=1,ncon
			ic(1,j)=irate(kl)
			ic(2,j)=jrate(kl)

			models(jgraph)%ic(1,j)=ic(1,j)
			models(jgraph)%ic(2,j)=ic(2,j)
			k=ic(1,j)
			l=ic(2,j)
			models(jgraph)%link(k,l)=1
			models(jgraph)%link(l,k)=1
			kl=kl+2		
		enddo
		do i=1,nrateq
				ratcons(indk)%qt(irate(I),jrate(i))=qt(irate(i),jrate(i))
				irate1(i)=irate(i)
				jrate1(i)=jrate(i)
		enddo
		do m=1,npar
				call intconv(irate(m),cnum)
				call intconv(jrate(m),cnum1)
				ratcons(indk)%qij(m)='q('//cnum(1:2)//','//cnum1(1:2)//')'
		enddo
		if(indmod) then
			do i=1,k
			do j=1,k
			ratcons(indk)%qt(i,j)= qt(i,j)
			enddo
			enddo
		endif
		
		close(unit=15)

		if(kf.eq.-1) goto 10
		models(jgraph)%ncon=ncon
		models(jgraph)%npar=npar
		models(jgraph)%indmod=indmod
		models(jgraph)%model=imodel
		models(jgraph)%title_model=mtitle
		models(jgraph)%ka=ka
		do i=1,nlig
			models(jgraph)%ligname(i)=ligname(i)
		enddo
		ratcons(indk)%title=rtitle
		if(npar.gt.100) npar=100
		do k=1,npar
				ratcons(indk)%titlep(k)=titlep(k)
				ratcons(indk)%value(k)=qt(irate(k),jrate(k))
		enddo
	!	models(jgraph)%n=k
	    models(jgraph)%ka=ka
	    models(jgraph)%kb=kb
	    models(jgraph)%ncon=ncon
		models(jgraph)%nlig=nlig
	    models(jgraph)%chardef=chardef
	    models(jgraph)%title_model=mtitle
	    ratcons(indk)%title=rtitle
        models(jgraph)%vref=vref
		if(indmod) then
			do m=1,npar
				call intconv(m,cnum)
				call intconv(irate(m),cnum)
				call intconv(jrate(m),cnum1)
				ratcons(indk)%qij(m)='q('//cnum(1:2)//','//cnum1(1:2)//')'
				if(ncdep.gt.0) then
				do n=1,ncdep
					i=ix(n)
					j=jx(n)
					
					if(i.eq.irate(m).and.j.eq.jrate(m)) then
					il1=IL(n)
					ratcons(indk)%iconc(m)=il1
					ratcons(indk)%ligant(m)=ligname(il1)		!ligand number
				
					endif
				enddo
				endif
			enddo
			k=1
			do m=1,ncon
				ic(1,m)=irate(k)
				ic(2,m)=jrate(k)
				call intconv(ic(1,m),cnum)
				call intconv(ic(2,m),cnum1)
				ratcons(indk)%qij(k)='q('//cnum(1:2)//','//cnum1(1:2)//')'
				ratcons(indk)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1:2)//')'
				k=k+2	
			
			enddo
			

			else
		k=1
		do m=1,ncon
				ic(1,m)=irate(k)
				ic(2,m)=jrate(k)
				i=IC(1,m)
				j=IC(2,m)
				call intconv(ic(1,m),cnum)
				call intconv(ic(2,m),cnum1)
				ratcons(indk)%qij(k)='q('//cnum(1:2)//','//cnum1(1:2)//')'
				ratcons(indk)%qij(k+1)='q('//cnum1(1:2)//','//cnum(1:2)//')'
				do l=1,ncdep
					if(ix(l).eq.i.and.jx(l).eq.j) then
						ratcons(indk)%iconc(k)=il(l)
						ratcons(indk)%ligant(k)=ligname(il(l))
					endif
				enddo
					do l=1,ncdep
					if(ix(l).eq.j.and.jx(l).eq.i) then
						ratcons(indk)%iconc(k+1)=il(l)
						ratcons(indk)%ligant(k+1)=ligname(il(l))
					endif
				enddo
				k=k+2
			enddo
			endif
			nchan=1
			if(nsetq.gt.0) then
				ratcons(indk)%nsetq=nsetq
				do i=1,nsetq
					ratcons(indk)%ieq(i)=ieq(i)
					ratcons(indk)%jeq(i)=jeq(i)
					ratcons(indk)%ifq(i)=ifq(i)
					ratcons(indk)%jfq(i)=jfq(i)
					ratcons(indk)%efacq(i)=efacq(i)
				enddo
			endif
		if(ncyc.gt.0) then
		do i=1,ncyc	
			ratcons(indk)%nsc(i)=nsc(i)
		enddo
		
		do l=1,ncyc
		do m=1,nsc(l)
		
			ratcons(indk)%im(l,m)=im(l,m)
			ratcons(indk)%jm(l,m)=jm(l,m)
		enddo
		enddo
		
		endif
10	continue
	if(discprt) then 

if(autosim.and.ksim.ne.-1) then
   	write(7,2009)
2009 format(/,/,'INITIAL GUESSES for fit of rate constants')
else
	write(7,209)
209	format(/,/,' DEFINE THE REACTION MECHANISM')
endif
		write(7,210) qmec,nrecs
210		format(' Mechanisms file: ',a40,/,&
        ' contains ',i4,' records of rate constants +model')
		write(7,211) irecq,irec
211		format(/,' Record number ',i4,' (starts at byte #',i6,')')
		write(7,214)mtitle(1:74),imod
214   	format(1x,a74,/,' Model number = ',i3,/)
	    do i=1,ilast
 		   
		   if(chardef)write(7,671) (charmod(i,j),j=1,jlast)
!671		   format(4x,35a2)
		enddo
		write(7,73) nchan,kA
73		format(/,' Number of channels = ',i8,/,' Number of open states = ',i3)
	
		
	 models(jgraph)%nwidth=nxmodel
	 if(nymodel.lt.9) nymodel=9
	 models(jgraph)%nheight=nymodel
	
		do i=1,ilast
!			write(7,671) (charmod(i,j),j=1,jlast)
		enddo
	  !  call generate_charmod(imodel,models,ilast,jlast,1)
		do i=1,ilast
!			write(7,671) (models(jgraph)%charmod(i,j),j=1,jlast)
671			format(4x,35a2)
		enddo
			do i=1,	kstat
			nbl=len_trim(statname(i))
			if(nbl.lt.1) nbl=1
			do imj=nbl+1,10
			statname(i)(imj:imj)=' '
			enddo
			statext='('//statname(i)(1:nbl)//')'
			nbl=len_trim(statext)
			do imj=nbl+1,20
			statext(imj:imj)=' '
			enddo
			write(7,333) i,statext,1.0d12*dgamma(i)
!			write(7,321) i,statname(i),1.0d12*dgamma(i)
321			format('  conductance of state ',i3,'(',a10,') (pS) = ',g13.6)
333			format('  conductance of state ',i3,2x,a12,'(pS) = ',g13.6)
		enddo
		endif
		models(jgraph)%ilast=ilast
			models(jgraph)%jlast=jlast
		do in=1,ilast
		do jn=1,jlast
			models(jgraph)%charmod(in,jn)=charmod(in,jn)
		enddo
		enddo
		models(jgraph)%kcon=kcon
	    models(jgraph)%nsub=nsub
	    models(jgraph)%kstat0=kstat0
		models(jgraph)%kstat=kstat
		
	   if(indmod) then
			if(nsub.gt.0) then
				iyw=kstat+2*nsub+1
			else
				iyw=kstat+3
			endif
			indwin = gmCreateComplexDialogueBox(Main, 24, 6,8,iyw, GALL, 'Basic States ', &
              	gmIconFormat=GDLLICON,gmIconFile='MBIG1052',gmvpos=gtop)
			
			if(kcon.gt.0) then
				iconPanel=gmCreatePanel(indwin, 0, 0,8,kcon+1, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle='Concerted States',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
	    
				do i=1,kcon
					models(jgraph)%statname(i)=statname(i)
					models(jgraph)%con_states(i)=statname(i)
					icons(i) = gmCreateTextEntry(iconPanel,1, i, 6, 1,models(jgraph)%con_states(i), 60, Gedit, &
              		gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              		gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              		gmVpos=GTOP, gmExpand=GOFF)
				enddo
				
			endif
			
			if(nsub.gt.0) then
				
				do i=1,nsub
					isubPanel(i)=gmCreatePanel(indwin, 0, kcon+1+(kstat0+1)*(i-1),8,kstat0+1, &
              		gmHpos=GLEFT, gmVpos=Gtop, gmtitle='subunit',gmExpand=GOff, gmType=GNOBOUNDARY, &
              		gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
					do j=1,kstat0
						models(jgraph)%sub_states(i,j) = statname(kcon+j+kstat0*(i-1))
						isub(i,j)=gmCreateTextEntry(isubPanel(i),1,j,&
						6, 1,models(jgraph)%sub_states(i,j), 60, Gedit, &
						gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              			gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              			gmVpos=GTOP, gmExpand=GOFF)
				enddo
				enddo
				
            else
				if(kcon.gt.0) then 
					iyp=kcon+1
				else
					iyp=0
				endif
				i=1
				isubPanel(i)=gmCreatePanel(indwin, 0, iyp,8,kstat-kcon+1, &
              	gmHpos=GLEFT, gmVpos=Gtop, gmtitle='subunit',gmExpand=GOff, gmType=GNOBOUNDARY, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
				do j=1,kstat-kcon
				models(jgraph)%sub_states(i,j)=statname(j+kcon)
				isub(i,j) = gmCreateTextEntry(isubPanel(i),1, j, 6, 1,&
				models(jgraph)%sub_states(i,j), 60, Gedit, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
              	gmBack1Col=14, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmExpand=GOFF)

				enddo
			endif
			indcall=6505
			iqb=gmCreatePushButton(indwin,3,0, 2, 1,'OK',&
              	gmType=GSTANDARD, gmSize=80,gmTextCol=0, &
              	gmVpos=Gbottom, gmHpos=Gleft,gmExpand=GOFF,gmCallback=indcall)
			l=1+kcon
			do i=1,kstat0
				do j=1,kstat0
				cnum5=models(jgraph)%sub_states(1,i)
				cnum51=models(jgraph)%sub_states(2,j)
				n1=len_trim(cnum5)
				n2=len_trim(cnum51)
				n1=n1-1
				n2=n2-1
				if(nsub.gt.0) then
				models(jgraph)%statname(l)=cnum5(1:4)//'+'//cnum51(1:4)
				else
				models(jgraph)%statname(l)='         '
				endif
				l=l+1
				enddo
			enddo
	   endif
	if(indmod.and.ixm.ne.-100) call gmdrawwindow(indwin)
	ENDIF
	if(mod_create.lt.1.or.(mod_create.eq.1.and.(nxmodel.eq.0.or.nymodel.eq.0))) then
		if(models(jgraph)%indmod) then
			nxmodel= models(jgraph)%kstat0*4+4
			nymodel=models(jgraph)%kstat0*4+4
			if(models(jgraph)%kcon.gt.0) nymodel=nymodel+4
		else
			if(models(jgraph)%n.le.16) then
			    nxmodel=24
			
			    nrline=4
			    nymodel=ceiling(float(models(jgraph)%n/nrline))*4+8
			else if(models(jgraph)%n.gt.16.and.models(jgraph)%n.lt.64) then
			    xq=sqrt(float(models(jgraph)%n))
			    ixq=ceiling(xq)
			    nrline=ixq+1
			    nrcol=ceiling(float(models(jgraph)%n/nrline))
			    nxmodel=nrline*4+8
			    nymodel=nrcol*4+8
			else if(models(jgraph)%n.gt.64) then
			    nrline=10
			    nxmodel=48
			    nymodel=32
			    nrcol=int(models(jgraph)%n/nrline)
			endif
		endif
	endif
	if(mod_create.lt.1) mtitle=models(jgraph)%title_model
	if(mod_create.ne.2) then
		if(ipbar_Progress2.eq.-100) iswicon=-100
		call graph2(ixm,iym,jgraph,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		nxmodel,nymodel,mtype,models(jgraph)%title_model,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
		iswicon=0
		ipbar_Progress2=0
	endif
!	dxs=dxs0
!	dys=dys0
	
	models(jgraph)%nwidth=nxmodel
	models(jgraph)%nheight=nymodel
	models(jgraph)%title_model=mtitle
	models(jgraph)%ix=ixm
	models(jgraph)%iy=iym
	models(jgraph)%model=imodel
	if(mod_create.ge.1) then
		do ind_m=1,models(jgraph)%n
		nlink=0
		call intconv(ind_m,cnum)
		models(jgraph)%name_link(ind_m)=' '        
		do k=1,models(jgraph)%n
			if(k.ne.ind_m.and.models(jgraph)%link(k,ind_m).eq.1) then
			    if(mod_create.eq.1) then
				call lincol(46)
				call gmoveto2d(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m))
				call gdrawlineto2d(models(jgraph)%X(k),models(jgraph)%y(k))
				diff=models(jgraph)%y(ind_m)-models(jgraph)%y(k)
				if(diff.ne.0.0) then 
				a=(models(jgraph)%X(ind_m)-models(jgraph)%X(k))/&
				(models(jgraph)%y(ind_m)-models(jgraph)%y(k))
				else
				    a=2.
				endif
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(models(jgraph)%X(ind_m)-0.1,models(jgraph)%y(ind_m)+0.1)
				call gdrawlineto2d(models(jgraph)%X(k)-0.1,models(jgraph)%y(k)+0.1)
				else
				call gmoveto2d(models(jgraph)%X(ind_m)-0.1,models(jgraph)%y(ind_m)-0.1)
				call gdrawlineto2d(models(jgraph)%X(k)-0.1,models(jgraph)%y(k)-0.1)
				endif
				endif
				nlink=nlink+1
				models(jgraph)%inter_link(ind_m)=nlink
				call intconv(k,cnum)
				conex=models(jgraph)%name_link(ind_m)
				nc=len_trim(conex)
				models(jgraph)%name_link(ind_m)=conex(1:nc)//cnum(1:3)//';'
				
            endif
        enddo
		enddo
		if(mod_create.eq.1) then
		do ind_m=1,models(jgraph)%n
			models(jgraph)%index(ind_m)=ind_m
			CALL LINCOL(models(jgraph).colour(ind_m))
			call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),-3,1.2,1.2,13,idev)
			models(jgraph)%statname(ind_m)=statname(ind_m)
			
			call write_string(models(jgraph)%name(ind_m)(1:3),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)+0.4,0.,0,1,0.35,48,dxs,dys)
			if(.not.indmod) then
			do l=1,10
			
		!		if(models(jgraph)%statname(ind_m)(l:l).eq.'*') models(jgraph)%statname(ind_m)(l:l)='x'
			enddo
				
			call write_string(models(jgraph)%statname(ind_m)(1:6),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)
			models(jgraph)%dgamma(ind_m)=dgamma(ind_m)
			call write_string(models(jgraph)%statname(ind_m)(7:12),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
			models(jgraph)%dgamma(ind_m)=dgamma(ind_m)
			
			endif
		enddo
		else
		
		do ind_m=1,models(jgraph)%n
			models(jgraph)%index(ind_m)=ind_m
		enddo
		endif
	else
		do i=1,100
		!	models(jgraph)%statname(i)=' '
		enddo
		chardef=.false.
		models(jgraph)%chardef=.false.
		!models(jgraph)%n=ka+kf
		models(jgraph)%model=imodel
		ka=models(jgraph)%ka
	!	if(models(jgraph)%indmod.eq..true.) then
		do ind_m=1,models(jgraph)%kcon
			if(ind_m.le.ka) then
				models(jgraph)%colour(ind_m)=ired
				models(jgraph)%dgamma(ind_m)=50.
			else
				models(jgraph)%colour(ind_m)=igreen
				models(jgraph)%dgamma(ind_m)=dgamma(ind_m)
			endif
			call intconv(ind_m,cnum)
			models(jgraph)%name(ind_m)=cnum(1:2)
			CALL LINCOLs(models(jgraph)%colour(ind_m),idev)
			models(jgraph)%X(ind_m)=4*ind_m
			models(jgraph)%y(ind_m)=nymodel-4
			
			call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),-3,1.2,1.2,13,idev)
			call write_string(models(jgraph)%name(ind_m)(1:3),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)+0.4,0.,0,1,0.35,48,dxs,dys)
		!	statname(ind_m)=' '
			models(jgraph)%name_link(ind_m)=' '  
			models(jgraph)%inter_link(ind_m)=0
			dgamma(ind_m)=1.
			
			models(jgraph)%statname(ind_m)=models(jgraph)%con_states(ind_m)
			call write_string(models(jgraph)%statname(ind_m)(1:6),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)
			call write_string(models(jgraph)%statname(ind_m)(7:12),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
		
		enddo
	!	endif
		l=models(jgraph)%kcon+1
		if(models(jgraph)%indmod) then	
		
			do i=1,models(jgraph)%kstat0
			do j=1,models(jgraph)%kstat0
				cnum5=models(jgraph)%sub_states(1,i)
				cnum51=models(jgraph)%sub_states(2,j)
				
				n1=len_trim(cnum5)
				n2=len_trim(cnum51)
				do lig=1,models(jgraph)%nlig
				models(jgraph)%nbound(l,lig)=models(jgraph+models(jgraph)%nsub-1)%nbound(i,lig)+&
				models(jgraph+models(jgraph)%nsub)%nbound(j,lig)
				enddo
				models(jgraph)%statname(l)=' '

				if(models(jgraph)%nsub.gt.0) then
					models(jgraph)%statname(l)=cnum5(1:n1)//'-'//cnum51(1:n2)
				else
					models(jgraph)%statname(l)='         '
				endif
				l=l+1
			enddo
			enddo
		else
			do i=1,models(jgraph)%nsub
			do j=1,models(jgraph)%kstat0
				
				models(jgraph)%statname(l)=models(jgraph)%sub_states(i,j)
				call intconv(j,cnum)
				models(jgraph)%name(l)=cnum(1:3)
			
				l=l+1
			enddo
			enddo
		endif
		kcon=models(jgraph)%kcon
		nsub=models(jgraph)%nsub
		kstat0=models(jgraph)%kstat0
		if(models(jgraph)%indmod) then
			do l=models(jgraph)%kcon+1,models(jgraph)%n
				call intconv(l,cnum)
				models(jgraph)%name(l)=cnum(1:3)
			enddo
			do j=1,kstat0
			do i=1,kstat0-1
					models(jgraph)%link(i+kstat0*(j-1)+kcon,&
					i+kstat0*(j-1)+1+kcon)=1
					
					models(jgraph)%link(i+kstat0*(j-1)+1+kcon,&
					i+kstat0*(j-1)+kcon)=1
			enddo
			enddo
			do j=1,kstat0
			do i=1,kstat0-1
					models(jgraph)%link(1+kstat0*(i-1)+j-1+kcon,1+kstat0*i+j+kcon-1)=1
					models(jgraph)%link(1+kstat0*i+j-1+kcon,1+kstat0*(i-1)+j+kcon-1)=1
				
			enddo
			enddo	
		endif
		
		! simple:
		
		kcount=1
		
		if(models(jgraph)%indmod) then
		kcol=4
		krow=8
		
		kcountm=models(jgraph)%kstat0
		if(models(jgraph)%kcon.eq.0) krow=4
		if(kcountm.eq.0) kcountm=4
		else
			kcoli=nxmodel-4
			krowi=4
			kcounti=1
		endif
		
		if(models(jgraph)%n.gt.16.and..not.models(jgraph)%indmod) kcountm=8
		do ind_m=models(jgraph)%kcon+1,models(jgraph)%n
			if(ind_m.le.ka) then
				models(jgraph)%colour(ind_m)=ired

			else
				models(jgraph)%colour(ind_m)=igreen
			endif
			call intconv(ind_m,cnum)
			CALL LINCOLs(models(jgraph)%colour(ind_m),idev)
		    if(models(jgraph)%indmod) models(jgraph)%name(ind_m)=cnum(1:2)
		
			
			models(jgraph)%X(ind_m)=kcol
			models(jgraph)%y(ind_m)=nymodel-krow
			kcol=kcol+4
			kcount=kcount+1
			if(kcount.gt.kcountm) then
				kcount=1
				kcol=4
				krow=krow+4
			endif
			kcounti=kcounti+1
			if(.not.models(jgraph)%indmod) then
			models(jgraph)%X(ind_m)=kcoli
			models(jgraph)%y(ind_m)=krowi
			call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),-3,1.2,1.2,13,idev)
			call write_string(models(jgraph)%name(ind_m)(1:3),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)+0.4,0.,0,1,0.35,48,dxs,dys)
			!statname(ind_m)=' '
			models(jgraph)%name_link(ind_m)=' '  
			models(jgraph)%inter_link(ind_m)=0
			dgamma(ind_m)=0.
			models(jgraph)%dgamma(ind_m)=dgamma(ind_m)
		
			call write_string(models(jgraph)%statname(ind_m)(1:6),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)
			call write_string(models(jgraph)%statname(ind_m)(7:12),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
		
			endif
			kcoli=kcoli-4
			if(kcounti.gt.nrline) then
				kcounti=1
				kcoli=nxmodel-4
				krowi=krowi+4
			endif
		enddo
		do ind_m=1,models(jgraph)%ka
			models(jgraph)%dgamma(ind_m)=50.
		enddo
	if(models(jgraph)%indmod) then
		do ind_m=1+kcon,models(jgraph)%n
		nlink=0
		call intconv(ind_m,cnum)
		models(jgraph)%name_link(ind_m)=' '        
		do k=1+kcon,models(jgraph)%n
			if(k.ne.ind_m.and.models(jgraph)%link(k,ind_m).eq.1) then
				call lincol(46)
				call gmoveto2d(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m))
				call gdrawlineto2d(models(jgraph)%X(k),models(jgraph)%y(k))
				a=(models(jgraph)%X(ind_m)-models(jgraph)%X(k))/&
				(models(jgraph)%y(ind_m)-models(jgraph)%y(k))
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(models(jgraph)%X(ind_m)-0.1,models(jgraph)%y(ind_m)+0.1)
				call gdrawlineto2d(models(jgraph)%X(k)-0.1,models(jgraph)%y(k)+0.1)
				else
				call gmoveto2d(models(jgraph)%X(ind_m)-0.1,models(jgraph)%y(ind_m)-0.1)
				call gdrawlineto2d(models(jgraph)%X(k)-0.1,models(jgraph)%y(k)-0.1)
				endif
				nlink=nlink+1
				models(jgraph)%inter_link(ind_m)=nlink
				call intconv(k,cnum)
				conex=models(jgraph)%name_link(ind_m)
				nc=len_trim(conex)
				models(jgraph)%name_link(ind_m)=conex(1:nc)//cnum(1:3)//';'
				
            endif
        enddo
		enddo
		
		do ind_m=models(jgraph)%kcon+1,models(jgraph)%n
			do l=1,10
				if(models(jgraph)%statname(ind_m)(l:l).eq.char(45)) i1=l
			enddo
			CALL LINCOLs(models(jgraph)%colour(ind_m),idev)
			call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),-3,1.2,1.2,13,idev)
		call write_string(models(jgraph)%name(ind_m)(1:3),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)+0.4,0.,0,1,0.35,48,dxs,dys)
		
			if(i1.ne.0) then
			call write_string(models(jgraph)%statname(ind_m)(1:i1-1),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)	
			call write_string(models(jgraph)%statname(ind_m)(i1+1:i1+4),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
			else
			call write_string(models(jgraph)%statname(ind_m),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)	
			
			endif
		enddo	
		endif	
		indk=imodel	
		do ind_m=1,models(jgraph)%n
			models(jgraph)%index(ind_m)=ind_m
		enddo
		do ind_m=1,models(jgraph)%ka
			models(jgraph)%dgamma(ind_m)=50.
		enddo
	endif
		
	if(mod_create.ge.1) then
	    ncols=4
		nrows=models(jgraph)%n
		k=ka+kf
		isw=1
		ihcell=24
		
		iwcell(1)=110
		iwcell(2)=100
		iwcell(3)=70
		iwcell(4)=140
		iwcell(5)=110
		do i=6,10
		iwcell(i)=110
		enddo
		if(models(jgraph)%ligname(1).eq.'') models(jgraph)%ligname(1)='lig 1'
		if(models(jgraph)%ligname(2).eq.'') models(jgraph)%ligname(2)='lig 2'
		ligname(1)=models(jgraph)%ligname(1)
		ligname(2)=models(jgraph)%ligname(2)
		nl1=len_trim(models(jgraph)%ligname(1))
		nl2=len_trim(models(jgraph)%ligname(2))
		if(models(jgraph)%nlig.eq.1) then
			ncols=5
			xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
			char(124)//'Link to'//char(124)//models(jgraph)%ligname(1)(1:nl1)
		else if(models(jgraph)%nlig.eq.2) then
			xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
			char(124)//'Link to'//char(124)//models(jgraph)%ligname(1)(1:nl1)//&
			char(124)//models(jgraph)%ligname(2)(1:nl2)
			ncols=6
		else
			xtext='State name '//char(124)//'Conductance'//char(124)//'Nr links'//&
		char(124)//'Link to'
	
		endif
		do i=1,nlig
		
			do j=1,nrows
				models(jgraph)%nbound(j,i)=nbound(j,i)
			enddo
		enddo
	!	allocate(text(ncols,nrows))
		DO I=1,NROWS
			models(jgraph)%dgamma(i)=1.0d12*dgamma(i)
			if(.not.indmod) then
			models(jgraph)%statname(i)=statname(i)
				
			endif
		ENDDO
		if(mod_create.eq.1) then
		if(indmod) then
			l=1+kcon
				do i=1,kstat0
				do j=1,kstat0
				cnum5=models(jgraph)%sub_states(1,i)
				cnum51=models(jgraph)%sub_states(2,j)
				
				n1=len_trim(cnum5)
				n2=len_trim(cnum51)
			
				models(jgraph)%statname(l)=' '
				if(nsub.gt.0) then
					models(jgraph)%statname(l)=cnum5(1:n1)//'-'//cnum51(1:n2)
				else
					models(jgraph)%statname(l)='         '
				endif
				l=l+1
				enddo
			enddo
			if(nsub.eq.0) then
			do l=1+kcon,models(jgraph)%n
			models(jgraph)%statname(l)='         '
			enddo
			endif
			if(nsub.gt.0) then
			do ind_m=1,models(jgraph)%n
			do l=1,10
		
			if(models(jgraph)%statname(ind_m)(l:l).eq.char(45)) then
			i1=l
			endif
			enddo
			if(i1.ne.0) then
			call write_string(models(jgraph)%statname(ind_m)(1:i1-1),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)	
			call write_string(models(jgraph)%statname(ind_m)(i1+1:i1+4),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
			else
			call write_string(models(jgraph)%statname(ind_m)(1:6),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)	
			endif		
			enddo

       ! models(jgraph)%statname(4)='PORTO-CALIE'
			endif
	   endif
			!call gmsetprogressvalue(ipbar_Progress2,20)
		if(ixm.ne.-100) then
		
		call text_array(jgraph,Main,form1,Form1_TxtArray1,ncols,nrows,iwcell,ihcell,xtext,1,&
       models(jgraph)%name_link,models(jgraph)%inter_link,models(jgraph)%statname,models(jgraph)%dgamma,&
	   models(jgraph)%title_model,titlep,models(jgraph)%nbound,&
	   models(jgraph)%ligname,models(jgraph)%nlig,ratcons(indk)%micro,ratcons(indk)%ligant,&
	   jfix,theta0,iformText)
	   !	imi=gmdisplaymessagebox(' ','Display rates table',gexclamation,gstop)
		endif
	endif
	endif

   
	continue
	end
