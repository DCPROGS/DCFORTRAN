	subroutine read_model(Main,Records,List7_1,
     & Button7,TEXT7,INDX,qmec,ijmod,rtitles,nrecs,gititle,ifiltype)

	use menu_f90

	character*2 charmod(25,40)	!to print model
	
	character*8 titles(100)
	logical nopath
	character qpath*30,qname*8,suffix*3,ndev*2,cnum*11,cnum0*11
	character rfile*40,qfilem*40,qfilmsav*40,qmec*60
	
	integer jstart(500),nrate(500)
	character*74 rtitle,rtitles(500),ratetitle(20,500) ! model->rate
	CHARACTER*80 text7(500),message,gititle	!title to describe rate constants
	
	logical readini,useini,present,repeat
	character*1 ans,UC,ans1,ch1,ch2
	character*74 mtitle	!title for model
	character*74 mtits(100)	!to read all ditto from disc
	integer ijmod(500),iwid(2)
	logical discprt
	common/dp/discprt

	character*60 :: qdir='.'
	character*120 :: qfilt='*.mec'//char(124)//'Mechanism Files(MEC)'//
     &	char(124)//'*.dat'//char(124)//'Old Files (DAT)'//
     &	char(124)//'*.*'//char(124)//'All Files'
	integer::Button7(10),RECORDS
	character*15 label7(2)

	logical obeymr(50)

	character*74 mtitles	!title for sim model
	character*2 charmods(25,40)	!to print model

	character*40 mtitle1*40,filnam*32,prtport*4	!for WINPRINT

	common/sm/nsmax		!for getqd/charq

	CALL gmFileBROWSER(qFILEm,qDIR,qFILT,gmBrowseType=0,
     &gmtitle=gititle)
	qfilmsav=qfilem
	newrate=.false.
	INQUIRE(file=qfilem,exist=present)
	if(present) then
		call PARSNAME(qfilem,qpath,ndev,qname,suffix,nopath,40)

		OPEN(unit=15,file=qfilem,status='UNKNOWN',
     & 	  access='DIRECT', form='BINARY', RECL=1)

		read(unit=15,rec=1) iver
		if(iver.eq.101) then
			call CONVQM(qfilem)		!converts v101 to v102
			OPEN(unit=15,file=qfilem,status='UNKNOWN',
     &		access='DIRECT',form='BINARY',RECL=1)	!re-open after convqm
		endif

		read(unit=15,rec=1) iver,nrecs,nextrec,ireclast,jstart
		imodmax=0
		do irq=1,nrecs
		irec=jstart(irq)	!previous nextrec
		read(unit=15,rec=irec) iver,imod,mtitle,k,kA,kB,kC,kD,rtitle
	if(imod.lt.1) then 
				imod=1
			    write(unit=15,rec=irec) iver,imod	
			endif
			if(imod.gt.imodmax) imodmax=imod
			mtits(imod)=mtitle	!so mtits(i)=title of model #i
			ijmod(irq)=imod
			rtitles(irq)=rtitle
	   
		enddo
		nmods=0
		do irq=1,nrecs
			imod=ijmod(irq)	!just defined
		!    nrate(imod)=1
		!	ratetitle(nrate(imod),imod)=rtitles(j)
			repeat=.false.
			if(irq.gt.1) then	!has imod occurred already?
			do j=1,irq-1
				if(ijmod(j).eq.imod) repeat=.true.	!imod already occurred
				if(nrate(imod).lt.20) then
				nrate(imod)=nrate(imod)+1
				ratetitle(nrate(imod),imod)=rtitles(j)
				endif
			enddo
			endif
			if(.not.repeat) nmods=nmods+1
		enddo
	
		irec=ireclast
		indx=0	
		do imd=1,imodmax  !list all the model titles (and details when iread=5)
			present=.false.
			do j=1,nrecs
			if(imd.eq.ijmod(j)) then
				present=.true.     	!model #im is present
			endif
			enddo
			if(present) then
				indx=indx+1
				CALL INTCONV(IMD,CNUM0)
				text7(indx)=cnum0(1:3)//MTITS(iMD)
			endif
		enddo
		
		ifiltype=10
		ncol=2
		label7(1)='Model'
		label7(2)='Title'
		iwid(1)=80
		iwid(2)=240
		call list_of_records(Main,Records,List7_1,ncol,indx,label7,text7,
     &	iwid,ifiltype,Button7,message)
		close(unit=15)
	endif
	nb=len_trim(qdir)
	qmec=qdir(1:nb)//'\'//qfilem
	end