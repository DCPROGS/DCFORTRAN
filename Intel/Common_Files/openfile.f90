subroutine openfile(MAIN,RECORDS,nplot,IFILTYPE,iplotype,iptype,DFILE,FOPEN_11,FOPEN_12,APFILE,DISCPRT,njset,&
			ncolr,nrowr,label,ftitle,nset,iwid,List7_1,ftitle1,button7,button6,&
			saveandplot,program_type)

!ifiltype=	1	cvdat
!			2	plotq  iplotype=1 graph, =2 histo =3sc, =4 3d
!			3   ascii
!			4	cjd
!			5	scan
!ifiltype=	6	scan
!			7	axon
!			8	consam
!			9
!			10   mechanism file


USE DFLIB
use gino_f90
use menu_f90

TYPE (FILE$INFO) info
integer :: Main
integer :: Records,button7(10),program_type
integer :: Data_list
integer :: DataMainPanel
integer*2 irecfst2(250)		!=500 bytes for CJUMP
	integer*4 irecfst(1000)		!now use int*4 version
integer :: Static6,button6(10)
integer :: Static7(3)
type(GARRAYCELL) arrayattribs
integer :: ValArray6(50,50)
character*64 title1
real*4 xdata(:,:)
allocatable xdata
character*4 filetype
CHARACTER*80 MESSAGE
CHARACTER*22 CNUM(10),CNUM1
character*60 :: dfile
LOGICAL PRESENT,FOPEN_11,FOPEN_12,dcfile,apfile,discprt,saveandplot,newfile,newform,cjump
character*18 ascnum
CHARACTER STRING(:),ascfil*33,stringf(80),pathdat*60
allocatable::string
INTEGER*4 jstrec(100),lstrec(100),jstrec1(100),lstrec1(100)
integer*2 jstrec2(200),lstrec2(200),ititw(5),nset1
CHARACTER*60  TITLEF,TITLED(20)
character*70 text
CHARACTER*40 TITLEDS(10)
real*4 Xnum(:,:)
allocatable::Xnum
ALLOCATABLE::colhead
character*20 colhead(:)
CHARACTER*60 FTITLE(200),pfile
CHARACTER*100 FTITLE1(200),jtitle(200)
CHARACTER*12 FACCESS,FBINARY,FFORM,FRTYPE,FACTION
character*75 title3
integer :: iwid(10)
character*15 label(10)
ALLOCATABLE::nj,njbase
integer*4 nj(:),njbase(:)
type (GACTION) :: actlist
character cDATEW2*11,ctimew2*11
character title2*79
logical vjump,sampv,control
common/jumps/cjump,irecfst
allocate(nj(njset),njbase(njset))

	IRECL=1024
	if(cjump) irecl=512
	if(fopen_12) then
		close(unit=12)
		fopen_12=.false.
    endif
	if(fopen_11) then
		close(unit=11)
		fopen_11=.false.
    endif
	INQUIRE (FILE=DFILE,EXIST=PRESENT,&
         ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 
    if(PRESENT) then
	ihandle=FILE$FIRST
	length = GETFILEINFOQQ(dfile, info, ihandle)
	nLEN=info%length 
	
    OPEN(UNIT=11,FILE=DFILE,STATUS='UNKNOWN',&
         ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		 INQUIRE(IOLENGTH=LEN) DFILE
    
    
	READ(11,REC=1,IOSTAT=I_VAR) NPLOT,JSTREC,LSTREC,IVER 
	READ(11,REC=1,IOSTAT=I_VAR) NPLOT,JSTREC2,LSTREC2,IVER            
    IF(I_VAR.NE.0) THEN
        CLOSE(UNIT=11)
		if(.not.saveandplot) Icall=gmDisplayMessageBox('Stop ','Not a proper data file',&
		GEXCLAMATION,GOK)
        
	    IFILTYPE=0
    ELSE
		if(cjump) then
			read(11,rec=1) njd
			newform=.false.	!old format file (512 byte records)
			if(njd.lt.1) then
			newform=.true.	!new format 'transparent' file
			CLOSE(unit=11)
			OPEN(unit=11,file=DFILE,status='UNKNOWN', &
     		access='DIRECT', form='BINARY', RECL=1)
			
			read(11,rec=1) jver,njd,nxtrec,ioffset,irecfst
			else
				read(11,rec=1) njd,nxtrec,irecfst2
			endif
			nplot=njd	!name used in this prog
			iptype=11		!use vplot
			if(.not.newform) then
				do i=1,njd
				irec=int4(irecfst2(i))
				if(irec.le.-1.and.irec.ge.-32768) then
					irec=irec+65536
				else if(irec.lt.-32768) then
					imes=gmdisplaymessagebox('',' IREC IS TOO BIG!!',gstop,gok)
					goto 99
				endif
				irecfst(i)=irec
				enddo
			endif
			ifiltype=4
			goto 33 
		endif
!		if(discprt) write(7,101) dfile
101			  format(' file:',40a,/)
		fopen_11=.true.
        CALL INTCONV(IVER,CNUM(1))
	    CALL INTCONV(NPLOT,CNUM(2))
        CALL INTCONV(JSTREC(1),CNUM(3))
	    IF (IVER.EQ.1001.OR.IVER.EQ.1100.OR.IVER.EQ.1200) THEN
		  CLOSE(unit=11)		!then reopen as transparent
          OPEN(unit=11,file=DFILE,status='UNKNOWN', &
     	  access='DIRECT', form='BINARY', RECL=1)
          ISTREC=((JSTREC(1))-1)*1024 + 1
		  ISTREC2=((JSTREC2(1))-1)*1024 + 1	
	      READ(11,REC=ISTREC,IOSTAT=I_VAR) IPTYPE
		  READ(11,REC=ISTREC2,IOSTAT=I_VAR) IPTYPE2
          IF(IPTYPE.LT.-302.AND.IPTYPE.GT.302) THEN
             IF(IVER.EQ.1001) THEN
                READ(11,REC=ISTREC) (TITLED(K),K=1,10)
				ns=len_trim(dfile)
                MESSAGE=DFILE(1:ns)//'Cvdat File : Version= ' &
     			//CNUM(1)(1:5)//'Contains   : '//CNUM(2)(1:4)//' File(s)'
				
              ELSE IF (IVER.EQ.1100) THEN
                READ(11,REC=ISTREC) NSET,TITLEF
                CALL INTCONV(NSET,CNUM(3))
				ns=len_trim(dfile)
                MESSAGE=DFILE(1:ns)//': Cvdat File,Version= '// &
     			CNUM(1)(1:5)//'Contains   : '//CNUM(2)(1:4)//' File(s)'
			
              ENDIF
			  IFILTYPE=1
			  if(discprt) write(7,100) message
100			  format(' type:',30a,/)
		!	  if(.not.saveandplot)Icall= gmDisplayMessageBox('File',MESSAGE,GINFORMATION,GOK)
          ELSE
              CALL INTCONV(IVER,CNUM(5))
			  ns=len_trim(dfile)
	          MESSAGE=DFILE(1:ns)//': Plotq File, Version= '// &
     	      CNUM(1)(1:5)//'Contains   : '//CNUM(2)(1:4)//' Plot(s)'
			  
		      IFILTYPE=2
			   if(discprt) write(7,100) message
		!	  if(.not.saveandplot)Icall= gmDisplayMessageBox('File',MESSAGE,GINFORMATION,GOK)
          ENDIF
        ELSE IF(IVER.EQ.1002) THEN
	      IRECL0=1024
		  CLOSE(unit=11)		!then reopen as transparent
          OPEN(unit=11,file=DFILE,status='UNKNOWN',&
     	  access='DIRECT', form='BINARY', RECL=1)
          ISTREC=((JSTREC(1))-1)*1024 + 1

          READ(11,REC=ISTREC,IOSTAT=I_VAR) TITLEF,TITLEDS
		  ns=len_trim(dfile)
          MESSAGE=DFILE(1:ns)//': Cvdat File, Version= '//&
          CNUM(1)(1:5)//'Contains   : '//CNUM(2)(1:4)//' File(s)'
	      IFILTYPE=1
		   if(discprt) write(7,100) message
		!  if(.not.saveandplot)Icall= gmDisplayMessageBox('File',MESSAGE,GINFORMATION,GOK)
        ELSE IF(IVER.EQ.1003) THEN
		
	      CLOSE(unit=11)		!then reopen as transparent
          OPEN(unit=11,file=DFILE,status='UNKNOWN',&
     	  access='DIRECT', form='BINARY', RECL=1)
		  ilen=10000 
          ISTREC=(JSTREC(1))
			iplotype=1
	      READ(11,REC=ISTREC,IOSTAT=I_VAR) NSET,TITLEF
          CALL INTCONV(NSET,CNUM(3))
		  ns=len_trim(dfile)
          MESSAGE=DFILE(1:ns)//': Cvdat File, Version= '// &
     	  CNUM(1)(1:5)//'Contains   : '//CNUM(2)(1:4)//' File(s)'
	      IFILTYPE=1
		   if(discprt) write(7,100) message
	!	  if(.not.saveandplot) Icall= gmDisplayMessageBox('File',MESSAGE,GINFORMATION,GOK)
        ELSE IF(IVER.GT.1003.AND.IVER.LT.1100) THEN
	      CLOSE(unit=11)		!then reopen as transparent
          OPEN(unit=11,file=DFILE,status='UNKNOWN',&
           access='DIRECT', form='BINARY',RECL=1)
          ISTREC=(JSTREC(1))

	      READ(11,REC=ISTREC,IOSTAT=I_VAR) NSET
          CALL INTCONV(NSET,CNUM(3))
		  ns=len_trim(dfile)
          MESSAGE=DFILE(1:ns)//': Cvdat File, Version= '//&
          CNUM(1)(1:5)//'Contains   : '//CNUM(2)(1:4)//' File(s)'
	      IFILTYPE=1
		   if(discprt) write(7,100) message
		!  if(.not.saveandplot)Icall= gmDisplayMessageBox('File',MESSAGE,GINFORMATION,GOK)
        ELSE
		  CLOSE(UNIT=11)
		  ihandle=FILE$FIRST
		  length = GETFILEINFOQQ(dfile, info, ihandle)
          OPEN(unit=11,file=DFILE,status='UNKNOWN',&
           access='DIRECT', form='BINARY',RECL=1)
		  INQUIRE(IOLENGTH=JLEN) DFILE
		  iplen = FULLPATHQQ(dfile, pathdat)
       
   
	      if(allocated(string)) deallocate(string)
		  ILEN=info%length
	      allocate(string(1:Ilen))
	     
		  read(11,REC=1) string(1:8)
	     
	      do i=1,8
	        ascnum(i:i)=string(i)
	      enddo
	      IF(ascnum(1:8).eq.'dcformat') then
		   if(discprt) write(7,102)
102			format(' ascii file -dcformat',/)
			close(unit=11)
	        IFILTYPE=3
			ascfil=dfile(1:33)
	        iver=1003	!otherwise not defined
	        ascinput=.true.
	        ioff=-1
	        ilinhead=-1

	        ncols=3*njset	!safe size for input to ascread1
	        if(allocated(colhead)) deallocate(colhead)
	        ALLOCATE(colhead(ncols))
	        call ASCREAD1(ioff,ilinhead,ncols,nrows,ilen,&
            ascfil,colhead,text,lentext,dcfile)
	        nd1=nrows
	        nd2=ncols
	        if(allocated(xnum)) deallocate(xnum)

	        ALLOCATE(xnum(nd1,nd2))
			if(allocated(xdata)) deallocate(xdata)

	        ALLOCATE(xdata(nd2,nd1))
	        call ASCREAD2(ioff,ncols,nrows,ilen,&
      	    ascfil,xnum,nd1,nd2)
	        nc1=ncols
	        if(dcfile) then
		    titlef=text(1:60)
	        else
		    do i=1,60
		      titlef(i:i)=' '
		    enddo
	        endif
	        nset=0
	   		setsd=.false.
	        if(ncols.eq.1) then
				nset=1
				ncset=1
	        else
				if(mod(ncols,2).eq.0) ncset=2		!default
				if(mod(ncols,3).eq.0) ncset=3		!default
				nset=ncols/ncset				!default
	        endif
	   		FTITLE(1)=TITLEF

	   		call intconv(nrows,cnum(4))
	   		call intconv(ncols,cnum(5))
	   		FTITLE(2)=cnum(4)(1:4)//' rows of data in '//cnum(5)(1:4)//&
       		' columns'
	   		CNUM(1)='          '
	   		CNUM(2)='          '
	   		CNUM(3)='          '
			DO I=1,NROWS
             CALL REALTOCH(XNUM(I,1),CNUM(1),22)
	         IF (NCOLS.GT.1) CALL REALTOCH(XNUM(I,2),CNUM(2),22)
	         IF (NCOLS.GT.2) CALL REALTOCH(XNUM(I,3),CNUM(3),22)
	         FTITLE(I+2)=CNUM(1)(1:20)//CNUM(2)(1:20)//CNUM(3)(1:20)
			ENDDO
			nplot=NROWS+2
			do i=1,nplot
!              CALL LISSET(ILISTA1,I,ftitle(i),1)
			enddo
			do i=1,nd2
				do j=1,nd1
					xdata(i,j)=xnum(j,i)
				enddo
			enddo
			jin=1
			iplot=1
!			call values_list(Main,Data_list,ValArray6,jin,iplot,ncols,nrows,xdata,ftitle(1),button6,&
!			nd1,1,1,iDataMainPanel)		
		  ELSE
			read(11,rec=1) iscan
			if(iscan.eq.103.or.iscan.eq.-103) then
			!	icall=gmDisplayMessageBox('','This is a Scan.scn file',GEXCLAMATION,GOK)
				ifiltype=5
				close(unit=11)
			else
				read(11,rec=1) name,idiscn,nfirst,nfiles,jstrec,nextrec
				if(idiscn.ne.0) then
					ifiltype=6
					close(unit=11)
				else
					if(program_type.le.4) then
						MESSAGE='Not a Plotq/Cvdat/Scan File'
						IFILTYPE=0
						CLOSE(UNIT=11)
						if(.not.saveandplot)Icall= gmDisplayMessageBox('Stop',MESSAGE,GEXCLAMATION,GOK)
					else
						read(11,rec=1) filetype
						if(filetype(1:3).eq.'ABF') then
							ifiltype=7 ! axon
						else
							READ(11,REC=1,IOSTAT=I_VAR) iver
							if(iver.eq.1002) then
								ifiltype=8 ! consam		
				
							else
								read(11,rec=1) title1,cDATEW,adctimew,idt,ioff,ilen1
								if(ilen1.eq.nlen-512) then
									ifiltype=8
								else
									Icall= gmDisplayMessageBox('Stop','Not a consam type file',GEXCLAMATION,GOK)
								endif
							endif
						endif
						close(unit=11)
					endif				
				
				endif
			endif ! end scan
		  ENDIF !
		  DEALLOCATE(STRING)
        ENDIF
33	    nlp=len_trim(dfile)
	    
	    nsetbase=nset
		if(ifiltype.eq.1) then
		
			READ(11,REC=1) NPLOT,JSTREC1,LSTREC1,IVER
			do i=1,nplot
				CALL INTCONV(I,CNUM(4))
				if(iver.lt.1003) then
					irec=(jstrec1(i)-1)*1024+1
					read(11,rec=irec) (titled(k),k=1,10),nset
					if(nset.ge.1.and.nset.le.10) then
						iver=1001		!old
					else
						iver=1002		!new
					endif
				endif
				if(iver.eq.1001) then
					read(11,rec=irec) (titled(K),K=1,10),nset
					CALL INTCONV(NSET,CNUM(5))
					FTITLE(I)=CNUM(4)(1:3)//'|'//CNUM(5)(1:3)
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'           '//CNUM(5)(1:3)
					else
					FTITLE1(I)=CNUM(4)(1:10)//'          '//CNUM(5)(1:3)
					endif
					ncolr=2
					label(1)='Record'
					label(2)='Sets'
					label(3)='Title'
				else if(iver.eq.1002) then
					read(11,rec=irec) titlef,(titleds(j),j=1,10),nset,(nj(k),&
     				k=1,10)
					do k=1,10
						njbase(k)=nj(k)
					enddo
					CALL INTCONV(NSET,CNUM(5))
					call SWAP(titled,titleds,nset,njset,1)  !copy titleds into titled
				
					FTITLE(I)=CNUM(4)(1:3)//'|'//CNUM(5)(1:3)//'|'//titlef
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'           '//CNUM(5)(1:10)&
					//'         '//titlef
					else
					FTITLE1(I)=CNUM(4)(1:10)//'          '//CNUM(5)(1:10)&
					//'         '//titlef
					endif
					ncolr=3
					label(1)='Record'
					label(2)='Sets'
					label(3)='Title'
				else if(iver.ge.1003) then
					READ(11,REC=1) NPLOT,JSTREC,LSTREC	!*4
					READ(11,REC=1) NPLOT,JSTREC2
					irec2=jstrec2(i)	!*2
					!read(11,rec=irec2) nset1
					
					
					irec=jstrec(i)
				
					read(11,rec=irec) nset
					read(11,rec=irec) nset,titlef,(titled(j),j=1,nset)
					ntot=0
					do j=1,nset
						ntot=ntot+nj(j)
					enddo
					CALL INTCONV(NSET,CNUM(5))
					FTITLE(I)=CNUM(4)(1:3)//'|'//CNUM(5)(1:3)//'|'//titlef
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'           '//CNUM(5)(1:10)//&
					'         '//titlef
					else
					FTITLE1(I)=CNUM(4)(1:10)//'           '//CNUM(5)(1:10)&
					//'         '//titlef
					endif
					ncolr=3
					label(1)='Record'
					label(2)='Sets'
					label(3)='Title'
				endif
			enddo   !end of loop to print titles
			iwid(1)=80
			iwid(2)=80
			iwid(3)=240
			nrowr=nplot
	!		call record_list(Main,Records,ncolr,nrowr,label,ftitle,iwid)
			ititi=1
			if(.not.saveandplot) then
			if(program_type.eq.-1) message='cvdat for popen'
		    call list_of_records(Main,Records,List7_1,ncolr,nrowr,label,ftitle1,iwid,&
			ifiltype,button7,message)
			endif
		else if(ifiltype.eq.2) then
			CLOSE(unit=11)		!then reopen as transparent
			OPEN(unit=11,file=DFILE,status='UNKNOWN',&
			access='DIRECT', form='BINARY',RECL=1)
			READ(11,REC=1) NPLOT,JSTREC2
			do i=1,nplot
				CALL INTCONV(I,CNUM(4))
				istrec=int4(jstrec2(i))	!1st record no for plot # iPLOT
				istrec=(int4(jstrec2(i))-1)*1024+1
				read(11,rec=istrec) iptype
				krn=istrec
				if(iptype.ge.15.and.iptype.le.25) then
				if(iptype.eq.2) then
					read(11,rec=istrec) iptype,ititi,title1
				else if(iptype.eq.21) then
					read(11,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,ititi,title1
				else if(iptype.eq.22.or.iptype.eq.23) then
					read(11,rec=krn) iptype,ndimd,ndv1,ndimc,ndc1,kmax,ititi,title1
				else if(iptype.eq.1) then
					read(11,rec=krn) iptype,ititi,title1(1:44)
				else if(iptype.eq.11.or.iptype.eq.12) then
					read(11,rec=krn) iptype,ititi,title1(1:44)
				else if(iptype.eq.14) then
					read(11,rec=krn) iptype,ndv2,ndimd2,ndc2,ndimc2,ititi,title1(1:44)
				else if(iptype.eq.15.or.iptype.eq.16) then
					 read(11,rec=istrec) iptype,ndv2,ndimd2,ndc2,ndimc2,&
     				kwi,kwj,kmax,ititi,title1(1:44)	
				else
     				read(11,rec=krn) iptype,itit,title1(1:44)	
				endif
				else if(iptype.eq.40.or.iptype.eq.41.or.iptype.eq.42) then
				    read(11,rec=istrec)iptype,title3
				else
					read(11,rec=istrec) iptype,ititi,title1
				endif
				iptype=iabs(iptype)
				if(iptype.eq.1.or.(iptype.ge.11.and.iptype.le.16)) then
					FTITLE(I)=CNUM(4)(1:3)//'|graph|'//title1
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'             graph                    -'//title1
					else
					FTITLE1(I)=CNUM(4)(1:10)//'            graph                    -'//title1
					endif
					iplotype=1
				else if(iptype.eq.2.or.iptype.eq.21.or.iptype.eq.22.or.iptype.eq.23) then
					FTITLE(I)=CNUM(4)(1:3)//'|histogram|'//title1
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'             histogram            -'//title1
					else
					FTITLE1(I)=CNUM(4)(1:10)//'            histogram            -'//title1
					endif
					iplotype=2
				else if(iptype.eq.3.or.iptype.eq.31.or.iptype.eq.32.or.&
					iptype.eq.33.or.j.eq.30.or.j.eq.301.or.j.eq.302) then
					FTITLE(I)=CNUM(4)(1:3)//'|single channel|'//title1
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'             single channel  -'//title1
					else
					FTITLE1(I)=CNUM(4)(1:10)//'            single channel  -'//title1
					endif
					iplotype=3
				else if(iptype.eq.40.or.iptype.eq.41.or.iptype.eq.42) then ! 3d plot	
					FTITLE(I)=CNUM(4)(1:3)//'|3D plot|'//title3
					if(i.lt.10) then
					FTITLE1(I)=CNUM(4)(1:10)//'            3D plot                -'//title3
					else
					FTITLE1(I)=CNUM(4)(1:10)//'            3D plot                -'//title3
					endif
					iplotype=4
				endif
			enddo
            fopen_11=.true.
		    label(1)='Plot'
			label(2)='Type'
			label(3)='Title'
			iwid(1)=80
			iwid(2)=80
			iwid(3)=240
			ncolr=3
			nrowr=nplot
		
		!	call record_list(Main,Records,ncolr,nrowr,label,ftitle,iwid) 
		if(.not.saveandplot) then
			 call list_of_records(Main,Records,List7_1,ncolr,nrowr,label,ftitle1,iwid,&
			 ifiltype,button7,message)
		endif
		else if(ifiltype.eq.3) then
		else if(ifiltype.eq.4) then
			do i=1,nplot
				CALL INTCONV(I,CNUM(4))
				istrec=irecfst(i)
				read(11,rec=istrec) cDATEW2,ctimew2,title2,naver,navc,iav,&
      			control,vjump,sampv,nsamp
				
				if(i.lt.10) then
					jTITLE(I)=CNUM(4)(1:10)//'             cjump                    -'//title2(1:40)
				else
					jTITLE(I)=CNUM(4)(1:10)//'            cjump                    -'//title2(1:40)
				endif
					
			enddo
			fopen_11=.true.
		    label(1)='Plot'
			label(2)='Type'
			label(3)='Title'
			iwid(1)=80
			iwid(2)=80
			iwid(3)=240
			ncolr=3
			nrowr=nplot
			
			if(.not.saveandplot) then
			 call list_of_records(Main,Records,List7_1,ncolr,nrowr,label,jtitle,iwid,&
			 ifiltype,button7,message)
			endif
		endif
endif
else
    if(program_type.eq.1) then
		ifiltype=1
    else
		Icall= gmDisplayMessageBox('Stop','The file does not exist',GEXCLAMATION,GOK)
	endif
endif
99	end         
      
		
