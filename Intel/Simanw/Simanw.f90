Program SIMANW

   
   USE IFCORE

	USE IFPORT
	use gino_f90
	use menu_f90
    use hjcrecords
	
	include '\intel\common_files\graphics_definitions.f90'
	
	TYPE (FILE$INFO) info
	type (GACTION) :: actlst
	type (Gdim) :: dim
	type (Gwidget) :: widget
	TYPE (RECORD_ATTRIBUTES) newrecords(:)
	real*8 thtrue(:),thtrsav(200),thsav(200)
	real*8 stpfac,confac,errfac,ec50,tresdum
	real*4 conc(10,10)!,weight,w
	real*8 ec50out,penfunc
	logical fixec50
	integer nfileb(10)
	logical abortw,present,readini,allocated
	character*1 ans,ans1
	character*11 cdatew,ctimew
	character*10 titlep(200)
	character*40 simfile1,simfile2	!file names for output of simulation
	character*60 simfiles(10)
	real*8 thetval,elmval,elmset
	allocatable thetval(:,:),elmval(:),elmset(:,:)!,weight(:,:),w(:,:)	!for simulations
	allocatable::nintval,ixval,iyval,izval,nevals,thtrue
	real*8 ec50val
	allocatable::ec50val(:)
	integer nintval(:,:),ixval(:),iyval(:),izval(:),nevals(:)
	allocatable::ytemp
	real*4 ytemp(:)
	allocatable::iomit
	integer iomit(:)
	logical dcmod
! For simval.dat from Windows version , iver=106
	character*74 mtitlesw
	character*40 qfilem
    integer program_type
	real*8 p1,p2,pv,sy,syy,fi,one,ybar,var,sd,sdm,cv
	real*8 a2,E2,E1a,E1b,ak2

	character mtitle1*40,filnam*32,prtport*4		!for WINPRINT
	character*40 mtitles 		!for sim model (v 104 only)
	character*60 :: ndir='.',text1
	character*90 :: nfiltsim='*.dat'
	character*60 pfiles(20,10)
	character*60 inifile,pfile,simfile(10)
	logical discprt,append,open7,poplot,landscape
	character*60 comm
    character*40 :: inipath='*.*',textcomp,textid
    character*40 :: inidef='*.ini'//char(124)//'Initialization file (INI)'//&
	char(124)//'*.*'//char(124)//'All Files'
	real RX(100),RY(100),RXBOX(4,100),RYBOX(4,100),rxbox1(4),rybox1(4)
    
real XBEG(50),YBEG(50),XEND(50),YEND(50)
real C_THICK(250),ANGLE(100),thick0(250),THICK(250),SIZEtext(100)
integer ICOL(250),ITYPE(250),IDRAW(250)
integer IFNT(100),IJUS(100),itgset(20)
	integer :: iwid(10),istat(200),ifstat(200),numset(200),iopen(200),jopen(200),jopen2(200)
	CHARACTER*60 SDIR,wdir,pdir,pfilt
	common/tty/ittypanel,itty
	common/dp/discprt
	ALLOCATABLE  newrecords
    COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,&
     SIZEtext,RXBOX,RYBOX,&
     RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,&
     NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
     NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
	
	COMMON/JLOGOS/t1c(10),t2c(10),t1v(10),t2v(10),xoff1,y1v,y2v,&
    y1c,y2c,ncjump,nvjump,ivplot
    common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape,icombo1,poplot,itext1,itext2,&
    itext_1,itext_2,itext_3,itext_4,ibut
    kwi=100
    kwj=100
    NJSET=20
	NIOBS=100
	ndimc=20
	d_arrow=.false.
		 d_line=.false.
		 d_vline=.false.
		 d_hline=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 ndth=100
	ncalc=512
	autplt=.false.
	plotcols=.true.
	idev=0
	angit=5.0
		ifont0=101
	idatyp=0
	do i=1,100
	  ANGLE(i)=0.
	  ifnt(i)=2
	  sizetext(i)=4.0
	  istat(i)=-1
	  ifstat(i)=-1
	  iopen(i)=-1
	enddo
	do i=1,250
	  idraw(i)=-2		!until defined
	!  thick(i)=1.
	!  thick0(i)=1.
	  c_thick=1.
	  itype(i)=0
	  icol(i)=-1
	enddo
	idraw(243)=1		!c-jump logo
	idraw(244)=1		!v-jump logo
	ijus(2)= 0
	sizetext(2) =12./4.	!12 point for param values
	ijus(3)=0		!for x-axis label
	ijus(4)=0
	ANGLE(4)=90.		!for y-axis label
	itx=1
	ity=1
	ilog=0
	izoom=0
	super=.false.	!last fit not superimposed
	ivplot=.false.
	inumx=0
	inumy=0
	iscal=1		!scale internally
	xlo=-1		!whole screen
	ntx=5
	nty=5
	ijus(6)=0
	ijus(31)=-1							
	ixgraph=24
	iygraph=18
	ncols=4
	nrows=20
	NJSET=20
	NIOBS=100
	ndimc=20
	ndc1=ncalc
	ndv1=niobs
	ndimd=njset !!
							
	kmax=20
	ndimd=njset 
   call gOpenGino
   call gGuiwin
   
   call gmInitializeMenu
   call gmSetGuiGridMode(GON)
   call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
   if(ixgrid.lt.36) then
	ixng=48
	iyng=36
   else if (ixgrid.ge.36.and.ixgrid.le.43) then
	ixng=40
	iyng=30
   else if (ixgrid.ge.44.and.ixgrid.lt.50) then
	ixng=36
	iyng=27
   else
	ixng=32
	iyng=24
   endif
 
   call gmdefineguigrid(ixng,iyng)
   call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)

	call gsethardchars()
	call gsetbrokenlinemode(gon)
	iwidth_main=40
	iheight_main=32

	call gSetEscapeChar('£')
	call define_colours(1,.true.)
	call gSetCharFont(ifont0)
	call gSetLineEnd(GROUND)
	call gseterrormode(gerroron)
	program_type=5
	call main_window(program_type,Main,imainpanel,cDATEW,modelw,eqfit,Status_bar1,&
	        new_file,open_file,import_file,izoomy, &
           isave_bmp,isave_wmf,export_file,print_file,exit_file,view_record,&
		   view_data,title_record,iparameters,labels,&
		   jtitle_record,jparameters,jlabels,jnewtext,jnumbers,jaxis,jframe,&
		   jlines,jarrows,jraw_data,jcurves,label_x,label_y, &
		   label_z,number_x,number_y,number_z,Icon1_1,Icon1_2,Icon1_3,Icon1_4,&
		   Icon1_5,Icon1_6,Icon1_7,Icon1_8,Icon1_9,Icon1_10,Icon1_11,Icon1_12,&
		   Combo1_1,Combo1_2,Combo1_3,Combo1_4,Combo1_5,Combo1_6, combo1_8,combo1_9,combo1_10,&
		   toolbar1_1,toolbar1_2,toolbar1_3,Toolbar1_4,&
		   new_text,new_lines,new_arrows,new_lines_v,new_lines_h,&
		   i3d,irot,iview3d,iaratxy,iarathb,igridd,isurfdr,icross,ifill3d,imarkbad,&
		   combo1_11,combo1_12,ipl1,ipl2,ipl3,ipl4,ipl5,ipl6,icprev) 
    
    allocate(newrecords(25))    
    
    call ACTWIN(Main)
    ittypanel=gmCreatePanel(imainpanel, 1, 3, iwidth_main-2, iheight_main-12, &
              	gmHpos=Gleft, gmVpos=Gtop, gmExpand=GON, gmborderType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0)
linestty=100
itty= gmCreateTTYEntry(ittypanel, 0, 0, iwidth_main-2, iheight_main-12, linestty,'',&
				gmoffcol=1,gmoncol=1,gmtextcol=14) 	
    itcall=-10
    pfile="siman.txt"
    
    readini=.false.
	inifile='SIMAN.INI'
    call sim_ini(main,initwin,intoggle,itcall,pfile,initext,inifile,initext1,append,ivaln)
    
    discprt=.true.
    call gmManage    
1	continue
! Action loop
    do while (gmAction(icallid) /= -1)
2       continue
	
		
    
	    select case(icallid)   
	    case(-6)
	        imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
		    if(imes.eq.gnobutton) then
			    goto 3
		    else
			    comm='c:\windows\notepad '// pfile
			    res=systemqq(comm)
			    goto 3
		    endif
	    case(-7)! Browse ini
	        CALL gmFileBrowser(iniFILE,inipath,inidef,gmType=GinPUT, &
				gmTitle='Ini files')
		    continue
		    if(inifile.ne.'') then
		        nl=nblank1(inipath)
		        if(nl.gt.1) then
		        inifile=inipath(1:nl)//'\'//inifile
		        readini=.true.
		        endif
		    else
		        readini=.false.
		    endif
			call gmsettextsetting(initext1,inifile)
	    case(-8)! use ini
	        readini=.true.
	        call gmsetwidgetstatus(ivaln,gunselectable)
	    case(-9)! no ini
	        readini=.false.
	        call gmsetwidgetstatus(ivaln,gselectable)
		    call gmsettoggleswitch(intoggle(11),goff)
		    call gmsettoggleswitch(intoggle(12),gon)
	    case(-5) !browse print
	        CALL gmFileBrowser(pFILE,pDIR,pFILT,gmType=GinPUT, &
				gmTitle='Print files')
		    continue
		    nl=nblank1(pdir)
		    pfile=pdir(1:nl)//'\'//pfile
		    if(icprev.ne.109) call gmsettextsetting(initext,pfile)
	    case(-2) ! append
	        append=.true.
		    discprt=.true.
	    case(-3)!OVERWRITE
	        append=.false.
		    discprt=.true.
	    case(-4) !NO PRINT
	        append=.false.
		    discprt=.false.
	    case(-10) ! continue
	       if(readini) then
		    call gmenqtextsetting(initext1,inifile)
		    INQUIRE (FILE=iniFILE,EXIST=PRESENT) 

	        if(PRESENT) then
	
		    ihandle=FILE$FIRST
		    length = GETFILEINFOQQ(inifile, info, ihandle)
		    nLEN=info%length
	
		
		    OPEN(unit=12,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)
		
		    read (12,rec=1) ivi
	        if(ivi.eq.1000) then
	            read(12,rec=1) ivi,inifile,nfile,( simfiles(i),i=1,nfile)
	        else
	            read(12,rec=1) inifile,simfile1,simfile2,ans1
	            if(ans1.ne.'N'.and.ans1.ne.'Y') ans1='N'
	            if(ans1.eq.'Y') then
	                nfile=2
	            else
	            endif
	        endif
		    close(unit=12)
	        else
	        imsgb=gmdisplaymessagebox(inifile,'The file does not exist.Try again',&
			ginformation,gok)
	            goto 1
	        endif 
		else
		 nfile=gmenqvaluesetting(ivaln)
		endif
	        
	        if(discprt) then
			call gmenqtextsetting(initext,pfile)
			if(pfile.ne.'          '.and.discprt) then
		    INQUIRE(file=pfile,exist=present)
		    if(present) then
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(pfile, info, ihandle)
				nLEN=info%length
		
				if(nlen.gt.0)then
				 !   if(open7.eq..false.) then
					open7=.true.
					OPEN(unit=7,file=pfile,status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!	endif
					if(.not.append) then 
						REWIND(unit=7)
						append=.true.
					    write(7,102) cdatew
                    endif
				else
					imes=gmdisplaymessagebox(pfile,' does not exit.Create a new file',ginformation,gok)
	
			!		pfile=wdir(1:nlw)//'\'//filnam
					open7=.true.
					OPEN(unit=7,file=pfile,status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!	write(8,1001) 'Date','Computer','User','File','Exp Date','Tape details'
					write(7,102) cdatew
				endif
			else
			!	pfile=wdir(1:nlw)//'\'//filnam
				open7=.true.
					OPEN(unit=7,file=pfile,status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!		write(8,1001) 'Date','Computer','User','File','Exp Date','Tape details'
					write(7,102) cdatew
			endif
		
		else
			!	pfile=wdir(1:nlw)//'\'//filnam
				open7=.true.
				OPEN(unit=7,file=pfile,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!	write(8,1001) 'Date','Computer','User','File','Exp Date','Tape details'

			write(7,102) cdatew
		endif
		else
		open7=.false.
		endif
		
		nset=nfile
		icallid=-101
		nfileb(1)=1
		call change_path(main,initwin1,initwin1_TxtArray,nset,icallid,&
		pfiles,nfileb,-1)

    
    case(-220)		
		do i=1,nfile
		    simfile(i)=pfiles(1,i)
		    nb=nblank1(simfiles(i))
			ilp=1
			do m=1,nb
			    if(simfiles(i)(m:m).eq.'\') ilp=m
			enddo
			if(ilp.eq.1) then
			    text1=simfiles(i)
			else
			    text1=simfiles(i)(ilp+1:nb)
			endif
			
	        CALL gmFileBROWSER(text1,ndir,nfiltsim,gmBrowseType=0)
		    if(text1.ne.'  ') then
				INQUIRE (FILE=text1,EXIST=PRESENT)
				nb=nblank1(ndir)
			    simfiles(i)=ndir(1:nb)//'\'//text1
			
			    
			    pfiles(1,i)=simfiles(i)
				call gmSetCellSetting(initwin1_TxtArray, 1,i ,gmString=pfiles(1,i))
				
				call gmdrawwidget(initwin1_TxtArray)
				
			endif
		enddo	
				
	
	case(-101)
        ibady=0
	
		do i=1,nfile
			call gmenqcellsetting(initwin1_TxtArray, 1,i,valr,text1)
			present=.false.
		
			if (text1.ne.' ') then
			
			    INQUIRE(FILE=text1,EXIST=PRESENT)
			    if(present) then
			        ihandle=FILE$FIRST
					length = GETFILEINFOQQ(text1, info, ihandle)
					nLEN=info%length
					if(nlen.lt.50) then
						imko=gmdisplaymessagebox('text1','Not a proper simval.dat file',ginformation,gok)
						ibady=-1
						goto 1
					else
					    OPEN(unit=10,file=simfiles(i),status='UNKNOWN',&
     				    access='DIRECT', form='BINARY',RECL=1)
					    read(unit=10,rec=1) iver2
					    close(unit=10)
					    if(iver2.gt.106) then 
					        ibady=-1
					        imko=gmdisplaymessagebox('text1','Not a proper simval.dat file',ginformation,gok)
						    goto 1
						endif
					endif
					
				else
					    imko=gmdisplaymessagebox('text1','Not present',ginformation,gok)
					    ibady=-1
					    goto 1
				endif
			    nb=nblank1(text1)	
			    simfiles(i)=text1
			    
			else
			    
			    ibady=-1
			    goto 1 
			endif
			
			nb=nblank1(text1)	
			simfiles(i)=text1
		enddo
		
		call gmremovewindow(initwin1)
		nsims=0
		npar=0
	    do i=1,nfile  
	        OPEN(unit=10,file=simfiles(i),status='UNKNOWN',&
     				    access='DIRECT', form='BINARY',RECL=1)  
     	    read(unit=10,rec=1) iver2
     	    
	        if(iver2.le.101) then
		        read(unit=10,rec=1) iver2,nsims2,abortw,npar2,nset
	        else
		        read(unit=10,rec=1) iver2,nsims2,npar2,nset
	        endif
	        nsims=nsims+nsims2
	        if(npar2.gt.npar) npar=npar2
	        close(unit=10)
		enddo
	
	    
	    if(allocated(thetval)) deallocate(thetval)
	    if(allocated(nintval)) deallocate(nintval)
	    if(allocated(ixval)) deallocate(ixval,iyval,izval)
	    if(allocated(elmval)) deallocate(elmval)
	    if(allocated(elmset)) deallocate(elmset)
	    if(allocated(nevals)) deallocate(nevals)
	    if(allocated(ec50val)) deallocate(ec50val)
	    if(allocated(ytemp)) deallocate(ytemp)
	    if(allocated(iomit)) deallocate(iomit)
	    ALLOCATE(thetval(npar,nsims),nintval(nset,nsims),&
		    ixval(nsims),iyval(nsims),izval(nsims),elmval(nsims),&
			elmset(nset,nsims),nevals(nsims),ec50val(nsims))
	    ALLOCATE(ytemp(nsims),iomit(nsims))
	    if(allocated(thtrue)) deallocate(thtrue)
	    allocate(thtrue(200))
	    do i=1,200
					thtrue(i)=0.d0
	    enddo
	    igraphsim=0
	    call gmremovewindow(initwin)
	    call simandis(main,thetval,nintval,ixval,iyval,izval,elmval,elmset,nevals,ec50val,&
			nsims,npar,nset,imod0,ndimd,ndimc,titlep,simfiles,thtrue,id,iomit,nomit,&
			icallsim,newrecords,igraphsim,combo1_1,COMBO1_2,nfile)

end select
enddo
if(open7) then
	imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
		if(imes.eq.gyesbutton) then
			comm='c:\windows\notepad '// pfile
			res=systemqq(comm)
		endif
		close(unit=7)
endif
3   continue
imes=gmdisplaymessagebox('','Write to ini file',gquestion,gyesno)
if(imes.eq.gyesbutton) then

	CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=1)
			 IF(iniFILE.ne.' ') then
			 nl=nblank1(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
		!	 call gmsettextsetting( inittext,inifile)
		!	 inifile0=inifile
			 endif
	INQUIRE (FILE=iniFILE,EXIST=PRESENT) 

	if(PRESENT) then
	
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(inifile, info, ihandle)
		nLEN=info%length
	
		
		OPEN(unit=12,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)
		
		read (12,rec=1) ivi
	    if(ivi.eq.1000) then
	        write(12,rec=1) ivi,inifile,nfile,( simfiles(i),i=1,nfile)
	    else
	        read(12,rec=1) inifile,simfile1,simfile2,ans1
	        if(ans1.ne.'N'.and.ans1.ne.'Y') ans1='N'
	        simfile1=simfiles(1)
	        simfile2=simfiles(2)
	        write(12,rec=1) inifile,simfile1,simfile2,ans1
	    endif
		close(unit=12)
	else
	    
	    OPEN(unit=12,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)
        write(12,rec=1) ivi,inifile,nfile,( simfiles(i),i=1,nfile)
		close(unit=12)
	
	endif 

endif

102		format(' Analyse simulations-; Date of analysis: ',a11)
end