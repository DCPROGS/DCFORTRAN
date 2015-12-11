Program plotsamp

! callid: 1  - 20 : data type options
! program options : 21:24 
!	21: display, 22: filter, 23: write, 24 : omit  
! 100:199 display  (141-... popen)
! 200:299 filtsam
! iplot=1 main graph
! iplot=99 popen
! iplot=51 zoom edges
!
USE DFLIB

use gino_f90
use menu_f90
real*4 xnum(:,:)
allocatable xnum
integer*4 ieditout(10),ivalout(20),ibut(10),npag(100,2),npopointsf(100)
real yshutf(100),yopenf(100),tsegf(100)
character*60 teditout(10),iniinifile
real tvalout(20),popen(100)
integer pixbufo(800,10),pixbufs(800,10),pixbufi(10,800),pixbuff(10,800)
integer pixbuf1(10,800),pixbuf2(10,800)
integer pixbuf(10,800),pixbufz(800,10)
character*60 text_tog(100)
character*200 textt1,textt2
character*60 comm,tempfile,tempfile0,inifile0
character*8 iticks
character	drives*26,text1*40,text2*40
real valdat(100)
CHARACTER STRING(:)
allocatable::string
character*80 actext

integer iargs(80)
real args(80)
CHARACTER*11 CXTRUE,CYTRUE,cset
CHARACTER*12 FACCESS,FBINARY,FFORM,FRTYPE,FACTION
LOGICAL FOPEN_11,FOPEN_12,append,saveandplot,FILTSAM,open8,res,samepag
integer*2 imean,imax,imin,idatf
integer intoggle(100),idat(100),ivall(50),itype(50),ifstat(100)
character cDATEW*11,qDATEW*9
integer*2 ibuf(:)
character*11 cdate,expdate,CNPATCH,prevdate,cnum1,cnum2,cnum3,cnum4,cnum5
character*14 ptype(5)
character*24 tapeID
character*15 cnum0(10)
character*40 :: inipath,tempath
character*40 :: inidef='*.ini'//char(124)//'Initialization file (INI)'//&
char(124)//'*.*'//char(124)//'All Files'
character*40 :: tempdef='*.tmp'//char(124)//'temporary file (tmp)'//&
char(124)//'*.*'//char(124)//'All Files'
character*76 title1
character*6  defname,defname0,cctrig
character*70 title,messg,textcombo
character cs*3,adctime*8,qdate*9, dname*2,filetype*4
CHARACTER*60 SDIR,wdir,pdir,pfilt
character*30 adcfil
character*50 PFILE
character*60 testfil,sfile,tempf,testfil1,adcfil1
character*11 cnbyte,cnd1,cnd2,cnum
integer nprime(1900)	!holds primes up to 16381 (see PRIMGEN.FOR)
integer*2 idt,inchan,id1,id2,irev,iver,idataformat
ALLOCATABLE::idata,data8
integer*2 idata(:)
real*8 data8(:)
integer*4 ilen,iedit(20),ival(20),ival1(20),ival2(20),ival3(11)
character*60 tedit(20),dfile,intgFIL,edrfil
character*3 ndev
real tval(20),tval1(20),tval2(20),val(50),tval3(11)
logical present,setbase,readrec
logical moveline
logical chooseseg
logical choosepopen
TYPE (FILE$INFO) info
logical noconsam,newform,landplot,meta,badname,wmeta,edr
logical autplt,plotcols
ALLOCATABLE::yval,xval,popend,xpval

real*4 yval(:,:),xval(:,:),popend(:),xpval(:)
real*4 pgrid(10)
logical mono,doframe,lastpage,pwmf
character*32 filnam
logical discprt,plot,landscape

logical poplot,offset,popened,zoomed
character*40 mtitle		!machine name
logical adcdef,cjdat,integ,asc,consm,axfile,good,setgain,keeprev

common/dp/discprt
character*33 ascfil
character*18 ascnum
character*60 inifile
integer Graph1_1(100)
integer GraphMainPanel1_1(100)
integer Graphics1_1(100)

integer*2 idatin(:),idatout(:)

integer :: iwid(10)
character*10 label(10)
integer nrows(1),indf(100,2)
real yline(10),data1(:)	

character*20 colhead(10)
character*70 text
character*90::sfilt
CHARACTER*60 FTITLE(200)
CHARACTER*80 FTITLE1(200)
logical digchar1,readini,newprev,readtemp
logical dcfile,idatcal
logical logx,logy,sqrty,logity
logical bold,italik,underline,nolabx
type (Gwidget) :: widget
ALLOCATABLE::ibuf,xval1(:,:),yval1(:,:),xval2(:),yval2(:),yval0(:)
ALLOCATABLE::idatin,idatout,data1
common/limit/wxmin0,wxmax0,wymin0,wymax0,xmin1,xmax1,ymin1,ymax1,xmin0,xmax0,ymin0,ymax0				
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue 
common/logval/logx,logy,sqrty,logity
common/details/text1,text2,drives
common/pixpos/ixgrid,iygrid,ixpix,iypix,yt,landscape
common/plotopen/icombo1,poplot,&
itext1,itext2,itext_1,itext_2,itext_3,itext_4,ivalp1,ivalp2,ivalp3,ivalp4,&
iradiop1,iradiop2,iradiop3,iradiop4
wxmin0=0.
wxmax0=0.
wymin0=0.
wymax0=0.
xmin1=0.
xmax1=0.
ymin1=0.
ymax1=0.
xmin0=0.
xmax0=0.
ymin0=0.
ymax0=0.
sdir='.'
ngrid=1	
idataformat=-1
tempfile='plotsamp.tmp'
filnam='PLOTSAMP.txt'
setbase=.false.
delay=0.0
keeprev=.false.                                                                                                 
call gEnqWorkingDir(wdir)
!pfile='plotsamp.txt'
pfilt='*.txt'//char(124)//'Print file (txt)'//char(124)//&
						  '*.prt'//char(124)//'Old print files'
pdir=wdir
nl=len_trim(wdir)
newprev=.true.
npopen=0
inipath=wdir
tempath=wdir
nlw=nl
plot=.false.
inifile0=wdir(1:nl)//'\plotsamp.ini'
pfile=wdir(1:nl)//'\'//filnam
tempfile0=wdir(1:nl)//'\plotsamp.tmp'
adc=32768
ad=5.
adcv=6553.6
!adcv=adc/ad
do k=1,100
    GraphMainPanel1_1(k)=0
    Graphics1_1(k)=0
    Graph1_1(k)=0
enddo
	iniinifile=wdir(1:nl)//'\plotini.ini'

			discprt=.true.
			append=.true.
			itogrec=1
    INQUIRE (FILE=iniiniFILE,EXIST=PRESENT)
	if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(iniinifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    INQUIRE(IOLENGTH=LEN) iniiniFILE	
    	    if(nlen.gt.0) then
			
				read(19,rec=1) itogrec,inifile,pfile,discprt,append,tempfile,initog

				
			else
			
				write(19,rec=1) itogrec,inifile,pfile,discprt,append,tempfile,initog
			endif			
    else	
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
			
			
			write(19,rec=1) itogrec,inifile,pfile,discprt,append,tempfile,initog
    endif
	close(unit=19)


	ijoinj=0		!solid line
	thj=0.0
nb=len_trim(tempfile)
if(nb.lt.4) then 
	tempfile=tempfile0
endif
nb=len_trim(inifile)
if(nb.lt.4) inifile=inifile0


	call DATE(qdate)
   cdate=qdate(1:7)//'20'//qdate(8:9)
	call gOpenGino
	call gGuiwin
	call gmInitializeMenu
	call gmSetGuiGridMode(GON)
	call gmenqguigrid(ixgrid,iygrid,ixpix,iypix)
	call define_colours(1,.true.)
	 
call genqlinewidthmode(is9)
call gSetLineWidth(0.0)
call gsethardchars()
call gsetlinewidthmode(ghardware)
!call gseterrormode(gerroron)
call genqselectedpen(icol9,wid9,itip9)
call gEnqLineWidthScaling(scale9) 
call gSetLineEnd(GROUND	)
! Start management
call gsetbrokenlinemode(gsoft)
call gDefineRGB(122,0.80,0.89,0.87)
	 call gDefineRGB(123,0.73,0.82,0.79)
	 call gDefineRGB(124,0.66,0.97,0.87)
	 call gDefineRGB(125,0.89,0.97,0.94)
	 call gDefineRGB(121,0.87,0.97,0.94)
	 call gDefineRGB(227, 1.000000, 0.458824, 0.458824)
 call gDefineRGB(228, 1.000000, 1.000000, 0.517647)
  call gDefineRGB(225, 1.000000, 0.866667, 0.733333)
   call gDefineRGB(226, 0.996078, 0.780392, 0.705882)
    call gDefineRGB(220, 1.000000, 1.000000, 0.807843)
   call gDefineRGB(221, 0.796078, 1.000000, 0.592157)
   call gDefineRGB(222, 0.682353, 0.843137, 1.000000)
   call gDefineRGB(223, 1.000000, 0.909804, 0.909804)
  call gDefineRGB(141, 0.192157, 0.984314, 0.964706)
  call gDefineRGB(229, 0.836078, 0.950000, 0.592157)
   call gDefineRGB(101, 0.831372, 0.901960, 0.901960)
   call gDefineRGB(102, 0.772549, 0.850980, 0.850980)
   call gDefineRGB(103, 0.745098, 0.819607, 0.819607)
   call gDefineRGB(104, 0.704117, 0.760784, 0.760784)
   call gDefineRGB(105, 0.662745, 0.729411, 0.729411)
! pink palette
	 call gDefineRGB(112,0.96,0.82,0.82)
	 call gDefineRGB(113,0.93,0.78,0.78)
	 call gDefineRGB(114,0.90,0.74,0.74)
	 call gDefineRGB(115,0.87,0.70,0.70)
	 call gDefineRGB(111,0.98,0.92,0.92)
idir=-1

ipos=0
disfacx=0.7
disfacy=0.7
discprt=.true.
!append=.false.
imark=0
icross=0
call DATE(qDATEW)
cDATEW=qDATEW(1:7)//'20'//qDATEW(8:9)

if(secline0.le.0.) secline0=1.
tval1(3)=secline0
ntrace=10		!default value
if(poplot) ntrace=5
tseg=50
tval1(4)=ntrace
itseg=ifixr(tseg)
tval1(7)= itseg
gainfac=1.0
nbuf=1000000		!size of data arrays
novlap=1000		
tval2(3)=gainfac
tval2(4)=nbuf
tval2(5)=novlap			

tval1(10)=1
jnth=1
text_tog(1)='Continuous data from CONSAM file'
text_tog(2)='Continuous data from AXON file'
text_tog(5)='CJUMP data'
text_tog(3)='ASCII file data'
text_tog(4)='16 bit integer binary file data'
text_tog(6)='EDR data'
text_tog(7)='Other files'
itcall = 1
ntog=6
!ntog=7
filtsam=.false.
	iplot=0
	jplot=0
ifirst=-1
ipatch=0
!ntrace=10
itrace=0
call main_plot(iform,ipanel0,ipanel,ipanel2,iedit,tedit,ival,tval,itoggle0,&
itoggle1,itoggle2,itoggle3,ibutton1,ibutton2,icombo2,ibutton)

call gmsetwidgetstatus(ibutton2,GSELECTABLE)
	
  call samp_ini(iform,isampw,tempfile,istoggle1,istoggle2,istext)
  	call gmDefineKeyselectCallback(13,-2)	
 if(initog.eq.1) then
	call gmsettoggleswitch(isToggle1,gon)
 else
	call gmsettoggleswitch(isToggle2,gon)
 endif

call gmManage
! Action loop
1   do while (gmAction(icallid) /= -1)
		CALL ACTENQ(iCALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
		
		goto 2
		indx=1
		if(ikey.eq.-1) then
		
		if(iplot.eq.99.and.moveline) then
	
			
			if(indx.eq.2) then
					nx=800
					ny=10
					rval=yshut
					call drawpixelarray(xmin0,rval,xmax0,rval,pixbufs,nx,ny)
	   				rval=ymov
			else if(indx.eq.1) then
				nx=800
				ny=10
				rval=yopen
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufo,nx,ny)
				rval=ymov
			else if(indx.eq.3) then
				ny=800
				nx=10
			    rval=xi
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbufi,nx,ny)
			 		!green
			    rval=xmov
			else if(indx.eq.4) then
				ny=800
				nx=10
			    rval=xf
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbuff,nx,ny)
			    rval=xmov
			endif
		if(indx.eq.2) then
				nx=800
				ny=10
			    yshut=rval
				call getpixelarray(xmin0,yshut,xmax0,yshut,pixbufs,nx,ny)
			 	ic=10		!green
				call drawline(xmin0,yshut,xmax0,yshut,ic,idev)
				call gmsetvaluesetting(ivalp2,rval)
			else if(indx.eq.1) then
				nx=800
				ny=10
			    yopen=rval
				call getpixelarray(xmin0,yopen,xmax0,yopen,pixbufo,nx,ny)
				ic=12		!green
				call drawline(xmin0,yopen,xmax0,yopen,ic,idev)
				call gmsetvaluesetting(ivalp1,rval)
			else if(indx.eq.3) then
				ny=800
				nx=10
			    xi=rval
				call getpixelarray(xi,ymin0,xi,ymax0,pixbufi,nx,ny)
			 	ic=13		!green
				call drawline(xi,ymin0,xi,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp3,rval)
			else if(indx.eq.4) then
				ny=800
				nx=10
			    xf=rval
				call getpixelarray(xf,ymin0,xf,ymax0,pixbuff,nx,ny)
			 	ic=13		!green
				call drawline(xf,ymin0,xf,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp4,rval)
			endif
					
		else if(iplot.eq.51.and.moveline) then	
					
        	    	nx=10
				ny=800
				rval=x0
				call drawpixelarray(rval,yminz,rval,ymaxz,pixbuf,nx,ny)
				
				rval=xmov
				
				x0=rval
				call getpixelarray(x0,yminz,x0,ymaxz,pixbuf,nx,ny)
				ic=13
				call drawline(x0,yminz,x0,ymaxz,ic,idev)
				call gmsetvaluesetting(ivalpz,rval)	
		endif
		
		endif
2		continue
	select case(icallid)
	
	case(-3) ! browse for tempfile
	tempf=tempfile
		CALL gmFileBrowser(tempF,tempath,tempdef,gmType=GinPUT, &
				gmTitle='Tempfiles')
		if(tempf.ne.' ') then
		nl=len_trim(tempath)
		tempfile=tempath(1:nl)//'\'//tempf
	    call gmsettextsetting(istext,tempfile)
		endif
	case(-2)
		istat=gmEnqToggleSwitch(istoggle1)
		call gmDefineKeySelectCallback(13,0)
		
		if(istat.eq.1) then
		initog=1
			indt=itogrec
				call gmenqtextsetting(istext,tempfile)
			INQUIRE(file=tempfile,exist=present)
		    if(present) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(tempfile, info, ihandle)
			nLEN=info%length
		
		
		
			if(nlen.gt.0) then
			    append=.true.
				OPEN(unit=15,file=tempfile,status='UNKNOWN', access='DIRECT',form='BINARY',recl=1)
				readini=.true.
				readtemp=.true.
				read(15,rec=1) ivit
				if(ivit.eq.101) then
					read (15,rec=1) ivit,npagsav,npopen,ntrace
					if(npopen.ge.10.and.nlen.le.512) then
						read (15,rec=1) ivit,npagsav,npopen,ntrace,&
						(Popen(i),i=1,npopen)
						imes=gmdisplaymessagebox('','This temp file cannot be read',&
						ginformation,gok)
						readtemp=.false.
							icprev=0
					call gmremovewindow(isampw)
					initog=0
					append=.false.
					if(itogrec.le.0.and.itogrec.ge.6) itogrec=1
					ntog=6
			call plot_ini(iform,initwin,ntog,text_tog,intoggle,itcall,itogbutton,pfile,&
			initext,inifile,initext1,append,itogrec)
			goto 1
						continue
					else
					read (15,rec=1) ivit,npagsav,npopen,ntrace,&
				    (Popen(i),i=1,npopen),(tsegf(i),i=1,npopen),(npag(i,1),i=1,npopen),&
					(npag(i,2),i=1,npopen),(npopointsf(i),i=1,npopen),(indf(i,1),i=1,npopen),&
					(indf(i,2),i=1,npopen),testfil,delay,poplot,tseg,ngrid,&
					secline0,sdir,ytsepin
					if(secline0.le.0) secline0=1.
					endif
					nl=len_trim(testfil)
					do i=1,nl
						if(testfil(i:i).eq.'\') ilp1=i
					enddo
					adcfil=testfil(ilp1+1:nl)
					testfil1=adcfil
					if(ngrid.lt.1) then
						ngrid=1
						pgrid(1)=0.5
					endif
					close(unit=15)
				else if(ivit.eq.100) then
					read (15,rec=1) ivit,npagsav,npopen,ntrace,&
				    (Popen(i),i=1,npopen),(tsegf(i),i=1,npopen),(npag(i,1),i=1,npopen),&
					(npag(i,2),i=1,npopen),(npopointsf(i),i=1,npopen),(indf(i,1),i=1,npopen),&
					(indf(i,2),i=1,npopen),testfil,delay,poplot,tseg,ngrid,&
					secline0,sdir
					if(secline0.le.0) secline0=1.
					write (15,rec=1) ivit,npagsav,npopen,ntrace,&
				    (Popen(i),i=1,npopen),(tsegf(i),i=1,npopen),(npag(i,1),i=1,npopen),&
					(npag(i,2),i=1,npopen),(npopointsf(i),i=1,npopen),(indf(i,1),i=1,npopen),&
					(indf(i,2),i=1,npopen),testfil,delay,poplot,tseg,ngrid,&
					secline0,sdir,ytsepin
					nl=len_trim(testfil)
					do i=1,nl
						if(testfil(i:i).eq.'\') ilp1=i
					enddo
					adcfil=testfil(ilp1+1:nl)
					testfil1=adcfil
					if(ngrid.lt.1) then
						ngrid=1
						pgrid(1)=0.5
					endif
					close(unit=15)
				else
				endif
					call gmremovewindow(isampw)
					nl=len_trim(testfil)
					if(nl.gt.3) then
					if(testfil(nl-3:nl).eq.'.SSD'.or.testfil(nl-3:nl).eq.'.ssd') iftype=1
					if(testfil(nl-3:nl).eq.'.ABF'.or.testfil(nl-3:nl).eq.'.abf') iftype=2
					endif
			icprev=109
			icallid=1
			
			goto 2
			else
			    imes=gmdisplaymessagebox(testfil,'File not on this directory.Look for file?',&
				gquestion,gyesno)
			    if(imes.eq.gyesbutton) then
				icallid=-3
				goto 2
				else
					call gmremovewindow(isampw)
					icprev=0
					if(itogrec.le.0.and.itogrec.ge.6) itogrec=1
					ntog=6
					call plot_ini(iform,initwin,ntog,text_tog,intoggle,itcall,itogbutton,pfile,&
					initext,inifile,initext1,append,itogrec)
				endif
			endif
			else
				imes=gmdisplaymessagebox(testfil,' not on this directory.Look for file?',&
					gquestion,gyesno)
					if(imes.eq.gyesbutton) then
					icallid=-3
					goto 2
					else
					call gmremovewindow(isampw)
					icprev=0
					if(itogrec.le.0.and.itogrec.ge.6) itogrec=1
					ntog=6
					  call plot_ini(iform,initwin,ntog,text_tog,intoggle,itcall,itogbutton,pfile,&
		              initext,inifile,initext1,append,itogrec)
					endif
			endif
		
			! read temporally file		
		else
			readtemp=.false.
		icprev=0
			call gmremovewindow(isampw)
			initog=0
			append=.false.
			if(itogrec.le.0.and.itogrec.ge.6) itogrec=1
			ntog=6
			call plot_ini(iform,initwin,ntog,text_tog,intoggle,itcall,itogbutton,pfile,&
			initext,inifile,initext1,append,itogrec)
	    endif
		case(-5)
          	do i=1,ntog
			istatus1=gmEnqToggleSwitch(intoggle(i))
			if(istatus1.eq.1) indt=i 
			enddo
		readini=.false. 
			icallid=-6
		goto 2
	case(1) ! choose type of data
		call gmsetwidgetstatus(ibutton,GSELECTABLE)
	
		
			if(discprt) then
			if(icprev.ne.109) call gmenqtextsetting(initext,pfile)
			if(pfile.ne.'          '.and.discprt) then
		    INQUIRE(file=pfile,exist=present)
		    if(present) then
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(pfile, info, ihandle)
				nLEN=info%length
		
				if(nlen.gt.0) then
					open8=.true.
		
					OPEN(unit=8,file=pfile,status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
					if(.not.append) then 
						REWIND(unit=8)
						append=.true.
					    write(8,102) cdate,text1,text2
                    endif
				else
			!		imes=gmdisplaymessagebox(pfile,' does not exit.Create a new file',ginformation,gok)
	
					pfile=wdir(1:nlw)//'\'//filnam
					open8=.true.
					OPEN(unit=8,file=pfile,status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!	write(8,1001) 'Date','Computer','User','File','Exp Date','Tape details'
					write(8,102) cdate,text1,text2
				endif
			else
				pfile=wdir(1:nlw)//'\'//filnam
				open8=.true.
					OPEN(unit=8,file=pfile,status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!		write(8,1001) 'Date','Computer','User','File','Exp Date','Tape details'
					write(8,102) cdate,text1,text2
			endif
		
	  else
				pfile=wdir(1:nlw)//'\'//filnam
				open8=.true.
				OPEN(unit=8,file=pfile,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
				!	write(8,1001) 'Date','Computer','User','File','Exp Date','Tape details'

			    write(8,102) cdate,text1,text2
		endif
		else
		open8=.false.
		endif

	if(icprev.ne.109) then
			istatus14=gmEnqToggleSwitch(intoggle(14))
			if(istatus14.eq.0) readini=.false.
			do i=1,ntog
			istatus1=gmEnqToggleSwitch(intoggle(i))
			if(istatus1.eq.1) indt=i 
			enddo
			call gmenqtextsetting(initext1,inifile)
	         present=.false.
			if(readini) then
			INQUIRE(file=inifile,exist=present)
			if(present) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(inifile, info, ihandle)
			nLEN=info%length
			
			OPEN(unit=12,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)

			if(nlen.gt.0) then
				readini=.true.
				read(12,rec=1) ivi
				if(ivi.eq.1000) then
					read (12,rec=1) ivi,testfil,delay,poplot,tseg,ngrid,(pgrid(i),i=1,ngrid),&
					secline0,sdir
					if(secline0.le.0) secline0=1.
					nl=len_trim(testfil)
					do i=1,nl
						if(testfil(i:i).eq.'\') ilp1=i
					enddo
					adcfil=testfil(ilp1+1:nl)
					if(ngrid.lt.1) then
						ngrid=1
						pgrid(1)=0.5
					endif
				else
				if(ivi.eq.1002) then
					read (12,rec=1) ivi,testfil,delay,poplot,tseg,ngrid,&
					secline0,sdir
					if(secline0.le.0) secline0=1.
					nl=len_trim(testfil)
					do i=1,nl
						if(testfil(i:i).eq.'\') ilp1=i
					enddo
					adcfil=testfil(ilp1+1:nl)
					
				
				else
					sdir=wdir
					read(12,rec=1) adcfil,delay,poplot,tseg,ngrid,secline0
					if(secline0.le.0) secline0=1.
				endif
				ivi=1000
				npagsav=1
				ngrid=1
				ngrid1=ngrid
				pgrid(1)=0.5
				iplen = FULLPATHQQ(adcfil, testfil)
				write (12,rec=1) ivi,testfil,delay,poplot,tseg,ngrid,(pgrid(i),i=1,ngrid),&
					secline0,sdir
			
				endif

			else
				imes=gmdisplaymessagebox(inifile,' not on this directory.Look for file?',&
				gquestion,gyesno)
				if(imes.eq.gyesbutton) then
				icallid=50
				goto 2
				else
				delay=60.
				ivi=1002
				sdir=wdir
				secline0=1.
				npopen=0
				readini=.false.
				endif
			endif
			else
			imes=gmdisplaymessagebox(inifile,' not on this directory.Look for file?',&
			gquestion,gyesno)
			if(imes.eq.gyesbutton) then
				icallid=50
				goto 2
   			else
			delay=60.
			ivi=1002
			sdir=wdir
			secline0=1.
			npopen=0
			readini=.false.
			endif
		endif
		CLOSE(unit=12)
		nl=len_trim(testfil)
		if(nl.gt.3) then
			if(testfil(nl-3:nl).eq.'.SSD'.or.testfil(nl-3:nl).eq.'.ssd') iftype=1
			if(testfil(nl-3:nl).eq.'.ABF'.or.testfil(nl-3:nl).eq.'.abf') iftype=2
		endif
		endif
		endif
		secline1=secline0*1000.	
		npopen1=npopen
		if(secline0.le.0.) secline0=1.
		tval1(3)=secline0

		if(poplot.and.icprev.ne.109) ntrace=5
		tval1(4)=ntrace
		itseg=ifixr(tseg)
		tval1(7)= itseg
		icallid=-6
		goto 2
case(-6)
		itogrec=indt
		if(icprev.eq.109) then
			nl=len_trim(testfil)
					if(nl.gt.3) then
					if(testfil(nl-3:nl).eq.'.SSD'.or.testfil(nl-3:nl).eq.'.ssd') then
						iftype=1
						indt=1
					else if(testfil(nl-3:nl).eq.'.ABF'.or.testfil(nl-3:nl).eq.'.abf') then
						iftype=2
						indt=2
					endif
				endif	
		else
			call gmremovewindow(initwin)
		endif


		if(indt.eq.1) then
	
		axfile=.false.
			cjdat=.false.
			consm=.true.
			asc=.false.
			integ=.false.
			tedit(10)='Consam file'
		if(readini) then
		
		
			INQUIRE (FILE=testfil,EXIST=PRESENT,IOSTAT=I_VAR)
			i_var=0
			if(PRESENT) then
					tedit(1)=testfil
					icallid= 12
					ifirst=1
					 call gmsettextsetting(iedit(1),tedit(1))
					goto 2
				
			else
				imes=gmdisplaymessagebox(testfil,&
				'Does not Exist.Browse for file',ginformation,gok)
				goto 1
			endif
			    
			ifirst=1
			tedit(1)='Enter/Browse for Consam file'
			call gmsettextsetting(iedit(1),tedit(1))
			if(iftype.eq.1) then
				!	sfile=adcfil
						sfile='*.ssd'
			else
					sfile='*.ssd'
			endif
			icallid= 11
			goto 2
		else
		
			sfilt='*.ssd'//char(124)//'Comsam file (ssd)'//char(124)//&
						  '*.abf'//char(124)//'Axon files (abf)'//char(124)//&
						   '*.dat'//char(124)//'Data files (dat)'//char(124)//&
                          '*.*'//char(124)//'All Files'
			
			if(ifirst.eq.-1.and.iftype.eq.1) then
				INQUIRE (FILE=testfil,EXIST=PRESENT)
				i_var=0
				if(PRESENT) then
					tedit(1)=testfil
					icallid= 12
					ifirst=1
					 call gmsettextsetting(iedit(1),tedit(1))
					goto 2
				endif
			endif
			ifirst=1
			tedit(1)='Enter/Browse for Consam file'
			call gmsettextsetting(iedit(1),tedit(1))
			if(iftype.eq.1) then
				!	sfile=testfil
						sfile='*.ssd'
			else
					sfile='*.ssd'
			endif
			icallid= 11
			goto 2
		endif
			
		else if(indt.eq.2) then
			sfilt='*.abf'//char(124)//'Axon files (abf)'//char(124)//&
			'*.ssd'//char(124)//'Comsam file (ssd)'//char(124)//&			  
			 '*.dat'//char(124)//'Data files (dat)'//char(124)//&
             '*.*'//char(124)//'All Files'
			axfile=.true.
			cjdat=.false.
			consm=.false.
			asc=.false.
			integ=.false.
				tedit(10)='Axon file'
			if(ifirst.eq.-1.and.iftype.eq.2) then
			INQUIRE (FILE=testfil,EXIST=PRESENT)
			i_var=0
			if(PRESENT) then
				tedit(1)=testfil
				icallid= 14
				ifirst=1
				call gmsettextsetting(iedit(1),tedit(1))
				goto 2
			endif
			endif
			    ifirst=1
				tedit(1)='Enter/Browse for AXON file'
				call gmsettextsetting(iedit(1),tedit(1))
				icallid= 13
					if(iftype.eq.1) then
					!sfile=testfil
					sfile='*.abf'
				else
					sfile='*.abf'
				endif
			!	testfil='D:\axon.abf'
				goto 2
			
		else if(indt.eq.3) then
			tedit(10)='ASCII file'
			cjdat=.false.
			axfile=.false.
			consm=.false.
			asc=.true.
			integ=.false.
			ifirst=1
			sfile='*.txt'
				tedit(1)='Enter/Browse for ASCII file'
				call gmsettextsetting(iedit(1),tedit(1))
				sfilt='*.txt'//char(124)//'ASCII text (txt)'//char(124)//&
				'*.dat'//char(124)//'ASCII file (dat)'//char(124)//&
            '*.*'//char(124)//'All Files'
			 CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GinPUT, &
			gmTitle='ASCII files')
			if (sfile.ne.'     ') then
				ascfil=sfile
				INQUIRE (FILE=ascfil,EXIST=PRESENT)
			
				i_var=0


				if(PRESENT) then
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(ascfil, info, ihandle)
				nLEN=info%length 
				if(nlen.eq.0) then
					goto 1
				endif
				ilenint=nlen
				nig=len_trim(ascfil)
				imes=gmdisplaymessagebox('','The file:'//ascfil(1:nig+1)//&
				' will be converted to Consam format',ginformation,gok)
                sfile='*.ssd'
                sfilt='*.ssd'//char(124)//'Comsam file (ssd)'//char(124)//&
				'*.dat'//char(124)//'Data files (dat)'//char(124)//&
                '*.*'//char(124)//'All Files'
                CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GoutPUT, &
				gmTitle='Convert to CONSAM format file')
                if (sfile.ne.'     '.or.sfile.ne.'*.ssd') then
					testfil=sfile
                    !   call WRITCSAM(intgfil,testfil,cdate,ctime)
                    filt1=0.0
					inchan=1

				!	adctime=ctime(1:8)
					idt=0
					id1=0
					id2=0
					cs(1:3)='H  '              
                    integ=.false.

                    consm=.true.
                    call gmsettextsetting(iedit(1),sfile)
					tedit(1)=sfile
					ifilt_opt=-1
					ipatch=1
					tvalout(19)=1
					nbyte=nlen		!NB ilen is integer*4
                    tvalout(4)=100000
					ioff16=0
					tvalout(14)=ioff16
					nint=(nbyte-ioff16)/2
					tvalout(20)=nint
					nbuf=1000000
					tvalout(10)=10000
					tvalout(5)=100
					tvalout(6)=21
					tvalout(1)=nlen
					!calfac=0.000508626
					pAv=2.
					tvalout(9)=pAv
					calfac=pav/adcv
					tvalout(14)=0.0
					teditout(5)=cdate
					teditout(4)='ASCII'
					teditout(6)='Tape'
					ioffn=0
	                ilinhead=-1
	                ncols1=10
	                nrows1=10
	                dcfile=.false.
	                lentext=0
	                call ASCREAD1(ioffn,ilinhead,ncols1,nrows1,nlen,&
                    ascfil,colhead,text,lentext,dcfile)
                    nd2=ncols1
                    nd1=nrows1
                    if(allocated(xnum)) deallocate(xnum)

	                ALLOCATE(xnum(nd1,nd2))
                    call ASCREAD2(ioffn,ncols1,nrows1,nlen,ascfil,xnum,nd1,nd2)
                    continue
                    OPEN(unit=11,file=ascFIL,status='UNKNOWN',access='DIRECT',form='BINARY',RECL=1)
                    read(11,REC=1) text
	                idatcal=.false.
                    if(text(1:10).eq.'"Time (s)"') then
                        idatcal=.true.
                        do i=11,19
                            if(text(i:i).eq.'"') indb=i
                            if(text(i:i).eq.'k') inde=i
	                    enddo
	                    call chtoreal(text(indb+1:inde-1),filt)
	                    nlen=nrows1*2
	                    nbyte=nlen
	                    nint=nrows1
	                    tvalout(20)=nint
	                    tvalout(10)=filt*1000.0
	                    tvalout(1)=nlen
	                    srate=round(((xnum(3,1)-xnum(2,1))*1e6),1)
	                    tvalout(4)=srate
	                endif
	                CLOSE(unit=11)
                    
					call fillout(iform,ifillwin,testfil,ieditout,teditout,ivalout,tvalout,-1,ascfil)
                  !  icallid=17
                   ! GOTO 2
                 else
					goto 1
				 endif
				
				endif
			else
			    goto 1
			endif
		else if(indt.eq.4) then
			cjdat=.false.
			axfile=.false.
			consm=.false.
			asc=.false.
			integ=.true.
			edr=.false.
			ifirst=1
			sfile=' '
			tedit(10)='16 bit integer data converted to consam file'
            sfilt='*.dat'//char(124)//'Data files (dat)'//char(124)//&
			'*.edr'//char(124)//'EDR files (edr)'//&
			char(124)//'*.*'//char(124)//'All Files'
            CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GinPUT, &
			gmTitle='Input Binary File')
            if (sfile.ne.'     ') then
				intgFIL=SFILE
                nl=len_trim(intgfil)
				INQUIRE (FILE=intgfil,EXIST=PRESENT)
			
				i_var=0


				if(PRESENT) then
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(intgfil, info, ihandle)
				nLEN=info%length 
				if(nlen.eq.0) then
					goto 1
				endif
				ilenint=nlen
				nig=len_trim(intgfil)
				imes=gmdisplaymessagebox('','The file:'//intgfil(1:nig+1)//&
				' will be converted to Consam format',ginformation,gok)
                sfile='*.ssd'
                sfilt='*.ssd'//char(124)//'Comsam file (ssd)'//char(124)//&
				'*.dat'//char(124)//'Data files (dat)'//char(124)//&
                '*.*'//char(124)//'All Files'
                CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GoutPUT, &
				gmTitle='Convert to CONSAM format file')
                if (sfile.ne.'     '.or.sfile.ne.'*.ssd') then
					testfil=sfile
                    !   call WRITCSAM(intgfil,testfil,cdate,ctime)
                    filt1=0.0
					inchan=1

				!	adctime=ctime(1:8)
					idt=0
					id1=0
					id2=0
					cs(1:3)='H  '              
                    integ=.false.

                    consm=.true.
                    call gmsettextsetting(iedit(1),sfile)
					tedit(1)=sfile
					ifilt_opt=-1
					ipatch=1
					tvalout(19)=1
					nbyte=nlen		!NB ilen is integer*4
                    tvalout(4)=100000
					ioff16=0
					tvalout(14)=ioff16
					nint=(nbyte-ioff)/2
					tvalout(20)=nint
					nbuf=1000000
					tvalout(10)=10000
					tvalout(5)=100
					tvalout(6)=21
					tvalout(1)=nlen
					pav=3.33
					calfac=0.000508626
					tvalout(9)=pav
					adcv=6553.6
					calfac=pav/adcv
					tvalout(14)=0.0
					teditout(5)=cdate
					teditout(4)='16 bit'
					teditout(6)='Tape'
					call fillout(iform,ifillwin,testfil,ieditout,teditout,ivalout,tvalout,-1,intgfil)
                  !  icallid=17
                   ! GOTO 2
                 else 
					goto 1
				 endif
				 else
                     GOTO 1
                 endif
			else
				goto 1
			endif
		else if(indt.eq.5) then
			cjdat=.true.
			axfile=.false.
			consm=.false.
			asc=.false.
			integ=.false.
			ifirst=1
			
		else if(indt.eq.6) then
		    tedit(10)='ASCII file'
			cjdat=.false.
			axfile=.false.
			consm=.false.
			asc=.false.
			integ=.false.
			edr=.true.
			ifirst=1
			sfile='*.edr'
				tedit(1)='Enter/Browse for EDR file'
				call gmsettextsetting(iedit(1),tedit(1))
				sfilt='*.edr'//char(124)//'EDR file (edr)'//char(124)//&
				'*.dat'//char(124)//'ASCII file (dat)'//char(124)//&
            '*.*'//char(124)//'All Files'
			 CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GinPUT, &
			gmTitle='ASCII files')
			if (sfile.ne.'     ') then
				edrfil=sfile
				INQUIRE (FILE=edrfil,EXIST=PRESENT)
			
				i_var=0


				if(PRESENT) then
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(edrfil, info, ihandle)
				nLEN=info%length 
				if(nlen.eq.0) then
					goto 1
				endif
				ilenint=nlen
				nig=len_trim(edrfil)
				imes=gmdisplaymessagebox('','The file:'//edrfil(1:nig+1)//&
				' will be converted to Consam format',ginformation,gok)
                sfile='*.ssd'
                sfilt='*.ssd'//char(124)//'Comsam file (ssd)'//char(124)//&
				'*.dat'//char(124)//'Data files (dat)'//char(124)//&
                '*.*'//char(124)//'All Files'
                CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GoutPUT, &
				gmTitle='Convert to CONSAM format file')
                if (sfile.ne.'     '.or.sfile.ne.'*.ssd') then
					testfil=sfile
                    !   call WRITCSAM(intgfil,testfil,cdate,ctime)
                    filt1=0.0
					inchan=1

				!	adctime=ctime(1:8)
					idt=0
					id1=0
					id2=0
					cs(1:3)='H  '              
                    integ=.false.

                    consm=.true.
                    call gmsettextsetting(iedit(1),sfile)
					tedit(1)=sfile
					ifilt_opt=-1
					ipatch=1
					tvalout(19)=1
					nbyte=nlen		!NB ilen is integer*4
                    tvalout(4)=100000
					ioff16=0
					tvalout(14)=ioff16
					nint=(nbyte-ioff16)/2
					tvalout(20)=nint
					nbuf=1000000
					tvalout(10)=10000
					tvalout(5)=100
					tvalout(6)=21
					tvalout(1)=nlen
					!calfac=0.000508626
					pAv=2.
					tvalout(9)=pAv
					calfac=pav/adcv
					tvalout(14)=0.0
					teditout(5)=cdate
					teditout(4)='EDR'
					teditout(6)='Tape'
					
	                dcfile=.false.
	                call READ_EDR(edrfil,nint,ioffIN,adcmax,ad,&
                    nchan,id1,id2,calfac,srate,filt,vpa,pav,ioff0,gainfac,&
                    calfac1,srate1,filt1,vpa1,pav1,ioff1,gainfac1)
                    IOFFin=2048
	                tvalout(14)=ioffin
					tvalout(20)=nint
                    tvalout(9)=pAv
                    tvalout(10)=filt
                  !!!  adcv=(adcmax/(ad))
					calfac=pav/adcv
					tvalout(4)=srate
					INCHAN=INT2(NCHAN)
					call fillout(iform,ifillwin,testfil,ieditout,teditout,ivalout,tvalout,-1,edrfil)
                else
					goto 1
				 endif
				
				endif
			else
			    goto 1
			endif  
                 
		else if(indt.eq.7) then
			sfilt='*.*'//char(124)//'All Files'
			axfile=.false.
			cjdat=.false.
			consm=.false.
			asc=.false.
			integ=.false.
			tedit(10)='Acquire file'
			if(ifirst.eq.-1.and.iftype.eq.2) then
			    INQUIRE (FILE=testfil,EXIST=PRESENT)
			    i_var=0
			    if(PRESENT) then
				tedit(1)=testfil
				icallid= 14
				ifirst=1
				call gmsettextsetting(iedit(1),tedit(1))
				goto 2
			    endif
			endif
			ifirst=1
			tedit(1)='Enter/Browse for file'
			call gmsettextsetting(iedit(1),tedit(1))
			CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GinPUT, &
			gmTitle='ASCII files')
			if (sfile.ne.'     ') then
				testfil=sfile
				INQUIRE (FILE=testfil,EXIST=PRESENT)
			
				i_var=0


				if(PRESENT) then
				    ihandle=FILE$FIRST
				    length = GETFILEINFOQQ(testfil, info, ihandle)
				    nLEN=info%length 
				    if(nlen.eq.0) then
					goto 1
				    endif
				    ilenint=nlen
				    icallid= 40
				
				    goto 2 
			    endif
		
		    endif
		endif
		ipini=1
		
	case(2,3) ! printout
		append=.false.
		if(icallid.eq.2) append=.true.
		discprt=.true.
		
	case(4)
		append=.false.
		discprt=.false.
	case(5)
		
		CALL gmFileBrowser(pFILE,pDIR,pFILT,gmType=GinPUT, &
				gmTitle='Print files')
		continue
		nl=len_trim(pdir)
		pfile=pdir(1:nl)//'\'//pfile
		if(icprev.ne.109) call gmsettextsetting(initext,pfile)
	case(50)
		
		CALL gmFileBrowser(iniFILE,inipath,inidef,gmType=GinPUT, &
				gmTitle='Ini files')
		continue
		if(inifile.ne.'') then
		nl=len_trim(inipath)
		if(nl.gt.1) then
		inifile=inipath(1:nl)//'\'//inifile
		endif
		else
		  icallid=50
		  goto 2
		endif
			if(icprev.ne.109) call gmsettextsetting(initext1,inifile)
	case(6) ! exit 102
		imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
		if(imes.eq.gnobutton) then
			goto 3
		else
			comm='c:\windows\notepad '// pfile
			res=systemqq(comm)
			goto 3
		endif
	case(7) ! save 101
		do i=2,6
			call gmenqtextsetting(iedit(i),tedit(i))
		
		enddo
		
		do i=2,6
			tval(i)=gmenqvaluesetting(ival(i))
		enddo
		do i=9,13
			if(i.ne.12) tval(i)=gmenqvaluesetting(ival(i))
		enddo
			    
				cdate=tedit(2)
				adctime=tedit(3)
				
			title=tedit(4)
				expdate=tedit(5)
				tapeID=tedit(6)
					ilen=2*tval(2)

				inchan=tval(3)
					srate=tval(4)
					emem=tval(5)  
				temp=tval(6)

				calfac=tval(9)
				filt=tval(10)
			                

				filt1=tval(13)
				calfac1=tval(12)
			
				CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GoutPUT, &
				gmTitle='Consam files')
				if (sfile.ne.'     ') then
					ADCFIL1=SFILE
					if(testfil.ne.adcfil1) then
				    OPEN(unit=15,file=ADCFIL1,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
				    IF(I_VAR.EQ.0) continue
				
				    write(15,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
				    id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
				    expdate,defname,tapeID,ipatch,npatch,Emem,temp
				    if(i_var.ne.0) continue
				
					open(unit=14,file=testFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
				    if(allocated(idata)) deallocate(idata)
				    allocate(idata(nint))
				    if(axfile.and.idataformat.eq.1) then
				        if(allocated(idata)) deallocate(idata)
				        allocate(data1(nint))
                        read(14,rec=ioff+1) (data1(i),i=1,nint)
                        do ik=1,nint
                            idata(ik)=int2(data1(ik))
                        enddo
                    else
				        
			            read (14,REC=513,IOSTAT=I_VAR) (idata(i),i=1,nint)
			        endif
			     	close(unit=14)
			        
				    write(15,REC=513,IOSTAT=I_VAR) (idata(i),i=1,nint)
				
				iStatics = gmCreateTextEntry(iPanel, 1, 3, 12, 1,'Saved as   :'//sfile, 32768, GDISPLAY, &
              	gmType=GSTANDARD, gmFont=GDEFAULT, gmJustify=GLEFT, &
             	gmBack1Col=0, gmBack2Col=0, gmTextCol=5, &
				gmHpos=Gleft, gmVpos=GTOP, gmExpand=GOFF)

		 			
				close(unit=15)	
				else
				
				endif	
				else
					goto 1
				endif
				
	
				
	
    case(8) ! help !103
		call plothelp(iform)
	CASE(9) ! browse again for file
         itcall=-5
		 text_tog(1)='Continuous data from CONSAM file'
		 text_tog(2)='Continuous data from AXON file'
         text_tog(5)='CJUMP data'
         text_tog(3)='ASCII file data'
         text_tog(4)='16 bit integer binary file data'
         text_tog(6)='EDR data'
         text_tog(7)='Other files'
         ifirst=0
         ntog=6
         if(itogrec.le.0.and.itogrec.ge.6) itogrec=1
         call plot_ini(iform,initwin,ntog,text_tog,intoggle,itcall,itogbutton,pfile,&
		 initext,inifile,initext1,append,itogrec)

          continue
	case(10,11,12,13,14,17) ! open consam,axon,converted binary
		
		if(readini) idir=1
		if(icallid.eq.11.or.icallid.eq.13) then
			
				if(idir.eq.-1) then 
					call gEnqWorkingDir(wdir)
					idir=1
					SDIR=wdir
				endif
				CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GinPUT, &
				gmTitle='Consam files')
				if (sfile.ne.'     ') then
					nb=len_trim(sfile)
				ilp=0
				do i=1,nb
					if(sfile(i:i).eq.'\') ilp=i
				enddo
				if(ilp.gt.1) then
					ADCFIL=SFILE(ilp+1:nb)
				else

					ADCFIL=SFILE
				endif
			
				    iplen = FULLPATHQQ(adcfil, testfil)
		 			call gmsettextsetting(iedit(1),testfil)
					tedit(1)=sfile
				else
					goto 1
				endif	


		else
				call gmenqtextsetting(iedit(1),sfile)
				if (sfile.ne.'     ') then
				nb=len_trim(sfile)
				do i=1,nb
					if(sfile(i:i).eq.'\') ilp=i
				enddo
				if(ilp.gt.1) then
					ADCFIL=SFILE(ilp+1:nb)
				else

					ADCFIL=SFILE
				endif
					! iplen = FULLPATHQQ(adcfil, testfil)
					testfil=sfile
					tedit(1)=sfile
				else
					goto 1
				endif
		endif
		INQUIRE (FILE=testfil,EXIST=PRESENT)
			
			i_var=0


			if(PRESENT) then
				nlen=0
				ihandle=FILE$FIRST
				length = GETFILEINFOQQ(testfil, info, ihandle)
				nLEN=info%length 

				if(consm.and.nlen.gt.0) then
				    good=.true.
				    nint=(nlen-512)/2
				    OPEN(unit=14,file=testFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
				    IF(I_VAR.EQ.0) continue
				    READ(14,REC=1,IOSTAT=I_VAR) iver
				    if(iver.eq.1002) then
				        READ(14,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
				        id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
				        expdate,defname,tapeID,ipatch,npatch,Emem,temp
				    else
				        read(14,rec=1) title1,cdate,adctime,idt,ioff,ilen,inchan,id1,id2,cs(1:3),&
				        calfac,srate,filt,filt1,calfac1,iver
				        title=title1(1:70)
				    endif
				    close(unit=14)
				    if((nlen-512).ne.ilen) then
					    imsi=gmdisplaymessagebox('','This might not be a proper CONSAM file; Continue?',gquestion,gyesno)
					    if(imsi.eq.gnobutton) then
                            goto 1
					    endif
				    endif
				else
					call READAXON(testfil,title1,cdate,adctime,ilen,inchan,calfac,pAV,srate,&
					filt,iver,idataformat,good,ioff)
					if(.not.good) goto 1
!				
					faccal=pAV/calfac	!to convert calfac if pAV corrected
	               
					idt=0
					id1=0
					id2=0
					cs(1:3)='H  '
					iver=1002	!new format header
					filt1=0.0
					calfac1=-1.0
					title=title1(1:70)
				!	ioff=nlen-ilen
					temp=21
				endif
				nsam=ilen/2
				if(axfile.and.idataformat.eq.1)nsam=ilen/4
				nint=nsam
				dtx=1000./srate		!ms between points
				tsamlen=float(nsam)/srate	!seconds
				if(consm) then
					pAV=calfac*adcv
					pAV1=calfac1*adcv
				endif
				np1=1
				np2=nsam
				i1=np1
				i2=np2
				tval1(1)=np1
				tval1(2)=np2
				tval2(1)=np1
				tval2(2)=np2
					
				if(inchan.lt.1.or.inchan.gt.3) inchan=1
				if(cctrig(1:3).ne.'H'.or.cctrig(1:3).ne.'HT') cctrig='H'
			
				tedit(1)=testfil
				tedit(2)=cdate
				tedit(3)=adctime
				tedit(4)=title
				
				tedit(5)=expdate
				tedit(6)=tapeID
				ptype(1)='Outside-out'
					
				ptype(2)='Inside-out'
					
				ptype(3)='Cell attached'
					
				ptype(4)='Whole cell'
					
				ptype(5)='Simulated data'
			    call gmSetListEntry(iCombo2,GSELECT,gmEntry=ipatch)
				tval(1)=nlen
			!	if(iver.ne.1002) iver=1002
				tval(2)=nsam
				tval(3)=inchan
				if(ioff.lt.512) ioff=512
				tval(4)=srate
				tval(5)=emem
				tval(6)=temp
				tval(8)=pav
				tval(9)=calfac
				tval(10)=filt
				tval(11)=pav1
				tval(12)=calfac1
				tval(13)=filt1
				if(ipatch.eq.0) ipatch=1
				if(discprt.and.icprev.ne.109) write(8,28) testfil,expdate,npatch,tapeID,ptype(ipatch),Emem,temp
		!		if(discprt) write(8,1001) cdate,text1,text2,testfil,expdate,tapeID,ptype(ipatch)
			
				tval1(11)=2
		!		if(icprev.eq.109) goto 76
				tval1(3)=1.0
				call plots(iform,ipanelp,ipanel10,ival1,tval1,iTogg11,itogg12,iTogg13,itogg14,iTogg15,&
				ibutt1,itext3,itext4,itext5,itext6,itoggle01,&
				itoggle02,itoggle03,itoggle04,poplot,ngrid,ibutton11,ibutton21) 
if(poplot) then
	call gmSetToggleSwitch(itogg11,gon)
	call gmSetToggleSwitch(itogg12,goff)

else
		call gmSetToggleSwitch(itogg11,goff)

endif
	call gmSetToggleSwitch(itoggle0,gon)
!call gmSetToggleSwitch(itogg12,gon)
76	continue
				call gmsettextsetting(iedit(10),tedit(10))
				
				do i=2,6
				call gmsettextsetting(iedit(i),tedit(i))
				enddo
				do i=1,6
				call gmsetvaluesetting(ival(i),tval(i))
				enddo
				do i=8,13
				call gmsetvaluesetting(ival(i),tval(i))
				enddo
			
				!	if(icprev.eq.109) goto 77			
				if(filtsam) then
				do i=1,5
				
				call gmsetvaluesetting(ival2(i),tval2(i))
				enddo
				else
				do i=1,4
				if(i.ne.5) call gmsetvaluesetting(ival1(i),tval1(i))
			
				
				enddo
				call gmsetvaluesetting(ival1(11),tval1(11))
				call gmsetvaluesetting(ival1(10),tval1(10))
				endif
					if(nlen.le.ilen) then
					jkstatus=gmdisplaymessagebox('','Bad file.Try another one',gstop,gok)
					goto 1
				endif
				call gmsetwidgetstatus(ibutt1,GSELECTABLE)
			!	if(icprev.eq.109) call gmsetwidgetstatus(ibutt1,GunSELECTABLE)
			!	call gmsetwidgetstatus(itogg15,GSELECTABLE)
77				continue		
				ifi=1
			if(icprev.eq.109) then
		
				icallid=107
			
				goto 2
			endif
			else
				goto 1	
			endif
	case(40) !other files
	    call read_other(testfil,title,ilen,inchan,calfac,pAV,srate,ffilt,iver,ioff)
	    
	
	case(15,16) ! ascii?
	   ioffn=0
	   ilinhead=-1
	   ncols1=10
	   nrows1=10
	   dcfile=.false.
	   lentext=0
	   call ASCREAD1(ioffn,ilinhead,ncols1,nrows1,nlen,&
       ascfil,colhead,text,lentext,dcfile)
       nd2=ncols1
       nd1=nrows1
       if(allocated(xnum)) deallocate(xnum)

	   ALLOCATE(xnum(nd1,nd2))
       call ASCREAD2(ioffn,ncols1,nrows1,nlen,ascfil,xnum,nd1,nd2)
       continue
       OPEN(unit=11,file=DATFIL,status='UNKNOWN',access='DIRECT',form='BINARY',RECL=1)
       read(11,REC=1) text
	   idatcal=.false.
       if(text(1:10).eq.'"Time (s)"') then
        idatcal=.true.
        
	   endif
	   CLOSE(unit=11)
       
	case(18)
		readini=.true.
	case(19)
		readini=.false.
		call gmsettoggleswitch(intoggle(11),goff)
		call gmsettoggleswitch(intoggle(12),gon)
	case(20) !40
        call gmremovewindow(ipanelf)
		tval1(11)=2
		tval2(7)=0.
		tval2(8)=0.
		tval1(3)=1.
		call plots(iform,ipanelp,ipanel10,ival1,tval1,iTogg11,itogg12,iTogg13,itogg14,iTogg15,&
		ibutt1,itext3,itext4,itext5,itext6,itoggle01,itoggle02,itoggle03,itoggle04,&
		poplot,ngrid,ibutton11,ibutton21)
		FILTSAM=.FALSE.
		call gmsetwidgetstatus(ibutt1,GSELECTABLE)
		if(readini) then
			icallid=107
			goto 2
		endif
	case(21,22,23) ! filtsam 51,52,53
!		call gmremovewidget(ipanelp)
	if(filtsam) then
		call gmremovewindow(ipanelf)
	else
		call gmremovewindow(ipanelp)
	endif
	    nl=len_trim(testfil)
		tedit(9)=testfil(1:nl-4)//'-1.ssd'
		tval2(6)=1000.
		FC=TVAL2(6)
		call filts(iform,ipanelf,ipanel20,ipanel21,iedit,tedit,ival2,tval2,itogg17,itogg18,itogg19,&
		itogg16,itogg21,itogg22,ibutt3,ibutt4,ibutt5,icallid,ibuttonf)
		if(nlen.le.ilen) then
			jkstatus=gmdisplaymessagebox('','Bad file.Try another one',gstop,gok)
			goto 1
		endif
	!	call gmsetwidgetstatus(itogg16,GSELECTABLE)
		FILTSAM=.TRUE.
		ifilt_opt=icallid-20
	
		if(ifilt_opt.eq.3) then
			idelt=1
			tval2(10)=1
			noutt=0		!total # of points written to output
			srate1=srate
			r=srate/filt
			tval2(9)=srate1
		
			call gmsetvaluesetting(ival2(10),tval2(10))
			call gmsetvaluesetting(ival2(9),tval2(9))
				
		else if(ifilt_opt.eq.2) then
			srate1=srate	!all points
			noutt=0	
			tval2(9)=srate1
			call gmsetvaluesetting(ival2(9),tval2(9))
		
		endif
		!call gmsetwidgetstatus(ibutt5,GSELECTABLE)
    
	
	case(101) !44


		poplot=.true.
		call gmsetwidgetstatus(ival1(7),GSELECTABLE)
		call gmsetwidgetstatus(ival1(11),GSELECTABLE)
		!call gmsetwidgetstatus(itoggle01,GUNSELECTABLE)
		call gmsetwidgetstatus(itoggle02,GSELECTABLE)
		call gmsetwidgetstatus(itoggle03,GSELECTABLE)
		call gmsetwidgetstatus(itoggle04,GSELECTABLE)
	!	call gmsetwidgetstatus(itogg12,0)
	call gmSetToggleSwitch(itogg12, goff) 

	

	
		
	case(102) !45
	
		poplot=.false.
		call gmsetwidgetstatus(ival1(7),GUNSELECTABLE)
		call gmsetwidgetstatus(ival1(11),GunSELECTABLE)
		!call gmsetwidgetstatus(itoggle01,GUNSELECTABLE)
		call gmsetwidgetstatus(itoggle02,GUNSELECTABLE)
		call gmsetwidgetstatus(itoggle03,GUNSELECTABLE)
		call gmsetwidgetstatus(itoggle04,GUNSELECTABLE)
		!call gmsetwidgetstatus(itogg11,0)
		call gmSetToggleSwitch(itogg11, goff) 

	case(103) ! 1 grid 41
		ngrid=1
		pgrid(1)=0.5
		ngrid1=ngrid
		

		 
	case(104) ! 3 grid 42

		   ngrid=3
		   pgrid(1)=0.25
		   pgrid(2)=0.5
		   pgrid(3)=0.75
		   ngrid1=ngrid

	case(105) ! 30 ! calculate for display
		
		if(ifi.ne.1) then
		call gmsetvaluesetting(ival1(5),0)
		call gmsetvaluesetting(ival1(6),0)
		call gmsetvaluesetting(ival1(8),0)
		call gmsetvaluesetting(ival1(9),0)
	    endif
		ifi=-1
	
		pav=gmenqvaluesetting(ival(8))
		calfac=gmenqvaluesetting(ival(9))
		filt=gmenqvaluesetting(ival(10))
		if(poplot) then
				tval1(7)=gmenqvaluesetting(ival1(7))
				tseg=tval1(7)
				istatus1=gmEnqToggleSwitch(itoggle04)
				if(istatus1.eq.gon) then
					tval1(11)=gmenqvaluesetting(ival1(11))
					itva=int(tval1(11))
					if (itva.eq.0) then
					imsd=gmdisplaymessagebox('','No value',ginformation,gok)
					goto 1
					endif
					tval3(11)=itva
					ngrid=itva
					ngrid1=itva
					do ij=1,itva
						tval3(ij)=float(ij)/float(itva+1)
					enddo
					call grid(iform,igrid,tval3,ival3)
				else
				    istatus1=gmEnqToggleSwitch(itoggle02)
					if(istatus1.eq.gon) then
						ngrid=1
						pgrid(1)=0.5
						ngrid1=ngrid
					else
						ngrid=3
						pgrid(1)=0.25
						pgrid(2)=0.5
						pgrid(3)=0.75
						ngrid1=ngrid
					endif
					icallid=107
					goto 2
				endif
		else
			icallid=107
			goto 2
		endif
	
	
	case(106) ! grid 46 :specify
			do i=1,ngrid
				tval3(i)=gmenqvaluesetting(ival3(i))
				pgrid(i)=tval3(i)
			enddo
			ngrid1=ngrid
			icallid=107
			call gmremovewindow(igrid)
			goto 2
	
	case(107) ! 43+33 : final calculations
       ! if(icprev.eq.109) then
	!		call gmremovewindow(isampw)
		!else
		if(icprev.ne.109) then
		do i=1,4
			tval1(i)=gmenqvaluesetting(ival1(i))
		enddo
	!	jnth=1
		tval1(10)=gmenqvaluesetting(ival1(10))
		jnth=tval1(10)
		np1=tval1(1)
		np2=tval1(2)
	    secline0=tval1(3)
		ntrace=tval1(4)
        endif
		secline=secline0*1000.
		
	    npuse=np2-np1+1
		npline=ifixr(secline/dtx)		!points/line
		nppage=ntrace*npline			!points/page
		secpage=float(ntrace)*secline
		samlen1=dtx*(float(npuse-1))
		npagtot=1+ifix((samlen1-1.)/secpage)
		call intconv(npagtot,cnum)
	!	imes=gmdisplaymessagebox('','This corresponds to '//cnum//' pages for whole sample',gquestion,gyesno)
	!	if(imes.eq.gnobutton) goto 1
	!	 if(icprev.ne.109) then
		tval1(6)=npagtot
		tval1(9)=npline
		tval1(8)=float(npuse)/srate	
		call gmsetvaluesetting(ival1(6),npagtot)
		call gmsetvaluesetting(ival1(9),npline)
		call gmsetvaluesetting(ival1(8),tval1(8))
	!	endif
		if(nppage.gt.npuse) nppage=npuse
	   ! call gmSetToggleSwitch(itogg15,gon)
		ntrtot=1 + (npuse-1)/npline		!total number of traces=lines
		ntrlast=mod(ntrtot,ntrace)		!number of traces on last page
		if(ntrlast.eq.0) ntrlast=ntrace
		nptlast=npuse-npline*(ntrtot-1)	
	
		istart=np1	!to read first page in readsq1
		npage=1
	!	if(icprev) npage=npagsav
		if(allocated(idata)) DEALLOCATE(idata,yval,xval,data1,ibuf)
		ALLOCATE(idata(nppage),yval(nppage,1),xval(nppage,1),data1(nppage),ibuf(3*nppage))
		istart=np1+(npage-1)*nppage
		iend=istart+nppage-1
		if(iend.gt.np2) iend=np2
		iread=iend-istart+1		!normally=nppage
		irec=ioff + (2*istart-1)    !record # for 1st value to be read
		!if(axfile.and.idataformat.eq.1) irec=ioff + (4*istart-1)
		INQUIRE (FILE=testFIl,EXIST=PRESENT,&
         ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL)
	
		open(unit=14,file=testFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
	    if(axfile.and.idataformat.eq.1) then
            read(14,rec=irec) (data1(i),i=1,iread)
        else  if(axfile.and.inchan.eq.3) then
            read(14,rec=irec) (ibuf(i),i=1,3*iread)
            
            do i=1,iread
                idata(i)=0
                idata(i)=ibuf(3*i-2)
            enddo 
        else
		    read(14,rec=irec) (idata(i),i=1,iread)
		endif
		
		
!	    call time(iticks)
!		call gmsettextsetting(itext4,'f1:'//iticks)
		CLOSE(unit=14)
	
		ymin1=1.e37
		ymax1=-1.e37
		do i=1,iread
		    if(axfile.and.idataformat.eq.1) then
		        Yv=calfac*data1(i)
		    else
			    Yv=calfac*float(idata(i))
			endif
			yval(i,1)=yv
			if(yv.lt.ymin1) ymin1=yv
			if(yv.gt.ymax1) ymax1=yv
		enddo
		
	    ybase=yshut		!shut level in original data (no offset)

		yopen=ymin1+0.15*(ymax1-ymin1)		!initial default for poplot
		!!if(poplot) then
			offset=.false. 	!data NOT offset to zero baseline yet
			s=0.0
			do i=1,20
				s=s+yval(i,1)
			enddo
			yshut=s/20.		!guess for baseline position (or use ymin?)
			ybase=yshut		!shut level in original data (no offset)
			yopen=ymin1+0.15*(ymax1-ymin1)		!initial default for poplot
	!!	endif
	!	ytsep=1.3*abs(ymax1-ymin1)	!default
		ytsep=abs(ymax1-ymin1) ! vertical separation
		tval1(5)=ytsep
		
		if(poplot) then
			ntrace2=2*ntrace
		else
			ntrace2=ntrace
		endif

		ymin=0.0
			xmin=0.0
		xmax=secline
		dx=(xmax-xmin)/float(npline-1)
		nxval=npline
		dxop=dx
		if(npline.gt.nppage) nxval=nppage
		do i=1,nxval
			xval(i,1)=xmin + float(i-1)*dx
		enddo
	
		tval1(6)=npagtot
		call gmsetvaluesetting(ival1(5),tval1(5))
	
	
	!	call gmsetwidgetstatus(ibutton11,GSELECTABLE)
		 if(icprev.ne.109) then
		call gmsetwidgetstatus(ibutton21,GSELECTABLE)
		endif
				imode=1
			ytsep=tval1(5)
		   ! iyposv=npagtot
		if(poplot) then	!define shut and open levels
		
		!	ymax=ymin+float(ntrace2)*ytsep*1.1	!10% extra for cal bars, in pA
			ymax=ymin+float(ntrace2+1)*ytsep
			endif
		
			if(poplot) then
			if(icprev.ne.109) then
		    npseg=ifixr(tseg/dtx)
			nsegline=ifixr(float(npline)/float(npseg))	!popen segments per line
			nseglast=ifixr(float(nptlast)/float(npseg))	!popen segments on last line
			npop=1+ifixr(float(nppage)/float(npseg))
			npop=nppage
	        if(allocated(popend)) deallocate(popend,xpval) 
			ALLOCATE(popend(npop),xpval(npop))
			do j=1,npop
			popend(j)=0.0
			xpval(j)=0.0
			enddo
	        else
				if(npopen.gt.0) then
                  
				endif
		    endif
		    nsegline=0
			endif
	    if(readini) then
		!	icallid=110
		!	goto 2
		endif
		if(icprev.eq.109) then
			icallid=109
			goto 2
		endif
	case(109,108) ! 31 continue and draw graph

		keeprev=.false.
		npopen=0
		if(icallid.eq.109) then
			npage=npagsav
			imo=-1
			keeprev=.true.
			npopen=npopen1
		endif
		icallid=110
		goto 2
	case(110)
		! display sample +vertical separation
	    if(iyposv.eq.-1) then
			val(1)=gmenqvaluesetting(ivall(1))		
			if(val(1).gt.0.0 ) then
				ytsep=val(1)
				tval1(5)=ytsep
				call gmsetvaluesetting(ival1(5),tval1(5))
				call gmremovewindow(Graph1_1(iplot))
				call gmremovewindow(ivw)
			else
				goto 1
			endif
			
	    else
			if(icprev.eq.109) then
			if(ytsepin.gt.0) ytsep=ytsepin
			else
			tval1(5)=gmenqvaluesetting(ival1(5))		
			ytsep=tval1(5)
			endif
		!	iyposv=npagtot
		endif
	
		!ymax=ymin+float(ntrace2)*ytsep*1.1	!10% extra for cal bars, in pA
			ymax=ymin+float(ntrace2+1)*ytsep
	
!!	if(poplot.and.imode.ge.1) then
		if(allocated(xval1)) deallocate(xval1,yval1)
	   amp=yopen-yshut
	   amp1=amp

	   if(amp.lt.0.0) then	!check sign
		amp1=-abs(amp1)
	   endif
	   if(amp1.ne.amp) then
		yopen=yshut+amp1	!overide yopen with input value
	   endif

	   do i=1,iread
	!	yval(i,1)=yval(i,1) - yshut
	   enddo
	   offset=.true.

		if(imode.eq.2)  then
			ybase=ybase + (yshut-ysav)	
			icallid=112 !34
			goto 2
		else
			ybase=yshut		!baseline for raw data
			yopen=yopen-yshut
			yshut=0.
		endif
!!	endif
	
		icallid=111 !36
	    goto 2
	
	case(111) !36

	!	yofftop=ymax-abs(ymax1/2.)
	!	yofftop=ymax-abs(ymax1/2.)
	yofftop=ymax-abs(ymax1)
		xlth=xmax-xmin
		ylth=ymax-ymin
		deltax=xlth/disfacx
		deltay=ylth/disfacy
		dxg=(deltax-xlth)/2.
		dyg=(deltay-ylth)/2.

		wxmin=xmin-0.5*dxg
		wxmax=xmax+0.5*dxg
		wymin=ymin-0.5*dyg
		wymax=ymax+0.5*dyg

		dxsw=xlth*0.007
		dysw=ylth*0.010
	
		wid=0.5*dysw*1.000001
		ix=100
		iy=100
	
		iplot=1 ! first graph -main 
		jplot=1 !
		call graph1(iplot,jplot,iform,ix,iy,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,testfil,plot,-1,0,npagtot)
			readrec=.true.
		if(imo.eq.-1) then
			icallid=112
			else
		icallid=115 !35
		endif
		goto 2
		
case(112)  ! 34
		 call gmcleargraphicsframe(Graphics1_1(iplot))
		imo=0
		if(imode.eq.2) goto 10
		istart=np1+(npage-1)*nppage
		iend=istart+nppage-1
		if(iend.gt.np2) iend=np2
	
		iread=iend-istart+1		!normally=nppage
		irec=ioff + (2*istart-1)    !record # for 1st value to be read
	
		open(unit=14,file=testFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
		if(axfile.and.idataformat.eq.1) then
            read(14,rec=irec) (data1(i),i=1,iread)
        else
		read(14,rec=irec) (idata(i),i=1,iread)
		endif
	!	read(14,rec=irec) idata(1:iread)
		CLOSE(unit=14)
		do i=1,iread
		if(axfile.and.idataformat.eq.1) then
		    Yv=calfac*data1(i)
		else
			Yv=calfac*float(idata(i))
		endif
			yval(i,1)=yv
		
		enddo
!!		if(poplot) then
		   
!!		endif
10		icallid=115 !35
		goto 2

	case(115) ! draw graph
	do i=1,iread
			yval(i,1)=yval(i,1)-ybase
			offset=.true. 	!data NOT offset to zero baseline yet
		   enddo
	    if(imode.ne.2) call gmSetListEntry(iCombo1,GSELECT,gmEntry=npage+1)
		linetype=ijoinj
		
		ic=9
		call glincols(ic,idev)
		thj=0.0		!set line thickness=0 for screen (or too slow)
		call LINWID(thj)
		ip=0		!index for popen(ip), xpval(ip), ip=1,..., npops
		iy1=1		!element of yval to start at
	!	yoff=yofftop + ytsep	!so yoff=yofftop for first plot
	    yoff=ymax
		ntrace1=ntrace
		lastpage=npage.eq.npagtot
		if(lastpage) ntrace1=ntrlast
		call time(iticks)
	!	call gmsettextsetting(itext1,'start:'//iticks)
		!	call system_clock(icount1)
		
		do j=1,ntrace1
			yoff=yoff-ytsep
			yline(j)=yoff
			n2=npline
			if(lastpage.and.(j.eq.ntrace1)) n2=nptlast	!for last trace on last page
			jflag=0
			do i=1,n2,jnth
				xv=xval(i,1)
				i1=i+iy1-1
			
				yv=yoff + yval(i1,1)     !NB j=index of loop!
				if(xv.ge.xmin.and.xv.le.xmax.and.yv.ge.ymin.and.yv.le.ymax)then
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv)   !join with line from last point
				!	call gdrawpixel(ix,iy,11)
					endif
				jflag=1
				endif
			enddo

			iy1=iy1+npline
			if(poplot) then
		
				yoff=yoff-ytsep
			!	yline(j)=yoff
				yboxtop=yoff+0.2*ytsep		!top of box (Popen=1)
				yboxh=0.8*ytsep		!box height
				yboxbot=yboxtop-yboxh	!bottom of box (Popen=0)
				call broken(0)
				call movto2(0.,yboxtop)
				call linto2(0.,yboxbot)
				call linto2(xmax,yboxbot)
				call linto2(xmax,yboxtop)
				call linto2(0.,yboxtop)
				call broken(2)
				do i=1,ngrid
					y=yboxbot+pgrid(i)*yboxh
					call movto2(0.,y)
					call linto2(xmax,y)
				enddo
				call broken(0)
				xv=0.		!each line starts at x=0, and goes up to xmax
				call glincols(12,idev)
				xv=0.		!each line starts at x=0, and goes up to xmax
			
				ns0=nsegline
				if(lastpage.and.(j.eq.ntrace1)) ns0=nseglast	!for last trace on last page
				do io=1,ns0
					ip=ip+1
					yv=yboxbot + popend(ip)*yboxh
					if(io.eq.1) then
					call movto2(xv,yv)
					else
					call linto2(xv,yv)
					endif
					if(xv.le.secline) then
					call linto2(xv+tseg,yv)
					else
						call linto2(secline,yv)
					endif
					xv=xv+tseg
				enddo
				
				call glincols(ic,idev)
				if(readini) then
				endif
			endif
		enddo		!end of j=1,ntrace1
		if(keeprev.or.newprev) then
		do k=1,npopen
			jflag=0
			nline=0
			if(npag(k,1).eq.npage) then
			    nline=1+indf(k,2)/npline
				nline0=nline
				jx=indf(k,1)
				jy=indf(k,2)
				call glincols(12,0)
				do l=1,npopointsf(k)
				    xv=jx*dtx+secline-secline1
					
					yv=yval(jy,1)+yline(nline)
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv) 

					endif
					jflag=1
				
					jx=jx+1
					jy=jy+1
					if(xv.gt.secline) then 
					    nline=nline+1
						jx=0
						jflag=0
					endif
					if(jy.gt.nppage) goto 552 
				enddo
				goto 552
			endif
			
			if(npag(k,2).eq.npage) then
				nline=1
				nline0=nline
				jx=0
				jy=1
				call glincols(12,0)
				do l=nppage-indf(k,2),npopointsf(k)
				    xv=jx*dtx+secline-secline1
					
					yv=yval(jy,1)+yline(nline)
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv) 

					endif
					jflag=1
				
					jx=jx+1
					jy=jy+1
					if(xv.gt.secline) then 
					    nline=nline+1
						jx=0
						jflag=0
					endif
					if(jy.gt.nppage) goto 552 
				enddo
				goto 552
		
			endif
552			continue
		enddo
		continue
		endif
	    call glincols(ic,0)
		goto 551
		if(npopen.gt.0) then
					do m=1,npopen
						if(i1.eq.indf(m,2).and.npag(m,1).eq.npage)then
							call glincols(12,idev)
							m0=i1
							xvi=xv
							goto 44
						endif
					enddo
44					continue
                    if((xv-xvi).gt.tsegf(npopen)) then
						call glincols(ic,idev)

					endif
					
		endif
551	continue
        
		call time(iticks)
	!	call gmsettextsetting(itext2,'finish:'//iticks)
		if(doframe) call DFRAME(xmin,xmax,ymin,ymax,48) 
		call gFlushGraphics()
		IF(POPLOT.and..not.readini) THEN
		IMES=gmdisplaymessagebox('','Now choose a segment to calculate Popen',&
		ginformation,gok)
		ENDIF
	
	case(121) ! 80 doframe
		if(doframe) then
			doframe=.false.
			call glincols(0,idev)
			call DFRAME(xmin,xmax,ymin,ymax,0) 
			call gFlushGraphics()
		else
		    call glincols(48,idev)
			doframe=.true.
			call DFRAME(xmin,xmax,ymin,ymax,0) 
			call gFlushGraphics()
		endif

	
	case(122) ! 80 view page
	   do i=2,npagtot+1
	         ifsta=0
			ifsta=gmEnqListEntry(iCombo1,i,textcombo)
			if(ifsta.eq.2) then
				npage=i-1
				call gmsettextsetting(itext1,'start:')
			call gmsettextsetting(itext2,'finish:')
				icallid=112 !34
				goto 2
			endif
		enddo
	case(123) ! 83 next page
		if(npage.lt.npagtot) then
			npage=npage+1
			icallid=112 !34
			call gmsettextsetting(itext1,'start:')
			call gmsettextsetting(itext2,'finish:')
			goto 2
		else
			imes=gmdisplaymessagebox('','This is the last page',ginformation,gok)
		endif
	case(124) ! 84 previous page
		if(npage.gt.1) then
			npage=npage-1
			icallid=112 !34
			call gmsettextsetting(itext1,'start:')
			call gmsettextsetting(itext2,'finish:')
			goto 2
		else
				imes=gmdisplaymessagebox('','This is the first page',ginformation,gok)

		endif
		
	case(125) !vertical sep 81
		itype(1)=9
		text_tog(1)=' Vertical separation of traces (pA) '
        val(1)=ytsep
		
		iyposv=-1
	
		call value_table(iform,ivw,1,text_tog,ivall,val,110,itype)
	
	
	
	case(126) ! 110  
			!text_tog(1)=' From page '
			text_tog(1)=' Plot any nth point: '
			val(1)=jnth
			itype(1)=0
	
		call value_table(iform,ivwin,1,text_tog,ivall,val,127,itype)
	case(127) ! change nr points
			val(1)=gmenqvaluesetting(ivall(1))	
			if(val(1).lt.1) val(1)=1
			jnth=val(1)
			!tval1(10)=jnth
			call gmremovewindow(ivwin)
			call gmcleargraphicsframe(Graphics1_1(iplot))
			call gmsettextsetting(itext1,'start:')
			call gmsettextsetting(itext2,'finish:')
			icallid=115
			goto 2
	
	
	case(128,129) !print 85,86
	!	ntog=3
		if(icallid.eq.129) then
			pwmf=.true.
			
	   			idpi=600
	   			ixoff=0
	   			iyoff=0
	   			iwidi=4800
	   			ihei=3300 
		endif
	!	call toggle_panel(iform,ITOGGLE,ittoglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton,itype)
        indt=1
		indt2=1
		nq=len_trim(testfil)
		if(nq.gt.3) then
			do j=1,nq
				if(testfil(j:j).eq.'\') nqf=j
			enddo
			
		endif
		call print_options(iform,initwin)
	
	case(131,132) !871,872
	    indt=icallid-130
	case(133,134,135)	!873,874,875
		indt2=icallid-132
	case(136) ! 87 print options continue
		plot=.true.
	
		call gmremovewindow(initwin)
		npageref=npage
		if(indt.eq.1) then
			idev=6
			if(pwmf) idev=1
			ic=9
		else
			idev=3
			ic=48
		endif	
		if(indt2.eq.1) then
			npage1=npage
			npage2=npage
		    icallid=138
			goto 2
		else if (indt2.eq.2) then
			itype(1)=0
			itype(2)=0
				npage1=1
			npage2=npagtot	
			text_tog(1)=' From page '
				text_tog(2)=' To page '
			val(1)=npage1
			val(2)=npage2
		
	
		call value_table(iform,ivw,2,text_tog,ivall,val,137,itype)
		else
			npage1=1
			npage2=npagtot	
			icallid=138 !89
			goto 2
		endif
		
	case(137) !88
	    do i=1,2
		val(i)=gmenqvaluesetting(ivall(i))	
		enddo
		npage1=val(1)
		npage2=val(2)
		call gmremovewindow(ivw)
		icallid=138
		goto 2
	case(138) !89
		plot=.true.
        if(plot) then
			if(.not.pwmf) then
				istatus=gmprintercontrol(gprintersetup)
				if(istatus.eq.0) goto 4488
				istatus=gmprintercontrol(GOPENPRINTER)
					
				IF(ISTATus.Eq.0)THEN
					plot=.false.
					CALL GUIPRT(1,ISTATus)
					imessy=gmDisplayMessageBox('','Printer not available',Gexclamation,gok)
					goto 1
				endif
			endif
		endif
		npagref=npage
		do npage=npage1,npage2
			if(pwmf) then
			
				call intconv(npage,cnum)
				nlp=len_trim(adcfil)
				
				sfile=adcfil(nqf+1:nlp-4)//cnum
				nlp=len_trim(sfile)
				sfile=sfile(1:nlp)//'.wmf'
				call devsus
				call wmfp(idpi,ixoff,iyoff,iwidi,ihei)
				call devfil(sfile,0)
			endif
			
			call graph1(iplot,jplot,iform,ix,iy,graph1_1,GraphMainPanel1_1,graphics1_1,&
			wxmin,wxmax,wymin,wymax,ipos,testfil,plot,-1,iyposv,npagtot)
			istart=np1+(npage-1)*nppage
			iend=istart+nppage-1
			if(iend.gt.np2) iend=np2
			iread=iend-istart+1		!normally=nppage
			open(unit=14,file=testFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
	
			irec=ioff + (2*istart-1)    !record # for 1st value to be read
			if(axfile.and.idataformat.eq.1) then
                read(14,rec=irec) (data1(i),i=1,iread)
            else
			    read(14,rec=irec) (idata(i),i=1,iread)
			endif
		!	read(14,rec=irec) idata(i:iread)
			CLOSE(unit=14)
			do i=1,iread
			    if(axfile.and.idataformat.eq.1) then
		                Yv=calfac*data1(i)
		        else
			            Yv=calfac*float(idata(i))
			    endif
			    yval(i,1)=yv
			enddo
			if(poplot) then
				do i=1,iread
					Yval(i,1)=Yval(i,1) - ybase
				enddo
				offset=.true.		!offset to zero baseline
				imode=0
				call GETPOP(xval1,yval1,iread,ymin,ymax,xmin,xmax,yopen,yshut,offset,&
				tseg,npseg,dtx,npline,imode,nppage,iform,&
				graph1_1,graphics1_1,GraphMainPanel1_1,pixbufs,pixbufo,readrec,npopoints,indx1,indx2,&
				indy1,indy2,xi,xf,pixbuf1,pixbuf2,ntrace,ytsep,calfac,ioff,iend,np1,np2,&
				adcfil,srate,nsam,cjdat)
			endif
			iy1=1		!element of yval to start at
			ip=0
			yoff=yofftop + ytsep	!so yoff=yofftop for first plot
			ntrace1=ntrace
			lastpage=npage.eq.npagtot
			call broken(0)
			call LINWID(thjp)
			!ic=9
			call glincols(ic,idev)
			if(lastpage) ntrace1=ntrlast
			do j=1,ntrace1
			yoff=yoff-ytsep
			yline(j)=yoff
			n2=npline
			if(lastpage.and.(j.eq.ntrace1)) n2=nptlast	!for last trace on last page
			jflag=0
			do i=1,n2,jnth
				xv=xval(i,1)
				i1=i+iy1-1
			
				yv=yoff + yval(i1,1)     !NB j=index of loop!
				if(xv.ge.xmin.and.xv.le.xmax.and.yv.ge.ymin.and.yv.le.ymax)then
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv)   !join with line from last point
				!	call gdrawpixel(ix,iy,11)
					endif
				jflag=1
				endif
			enddo

			iy1=iy1+npline
			if(poplot) then
				yoff=yoff-ytsep
			!	yline(j)=yoff
				yboxtop=yoff+0.2*ytsep		!top of box (Popen=1)
				yboxh=0.8*ytsep		!box height
				yboxbot=yboxtop-yboxh	!bottom of box (Popen=0)
				call broken(0)
				call movto2(0.,yboxtop)
				call linto2(0.,yboxbot)
				call linto2(xmax,yboxbot)
				call linto2(xmax,yboxtop)
				call linto2(0.,yboxtop)
				call broken(2)
				do i=1,ngrid
					y=yboxbot+pgrid(i)*yboxh
					call movto2(0.,y)
					call linto2(xmax,y)
				enddo
				call broken(0)
				xv=0.		!each line starts at x=0, and goes up to xmax
				call glincols(12,idev)
				xv=0.		!each line starts at x=0, and goes up to xmax
			
				call glincols(ic,idev)
			endif
		enddo		!end of j=1,ntrace1
		
		if(keeprev.or.newprev) then
		do k=1,npopen
			jflag=0
			nline=0
			if(npag(k,1).eq.npage) then
			    nline=1+indf(k,2)/npline
				nline0=nline
				jx=indf(k,1)
				jy=indf(k,2)
				call glincols(12,0)
				do l=1,npopointsf(k)
				    xv=jx*dtx+secline-secline1
					
					yv=yval(jy,1)+yline(nline)
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv) 

					endif
					jflag=1
				
					jx=jx+1
					jy=jy+1
					if(xv.gt.secline) then 
					    nline=nline+1
						jx=0
						jflag=0
					endif
					if(jy.gt.nppage) goto 52 
				enddo
				goto 52
			endif
			
			if(npag(k,2).eq.npage) then
				nline=1
				nline0=nline
				jx=0
				jy=1
				call glincols(12,0)
				do l=nppage-indf(k,2),npopointsf(k)
				    xv=jx*dtx+secline-secline1
					
					yv=yval(jy,1)+yline(nline)
					if(jflag.eq.0) then
					call movto2(xv,yv)   !move to 1st point in range
					else
					call broken(linetype)
	  				call linto2(xv,yv) 

					endif
					jflag=1
				
					jx=jx+1
					jy=jy+1
					if(xv.gt.secline) then 
					    nline=nline+1
						jx=0
						jflag=0
					endif
					if(jy.gt.nppage) goto 52 
				enddo
				goto 52
		
			endif
52			continue
		enddo
		continue
		endif
	    call glincols(ic,0)
		goto 511
			if(npopen.gt.0) then
					do m=1,npopen
						if(i1.eq.indf(m,2).and.npag(m,1).eq.npage)then
							call glincols(12,idev)
							m0=i1
							xvi=xv
							goto 441
						endif
					enddo
441					continue
                    if((xv-xvi).gt.tsegf(npopen)) then
						call glincols(ic,idev)

					endif
					
				endif
511	continue
		call time(iticks)
	!	call gmsettextsetting(itext2,'finish:'//iticks)
		if(doframe) call DFRAME(xmin,xmax,ymin,ymax,48) 
		
		
			yt1=ymin-0.05*(ymax-ymin) !new
			yt2=ymax+0.05*(ymax-ymin) !new
			call intconv(npage,cnum)
			call intconv(npagtot,cnum4)
			call INTCONV(istart,cnum1)
			call INTCONV(iend,cnum2)
			call realtoch(secline0,cnum3,11)
			call realtoch(ytsep,cnum5,11)
			icp=9
			call glincols(icp,idev)
		!	if(idev.ge.3) icp=48
			theg=1.35
			nl=len_trim(testfil)
			nl1=len_trim(cnum1)
			nl2=len_trim(cnum2)
			nl3=len_trim(cnum3)
			nl4=len_trim(cnum4)
			nl5=len_trim(cnum5)
			textt1='points '//cnum1(1:nl1)//' to '//cnum2(1:nl2)//&
			';seconds/line:'//cnum3(1:nl3)//';line sep [pA]:'//cnum5(1:nl5) 			
		
			textt2=testfil(1:nl)//'; page:'//cnum(1:4)//' of '//cnum4(1:nl4)//'; '//title		
			call write_string(textt1,xmin,yt1,0.,-1,101,theg,ic,dxsw,dysw)
		
			call write_string(textt2,xmin,yt2,0.,-1,101,theg,ic,dxsw,dysw)
					
			
			if(pwmf) then
				call devend
			else
				if(npage.lt.npage2) istatp=gmPrinterControl(gnewpage)
			endif
		enddo
4488	continue
		if(pwmf) then
						pwmf=.false.
						CALL DEVEND
      					CALL guiwin
				else
						CALL GUIPRT(1,ISTATus)
						plot=.false.
				
		endif
				
		idev=0
		npage=npageref

case(140) ! cancel print options continue
		plot=.false.
	
		call gmremovewindow(initwin)

case(141) !99 popen
	 if(allocated(yval0)) deallocate(yval0)
	 allocate(yval0(nppage))
	 chooseseg=.true.
	 if(modplot.le.0.or.modplot.ge.1000) modplot=1
	 call gmActivateGraphicsFrame(graphics1_1(modplot))
	 lframe=graphics1_1(modplot)
	 if(icross.eq.0) then
		call gmSetGuiCursor(lframe,Glargecross,gdefault)
		icross=1
	 else if(icross.eq.1) then
	 	call gmSetGuiCursor(lframe,Gdefault,gdefault)
		icross=0
	 endif
	 imark=0


case(142) !301 open graph to set popen
		call gmActivateGraphicsFrame(graphics1_1(modplot))
			lframe=graphics1_1(modplot)
	call gmSetGuiCursor(lframe,Gdefault,gdefault)	
	chooseseg=.false.

	if(samepag) then
		indx1=int(npline/secline*xbeg)
		indx2=int(npline/secline*xend)
		if(niline0.eq.niline) then
			if(indx1.gt.indx2) then
			iab=indx1
			indx1=indx2
			indx2=iab
			endif
			npopoints=indx2-indx1
			npopmsec=npopoints/secline
			do k=1,ntrace
			if(ybeg.lt.yline(k)+0.5*ytsep.and.ybeg.gt.yline(k)-0.5*ytsep) niline=k
			enddo
			indy1=indx1+npline*(niline-1)
			do k=1,ntrace
			if(yend.lt.yline(k)+0.5*ytsep.and.yend.gt.yline(k)-0.5*ytsep) niline=k
			enddo
			indy2=indx2+npline*(niline-1)
			npy=indy2-indy1
		else
			if(niline0.gt.niline) then
			iab=indx1
			indx1=indx2
			indx2=iab
			nil=niline
			niline=niline0
			niline0=nil
			endif
			
			
				npopmsec=npopoints/secline
				indy1=indx1+npline*(niline0-1)
				indy2=indx2+npline*(niline-1)
				npopoints=indy2-indy1
			
		endif
		
		

	endif
	
		setbase=.true.
		imode=2
		ysav=yshut
	
	  
	   k=indx1
	   l=indy1
	   xv0=xval(k,1)
	   
	   
	   if (allocated(xval1)) deallocate(xval1,yval1) 
	   allocate(xval1(npopoints,1),yval1(npopoints,1))
		do i=1,npopoints
		!	xval1(i,1)=xval(k,1)-xv0
			xval1(i,1)=(i-1)*dtx
			if(samepag) then
				yval1(i,1)=yval(l,1)
			else
				yval1(i,1)=yval0(i)
		    endif
			yb=yval1(i,1)
			ye=yval1(i,1)
			k=k+1
			l=l+1
	    enddo
		totsec=xval1(npopoints,1)
	
		call gmDefineKeySelectCallback(533,301) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(534,302) !right
		call gmDefineKeySelectCallback(535,303) !up
		call gmDefineKeySelectCallback(536,304) !down
		call gmDefineKeySelectCallback(108,301) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(114,306) !reject
		call gmDefineKeySelectCallback(82,306) !reject
		call gmDefineKeySelectCallback(65,307) !accept
		call gmDefineKeySelectCallback(97,307) !accept
		call gmDefineKeySelectCallback(13,307)
		call gmDefineKeySelectCallback(27,306)
	!	call gmDefineKeySelectCallback(-1,500)
	    zoomed=.false.
		popened=.true.
		mtrace=1
	    call GETPOP(xval1,yval1,iread,ymin,ymax,xmin,xmax,yopen,yshut,offset,&
			tseg,npseg,dtx,npline,imode,nppage,iform,&
			graph1_1,graphics1_1,GraphMainPanel1_1,pixbufs,pixbufo,readrec,npopoints,indx1,indx2,&
			indy1,indy2,xi,xf,pixbuf1,pixbuf2,mtrace,ytsep,calfac,ioff,iend,np1,np2,adcfil,srate,nsam,cjdat)
        	wxmin0p=wxmin0
		wxmax0p=wxmax0
		wymin0p=wymin0
		wymax0p=wymax0
	   
    case(152,154,157,160)
		moveline=.false.
		chooseopen=.false.
		call gmActivateGraphicsFrame(graphics1_1(modplot))
		lframe=graphics1_1(modplot)
		call gmSetGuiCursor(lframe,Gdefault,gdefault)
		indx=0	
		call gmDefineKeySelectCallback(533,0) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(534,0) !right
		call gmDefineKeySelectCallback(535,0) !up
		call gmDefineKeySelectCallback(536,0) !down
		call gmDefineKeySelectCallback(108,0) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(114,0) !right
		call gmDefineKeySelectCallback(82,0) !up
		call gmDefineKeySelectCallback(65,0) !down
			call gmDefineKeySelectCallback(97,0) !down
		call gmDefineKeySelectCallback(13,0)
	call gmDefineKeySelectCallback(13,0)
		call gmDefineKeySelectCallback(27,0)
	case(151,153,155,158,156,159)
	
		if(icallid.eq.151.or.icallid.eq.153.or.icallid.eq.155.or.icallid.eq.158) then
		moveline=.true.
		chooseopen=.false.
		if(icallid.eq.151.or.icallid.eq.153) epsi=(ymax0-ymin0)*0.01
		if(icallid.eq.155.or.icallid.eq.158) epsi=(xmax0-xmin0)*0.01
		call gmSetGuiCursor(lframe,Gdefault,gdefault)
		call gmDefineKeySelectCallback(533,301) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(534,302) !right
		call gmDefineKeySelectCallback(535,303) !up
		call gmDefineKeySelectCallback(536,304) !down
		call gmDefineKeySelectCallback(108,301) !left Sets up escape key as escape key!
	
		call gmDefineKeySelectCallback(114,306) !reject
		call gmDefineKeySelectCallback(82,306) !reject
		call gmDefineKeySelectCallback(65,307) !accept
		call gmDefineKeySelectCallback(97,307) !accept
	call gmDefineKeySelectCallback(13,307)
		call gmDefineKeySelectCallback(27,306)
		
	    else
		moveline=.false.
		choosepopen=.true.
		call gmDefineKeySelectCallback(533,0) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(534,0) !right
		call gmDefineKeySelectCallback(535,0) !up
		call gmDefineKeySelectCallback(536,0) !down
		call gmDefineKeySelectCallback(108,0) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(114,0) !right
		call gmDefineKeySelectCallback(82,0) !up
		call gmDefineKeySelectCallback(65,0) !down
			call gmDefineKeySelectCallback(97,0) !reject
		call gmDefineKeySelectCallback(13,0)
		call gmDefineKeySelectCallback(27,0)
		
		endif
		if(icallid.eq.151) then
			indx=1
			rval=yopen
			
		else if(icallid.eq.153) then
			indx=2
			rval=yshut
				
		else if(icallid.eq.155.or.icallid.eq.156) then
			indx=3
			rval=xi
			
		else
			
			indx=4
			rval=xf
		endif
		call gmActivateGraphicsFrame(graphics1_1(modplot))
			lframe=graphics1_1(modplot)
		if(choosepopen) then
		
		!	call gmSetGuiCursor(lframe,Glargecross,gdefault)
			icallid=161
			goto 2
		endif

	case(161) ! choose edge)
				
			!call gmActivateGraphicsFrame(graphics1_1(modplot))
			!lframe=graphics1_1(modplot)
			!call gmSetGuiCursor(lframe,Gdefault,gdefault)
				if(indx.eq.3) then
				x0=xi
				xi0=xi
				else if(indx.eq.4) then
				x0=xf
				xf0=xf
				else
					if(abs(xmov-xi).lt.abs(xmov-xf)) then
					x0=xi
					indx=3
					xi0=xi
					else
					indx=4
						x0=xf
				xf0=xf
					endif
				endif
				xbegp=x0-0.1*(xmax0-xmin0)
				xendp=x0+0.1*(xmax0-xmin0)
				if(xbegp.lt.xmin0) xbegp=xmin0
				if(xendp.gt.totsec) xendp=totsec
				choosepopen=.false.
				indx1p=int(xbegp/dtx)
				indx2p=int(xendp/dtx)	
				if(indx1p.gt.indx2p) then
					iab=indx1p
					indx1p=indx2p
					indx2p=iab
				endif
				
				nppoints2=indx2p-indx1p
	
				indy1p=indx1p
       			indy2p=indx2p
				if(allocated(xval2)) deallocate(xval2,yval2)
				allocate(xval2(nppoints2),yval2(nppoints2))
				do i=1,nppoints2
					xval2(i)=xval1(i+indx1p,1)
					yval2(i)=yval1(i+indy1p,1)
				enddo
				iplot=51 ! for zoom-edges
				jplot=51
				modplot=51
				
				indxl=int(x0/dtx)
				rval=x0
				moveline=.true.
				nx=10
				ny=800
				x01=x0
				yopen0=yopen
				!call getpixelarray(x0,ymin0,x0,ymax0,pixbufi,nx,ny)
				!call getpixelarray (xmin0,yopen0,xmax0,yopen0,pixbufo,800,10)
				call gmDefineKeySelectCallback(533,301) !left Sets up escape key as escape key!
				call gmDefineKeySelectCallback(534,302) !right
				call gmDefineKeySelectCallback(108,301) !left Sets up escape key as escape key!
				
				call gmDefineKeySelectCallback(13,305)
				call gmDefineKeySelectCallback(535,303) !up
				call gmDefineKeySelectCallback(536,304) !down
				call gmDefineKeySelectCallback(114,306) !reject
				call gmDefineKeySelectCallback(82,306) !reject
				call gmDefineKeySelectCallback(65,307) !accept
				call gmDefineKeySelectCallback(97,307) !accept
				call gmDefineKeySelectCallback(13,307)
		call gmDefineKeySelectCallback(27,306)
		        zoomed=.true.
				popened=.false.
				call zoom_edges(iform,iplot,jplot,graph1_1,GraphMainPanel1_1,&
				graphics1_1,xval2,yval2,nppoints2,pixbuf,x0,&
				wxminz,wxmaxz,wyminz,wymaxz,xminz,xmaxz,yminz,ymaxz,ivalpz,&
				yopen,pixbufz,ivalpz1)
			
		epsi=(xmaxz-xminz)*0.01
	case(181,182) ! reject 311
	 if(modplot.eq.0) modplot=99
		call gmremovewindow(graph1_1(51))
		zoomed=.false.
				popened=.true.
		wxmin0=wxmin0p
		wxmax0=wxmax0p
		wymin0=wymin0p
		wymax0=wymax0p
	    moveline=.false.
		chooseseg=.false.
		choosepoen=.false.
			
		modplot=99
		iplot=99 ! choose opening
		jplot=99
		call gmActivateGraphicsFrame(graphics1_1(modplot))
		lframe=graphics1_1(modplot)
		call gmSetGuiCursor(lframe,Gdefault,gdefault)	
		

		modplot=99
		call gmActivateGraphicsFrame(graphics1_1(modplot))
		
		
	
	!		call drawline(xiz,ymz-0.05*ytsep,xiz,ymz+0.05*ytsep,0,idev)
	
	!		call drawline(xfz,ymz-0.05*ytsep,xfz,ymz+0.05*ytsep,0,idev)
		
		
		if(icallid.eq.182) then

	    	call drawline(x01,ymin0,x01,ymax0,0,idev)
			call drawline(xmin0,yopen0,xmax0,yopen0,0,idev)
			call drawpixelarray(x01,ymin0,x01,ymax0,pixbufi,10,800)
		    call drawpixelarray (xmin0,yopen0,xmax0,yopen0,pixbufo,800,10)
			if(indx.eq.3) then
			xi=x0
			call gmsetvaluesetting(ivalp3,x0)
			else if(indx.eq.4) then
			xf=x0
			call gmsetvaluesetting(ivalp4,x0)
			endif
			!rval=x0
			call gmsetvaluesetting(ivalp1,yopen)
			call getpixelarray(x0,ymin0,x0,ymax0,pixbufi,10,800)
		    call getpixelarray (xmin0,yopen,xmax0,yopen,pixbufo,800,10)
			call drawline(x0,ymin0,x0,ymax0,13,idev)
			call drawline(xmin0,yopen,xmax0,yopen,12,idev)
			call gflushgraphics()
	! draw new place
		endif
			call gmActivateGraphicsFrame(graphics1_1(modplot))
		continue
	case(191,192) ! reject 311
	imode=1
	
		call gmremovewindow((graph1_1(99)))
		
	    moveline=.false.
	
		choosepoen=.false.
		zoomed=.false.
				popened=.false.
		newprev=.true.
		call gmDefineKeySelectCallback(533,0) !left Sets up escape key as escape key!
		call gmDefineKeySelectCallback(534,0) !right
		call gmDefineKeySelectCallback(535,0) !up
		call gmDefineKeySelectCallback(536,0) !down
		call gmDefineKeySelectCallback(108,0) !left Sets up escape key as escape key!
		!call gmDefineKeySelectCallback(114,0) !right
		!call gmDefineKeySelectCallback(82,0) !up
		!call gmDefineKeySelectCallback(97,0) !down
		!	call gmDefineKeySelectCallback(65,0) !down
		
		call gmDefineKeySelectCallback(13,0)
		call gmDefineKeySelectCallback(27,0)

		nx=10
		ny=800
		wxmin0=wxmin
		wxmax0=wxmax
		wymin0=wymin
		wymax0=wymax
		modplot=1
		jplot=1
		iplot=1
		call gmActivateGraphicsFrame(graphics1_1(modplot))
	
		lframe=graphics1_1(modplot)
	if(samepag) then	
	!	call drawpixelarray(xbeg0,ybeg0-0.5*ytsep,xbeg0,ybeg0+0.5*ytsep,pixbufi,nx,ny)
	!	call drawpixelarray(xend0,yend0-0.5*ytsep,xend0,yend0+0.5*ytsep,pixbuff,nx,ny)
		call drawline(xbeg,ybeg-0.5*ytsep,xbeg,ybeg+0.5*ytsep,0,idev)
		call drawline(xend,yend-0.5*ytsep,xend,yend+0.5*ytsep,0,idev)
	else
	!	call drawpixelarray(xend0,yend0-0.5*ytsep,xend0,yend0+0.5*ytsep,pixbuff,nx,ny)
       call drawline(xend,yend-0.5*ytsep,xend,yend+0.5*ytsep,0,idev)
	endif
		if(samepag) niline=niline0
		tseg0=xf-xi
		npopoints=tseg0/dtx
		idr=0
		k=int(xi/dtx)
		indx1f=indx1+k   ! for xval
		indy1f=indy1+k! for yval
		m=k+xbeg/dtx
		INDX1F=M
		if(.not.samepag) then
			if(xi.le.(secline-xbeg).and.xf.le.(secline-xbeg)) then
				samepag=.true.
				npagef=npagei
				idr=-1				
			else if( xi.gt.(secline-xbeg).and.xf.gt.(secline-xbeg)) then
				samepag=.true.
				npagei=npagef
				m=int((xi-(secline-xbeg))/dtx)
				indx1f=m
			
				indy1f=indx1f+npline*(niline-1)
				indy1=indy1f-k			
			
			endif
		endif
		if(icallid.eq.192) then
		   		
			
            npopen=npopen+1
			npseg=ifixr((xf-xi)/dtx)
			!	i1=1	!start at yval(1)
		!	i2=i1+npseg-1
		!	ns=0
			i1=indy1+k
			i2=i1+npopoints-1
			s=0.0
			if(samepag) then
			do i=i1,i2
				s=s + yval(i,1)-yshut		!calculate with yval
			enddo
		!	s=0.0
		!	do i=1,npopoints-1
		!		s=s + yval1(k+i,1)		!calculate with yval1 -same result as above
		!	enddo
			else
			    do i=1,nppage-indy1
					s=s+yval0(i)-yshut
				enddo
				do i=nppage-indy1,indy2+nppage-indy1
			     	s=s+yval0(i)-yshut
				enddo
			endif
			s=s/float(npseg)		!mean
		!	ns=ns+1
		    ns=npopen
			popen(ns)=s/(yopen-yshut)
		
		
		!	xpval(ns)=float(i1-1)*dtx		!time (ms) at start of segment
	        
		
		!tseg=tsegf(npopen)
		npops=ns
			itype(1)=0
			text_tog(1)='Segment'
			val(1)= npopen
			itype(2)=9
			text_tog(2)='P open '
			val(2)=Popen(npopen)
			text_tog(3)='Cluster of [ms]'
			val(3)=xf-xi
				itype(3)=9
			text_tog(4)='Y open '
				itype(4)=9
			val(4)=yopen
			text_tog(5)='Y shut'
				itype(5)=9
			val(5)=yshut
			itype(6)=0
			text_tog(6)='points/segment'
			val(6)=npseg
			n=6
			icall= 195
			itabon=1
			call gmDefineKeySelectCallback(13,195)
		call gmDefineKeySelectCallback(27,196)
			call value_table(iform,ivwin,n,text_tog,ivall,val,icall,itype)

		endif
	 
		imark=0

	    call gmSetGuiCursor(lframe,Glargecross,gdefault)
	
		chooseseg=.true.
	
case(195)
	call gmremovewindow(ivwin)	
	if(idr.ne.-1) then
			call glincols(12,0)
			jflag=0
			xe=0
			xg=0
			xv=0
			if(samepag) then
			    indy2f=indy1f+npopoints
				indyfer=indy1f
                j=1
			
				do i=1,npopoints-1
					
			!	if(xv.lt.xf+xbeg) then
			!	xv=xbeg-xe+(j-1)*dtx+xi-xg
				yv=yval1(k+i,1)+yline(niline)
				xv=m*dtx
			
			   
				if(jflag.eq.0) then
					call movto2(xv,yv)
					jflag=1
				else
					call linto2(xv,yv)
				endif
				if(xv.gt.secline) then
				    m=-1
					niline=niline+1
					jflag=0
				endif
               ! if(indyfer.eq.npline*niline) then
				!	j=0
				!	niline=niline+1
				!	xe=xbeg
				!	xg=xi	
				!	jflag=0			
				!endif
				j=j+1
				m=m+1
				indyfer=indyfer+1
				!endif
				
				enddo
			else
			j=0
			jflag=0
			
		!	do i=nppage-indy1,indy2+nppage-indy1
			!	xv=(i-(nppage-indy1f))*dtx
				do i=nppage-indy1,npopoints
			    xv=j*dtx
				yv=yval0(i)+yline(niline)
				if(jflag.eq.0) then
					call movto2(xv,yv)
					jflag=1
				else
					call linto2(xv,yv)
				endif
				j=j+1
				if(xv.eq.secline) j=0
			enddo
			
			endif
			endif
			call gmDefineKeySelectCallback(13,0)
		call gmDefineKeySelectCallback(27,0)
			  call gflushgraphics()
1122		idr=0	
	itabon=0
		npag(npopen,1)=npagei
			npag(npopen,2)=npagef
			npopointsf(npopen)=int((xf-xi)/dtx)
			yshutf(npopen)=yshut
			yopenf(npopen)=yopen
			tsegf(npopen)=xf-xi
			indf(npopen,1)=indx1f
			indf(npopen,2)=indy1f
			npseg=ifixr(tsegf(npopen)/dtx)
		
			nsegline=ifixr(float(npline)/float(npseg))	!popen segments per line
			nseglast=ifixr(float(nptlast)/float(npseg))	!popen segments on last line
			npop=1+ifixr(float(nppage)/float(npseg))
			npop=nppage
	        if(allocated(popend)) deallocate(popend,xpval) 
			ALLOCATE(popend(npop),xpval(npop))
			do j=1,npop
			popend(j)=0.0
			xpval(j)=0.0
			enddo
			cnum0(1)=' N popen'
			cnum0(2)=' Y shut'
				cnum0(3)=' Y open'
					cnum0(4)=' Cluster length'
						cnum0(5)=' Popen'
							cnum0(6)=' Sample rate'
								cnum0(7)=' Points/segment'

			if(discprt.and.npopen.eq.1) write(8,1001) cnum0(1),cnum0(2),cnum0(3),&
			cnum0(4),cnum0(5),cnum0(6),cnum0(7)
!			if(discprt.and.npopen.eq.1) write(8,1002) npopen,yshut,yopen,tsegf(ns),&
!            popen(ns),srate,npseg
			call intconv(npopen,cnum1)
			cnum0(1)=cnum1
			call intconv(npseg,cnum2)
			cnum0(7)=cnum2
			call realtoch(yshut,cnum0(2),15)
			call realtoch(yopen,cnum0(3),15)
			call realtoch(tsegf(ns),cnum0(4),15)
			call realtoch(popen(ns),cnum0(5),15)
			call realtoch(srate,cnum0(6),15)

			if(discprt) write(8,1001) cnum1,cnum0(2),cnum0(3),&
			cnum0(4),cnum0(5),cnum0(6),cnum0(7)

		
			ivit=101
			ntracet=ntrace
			npagsav=npage
			if(.not.readtemp.and.ikik.ne.-1) then
			ikik=-1
			nb=len_trim(testfil)
			tempfile=testfil(1:nb-3)//'tmp'
			endif
				OPEN(unit=15,file=tempfile,status='UNKNOWN', access='DIRECT',form='BINARY',recl=1)
	
			write (15,rec=1) ivit,npagsav,npopen,ntrace, &
					(Popen(i),i=1,npopen),(tsegf(i),i=1,npopen),(npag(i,1),i=1,npopen),&
					(npag(i,2),i=1,npopen),(npopointsf(i),i=1,npopen),(indf(i,1),i=1,npopen),&
					(indf(i,2),i=1,npopen),testfil,delay,poplot,tseg,ngrid,&
					secline0,sdir,ytsep
			close(unit=15)
!			write(8,110) npopen,npagei,npagef
!			write(8,111) yshut,yopen
!			write(8,112) popen(ns),tsegf(npopen),npseg
!110			format(/,'  Segment=',i3,' page=',i3,' to page=',i3)
!111			format('  Yshut=',g13.5,' Yopen=',g13.5)

!112			format('  Popen=',g13.5,' cluster of [ms]=',g13.6,' npseg=',i8,/)
		
	
case(196)
call gmDefineKeySelectCallback(13,0)
		call gmDefineKeySelectCallback(27,0)
	call gmremovewindow(ivwin)	
	itabon=0
    npopen=npopen-1
! filtsamp.. 201 -299?.................
   case(201,202) !63,64
   
		if(icallid.eq.63) then
		   invert=.true.
		   call gmSetToggleSwitch(itogg21,gon)
		   call gmSetToggleSwitch(itogg22,goff)
		else
		   invert=.true.
		  call gmSetToggleSwitch(itogg21,gon)
		   call gmSetToggleSwitch(itogg22,goff)
		endif
		if(invert.or.gainfac.ne.1.0) then
			calfac=calfac/gainfac
			setgain=.true.
		endif
		tval2(11)=calfac
		call gmsetvaluesetting(ival2(11),tval2(11))
  
   
			
	case(203)  ! 57 bbrowsw
	    
		sfile='*.ssd'
		sfilt='*.ssd'//char(124)//'Comsam file (ssd)'
		CALL gmFileBrowser(SFILE,SDIR,SFILT,gmType=GoutPUT, &
				gmTitle='Consam files')
		if (sfile.ne.'     ') then
		TEDIT(9)=SFILE
		call gmsettextsetting(iedit(9),tedit(9))
		ADCFIL1=SFILE
		OPEN(unit=15,file=ADCFIL1,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
		IF(I_VAR.EQ.0) continue
		write(15,REC=1,IOSTAT=I_VAR) iver,title,cdate,adctime,idt,ioff,ilen,inchan, & !!ierr or iflen?
	    id1,id2,cctrig(1:3),calfac,srate,filt,filt1,calfac1, &
		expdate,defname,tapeID,ipatch,npatch,Emem,temp
		if(i_var.ne.0) continue
		close(unit=15)
			call gmsetwidgetstatus(ibutt4,GSELECTABLE)
		endif
	case(204) ! ok 58
		if(filtsam) then
	    do i=1,5
			tval2(i)=gmenqvaluesetting(ival2(i))
			if(tval2(i).le.0.) then
				imes=gmdisplaymessagebox('','Not all fields filled',gstop,gok)
				goto 1
			endif
		enddo
	    call gmenqtextsetting(iedit(9),tedit(9))	
		nl=len_trim(tedit(9))
		if(nl.lt.5) then
			imes=gmdisplaymessagebox('','Not all fields filled',gstop,gok)
			goto 1
		endif
		adcfil1=tedit(9)
		if(ifilt_opt.eq.1) then
			tval2(6)=gmenqvaluesetting(ival2(6))
			    if(tval2(6).le.0.) then
				imes=gmdisplaymessagebox('','Not all fields filled',gstop,gok)
				goto 1
			endif
			fc=tval2(6)
			fc0=fc
			fc1=1./sqrt((1.0/(fc*fc)) - (1.0/(filt*filt)))
			fc2=1./sqrt((1.0/(fc*fc)) + (1.0/(filt*filt)))
			tval2(7)=fc1
			tval2(8)=fc
			call gmsetvaluesetting(ival2(7),tval2(7))
			call gmsetvaluesetting(ival2(8),tval2(8))
				call gmsetvaluesetting(ival2(17),fc)
			call gmsetvaluesetting(ival2(18),fc2)
			call gmsetwidgetstatus(ibutt5,GSELECTABLE)
			fc=fc1		!used for GFILTER
		    fc3=fc0	
			 iofc=1
			idelt=1
			tval2(10)=idelt
			call gmsetvaluesetting(ival2(10),tval2(10))

		else if (ifilt_opt.eq.3) then
			tval2(10)=gmenqvaluesetting(ival2(10))
		!	if(tval2(10).le.0.) then
		!		imes=gmdisplaymessagebox('','Not all fields filled',gstop,gok)
		!		goto 1
		!	endif
		    if(tval2(10).le.0.) tval2(10)=1
			idelt=tval2(10)
			srate1=srate/float(idelt)
			r1=srate1/filt
			tval2(9)=srate1
		
			call gmsetvaluesetting(ival2(9),tval2(9))
			call gmsetvaluesetting(ival2(10),tval2(10))

		endif
		
		i1=tval2(1)
		i2=tval2(2)
		nbuf=tval2(4)
		gainfac=tval2(3)
		novlap=tval2(5)
		ndatin=i2-i1+1
		nsec=1 + (ndatin-1)/nbuf
		nlast=ndatin-(nsec-1)*nbuf	!points in last section
		if(nlast.eq.0) nsec=nsec-1
		if(nbuf.gt.ndatin) then
			nbuf=ndatin
			nsec=1 + (ndatin-1)/nbuf
		    nlast=ndatin-(nsec-1)*nbuf	!points in last section
	
		endif
		
		setgain=.false.
	
		if(invert.or.gainfac.ne.1.0) then
			calfac=calfac/gainfac
			setgain=.true.
		endif
		tval2(11)=calfac
		call gmsetvaluesetting(ival2(11),tval2(11))
		tval2(12)=float(i2-i1+1)/srate
		call gmsetvaluesetting(ival2(12),tval2(12))
		if (ifilt_opt.gt.1) then
			icallid=206 !56
			goto 2
		endif
		endif
	
	
	case(205) !55
		tval2(10)=gmenqvaluesetting(ival2(10))
	    idelt=tval2(10)
		srate1=srate/float(idelt)
		tval2(9)=srate1
		call gmsetvaluesetting(ival2(9),tval2(9))
	
		icallid=206
		goto 2
	case(206)
		call gmsetwidgetstatus(ibuttonf,GSELECTABLE)
		if(.not.asc) then
			if(allocated(idatin)) deallocate(idatin,idatout)
			ALLOCATE(idatin(-(novlap-1):nbuf+novlap))
			ALLOCATE(idatout(-(novlap-1):nbuf+novlap))
		endif
		if(invert.or.gainfac.ne.1.0) then
	 
	   irec=ioff+1
	   if(.not.asc) then
		read(14,rec=irec) (idatin(i),i=1,20)
	   endif
	   s=0.0
	   do i=1,20
		s=s + float(idatin(i))
	   enddo
	   imean=int2(ifixr(s/20.))

	   imax=int2(32767./gainfac)
	   imin=-imax
	endif
if(setgain) then
     	  
	   if(discprt)write(8,57) gainfac,calfac,imean
57	   format(' Gain increased by factor of ',f9.3,' (new CALFAC = ',g13.6,')',&
      /,' Mean of 1st 20 points = ',i6,' subtracted before gain applied.',/,&
      ' Any values that would exceed 32767 are reset to 32767',/,&
      ' Any values that would be below -32767 are reset to -32767')
	   if(invert) then
     	  
	   if(discprt)write(8,571)
571	   format(' Data inverted')
	   endif
	endif		




   case(208,209) !61,62
	if(icallid.eq.208) then
		   fc=fc1		!used for GFILTER
		   fc3=fc0	
		   call gmSetToggleSwitch(itogg17,gon)
		   call gmSetToggleSwitch(itogg18,goff)
		   iofc=1
		else
		   fc=fc0		!used for GFILTER
		   fc3=fc2
		   call gmSetToggleSwitch(itogg18,gon)
		   call gmSetToggleSwitch(itogg17,goff)
		   iofc=2
		endif
		r=srate/fc3
		srate1=srate
		if(r.ge.14.) then
		   do i=2,100
			srate1=srate/float(i)
			if(srate1/fc3.lt.10.) then
			   idelt=i-1
			   goto 50
			endif
		   enddo
50		   continue
		   srate1=srate/float(idelt)
		   r1=srate1/fc3
		endif
		tval2(9)=srate1
		tval2(10)=idelt
		call gmsetvaluesetting(ival2(9),tval2(9))
		call gmsetvaluesetting(ival2(10),tval2(10))
   

	
case(211) !65) continue filtsam		
		
		if(axfile) then
			ipatch=1
			tvalout(9)=pav
			tvalout(8)=calfac
			tvalout(10)=filt
			call fillout(iform,ifillwin,adcfil1,ieditout,teditout,ivalout,tvalout,0,testfil)
		else
			icallid=215 !67
			goto 2
		endif

case(215) !67)
	    iemp=0
	    
		if(axfile.or.ifilt_opt.eq.-1) then
		do i=5,6
			tvalout(i)=gmenqvaluesetting(ivalout(i))	
			
		enddo
		do i=9,10
			tvalout(i)=gmenqvaluesetting(ivalout(i))	
		enddo
		do i=5,6
			if(tvalout(i).eq.0) iemp=-1
			
		
		enddo
		do i=9,10
			if(tvalout(i).eq.0) iemp=-1
			
		
		enddo
		do i=5,6
			call gmenqtextsetting(ieditout(i),teditout(i))	
			if(teditout(i).eq.'         ') iemp=-1
				
			
		enddo
		if(iemp.eq.-1) then
		imes=gmdisplaymessagebox('','Please fill all fields',ginformation,gok)
		goto 1
		endif
		
		if(ifilt_opt.eq.-1) then
			

		do i=19,20
			tvalout(i)=gmenqvaluesetting(ivalout(i))
			
			if(tvalout(i).eq.0) then
				imes=gmdisplaymessagebox('','Please fill all fields',ginformation,gok)
				goto 1
			endif	
		enddo
		call gmenqtextsetting(ieditout(4),teditout(4))
		tvalout(4)=gmenqvaluesetting(ivalout(4))
		if(.not.asc) tvalout(14)=gmenqvaluesetting(ivalout(14))
			if(tvalout(4).eq.0) then
				imes=gmdisplaymessagebox('','Please fill all fields',ginformation,gok)
				goto 1
			endif
		endif
		emem=tvalout(5)
		temp=tvalout(6)
		tapeid=teditout(6)
		expdate=teditout(5)
	!	pav=tvalout(8)
		pAv=tvalout(9)
		calfac=pAv/adcv
		VpA=1/pAv
		filt=tvalout(10)
		call gmremovewindow(ifillwin)
		
		if(ifilt_opt.eq.-1) then
			title=teditout(4)
			tapeid=teditout(6)
			srate=tvalout(4)
			ioffin=tvalout(14)+1
			if(asc) ioffin=1
			nbuf=1000000
			i1=tvalout(19)
			i2=tvalout(20)
			inchan=1
			ioff=512
			ilen=2*(i2-i1+1)
			nlen=ilen+ioff
			if(open8.eq..false.) then
				open8=.true.
				OPEN(unit=8,file=pfile,status='UNKNOWN',&
				access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
					if(.not.append) REWIND(unit=8)
				append=.true.
			endif
			if(asc) then
			if(discprt)write(8,46) ascfil,i1,i2,testfil,title,calfac,&
			expdate,npatch,tapeID,ptype(ipatch),Emem,temp
			else
			if(edr)then
			if(discprt)write(8,46) edrfil,i1,i2,testfil,title,calfac,&
			expdate,npatch,tapeID,ptype(ipatch),Emem,temp
			else
			if(discprt)write(8,46) intgfil,i1,i2,testfil,title,calfac,&
			expdate,npatch,tapeID,ptype(ipatch),Emem,temp
			endif
			endif
46			format(/,' Conversion of 16 bit integer file to consam format',/,&
			' INPUT file: ',a40,/,' Points ',i9,' to ',i9,' written in consam format to ',/,&
			' OUTPUT file: ',a40,/,' title: ',a70,/,'  Calibration factor (pA per ADC unit) = ',g13.6,/,&
			'  Date of experiment: ',a11,' (patch number ',i3,')'/,'  Tape details: ',a24,/,&
			'  Patch type: ',a14,/,'  Membrane potential (mV) = ',f9.2,/,'  Temperature = ',f9.1,/)
			
			!ioffin=2*i1-1	!offset to start reading input at ibuf(i1)
			ndat=i2-i1+1
			if(nbuf.gt.ndat) nbuf=ndat
			nsec=1 + (ndat-1)/nbuf
			nlast=ndat-(nsec-1)*nbuf	!points in last section 
			if(nlast.eq.0) nsec=nsec-1
			if(asc)then
			    open(unit=14,file='results.txt',status='UNKNOWN',&
					access='APPEND',form='FORMATTED',carriagecontrol='FORTRAN')
					REWIND(unit=14)
			else
			if(edr)then
			open(unit=14,file=edrFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
			else
			    open(unit=14,file=intgFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
			endif
			endif
1111        format(i10)
			open(unit=15,file=testFIL,access='DIRECT',form='BINARY',STATUS='REPLACE', RECL=1,IOSTAT=I_VAR)
	!		REWIND(unit=15)
			iver=1002
			ioff=512
			write(15,rec=1)iver,title,cdate,adctime,idt,ioff,ilen,&
			inchan,id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac,&
			expdate,defname,tapeID,ipatch,npatch,Emem,temp
			irecin=ioffin
			irecout=513		!ioff=512 for consam
			if(allocated(ibuf)) deallocate(ibuf)
			ALLOCATE(ibuf(nbuf))
			
			do n=1,nsec
				nread=nbuf
				if(n.eq.nsec) nread=nlast
				if(asc) then
				    do mn=1,nread
				        if(idatcal)then
				            ibuf(mn)=int2(((xnum(irecin+mn-1,2)*1e12))/calfac)
				            write(14,1111) ibuf(mn)
				        else
				            ibuf(mn)=int2((xnum(irecin+mn-1,2))/calfac)
				        endif
				    enddo
				else
				    read(14,rec=irecin) (ibuf(i),i=1,nread)
				endif
			!	if(edr) then
			!	    do i=1,nread
			!	        a=float(ibuf(i))/calfac
			!	        ibuf(i)=int2(a)
			!	    enddo
			!	endif
				write(15,rec=irecout) (ibuf(i),i=1,nread)
				
				if(asc) then
				    irecin=irecin+nread
				else
				    irecin=irecin + 2*nread	!bytes
				endif
				irecout=irecout + 2*nread
	   
			enddo
		!	if(.not.asc)then
			    CLOSE(unit=14)
		!	endif
			CLOSE(unit=15)
			DEALLOCATE(ibuf)
			icallid=17
			goto 2
		endif
		
		endif
		
		open(unit=14,file=testFIL,access='DIRECT',form='BINARY',RECL=1,IOSTAT=I_VAR)
		open(unit=15,file=ADCFIL1,access='DIRECT',form='BINARY',STATUS='REPLACE',RECL=1,IOSTAT=I_VAR)
		call time(iticks)
		tval2(12)=float(i2-i1+1)/srate
		call gmsetvaluesetting(ival2(12),tval2(12))
		call finalwin(iform,ifinwin,adcfil1,srate1,iticks,ivalfin,itextfin,ibutfin1,ibutfin2)
		if(discprt) write(8,281) pAV,calfac,filt
		if(ifilt_opt.eq.1) then
			ndatin=i2-i1+1
			if(.not.asc) then
				nsec=1 + (ndatin-1)/nbuf
				nlast=ndatin-(nsec-1)*nbuf	!points in last section
				if(nlast.eq.0) nsec=nsec-1
			endif

 			irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 			irec1=ioff + 1  !initialise record # for 1st value to be written
			if(axfile) irec1=513	!output as for CONSAM\
			i11=1
			iov1=i1-1		!number of points before #i1
			if(iov1.gt.novlap) iov1=novlap  !number of points for prelim overlap
			j1=i1-iov1
			irec=ioff + (2*j1-1)  !initialise record # for 1st value to be read

			nread=iov1+nbuf+novlap	!check does not go past end?!
			i0=(1-iov1)			!index of 1st point read into idatin()
			in=i0+nread-1		!index of last point read into idatin()
			iovn=nint-i2		!number of points after i2
			if(in.gt.nint) in=nint
			if(iovn.gt.novlap) iovn=novlap  !number of points for final overlap
			if(nsec.eq.1) then
				nread=iov1+(i2-i1+1)+iovn
				in=i0+nread-1
			endif
			nmin=0
			nmax=0	
			do isec=1,nsec
				call intconv(isec,cnum)
				call gmsettextsetting(itextfin,'Reading section:'//cnum)
				
				if(.not.asc) then
					read(14,rec=irec) (idatin(i),i=i0,in)
				endif
				if(isec.eq.1.and.iov1.lt.novlap) then
				do m=1-novlap,i0-1
				idatin(m)=idatin(i0)
				enddo
				endif
				if(isec.eq.nsec.and.iovn.lt.novlap) then
				do m=in+1,nlast+novlap
				idatin(m)=idatin(in)
				enddo
				endif
				if(setgain) then
					if(isec.eq.nsec) n=nlast
					do i=1-novlap,n+novlap
					idatf=idatin(i)-imean
					if(idatf.gt.imax) then
					idatin(i)=32767
					nmax=nmax+1
					else if(idatf.lt.imin) then
					idatin(i)=-32767
					nmin=nmin+1
					else
					idatin(i)=int2(ifixr(float(idatf)*gainfac))
					endif
					enddo
					if(invert) then
					n=nbuf
					if(isec.eq.nsec) n=nlast
					do i=1-novlap,n+novlap
					idatin(i)=-idatin(i)
					enddo
					endif
				
				endif
				if(isec.lt.nsec) then
					npnt=nbuf+2*novlap
				else
					npnt=nlast+2*novlap
				endif
				call gmsettextsetting(itextfin,'Filtering section:'//cnum)
				call GFILTER(idatin,idatout,npnt,srate,fc,nerr)
				if(nerr.ne.0) then
					imes=gmdisplaymessagebox('',' error in GFILTER',ginformation,gok)
				endif
				if(isec.lt.nsec) then    !if isec=nsec there is NO next section
					irec=irec + 2*(nread-2*novlap)
					j=isec*nbuf+i1-novlap
					irec0=ioff + (2*j-1)
					if(irec0.ne.irec) then
						imes=gmdisplaymessagebox('',' ERROR: irec0.ne.irec = ',ginformation,gok)
					endif
					i0=1-novlap
					if(isec.eq.nsec-1) then	!next section is last
						nread=novlap + nlast + iovn
					else 			!next section not the last
						nread=novlap + nbuf + novlap
					endif
					in=i0+nread-1
				endif
				if(isec.lt.nsec) then
					nvals=nbuf
				else
					nvals=nlast
				endif
				nout=ifix(1. + float(nvals-i11)/float(idelt))
				call gmsettextsetting(itextfin,'Writing section:'//cnum)
				write(15,rec=irec1) (idatout(i),i=i11,nvals,idelt)

				irec1=irec1+2*nout	!1st record to read in next loop
				ilast=i11+(nout-1)*idelt
				noutt=noutt+nout	!total points written
				i11=ilast+idelt-nbuf

			enddo		!end of isec loop
			if(discprt) write(8,282) nmin,nmax
			if(iofc.eq.1) then
			   if(discprt)write(8,48) fc,fc0
48			   format(' Filtered at ',f9.2,' Hz to give final fc = ',f9.2,' Hz')
			else if(iofc.eq.2) then
			   if(discprt)write(8,49) fc0,fc2
49			   format(' Filtered at ',f9.2,' Hz (overall fc = ',f9.2,' Hz')
			endif
!			if(discprt)write(8,47) iofc
!47			format(' option=',i3)
			if(discprt)write(8,591) idelt
591			format(' kept every nth point: n = ',i3)
			if(.not.asc) then
				CLOSE(unit=14)
			endif
			srate=srate1
			ilen=noutt*2	!bytes
			ioffout=ioff
			if(axfile) ioffout=512
			write(15,rec=1)iver,title,cdate,adctime,idt,ioffout,ilen,&
     		inchan,id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1,&
     		expdate,defname,tapeID,ipatch,npatch,Emem,temp
			if(discprt)write(8,51) adcfil1,noutt,srate1
51			format(/,' OUTPUT file name (CONSAM format) = ',a40,/,&
     		1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
			CLOSE(unit=15)
		else if(ifilt_opt.eq.2) then
			srate1=srate	!all points
			noutt=0	
			irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 			irec1=ioff + 1  !initialise record # for 1st value to be written
			if(axfile) irec1=513	!output as for CONSAM\
			nmin=0
			nmax=0
			do isec=1,nsec
				nvals=nbuf
				if(isec.eq.nsec) nvals=nlast
				if(.not.asc) then
					read(14,rec=irec) (idatin(i),i=1,nvals)
				endif
				irec=irec+2*nvals
				if(setgain) then
					do i=1,nvals
					idatf=idatin(i)-imean
					if(idatf.gt.imax) then
					idatin(i)=32767
					nmax=nmax+1
					else if(idatf.lt.imin) then
					idatin(i)=-32767
					nmin=nmin+1
					else
					idatin(i)=int2(ifixr(float(idat)*gainfac))
					endif
					enddo
					if(invert) then
						do i=1,nvals
						idatin(i)=-idatin(i)
						enddo
					endif
				endif
				write(15,rec=irec1) (idatin(j),j=1,nvals)
				irec1=irec1+2*nvals	!1st record to write in next loop
				noutt=noutt+nvals
			enddo		!end of isec loop
			if(.not.asc) CLOSE(unit=14)
			ilen=2*noutt	!changed in header (bytes)
			ioffout=ioff
			if(axfile) ioffout=512
			write(15,rec=1)iver,title,cdate,adctime,idt,ioffout,ilen,&
     		inchan,id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1,&
     		expdate,defname,tapeID,ipatch,npatch,Emem,temp
			if(discprt)write(8,51) adcfil1,noutt,srate1
			if(discprt) write(8,282) nmin,nmax
282			format(' After gain applied,',/,&
     		'  number of values in input below -32767 = ',i10,/,&
     		'  number of values in input above 32767 = ',i10,/)
			CLOSE(unit=15)
		else
			ndatin=i2-i1+1
			if(.not.asc) then
				nsec=1 + (ndatin-1)/nbuf
				nlast=ndatin-(nsec-1)*nbuf	!points in last section
				if(nlast.eq.0) nsec=nsec-1
				
			endif
			irec=ioff + (2*i1-1)  !initialise record # for 1st value to be read
 			irec1=ioff + 1  !initialise record # for 1st value to be written
			if(axfile) irec1=513	!output as for CONSAM\
			i11=1
			do isec=1,nsec
				if(isec.lt.nsec) then
				nvals=nbuf
				else
				nvals=nlast
				endif
				if(.not.asc) then
					read(14,rec=irec) (idatin(i),i=1,nvals)
				endif
				irec=irec+2*nvals	!1st record to read in next loop
				if(setgain) then
					do i=1,nvals
					idatf=idatin(i)-imean
					if(idatf.gt.imax) then
					idatin(i)=32767
					else if(idatf.lt.imin) then
					idatin(i)=-32767
					else
					idatin(i)=int2(ifixr(float(idatf)*gainfac))
					endif
					enddo
					if(invert) then
					do i=1,nvals
						idatin(i)=-idatin(i)
					enddo
					endif
				endif
				write(15,rec=irec1) (idatin(i),i=i11,nvals,idelt)
				nout=ifix(1. + float(nvals-i11)/float(idelt))
				irec1=irec1+2*nout	!1st record to read in next loop
				ilast=i11+(nout-1)*idelt
				noutt=noutt+nout	!total points written
				i11=ilast+idelt-nbuf	!index of 1st point to write in next section	
			enddo
			if(.not.asc) then
				CLOSE(unit=14)
			endif
			srate=srate1
			ilen=noutt*2	!bytes
			ioffout=ioff
			if(axfile) ioffout=512
			write(15,rec=1)iver,title,cdate,adctime,idt,ioffout,ilen,&
     		inchan,id1,id2,cs(1:3),calfac,srate,filt,filt1,calfac1,&
     		expdate,defname,tapeID,ipatch,npatch,Emem,temp
	     
     		if(discprt)write(8,55) adcfil1,idelt,noutt,srate
55			format(' OUTPUT file name = ',a40,/,&
     		'  Every ',i3,'''th point kept in output',/,&
     		1x,i9,' points written to output: sample rate = ',f10.3,' Hz')
     		CLOSE(unit=15)
		endif
		call time(iticks)
		call gmsettextsetting(itextfin,'Finish writing:'//iticks)
		call gmsetvaluesetting(ivalfin,noutt)
			call gmsetwidgetstatus(ibutfin1,GSELECTABLE)
				call gmsetwidgetstatus(ibutfin2,GSELECTABLE)
	
	
	case(218) ! 102 exit
		imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
		if(imes.eq.gnobutton) then
			goto 3
		else
			comm='c:\windows\notepad '// pfile
			res=systemqq(comm)
			goto 3
		endif
	
	case(219) !69)
			call gmremovewindow(ifinwin)
	
	CASE(222)
		call gmremovewindow(ifinwin)
	
		TESTFIL=ADCFIL1
		CONSM=.TRUE.
		ICALLID=10
	call gmSETtextsetting(iedit(1),ADCFIL1)
		GOTO 2
	case(301:305)
	    mflag=0	
		if(iplot.eq.99) then	 
		if(icallid.eq.305) indx=0
			
			if(indx.eq.2) then
				nx=800
				ny=10
				rval=yshut
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufs,nx,ny)
	   			
			else if(indx.eq.1) then
				nx=800
				ny=10
				rval=yopen
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufo,nx,ny)
            else if(indx.eq.3) then
				ny=800
				nx=10
			    rval=xi
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbufi,nx,ny)
			 		!green
			
			else if(indx.eq.4) then
				ny=800
				nx=10
			    rval=xf
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbuff,nx,ny)
			
			endif
		
			if(icallid.eq.301) then ! left 
				if(rval.gt.xmin0+epsi) rval=rval-epsi
			else if(icallid.eq.304) then !down
				if(rval.gt.ymin0+epsi) rval=rval-epsi
			else if(icallid.eq.302) then ! right
				!	if(rval.lt.xmax0-epsi) rval=rval+epsi
						if(rval.lt.totsec-epsi) rval=rval+epsi
			else if(icallid.eq.303) then !up
				if(rval.lt.ymax0-epsi) rval=rval+epsi
			endif
			
			
			if(indx.eq.2) then
				nx=800
				ny=10
			    yshut=rval
				call getpixelarray(xmin0,yshut,xmax0,yshut,pixbufs,nx,ny)
			 	ic=10		!green
				call drawline(xmin0,yshut,xmax0,yshut,ic,idev)
				call gmsetvaluesetting(ivalp2,rval)
			else if(indx.eq.1) then
				nx=800
				ny=10
			    yopen=rval
				call getpixelarray(xmin0,yopen,xmax0,yopen,pixbufo,nx,ny)
				ic=12		!green
				call drawline(xmin0,yopen,xmax0,yopen,ic,idev)
				call gmsetvaluesetting(ivalp1,rval)
			else if(indx.eq.3) then
				ny=800
				nx=10
			    xi=rval
				call getpixelarray(xi,ymin0,xi,ymax0,pixbufi,nx,ny)
			 	ic=13		!green
				call drawline(xi,ymin0,xi,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp3,rval)
			else if(indx.eq.4) then
				ny=800
				nx=10
			    xf=rval
				call getpixelarray(xf,ymin0,xf,ymax0,pixbuff,nx,ny)
			 	ic=13		!green
				call drawline(xf,ymin0,xf,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp4,rval)
			endif
			else if(iplot.eq.51) then
			if(icallid.eq.305) indx=0
			if(indx1.eq.5) then
				nx=10
				ny=800
				rval=x0
				call drawpixelarray(rval,yminz,rval,ymaxz,pixbuf,nx,ny)
				if(icallid.eq.301) then
					if(rval.gt.xminz+epsi) rval=rval-epsi
					
				else if(icallid.eq.302) then 
					
					if(rval.lt.xmaxz-epsi) rval=rval+epsi
				endif
				x0=rval
				call getpixelarray(x0,yminz,x0,ymaxz,pixbuf,nx,ny)
				ic=13
				call drawline(x0,yminz,x0,ymaxz,ic,idev)
				call gmsetvaluesetting(ivalpz,rval)	
			else if(indx1.eq.6) then
				ny=10
					nx=800
					rval=yopen
					call drawpixelarray(xminz,yopen,xmaxz,yopen,pixbufz,nx,ny)
				
				if(icallid.eq.304) then !down
				if(rval.gt.yminz+epsi) rval=rval-epsi
			
			else if(icallid.eq.303) then !up
				if(rval.lt.ymaxz-epsi) rval=rval+epsi
			endif
				
					yopen=rval
					call getpixelarray(xminz,yopen,xmaxz,yopen,pixbufz,nx,ny)
					ic=12
					call drawline(xminz,yopen,xmaxz,yopen,ic,idev)
			
					call gmsetvaluesetting(ivalpz1,rval)

			endif 
			endif

	case(306)
		
		if(iplot.eq.99) then
			icallid=191
			goto 2
		else if(iplot.eq.51) then
			icallid=181
			goto 2
		endif
		if(itabon.and.iplot.eq.1) then
				icallid=196
			goto 2	
		endif
	case(307)
	
		if(iplot.eq.99) then
			icallid=192
			goto 2
		else if(iplot.eq.51) then
			icallid=182
			goto 2
		endif
		if(itabon.and.iplot.eq.1) then
				icallid=195
			goto 2	
		endif		
    case(401) ! help main
		call plothelpg(iform,1)
	case(402) ! help expand
		call plothelpg(iform,2)
	case(403) ! help zoom
		call plothelpg(iform,3)
	case(501)
			if(iopinote.eq.-1) then 
				call gmremovewindow(inote)
				iopinote=0
			endif
		if(open8) then
!			close(unit=7)
			!comm='c:\windows\notepad '// pfilem
			!resultcom=systemqq(comm)
			inote=gmCreateComplexDialogueBox(iform,10,2, 27, 20, GALL, '', &
              	 gmhpos=gleft,gmvpos=gtop,gmIconFormat=GDLLICON,gmIconFile='Gee')
			ipanelnote= gmCreatePanel(inote, 0, 0, 26, 19, &
              	gmType=GINVERSECHISEL, &
              	gmLineCol=0, gmLineBorder=0, gmFillCol=0, gmFillBorder=0, &
              	 gmVpos=GTOP,gmExpand=GOFF)
			inotetext=gmCreateTextEntry(ipanelnote,0,0, 26, 19,'Blow', 32768, gdisplay, &
              	gmType=Gstandard, gmFont=GDEFAULT, gmJustify=Gleft, &
              	gmBack1Col=150, gmBack2Col=0, gmTextCol=0, &
              	gmVpos=GTOP, gmScrollMode=Gbothbars,gmExpand=GOFF)
			inoteButton = gmCreatePushButton(inote, 1, 0, 25, 1, 'Close', &
            gmVpos=Gbottom,gmType=GDEFAULTBUTTON ,gmcallback=502)
			call gmdrawwindow(inote)
		!	string=pfile
			imes=gmgettextfromfile(inotetext,pfile)
		!	call gmdrawwindow(inote)
			iopinote=-1
				
		endif
	case(502)
		call gmremovewindow(inote)
		iopinote=0
	
	CASE(7101:7199)
	if(readrec) then
				modplot=icallid-7100
				igraph=modplot
					iplot=modplot
		jplot=modplot
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
		
          
			CALL ACTENQ(iCALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
			xtrue=xac
			ytrue=yac
			xmov=xtrue
			ymov=ytrue
		    rval=ytrue
			call valtyp(1,0)
			
			CALL REALTOCH(XTRUE,CXTRUE,11)
			CALL REALTOCH(YTRUE,CYTRUE,11)
			if(iplot.eq.99) then
				call gmsettextsetting(itext_1,'x='//cxtrue)
				call gmsettextsetting(itext_2,'y='//cytrue)
			else if(iplot.eq.51) then
				call gmsettextsetting(itext_3,'x='//cxtrue)
				call gmsettextsetting(itext_4,'y='//cytrue)
			else 
				call gmsettextsetting(itext1,'x='//cxtrue)
				call gmsettextsetting(itext2,'y='//cytrue)
			!	if(chooseseg) call gmSetGuiCursor(lframe,Glargecross,gdefault)
			
		!		if(choosepopen) call gmSetGuiCursor(lframe,Glargecross,gdefault)
			endif
		!	call gmSetStatusBarText(Status_bar1,2,CXTRUE)
		!	call gmSetStatusBarText(Status_bar1,3,CYTRUE)
 
			call valtyp(0,0)
				if(moveline.and.mflag.eq.1) then
					CALL ACTENQ(iCALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
				
					
			if(iplot.eq.99.and.popened) then	!.and.ikey.eq.-1
			if(indx.eq.2) then
				nx=800
				ny=10
				rval=yshut
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufs,nx,ny)
	   			rval=ymov
			else if(indx.eq.1) then
				nx=800
				ny=10
				rval=yopen
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufo,nx,ny)
				rval=ymov
			else if(indx.eq.3) then
				ny=800
				nx=10
			    rval=xi
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbufi,nx,ny)
			 		!green
			    rval=xmov
			else if(indx.eq.4) then
				ny=800
				nx=10
			    rval=xf
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbuff,nx,ny)
			    rval=xmov
			endif
		if(indx.eq.2) then
				nx=800
				ny=10
			    yshut=rval
			    if(yshut.lt.ymin0) yshut=ymin0
				if(yshut.gt.ymax0) yshut=ymax0
				call getpixelarray(xmin0,yshut,xmax0,yshut,pixbufs,nx,ny)
			 	ic=10		!green
				call drawline(xmin0,yshut,xmax0,yshut,ic,idev)
				call gmsetvaluesetting(ivalp2,rval)
			else if(indx.eq.1) then
				nx=800
				ny=10
			    yopen=rval
				if(yopen.lt.ymin0) yopen=ymin0
				if(yopen.gt.ymax0) yopen=ymax0
				call getpixelarray(xmin0,yopen,xmax0,yopen,pixbufo,nx,ny)
				ic=12		!green
				call drawline(xmin0,yopen,xmax0,yopen,ic,idev)
				call gmsetvaluesetting(ivalp1,rval)
			else if(indx.eq.3) then
				ny=800
				nx=10
			    xi=rval
				if(xi.lt.xmin0) xi=xmin0
				if(xi.gt.xmax0) xi=xmax0
				call getpixelarray(xi,ymin0,xi,ymax0,pixbufi,nx,ny)
			 	ic=13		!green
				call drawline(xi,ymin0,xi,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp3,rval)
			else if(indx.eq.4) then
				ny=800
				nx=10
			    xf=rval
				if(xf.lt.xmin0) xf=xmin0
				if(xf.gt.xmax0) xf=xmax0
				call getpixelarray(xf,ymin0,xf,ymax0,pixbuff,nx,ny)
			 	ic=13		!green
				call drawline(xf,ymin0,xf,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp4,rval)
			endif
					
			else if(iplot.eq.51.and.zoomed) then	
			if(indx1.eq.5) then
        	    	nx=10
				ny=800
				rval=x0
				call drawpixelarray(rval,yminz,rval,ymaxz,pixbuf,nx,ny)
				
				rval=xmov
				
				x0=rval
				call getpixelarray(x0,yminz,x0,ymaxz,pixbuf,nx,ny)
				ic=13
				call drawline(x0,yminz,x0,ymaxz,ic,idev)
				call gmsetvaluesetting(ivalpz,rval)	
					else if(indx1.eq.6) then
					ny=10
					nx=800
					rval=yopen
					call drawpixelarray(xminz,yopen,xmaxz,yopen,pixbufz,nx,ny)
				
					rval=ymov
				
					yopen=rval
					call getpixelarray(xminz,yopen,xmaxz,yopen,pixbufz,nx,ny)
					ic=12
					call drawline(xminz,yopen,xmaxz,yopen,ic,idev)
			
					call gmsetvaluesetting(ivalpz1,rval)

				endif
					endif
					endif
				!	mflag=0
		  ENDIF	
case(500)
		
		  if(iplot.eq.99) then	!.and.ikey.eq.-1
			if(indx.eq.2) then
				nx=800
				ny=10
				rval=yshut
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufs,nx,ny)
	   			rval=ymov
			else if(indx.eq.1) then
				nx=800
				ny=10
				rval=yopen
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufo,nx,ny)
				rval=ymov
			else if(indx.eq.3) then
				ny=800
				nx=10
			    rval=xi
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbufi,nx,ny)
			 		!green
			    rval=xmov
			else if(indx.eq.4) then
				ny=800
				nx=10
			    rval=xf
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbuff,nx,ny)
			    rval=xmov
			endif
		if(indx.eq.2) then
				nx=800
				ny=10
			    yshut=rval
			    if(yshut.lt.ymin0) yshut=ymin0
				if(yshut.gt.ymax0) yshut=ymax0
				call getpixelarray(xmin0,yshut,xmax0,yshut,pixbufs,nx,ny)
			 	ic=10		!green
				call drawline(xmin0,yshut,xmax0,yshut,ic,idev)
				call gmsetvaluesetting(ivalp2,rval)
			else if(indx.eq.1) then
				nx=800
				ny=10
			    yopen=rval
				if(yopen.lt.ymin0) yopen=ymin0
				if(yopen.gt.ymax0) yopen=ymax0
				call getpixelarray(xmin0,yopen,xmax0,yopen,pixbufo,nx,ny)
				ic=12		!green
				call drawline(xmin0,yopen,xmax0,yopen,ic,idev)
				call gmsetvaluesetting(ivalp1,rval)
			else if(indx.eq.3) then
				ny=800
				nx=10
			    xi=rval
				if(xi.lt.xmin0) xi=xmin0
				if(xi.gt.xmax0) xi=xmax0
				call getpixelarray(xi,ymin0,xi,ymax0,pixbufi,nx,ny)
			 	ic=13		!green
				call drawline(xi,ymin0,xi,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp3,rval)
			else if(indx.eq.4) then
				ny=800
				nx=10
			    xf=rval
				if(xf.lt.xmin0) xf=xmin0
				if(xf.gt.xmax0) xf=xmax0
				call getpixelarray(xf,ymin0,xf,ymax0,pixbuff,nx,ny)
			 	ic=13		!green
				call drawline(xf,ymin0,xf,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp4,rval)
			endif
			endif
	 case(7201:7300)	   !select
		  ! take out ikey=-1!!!
			if(readrec) then
			 
				modplot=icallid-7200
				igraph=modplot
				iplot=modplot
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				lframe=graphics1_1(modplot)
			
				jindex=indx
			    CALL ACTENQ(iCALLid,IDENT,IKEY,ISTATmove,Xac,Yac,acTEXT,nacargs,ARGS,IARGS)
				if(iplot.eq.99.or.iplot.eq.51) then
			!	if(ikey.eq.-2) then
				if(ikey.eq.-3) then
				!(indx.eq.3.or.indx.eq.4).and.
				if(iplot.eq.99.and.(indx.eq.3.or.indx.eq.4)) then !.and.mflag.eq.0
						moveline=.false.
					!	call gmSetGuiCursor(lframe,Gsmallcross,grubberbox)
						icallid=161
						goto 2					
		        endif
					else if(ikey.eq.-1) then	
			!	else if(ikey.eq.-1.or.ikey.eq.-3) then	
				! call gmSetGuiCursor(lframe,Gdefault,gdefault)
				if(ikey.eq.-1)	moveline=.true.
				if(moveline) then
				! find index 
				!	if(ikey.eq.-1) then
				!		mflag=1
				!	else if(ikey.eq.-3) then
				!		mflag=0
				!		indx=0
					if(mflag.eq.1) then
					mflag=0
					indx=0
					else if(mflag.eq.0) then
					mflag=1
					endif	
					if(iplot.eq.99) then
					! find index
					dxm0=0.05*(xmax0-xmin0)
					dym0=0.05*(ymax0-ymin0)
					if(ikey.eq.-1) then
						if(xmov.gt.xi-dxm0.and.xmov.lt.xi+dxm0) then
						indx=3
						epsi=(xmax0-xmin0)*0.01
						else if(xmov.gt.xf-dxm0.and.xmov.lt.xf+dxm0) then
						indx=4
						epsi=(xmax0-xmin0)*0.01
						else if(ymov.gt.yopen-dym0.and.ymov.lt.yopen+dym0) then
						indx=1
						epsi=(ymax0-ymin0)*0.01
						else if(ymov.gt.yshut-dym0.and.ymov.lt.yshut+dym0) then
						indx=2
						epsi=(ymax0-ymin0)*0.01
						endif
					endif
					if(indx.eq.2) then
					nx=800
					ny=10
					rval=yshut

					call drawpixelarray(xmin0,rval,xmax0,rval,pixbufs,nx,ny)
	   				rval=ymov
			else if(indx.eq.1) then
				nx=800
				ny=10
				rval=yopen
				call drawpixelarray(xmin0,rval,xmax0,rval,pixbufo,nx,ny)
				rval=ymov
			else if(indx.eq.3) then
				ny=800
				nx=10
			    rval=xi
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbufi,nx,ny)
			 		!green
			    rval=xmov
			else if(indx.eq.4) then
				ny=800
				nx=10
			    rval=xf
				call drawpixelarray(rval,ymin0,rval,ymax0,pixbuff,nx,ny)
			    rval=xmov
			endif
		if(indx.eq.2) then
				nx=800
				ny=10
			    yshut=rval
					if(yshut.lt.ymin0) yshut=ymin0
				if(yshut.gt.ymax0) yshut=ymax0
				call getpixelarray(xmin0,yshut,xmax0,yshut,pixbufs,nx,ny)
			 	ic=10		!green
				call drawline(xmin0,yshut,xmax0,yshut,ic,idev)
				call gmsetvaluesetting(ivalp2,rval)
			else if(indx.eq.1) then
				nx=800
				ny=10
			    yopen=rval
				if(yopen.lt.ymin0) yopen=ymin0
				if(yopen.gt.ymax0) yopen=ymax0
				call getpixelarray(xmin0,yopen,xmax0,yopen,pixbufo,nx,ny)
				ic=12		!green
				call drawline(xmin0,yopen,xmax0,yopen,ic,idev)
				call gmsetvaluesetting(ivalp1,rval)
			else if(indx.eq.3) then
				ny=800
				nx=10
			    xi=rval
					if(xi.lt.xmin0) xi=xmin0
				if(xi.gt.xmax0) xi=xmax0
				call getpixelarray(xi,ymin0,xi,ymax0,pixbufi,nx,ny)
			 	ic=13		!green
				call drawline(xi,ymin0,xi,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp3,rval)
			else if(indx.eq.4) then
				ny=800
				nx=10
			    xf=rval
					if(xf.lt.xmin0) xf=xmin0
				if(xf.gt.xmax0) xf=xmax0
				call getpixelarray(xf,ymin0,xf,ymax0,pixbuff,nx,ny)
			 	ic=13		!green
				call drawline(xf,ymin0,xf,ymax0,ic,idev)
				call gmsetvaluesetting(ivalp4,rval)
			endif
					
			else if(iplot.eq.51) then	
					dxmz=0.05*(xmaxz-xminz)
					dymz=0.05*(ymaxz-yminz)
					if(ikey.eq.-1) then
						if(xmov.gt.x0-dxmz.and.xmov.lt.x0+dxmz) then
						indx1=5
						epsi=(xmaxz-xminz)*0.01
						else if(ymov.gt.yopen-dymz.and.ymov.lt.yopen+dymz) then
						indx1=6
						epsi=(ymaxz-yminz)*0.01
						endif
        	    	endif
				if(indx1.eq.5) then
					nx=10
					ny=800
					rval=x0
					call drawpixelarray(rval,yminz,rval,ymaxz,pixbuf,nx,ny)
				
					rval=xmov
				
					x0=rval
					call getpixelarray(x0,yminz,x0,ymaxz,pixbuf,nx,ny)
					ic=13
					call drawline(x0,yminz,x0,ymaxz,ic,idev)
			
					call gmsetvaluesetting(ivalpz,rval)	
				else if(indx1.eq.6) then
					ny=10
					nx=800
					rval=yopen
					call drawpixelarray(xminz,yopen,xmaxz,yopen,pixbufz,nx,ny)
				
					rval=ymov
				
					yopen=rval
					call getpixelarray(xminz,yopen,xmaxz,yopen,pixbufz,nx,ny)
					ic=12
					call drawline(xminz,yopen,xmaxz,yopen,ic,idev)
			
					call gmsetvaluesetting(ivalpz1,rval)

				endif
			endif
			endif
			endif
			else if(chooseseg.and.iplot.eq.1.and.ikey.eq.-1) then	
					imark=imark+1
					nx=10
					ny=800
					wxmin0=wxmin
					wxmax0=wxmax
					wymin0=wymin
					wymax0=wymax
					if(imark.eq.1) then
						xmov0=xmov
						ymov0=ymov
						xbeg=xmov0
						ybeg=ymov0
						xbeg0=xbeg
						ybeg0=ybeg
						call getpixelarray(xmov0,ymov0-0.5*ytsep,xmov0,ymov0+0.5*ytsep,pixbufi,nx,ny)
					     npagei=npage

					!	call movto2(xmov0,ymov0)
					!	CALL lincol(2)
						ic=12
						samepag=.true.	
						indx1=int(npline/secline*xbeg)
						niline=0
						do k=1,ntrace
							if(ybeg.lt.yline(k)+0.5*ytsep.and.ybeg.gt.yline(k)-0.5*ytsep) niline=k
						enddo
						if(niline.le.0) then
							jkstatus=gmdisplaymessagebox('','Please select another point on the base line',ginformation,gok)
							imark=0
							goto 1
						endif
						call drawline(xmov0,ymov0-0.5*ytsep,xmov0,ymov0+0.5*ytsep,ic,idev)
					
					
						
						indy1=indx1+npline*(niline-1)
						l=1
						niline0=niline
						do i=indy1,nppage
						   
							yval0(l)=yval(i,1)
							l=l+1
						enddo
					else if(imark.gt.1) then
					  npagef=npage
					  if(npagef-npagei.lt.0.and.(npagef-npagei).gt.1) then
						imes=gmdisplaymessagebox('','Interval more than 1 page; try again',&
						ginformation,gok)
						goto 1
					  endif
						call getpixelarray(xmov,ymov-0.5*ytsep,xmov,ymov+0.5*ytsep,pixbuff,nx,ny)
						xend=xmov
						yend=ymov
					  	chooseseg=.false.
						xend=xmov
						yend=ymov
						xend0=xend
						yend0=yend
						imark=0
                    	ic=12		!green
						niline=0
						do k=1,ntrace
							if(yend.lt.yline(k)+0.5*ytsep.and.yend.gt.yline(k)-0.5*ytsep) niline=k
						enddo
						if(niline.le.0) then
							jkstatus=gmdisplaymessagebox('','Please select another point on the base line',ginformation,gok)
							imark=1
							goto 1
						endif
						call drawline(xmov,ymov-0.5*ytsep,xmov,ymov+0.5*ytsep,ic,idev)
					
						indx2=int(npline/secline*xend)
						
						indy2=indx2+npline*(niline-1)
						if(npagef.ne.npagei) then
						   samepag=.false.
						   l=nppage-indy1+1
						  ! do i=1,indy2+nppage-indy1
						  do i=1,indy2
						   l=l+1
						   yval0(l)=yval(i,1)
						enddo
						npopoints=l
						endif
						icallid=142
						goto 2
					endif
				else if(choosepopen.and.iplot.eq.99) then
					imark=imark+1

					if(imark.eq.1) then
					!	call getpixelarray(xmov0,ymov0-0.1*ytsep,xmov0,ymov0+0.1*ytsep,&
					!	pixbufo,nx,ny)
						xmov0=xmov
						ymov0=ymov
						call movto2(xmov0,ymov0)
						CALL lincol(2)
						ic=12
						xiz=xmov0
						ymz=ymov0
						call drawline(xmov0,ymov0-0.05*ytsep,xmov0,ymov0+0.05*ytsep,ic,idev)
					
						xbegp=xmov0
						ybegp=ymov0
					else if(imark.gt.1) then
					!	call getpixelarray(xmov,ymov0-0.1*ytsep,xmov,ymov0+0.1*ytsep,&
					!	pixbufo,nx,ny)
						xendp=xmov
						yendp=ymov0
					
						imark=0
                    	ic=12
						xfz=xmov		!green
						call drawline(xmov,ymov0-0.05*ytsep,xmov,ymov0+0.05*ytsep,ic,idev)
						icallid=161
						goto 2
					endif
				endif
			endif	
	end select
	end do
	if(open8) then
	imes=gmdisplaymessagebox('','View text file',gquestion,gyesno)
		if(imes.eq.gyesbutton) then
			comm='c:\windows\notepad '// pfile
			res=systemqq(comm)
		endif
		close(unit=8)
		endif
3   continue
if(.not.readtemp) then
imes=gmdisplaymessagebox('','Write to ini file',gquestion,gyesno)
if(imes.eq.gyesbutton) then
if(good) then
	CALL gmFileBROWSER(iniFILE,inipath,inidef,gmBrowseType=1)
			 IF(iniFILE.ne.' ') then
			 nl=len_trim(inipath)
			 inifile=inipath(1:nl)//'\'//inifile
		!	 call gmsettextsetting( inittext,inifile)
		!	 inifile0=inifile
			 endif
	INQUIRE (FILE=iniFILE,EXIST=PRESENT,&
			ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 

	if(PRESENT) then
	
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(inifile, info, ihandle)
		nLEN=info%length
		if(nlen.lt.100) then
	!		imes=gmdisplaymessagebox(inifile,' does not exit.Create a new file',ginformation,gok)
	
		endif
	
		ivi=1000
		OPEN(unit=12,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)
		secline0=secline/1000.
!		ngrid=ngrid1
		npagsav=npage
	
		write (12,rec=1) ivi,testfil,delay,poplot,tseg,ngrid,(pgrid(i),i=1,ngrid),&
					secline0,sdir
	
		close(unit=12)
	else
	!	imes=gmdisplaymessagebox(inifile,' does not exit.Create a new file',ginformation,gok)
		npagsav=npage
		ivi=1000
		OPEN(unit=12,file=inifile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)
		secline0=secline/1000.
	!	ngrid=ngrid1
	
		write (12,rec=1) ivi,testfil,delay,poplot,tseg,ngrid,(pgrid(i),i=1,ngrid),&
					secline0,sdir
	
		close(unit=12)
	
	endif 
endif    
endif
endif
imes=gmdisplaymessagebox('','Write to temporary file',gquestion,gyesno)
if(imes.eq.gyesbutton) then
    tempf=tempfile
	CALL gmFileBROWSER(tempF,tempath,tempdef,gmBrowseType=1)
			 IF(tempF.ne.' ') then
			 nl=len_trim(tempath)
			 tempfile=tempath(1:nl)//'\'//tempf
		!	 call gmsettextsetting( inittext,inifile)
		!	 inifile0=inifile
			 endif
	INQUIRE (FILE=tempFILE,EXIST=PRESENT,&
			ACCESS=FACCESS,FORM=FFORM,RECORDTYPE=FRTYPE,RECL=IRCL) 

	if(PRESENT) then
	
		ihandle=FILE$FIRST
		length = GETFILEINFOQQ(tempfile, info, ihandle)
		nLEN=info%length
		if(nlen.lt.100) then
			imes=gmdisplaymessagebox(tempfile,' does not exit.Create a new file',ginformation,gok)
	
		endif
	
		ivit=101
		OPEN(unit=15,file=tempfile,status='UNKNOWN', access='DIRECT',form='BINARY',recl=1)
			
	!	OPEN(unit=15,file=tempfile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)

		npagsav=npage
	    ntracet=ntrace
		write (15,rec=1) ivit,npagsav,npopen,ntrace,&
					(Popen(i),i=1,npopen),(tsegf(i),i=1,npopen),(npag(i,1),i=1,npopen),&
					(npag(i,2),i=1,npopen),(npopointsf(i),i=1,npopen),(indf(i,1),i=1,npopen),&
					(indf(i,2),i=1,npopen),testfil,delay,poplot,tseg,ngrid,&
					secline0,sdir,ytsep
	
	close(unit=15)
	
	else
	!	imes=gmdisplaymessagebox(tempfile,' does not exit.Create a new file',ginformation,gok)
	
		
	
		ivit=101
	!	OPEN(unit=15,file=tempfile,status='UNKNOWN',access='DIRECT',form='UNFORMATTED',recl=128)
        OPEN(unit=15,file=tempfile,status='UNKNOWN', access='DIRECT',form='BINARY',recl=1)
			
		ntracet=ntrace
		npagsav=npage
	
		write (15,rec=1) ivit,npagsav,npopen,ntrace, &
					(Popen(i),i=1,npopen),(tsegf(i),i=1,npopen),(npag(i,1),i=1,npopen),&
					(npag(i,2),i=1,npopen),(npopointsf(i),i=1,npopen),(indf(i,1),i=1,npopen),&
					(indf(i,2),i=1,npopen),testfil,delay,poplot,tseg,ngrid,&
				    secline0,sdir,ytsep
		close(unit=15)
	
	endif

endif

INQUIRE (FILE=iniiniFILE,EXIST=PRESENT)
if(PRESENT) then
			ihandle=FILE$FIRST
			length = GETFILEINFOQQ(iniinifile, info, ihandle)
			nLEN=info%length
			readini=.true.
			IRECL=10240
			OPEN(UNIT=19,FILE=iniiniFILE,&
            ACCESS='DIRECT',FORM='UNFORMATTED',RECL=irecl)
		    write(19,rec=1) itogrec,inifile,pfile,discprt,append,tempfile,initog
endif
! Close device MENU and GINO, and end program
   call gmCloseMenu
   call gCloseDevice
   call gCloseGino
	if(allocated(idata)) deallocate(idata)
	if(allocated(xval))deallocate(xval,yval)
	if(allocated(popend))deallocate(popend)


281	 format( '  Gain (pA/V) = ',g13.6,'  Calibration (pA/ADC unit) = ',g13.6,&
      '  Low pass filter (Hz, -3dB) = ',g13.6,/)
if(discprt) close(unit=8)	
28	format(' INPUT: continuous data file name: ',a30,'  Date of experiment: ',a11,' (patch number ',i3,')',/,&
      '  Tape details: ',a24,'  Patch type: ',a14,'  Membrane potential (mV) = ',f9.2,'  Temperature = ',f9.1)

102		format(' Plot/ filter sample-; Date of analysis: ',a11,' Computer : ',a20, ' User : ',a20)
1001	format(a15,1x,a15,1x,a15,1x,a15,1x,a15,1x,a15,1x,a15)
1002	format(i20,5(f16.5),i20)
1003    format(7(a16))

stop
end
