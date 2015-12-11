	subroutine READ_EDE(n,kfile,pfiles,nset,nfile,nval,irecs,
     &	srate,calfacs2,ffilt,nintt,ntot,nmax,main,hjcfitform,
     &   avamp,rms,fc,trise,tedit1,tedit2,
     &  tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,
     &  tval9,tvalt,tedit5,val9)
c Subroutine to read header of Dempster's dwell time files (*.ede format)
c and print the (few) details.
c
	USE DFLIB
	use menu_f90

	TYPE (FILE$INFO) info
	character*60 pfiles(20,10)
	logical autosim
      character*11 cdatew,qdatew
	character*33 pfile(20,10)	!path names for .EDE files
	integer kfile(20,10),nfile(10),nintt(10)
	real*4 calfacs2(20,10),ffilt(10)
	integer nval(20,10),irecs(20,10)
c
	integer*4 ChannelState,ZeroLevel,StartAt,Endat
	real*8    ExactStart,ExactEnd,Duration
	real*4    Average,Variance
	integer*4 Next,Previous
	logical*1 ignore,available
	real*4 avamp(10),rms(10),fc(10),trise(10)
	logical nopath
	character*40 tedit1(20,10),tedit2(20,10),tedit3(20,10)
	character*40 tedit5(20,10),tedit4(20,10)
	real tval1(20,10),tval2(20,10),tval3(20,10),tval4(20,10)
	real tval5(20,10),tval6(20,10)
	character*70 title
	real tvalt(5),tval7(20,10),tval8(20,10),tval9(20,10)
	logical discprt
	character*12 DataName
	common/dp/discprt
c
c <num event records>
c <Event Record 1>
c <Event Record 2>
c ...
c
c where
c
c <num event records> = No. of events in file (4 byte integer)
c
c and the event record is
c
c                                            start
c							   byte
c    <num event records>                       1
c  TEvent = packed record
c	     ChannelState : Integer ;  	4    0
c	     ZeroLevel : Integer ;     	4    4
c	     StartAt : Integer ;  	      4    8
c	     EndAt : Integer ;        	4    12
c	     ExactStart : double ;     	8    16
c	     ExactEnd : double ;       	8
c	     Duration : double ;      	8
c	     Average : single ;             4
c	     Variance : single ;       	4
c	     Next : Integer ;          	4
c	     Previous : Integer ;      	4
c	     Ignore : ByteBool  ;      	1
c	     Available : ByteBool ;    	1
c	     Pad : Array[1..2] of Byte ;    2 ??
c	     end ;
c                          Total length   60 bytes (if pad=2 bytes)
c Integer = 4 byte integer
c Double = 8 byte double precision floating point
c ByteBool = 1 byte flag
c
c The important fields are
c
c Channel State : 0 = closed, 1 = open
c Average : Mean current during event (pA)
c Variance : Variance of current during event (pA2)
c Duration : Duration of dwell time (s)
c Ignore : True = Rejected from analysis
c
c
c Now read all files
      call DATE(qDATEW)
      cDATEW=qDATEW(1:7)//'20'//qDATEW(8:9)
      SRATE=10.
      ignore=.false.
	do j=1,nset
	ffilt(j)=100.
	 nintt(j)=0		!# of obs for set j
	 do i=1,nfile(j)
	   CLOSE(unit=18)
	   OPEN(unit=18,file=pfiles(i,j),status='UNKNOWN',
     &		access='DIRECT', form='BINARY', RECL=1)
	   irec=1
	   read(unit=18,rec=irec) nint
	   irecs(i,j)=1	!offset to start of data records

	   irec1=5
	   nig=0
	   do n=1,nint
	     read(unit=18,rec=irec1) ChannelState,ZeroLevel,StartAt,Endat,
     &      ExactStart,ExactEnd,Duration,
     &	Average,Variance,
     &	Next,Previous,
     &	ignore,available
		if(ignore) nig=nig+1
	      irec1=irec1+60
	
	   enddo
cc end test read
	   ntot=ntot+nint
	   nintt(j)=nintt(j) + nint 	!accumulate total number for set j
	   nval(i,j)=nint
	   calfacs2(i,j)=1.0
	   tval1(i,j)=nint
	   		!amplitude already in pA
c      print details for each expt file
c	   print 28,j,pfile(i,j),nint,nig
28	   format(/,' SET # ',i3,/,
     &       ' Dempster .EDE file ',/,
     &	 ' Results file: ',a33,/,
     &	 ' Number of events = ',i8,/,
     &	 ' Number of events to be ''ignored'' = ',i8,/)
70	  continue 
c       print 281,ffilt(j)*1.e3
281	   format(
     &  ' Specify filter frequency (-3 dB) for this file (Hz) [',
     &	f10.3,'] = ')
c	   call INPUTr(ffilt(j))
	   if(ffilt(j).lt.100.) goto 70
	   ffilt(j)=ffilt(j)*1.e-3	!kHz
c
	   if(discprt) write(7,282) j,pfile(i,j),ffilt(j),nint,nig
282	   format(/,' SET # ',i3,/,
     &       ' Dempster file: ',/,
     &	 ' Results file: ',a33,/,
     &       ' Filter (kHz, -3 dB) = ',f12.5,4x,/,
     &	 ' Number of transitions = ',i8,/,
     &	 ' Number of events to be ''ignored'' = ',i8,/)

	 enddo		!end if i=1,nfile loop
	 if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!end if j=1,nset loop
         
         temp=21
         holding=-35.
	   emem=holding
	   
         do j=1,nset
		    avamp(j)=3.
		    rms(j)=0.3
		    atemp=atemp+temp
	      aavamp=aavamp+avamp(j)
	      arms=arms+rms(j)
	      aEmem=aEmem+Emem
	      n3=n3+1
	   enddo
         do j=1,nset
	
	   do i=1,nfile(j)
            tedit1(i,j)=pfiles(i,j)(1:40)
			tedit2(i,j)=cDATEW
			tedit3(i,j)=title
		!	tedit4(i,j)=tapeid
			
		
			tval3(i,j)=emem
			tval4(i,j)=temp
			tval5(i,j)=ffilt(j)
			tval6(i,j)=avamp(j)
			tval7(i,j)=rms(j)
			tval8(i,j)=calfacs2(i,j)
			tval9(i,j)=30.
	      tedit5(i,j)=dataname
         enddo
         enddo
         en=float(n3)
	   temp=atemp/en
	   aavamp=aavamp/en
	   arms=arms/en
	   Emem=aEmem/en
c
		tvalt(1)=ntot 
		tvalt(2)= temp
		tvalt(3)= aavamp
		tvalt(4)= arms
		tvalt(5)= emem
         if(discprt) write(7,46) ntot,nset
46	   format(/,
     &     ' Total number of transitions = ',i8,', in ',i4,' sets.',/)
	
c	
	
	do j=1,nset
		if(ffilt(j).le.0.) then
      		if(discprt) write(7,45) j
45			format(' Filtering and rise-time not defined for set ',i3)
			idest=0
		 else
			fc(j)=ffilt(j)		!kHz (-3dB)
			trise(j)=332.1/fc(j)		!rise time (microsec)
	     if(discprt) write(7,4011) j,fc(j),trise(j)
4011	      format(' SET ',i3,
     &': Filter: fc(kHz,-3 dB)= ',f8.2,';  risetime (microsec)= ',
     & g13.6,/)
		endif
	enddo
	if(main.ne.-1) then 
	idest=0
	icall=3031
	autosim=.false.
	idatyp=6
	call hjcfit_table(main,hjcfitform,nset,nfile,pfileS,tedit1,tedit2,
     & tedit3,tedit4,tval1,tval2,tval3,tval4,tval5,tval6,tval7,tval8,
     & tval9,tvalt,tedit5,fc,ffilt,trise,autosim,icall,val9,idatyp)
	endif
	RETURN
	end


