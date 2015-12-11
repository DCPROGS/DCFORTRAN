	subroutine READ_EDE(n,kfile,pfile,nset,nfile,nval,irecs,
     &	calfacs2,ffilt,nintt,ntot,nmax)
c Subroutine to read header of Dempster's dwell time files (*.ede format)
c and print the (few) details.
c
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
	logical debug
c
	logical discprt
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
	do j=1,nset
	 nintt(j)=0		!# of obs for set j
	 do i=1,nfile(j)
	   CLOSE(unit=18)
	   OPEN(unit=18,file=pfile(i,j),status='UNKNOWN',
     &		access='TRANSPARENT')
	   irec=1
	   read(unit=18,rec=irec) nint
	   irecs(i,j)=1	!offset to start of data records
cc test read -read anyway to count 'ignores'
c	   debug=.true.
	   debug=.false.
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
		if(debug) then
		   print 1,n,ChannelState,Duration*1.d3,Average
1		   format(
     &	' i = ',i3,' state = ',i3,' t (ms) = ',g11.4,' amp = ',f8.3)
		endif
	   enddo
cc end test read
	   ntot=ntot+nint
	   nintt(j)=nintt(j) + nint 	!accumulate total number for set j
	   nval(i,j)=nint
	   calfacs2(i,j)=1.0		!amplitude already in pA
c      print details for each expt file
	   print 28,j,pfile(i,j),nint,nig
28	   format(/,' SET # ',i3,/,
     &       ' Dempster .EDE file ',/,
     &	 ' Results file: ',a33,/,
     &	 ' Number of events = ',i8,/,
     &	 ' Number of events to be ''ignored'' = ',i8,/)
70	   print 281,ffilt(j)*1.e3
281	   format(
     &  ' Specify filter frequency (-3 dB) for this file (Hz) [',
     &	f10.3,'] = ')
	   call INPUTr(ffilt(j))
	   if(ffilt(j).lt.100.) goto 70
	   ffilt(j)=ffilt(j)*1.e-3	!kHz
c
	   if(discprt) write(8,282) j,pfile(i,j),ffilt(j),nint,nig
282	   format(/,' SET # ',i3,/,
     &       ' Dempster file: ',/,
     &	 ' Results file: ',a33,/,
     &       ' Filter (kHz, -3 dB) = ',f12.5,4x,/,
     &	 ' Number of transitions = ',i8,/,
     &	 ' Number of events to be ''ignored'' = ',i8,/)
c
c	   avamp(j)=avamp(j)*calfac2		!convert to pA
c
	 enddo		!end if i=1,nfile loop
	 if(nintt(j).gt.nmax) nmax=nintt(j)
	enddo		!end if j=1,nset loop

	RETURN
	end


