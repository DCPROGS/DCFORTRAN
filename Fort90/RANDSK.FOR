	subroutine RANDSK(ix,iy,iz,iwrite,repeat)
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from D:\
c
c Modified 05/10/04 09:20pm toadd option to use TIMER to select seeds
c (ranseed.for)
c
c Modified 01/28/02 09:59am by addition of
c  iwrite=2.  This writes the current ix, iy, iz, BUT do not alter
c  ixlast etc.  This is useful for repeated simulations when whole thing
c  may need to be repeated starting from ixlast etc, still need to record
c  the latest ix,iy,iz in case the simulations are restarted (eg after crash)
c
c MODIFIED 05/17/91 04:16pm so
c (3) iwrite=-1 skips question and does NOT repeat previous sequence
c
c
c (1) Read: (a) Asks whether to repeat last sequence. If NO then output
c		ix,iy,iz and set REPEAT=false. If YES output ixlast,
c		iylast,izlast and set REPEAT=true.
c (2) Write: If REPEAT is true on entry then write nothing to disc
c		in case another repeat is needed. If REPEAT false on
c		entry then (a) replace current ixlast etc by ix etc from
c		disc, and (b) replace ix etc by input values
c
c Modif 12/16/01 06:22am by adding common/ranlast/
c
	logical repeat,present
	character*1 ans,UC
	character*30 ranfile
	logical discprt
	common/dp/discprt
	common/ranlast/ixlast,iylast,izlast
	SAVE ranfile
c
c
	n=nblank1(ranfile)
c NB string when undefined is all ASCII 0, and nblank returns position of
c the first ascii 0
	if(n.ge.10) then
	   if(ranfile(n-9:n).ne.'random.dat') then
		ranfile='random.dat'  !file for seeds
	   endif
	else
	   ranfile='random.dat'  !file for seeds
	endif
22	INQUIRE(file=ranfile,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   ranfile='\random.dat  '		!file for seeds
	   INQUIRE(file=ranfile,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
		ranfile='random.dat  '		!file for seeds
		print 21
21	   	format(
     & ' File RANDOM.DAT not found in local directory or root')
	      call TITENT0('Path for RANDOM.DAT:',ranfile,30,.false.)
		goto 22
	   endif
	endif
      OPEN(unit=25,file=ranfile,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=256)
c NB recl is in bytes (4*value for PDP)
	if(iwrite.eq.1) goto 1
	if(iwrite.eq.2) goto 2
c READ
	read(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
30	if(iwrite.eq.0) then
	   ans='N'
	   if(repeat) ans='Y'
	   print 108
108	   format(/)
	   call DCASK('Repeat preceding random number sequence',ans,ans)
	   repeat=UC(ans).eq.'Y'
	else if(iwrite.eq.-1) then
	   if(ix.eq.0.or.iy.eq.0.or.iz.eq.0) then
		iwrite=1
		goto 30
	   endif
	   repeat=.false.
	   goto 2
	endif
	if(repeat) then
	   ix=ixlast		!if repeat
	   iy=iylast		!if repeat
	   iz=izlast		!if repeat
	endif
	if(ix.eq.0.or.iy.eq.0.or.iz.eq.0) then
	   print 161,ix,iy,iz
161	   format(
     & ' Seeds for RANDOM = ',3i8,': do not start with zero values',/,
     & ' Enter three non-zero ''random'' integers separated by  commas')
	   ans='N'
	else
	   print 16,ix,iy,iz
16	   format(
     & ' Seeds for RANDOM= ',3i8,' O.K. [Y] ?')
	   ans='Y'
	   call INPUTa(ans)
	endif
	if(UC(ans).eq.'N') then
171	   print 17
17	   format(
     & ' (1) Choose ''random'' seeds from system time',/,
     & ' (2) Enter three non-zero integers separated by commas',/,
     & ' Option number [1] = ')
	   iopt=1
	   call INPUTi(iopt)
	   if(iopt.eq.1) then
	      call RANSEED()
		print 16,ix,iy,iz
c16		format(
c     & ' Seeds for RANDOM= ',3i8,' O.K. [Y] ?')
		ans='Y'
		call INPUTa(ans)
		if(UC(ans).eq.'N') goto 171
	   else
		print 19,ix,iy,iz
19		format(
     &' Enter three non-zero integers separated by commas',/,
     & ' ix,iy,iz [',i8,',',i8,',',i8,'] = ')
	      read 18,ix,iy,iz
18	      format(3i8)
	   endif
	endif
	goto 2
c
c Write
1	continue
	if(repeat) goto 9		!no write
	read(25,rec=1) ix1,iy1,iz1,ixlast,iylast,izlast
	ixlast=ix1
	iylast=iy1
	izlast=iz1
	write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
	goto 9
c
c the new write option: iwrite=2. Leave ixlast etc unchanged
2	continue
	read(25,rec=1) ix1,iy1,iz1,ixlast,iylast,izlast
	write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
c
c
9	close(unit=25)
      if(discprt) write(8,1671)ix,iy,iz
1671	format(' Seeds for RANDOM= ',3i8)
	RETURN
	end


