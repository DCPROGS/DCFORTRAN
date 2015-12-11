	subroutine RANDSK(ix,iy,iz,iwrite,repeat)
c To read (iwrite=0), or write (iwrite=1) seeds for RANDOM from D:\
c (1) Read: (a) Asks whether to repeat last sequence. If NO then output
c		ix,iy,iz and set REPEAT=false. If YES output ixlast,
c		iylast,izlast and set REPEAT=true.
c (2) Write: If REPEAT is true on entry then write nothing to disc
c		in case another repeat is needed. If REPEAT false on
c		entry then (a) replace current ixlast etc by ix etc from
c		disc, and (b) replace ix etc by input values
c MODIFIED 05/17/91 04:16pm so
c (3) iwrite=-1 skips question and does NOT repeat previous sequence
c
	logical repeat,present
	character*1 ans,UC
	character*30 ranseed
	logical discprt,pon,slock
	common/dp/discprt
	SAVE ranseed
c
	pon()=slock()
c
c101	format(a1)
c
	n=nblank1(ranseed)
c NB string when undefined is all ASCII 0, and nblank returns position of
c the first ascii 0
	if(n.ge.10) then
	   if(ranseed(n-9:n).ne.'random.dat') then
		ranseed='random.dat'  !file for seeds
	   endif
	else
	   ranseed='random.dat'  !file for seeds
	endif
22	INQUIRE(file=ranseed,exist=present,flen=len)
	if(.not.present.or.len.eq.0) then
	   ranseed='random.dat   '		!file for seeds
	   INQUIRE(file=ranseed,exist=present,flen=len)
	   if(.not.present.or.len.eq.0) then
		print 21
21	   	format(
     & ' File RANDOM.DAT not found in local directory or root')
	      call TITENT0('Path for RANDOM.DAT:',ranseed,30,.false.)
		goto 22
	   endif
	endif
      OPEN(unit=25,file=ranseed,status='UNKNOWN',
     & access='DIRECT',form='UNFORMATTED',recl=256)
c NB recl is in bytes (4*value for PDP)
	if(iwrite.eq.1) goto 1
c READ
	read(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
30	if(iwrite.eq.0) then
	   ans='N'
	   if(repeat) ans='Y'
	   print 108
108	   format(/)
	   call DCASK('Repeat preceding random number sequence',ans,ans)
c	   print 10
c10	   format(' Repeat preceding random number sequence [N] ? ')
c	   read 101,ans
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
	print 16,ix,iy,iz
16	format(' Seeds for RANDOM= ',3i8,' O.K. [Y] ?')
	ans='Y'
	call INPUTa(ans)
c	read 101,ans
	if(UC(ans).eq.'N') then
	   print 17
17	   format(' ix,iy,iz = ')
	   read 18,ix,iy,iz
18	   format(3i8)
	endif
	goto 2
c
c Write
1	continue
	if(repeat) goto 2	!no write
	read(25,rec=1) ix1,iy1,iz1,ixlast,iylast,izlast
	ixlast=ix1
	iylast=iy1
	izlast=iz1
	write(25,rec=1) ix,iy,iz,ixlast,iylast,izlast
c
2	close(unit=25)
c	print 1671,ix,iy,iz
	if(pon()) write(7,1671)ix,iy,iz
      if(discprt) write(8,1671)ix,iy,iz
1671	format(' Seeds for RANDOM= ',3i8)
c	print 11,repeat
c11	format(' repeat= ',L2)
	RETURN
	end


