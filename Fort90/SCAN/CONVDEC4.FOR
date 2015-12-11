	subroutine CONVDEC4(cdec,array,n)
c	subroutine CONVDEC4(cdec,cf77,n)
c To convert array of n real*4 values from DEC to Lahey format.
c The real*4 values are read from disc as 4 byte characters in CDEC.
c This subroutine rearranges the bytes with result in CF77 which can
c be written to disc; when read back as real*4 in Lahey fortran get
c correct values if result divided by 4.0
c	This version uses a virtual disk (see config.sys) to do last
c part of conversion here (min size=16 kB, but this must have overheads
c since must be made 17 kB to have enough free space to write a
c 16 kB=4096 real record, so this is max size of array that can be
c converted in this routine).  Array(n) is output as real*4 array with
c correct Lahey format that can be written directly to hard disc.
c	Use unit=999 for virtual disk to try to avoid clash of unit numbers
c with those in calling program
c
c	character*4 CDEC(*),CF77(4096)
c	character*4 CDEC(1024),CF77(1024)
	character*4 CDEC(*),CF77(1024)
	integer*2 n
	real*4 array(n)
c
	do 1 i=1,n
	cf77(i)(1:1)=cdec(i)(3:3)
	cf77(i)(2:2)=cdec(i)(4:4)
	cf77(i)(3:3)=cdec(i)(1:1)
	cf77(i)(4:4)=cdec(i)(2:2)
1	continue
c Write to virtual disk (drive G on Tosh)
c	OPEN(UNIT=999,file='G:\CONVDEC4.DAT',
c	OPEN(UNIT=999,file='M:\CONVDEC4.DAT',           !on Dells
c Use actual disc for now
	OPEN(UNIT=999,file='H:\CONVDEC4.DAT',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=16384)
	write (999,rec=1) (cf77(i),i=1,n)
	read(999,rec=1) array
	CLOSE(unit=999)
c
	do 2 i=1,n
2	array(i)=array(i)/4.0
c
	return
	end


