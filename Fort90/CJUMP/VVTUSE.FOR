	program vvTUSE11

c	PROGRAM TO TEST 1401 WITH  FORTRAN
C	LINKS WITH MODULE 1401LIB.FOR
C	18.05.93

	integer*2	flag,err,status,point
	integer*2	flag1,flag2,flag3,flag4,flag5
	integer*2	obuff(0:4)
	integer*2	ibuff(0:4),ktype
	character*1 buff(0:20),ch,getch
	integer*4   lval(0:3)
	logical 	kbhit

	ERR_NOERR=0
	call ForOpen1401(flag)
	call ForSendString('ERR;',10,ierr)
	call ForGetString(buff,15,ierr)
	if(buff(0).ne.'0'.or.buff(2).ne.'0') then
		print*,buff(0),buff(1),buff(2),buff(3),buff(4),buff(5)
	endif
	print*,'Open flag was =',flag
	if (flag.ne.ERR_NOERR) goto 999
		obuff(0)=12
		obuff(1)=15
		obuff(2)=18
		obuff(3)=15
		obuff(4)=12
		flag=-1
		call ForSendString('KILL;EEE;ERR;',13,flag)
		print*,'Return string  '
		call ForGetString(buff,15,flag)
		call ForSendString('WRADR,1,2,0;',13,flag)
		call ForSendString('WRADR,1,3,0;',13,flag)
		call ForSendString('RDADR,1,3;',10,flag)
		print*,'RDADR before 1401 '
		call ForGetString(buff,15,flag)
		call ForTo1401(obuff,10,0,2,flag)
		print*,'Return value from To1401 ',flag
		call ForSendString('RDADR,1,2;',10,flag)
		print*,'RDADR after 1401 '
		call ForGetString(buff,15,flag)
		call ForLd('c:\1401\','ADCMEM,ADCBST,MEMDAC',1,13,err,flag)
		print*,'Return value from Ld ',flag
		inc=1
		print*,'START'
333		continue
		call ForSendString('ADCMEM,I,2,2048,16384,0,100,C,20,5;',
     &					32,flag)

1		continue
		call ForSendString('ADCMEM,P;',9,flag1)
		call ForLongsFrom1401(lval,2,flag2)
		point=lval(0)

		call ForSendString('ADCMEM,?;',9,flag3)
		call ForLongsFrom1401(lval,2,flag4)
		status=lval(0)
		call set_cursor(1,27)
		print*,'ADCMEM status',status,'pointer',point,
     &				'(',ibuff(0),ibuff(1),')'
		if(status.ne.0.and.point.ne.0) goto 1
		if (kbhit()) then
			ch=getch(ktype)
			print*,ktype
			call clrkb()
			if(ktype.eq.16) goto 666
		endif
		call ForToHost(ibuff,10,0,2,flag5)
		inc=inc+1
		print*,inc
		goto 333

666		do i=0,4
			if (obuff(i).ne.ibuff(i)) then
				print*,'Data error at locn',i,'write',obuff(i),
     &				 ' read',ibuff(i)
				flag=flag+1
			endif
		end do

	call ForClose1401()

999	end
