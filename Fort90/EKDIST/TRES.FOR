	program tres
c (3) integer*1 iprops(i) holds 'properties' of i'th duration and amp
c (integer*1 has range -128 to +127 (bit 7 set gives -128; can use bits 0-6)
c	 0=all OK;
c	+1=amplitude dubious = bit 0;
c	+2=amplitude fixed = bit 1;
c	+4 amplitude of opening constrained (see fixamp) = bit 2;
c	+8=duration unusable = bit 3;
c	etc
c and keep sum of values of more than one property is true
c ie set bits using VAXIBM3 library functions (see TBIT.FOR)
c	IBSET(iprops,i)	-sets bit #i=0,...,7   (integer*4 function)
c	IBCLR(iprops,i)	-clears bit #i=0,...,7  (integer*4 function)
c	BTEST(iprops,i)   -=true if bit #i is set (logical function)
c
c Now use allocatable arrays
	allocatable::tint,tint0
	allocatable::ampl0,ampl
	allocatable::iprops,iprops0
	allocatable::index
	real*4 tint(:),tint0(:),ampl0(:),ampl(:)
	integer*1 iprops(:),iprops0(:)
	integer*4 index(:)
	logical cjump
	integer*2 kjumps0(1),kjumps(1)
	logical discprt
	common/dp/discprt
	common/rblck/treso,tresg,acrit,avamp
c
	cjump=.false.
	nsweep=1
	discprt=.true.
	OPEN(unit=8,file='H:\tres.prt',status='UNKNOWN',
     & form='FORMATTED',carriage control='FORTRAN')
c
	nintt=10
c
	ALLOCATE(tint0(nintt),iprops0(nintt),ampl0(nintt))
	ALLOCATE(tint(nintt),ampl(nintt),iprops(nintt))
	ALLOCATE(index(nintt))
c
	ampl0(1)=5.
	tint0(1)=0.05
	iprops0(1)=0
	ampl0(2)=2.5
	tint0(2)=1.
	iprops0(2)=0
c
	ampl0(3)=0.
	tint0(3)=2.
	iprops0(3)=0
c
	ampl0(4)=5.
	tint0(4)=5.
	iprops0(4)=0
	ampl0(5)=4.
	tint0(5)=15.
	iprops0(5)=0
c
	ampl0(6)=0.
	tint0(6)=0.05
	iprops0(6)=0
c
	ampl0(7)=5.
c=	tint0(7)=5.
	tint0(7)=0.1
	iprops0(7)=0
	ampl0(8)=0.
	tint0(8)=.05
	iprops0(8)=0
	ampl0(9)=6.
c=	tint0(9)=10.
	tint0(9)=0.1
	iprops0(9)=0
c
	ampl0(10)=0.
	tint0(10)=5.
	iprops0(10)=0
c
	ffilt=2.
	fc=2.
	rms=0.14
	trise=166.
	avamp=5.
	treso=0.1		!in ms
	tresg=0.1		!in ms
c
	imode=0		!call RESINT from EKDIST
c
	call RESINT4(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
     & nintt,imode,ffilt,fc,rms,trise,index,
     & cjump,nsweep,kjumps0,kjumps)
c
c	ihjc=1		!now superfluous
C	call RESINT3(tint0,tint,ampl0,ampl,iprops0,iprops,nint,
C     & nintt,imode,ffilt,fc,rms,trise,index,ihjc,
C     & cjump,nsweep,kjumps0,kjumps)
c
c To eject page without picking up another, send 'Esc E' to laserjet (OK for
c deskjet too?).  If this is NOT done then another plot can be put on same page.
	write(8,*) char(27)//'E'
	end

