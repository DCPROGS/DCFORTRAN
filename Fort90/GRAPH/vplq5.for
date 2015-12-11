	subroutine VPLQ5(xval,yval,xcal,ycal,ndimd,ndimc,
     & ncurvd,ndat,icurvd,isym,ijoin,ncurvc,ncal,icurvc,iline,syms,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ntx,nty,itx,ity,
     & xlo,xhi,ylo,yhi,itit,title1,ifont,ilog,iscal,doframe,
     & titlex,titley,ilabel,inumx,inumy,idiskq,qfile,sval,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & theta,ifitype,ncomp,idest,icol,mono,interp,
     & RLTH,RX,RY,rxbox,rybox,IJUS,ANGle,idraw,ifnt,size,
     & cnumx,cnumy,cexpx,cexpy,numbx,numby,
     & narrow,xb,yb,xe,ye,ntext,nline,xlb,ylb,xle,yle,iltype,
     & newtext,y0,yinf,
     & nhline,yhline,xhlb,xhle,ilhtype,
     & nvline,xvline,yvlb,yvle,ilvtype,ivlinrel,ihlinrel,
     & ntrace,ytsep,ndv1,ndc1,
     & weight,kwi,kwj,icurvw,kmax)
c
c========================================================
c	29.08.96 Ioana
c	in gino line thikness and angles are real,
c     but you stored them in plotq.dat as integeres
c     for the moment I let them like this but I presume in the future you
c     should changle the file format.
c========================================================
c
c NB if iver value here is changed must check all refs to iver in
c SCQPLT and CONVQREC too
c
cNB WOULD BE MUCH SIMPLER TO WRITE TO PLOTQ AS TRANSPARENT FILE -BUT
cTHEN WOULD HAVE TO MAKE JSTREC() etc integer*4	AGAIN!!!
c Compromise would be to write as transparent, but start each new plot
c on a 1024 byte boundary, for consistency with writing in 1024 byte
c records as in all versions so far, and keep in jstrec(),lstrec() the
c number of the actual byte (as in transparent file) divided by 1024.
c TRY THIS NOW!
c========================================================
c VPLQ5 is to queue plots in VPLOT5: weight,kwi,kwj,icurvw added
c to args (weight moved to last line); isdev removed from args (kept internally
c and set=1 if ANY of the icurvw() are 0 or 1 so at least some weights
c are defined, otherwise set to -1)
c Also add kmax=declared dimension of theta() to call
c IPTYPE=15 for VPLOT5; order of writing has been altered.
c Now ndat,icurvd,icurvw,isym,ijoin,symsiz (all ndimd) and
c ncal,icurvc,iline (all ndimc) are not fixed in size so written with
c datcop() etc, rather than in first record; ditto for theta(kmax).
c
c 05/22/95 07:23am DATCOP dimension increased to 100000 in RDVPLQ and VPLQ5
c
c VPLQ4 is to queue plots in VPLOT4 (there is no VPLQ3: VPLOT3 used VPLQ2)
c iptype,ndv1,ndimd,ndc1,ndimc,... written first to simplify reading of
c dimensions of allocatable arrays in AUTPLOT
c
c Modified 09/10/94 09:06pm for multiple traces
c Modified 09/06/94 12:31pm to include horizontal/vertical lines (line 13 of
c   parameters); ihlinrel, ivlinrel are integer*1, but easier to store
c   them as integer*2 here (OK see TINT1.for) !!
c Modified 02/09/93 03:51pm to add ICOL(), mono and interp
c Modified 02/09/93 03:51pm to add ICOL(), mono and interp (and jcol=1002 if
c colours and interp have been queued)
c Modified 08/12/92 02:18pm so jstrec(), lstrec() both have dimension=200
c (must be read and written in two sections for compatibility with old files)
c Subroutine for VPLOTR to put data in PLOT QUEUE file (based on VPLQ).
c Signal whether jump logos to be drawn by setting iy1 negative to
c omit logo
c Last 3 lines are extra args to hold all details of posh plots.
c  (1) Arguments removed ipen,mpos,mlast,ivel,xlo,...,yhi,
c	ipdis,isup,nlenx,nleny
	allocatable::datcop,int2cop,int4cop,charcop
	real*4 DATCOP(:)
	integer*2 INT2COP(:)
	integer*4 INT4COP(:)
	character*10 CHARCOP(:)
	integer*1 idum(1)		!to fill file to length that is multiple of 1024
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character cnum1*11,cnum2*11		!for dialog
	real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
	real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
	dimension ndat(ndimd),icurvd(ndimd),isym(ndimd),ijoin(ndimd)
	dimension ncal(ndimc),icurvc(ndimc),iline(ndimc)
	real*4 syms(ndimd)
	real theta(kmax)
	integer*2 jstrec(200),lstrec(200)		!for queue
c=	integer*4 jstrec1(100),lstrec1(100)		!for old queue
c	integer*2 lrow
	character*1 ans,UC
	character ndev*2,qfile*40
	character*40 titlex,titley
	character*64 title1,title2	!plot title
	LOGICAL doframe,slock,pon,ivplot,present,newfil
	logical mono,interp
	logical discprt
	logical caplock,debug
c new arrays for horizontal lines
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
c new arrays for vertical lines
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
c extra args to hold all details of posh plots
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	real RLTH(100) 		!for line thickness
	real RX(100),RY(100)
	real rxbox(4,100),rybox(4,100)
	integer IJUS(100)
	integer idraw(100),icol(100)
	real angle(100)
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25)
	integer ifnt(30)
	real size(30)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	character*80 newtext(20)		!extra text
c new form for weights in VPLOT5
	real*4 weight(kwi,kwj)
	integer*4 icurvw(ndimd)
c integer*2 copies to save disc space
	integer*2 IJUS1(100)
	integer*2 idraw1(100),icol1(100)
c
	common/dp/discprt
c
	pon()=slock()
	debug()=caplock()
c
3	format(i8)
	idest=0
c	iver=1001
      iver=1100	!NB also set below
	ict=13	!text colour for DIALOG box
	if(mono) ict=15
	idum(1)=0		!to fill file to length that is multiple of 1024
c Modif 03/26/97 02:28pm to calculate suitable size for datcop
	ndim=0
	if(ncurvd.gt.0) then
	   do j1=1,ncurvd
		j=icurvd(j1)
		ndim=ndim+ndat(j)*2	!for xval, yval
	   enddo
	   if(icurvw(j).ge.0) then
		do j1=1,ncurvd
		   j=icurvd(j1)
		   ndim=ndim+ndat(j)	!for weight()
		enddo
	   endif
	endif
	if(ncurvc.gt.0) then
	   do j1=1,ncurvc
		j=icurvc(j1)
		ndim=ndim+ncal(j)*2	!for xcal, ycal
	   enddo
	endif
	ndim=ndim+kmax+80+ntext*8+numbx*7+numby*7+narrow*4+nline*4
	ndim=ndim+(nhline+nvline)*3
	ndim=ndim + 256		!+256 in case!
c
	ALLOCATE(datcop(ndim),int2cop(1000),charcop(100))
	ALLOCATE(int4cop(ndimd+ndimc))
c
c Set isdev = 1 if ANY of the icurvw() are 0 or 1 so at least some weights
c are defined, otherwise set to -1

c	isdev=-1		!no weights at all
c	do i=1,ncurvd
c 	  if(icurvw(i).ge.0) isdev=1
c	enddo
c
c Make integer*2 copies to save disc space
	do 50 j=1,100
	 IJUS1(j)=IJUS(j)
	 idraw1(j)=idraw(j)
	 icol1(j)=icol(j)
50	continue
c Signal whether jump logos to be drawn by setting iy1 negative to
c omit logo
c	if(idraw(28).eq.0) iy1c=-1	!omit c-jump logo
c	if(idraw(29).eq.0) iy1v=-1	!omit v-jump logo
c
c NB potential problem in writing arrays ndat,icurvd,...,ijoin as they
c have got variable dimensions!
c Not necessary (or desirable) to write the arrays icurvd,Icurvc because if
c arrays 2,4,5 say to be plotted better to write them, and read them back,
c as arrays 1,2,3 so need keep only the total number of data/curve arrays
c (ncurvd,ncurvc respectively)- so ndat(),isym(),ijoin() need to be
c renumbered 1,...,ncurvd; and ncal(),iline() renumbered 1,..,ncurvc. Put them
c in new arrays (suffix 1) of fixed dimension. At the same time
c get number of records for each (1) data (2) curve array and work out
c total number of records need. Have 512 reals per record.
c
c Make compressed data array
c If ISDEV=0 or 1 then write WEIGHT(i,j) for each set also, for SD bars;
c If icurvw(j) =0 or 1 then write WEIGHT(i,j) for each set also, for SD bars;
c TO SAVE space use following method
c (a) make sure records are full so write Xval,Yval,Wt with no gaps
c (b) no need to write Xcal,Ycal in most cases: can just specify theta
c	and model #, so calc curve(s) can be recalc in AUTPLT. Only
c	problem with this is that VPLOT is used in many programs so
c	may have problems in specifying in AUTPLT how calc curve is to
c	be calculated- better leave Ycal in for now
	k=0		!index for datcop
	if(ncurvd.le.0) goto 365
	do j1=1,ncurvd
	   j=icurvd(j1)
	   n=ndat(j)
	   do i=1,n
		k=k+1
		datcop(k)=Xval(i,j)
	   enddo
	   do i=1,n
		k=k+1
		datcop(k)=Yval(i,j)
	   enddo
	   k=k+1
	   datcop(k)=syms(j)
	   if(icurvw(j).ge.0) then
		do i=1,n
		   k=k+1
		   datcop(k)=weight(i,j)
		enddo
	   endif
	enddo
365	continue
c Write theta() here now (will be defined only if ncurvc>0)

	if(ncurvc.gt.0) then
	   do jk=1,ncurvc
		if(ncal(jk).gt.ndc1) ncal(jk)=ndc1
	   enddo
	   do j1=1,ncurvc
		j=icurvc(j1)
		n=ncal(j)
		do i=1,n
		   k=k+1
		   datcop(k)=xcal(i,j)
		enddo
		do i=1,n
		   k=k+1
 		   datcop(k)=ycal(i,j)
		enddo
	   enddo
	   do i=1,kmax
		k=k+1
 		datcop(k)=theta(i)
	   enddo
	endif		!end of ncurvc>0
366	continue
c
c Now add onto DATCOP the real*4 data for posh plots- keep incrementing k
c
	do 682 i=1,10
	   k=k+1
	   DATCOP(k)=size(i)
         k=k+1
         DATCOP(k)=angle(i)

c	if(debug()) print 800,i,size(i),k,datcop(k)
c800	format(i8,g13.6,5x,i8,g13.6)
	   k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 680 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
680	   continue
682	continue		!keep 1-10 always
c
	if(ntext.gt.0) then
	do 68 i1=1,ntext
	   i=i1+10
	   k=k+1
	   DATCOP(k)=size(i)
	   k=k+1
         DATCOP(k)=angle(i)
         k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 681 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
681	   continue
68	continue
	endif
	do 69 i1=1,numbx
	   i=i1+30

	   k=k+1
         DATCOP(k)=angle(i)
         k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 691 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
691	   continue
69	continue
	do 70 i1=1,numby
	   i=i1+55
	   k=k+1

         DATCOP(k)=angle(i)
         k=k+1
	   DATCOP(k)=rx(i)
	   k=k+1
	   DATCOP(k)=ry(i)
	   do 701 j=1,4
	     k=k+1
	     DATCOP(k)=rxbox(j,i)
	     k=k+1
	     DATCOP(k)=rybox(j,i)
701	   continue
70	continue
	if(narrow.gt.0) then
	   do 71 i=1,narrow
	   k=k+1
	   DATCOP(k)=xb(i)
	   k=k+1
	   DATCOP(k)=xe(i)
	   k=k+1
	   DATCOP(k)=yb(i)
	   k=k+1
	   DATCOP(k)=ye(i)
71	   continue
	endif
	if(nline.gt.0) then
	   do 72 i=1,nline
	   k=k+1
	   DATCOP(k)=xlb(i)
	   k=k+1
	   DATCOP(k)=xle(i)
	   k=k+1
	   DATCOP(k)=ylb(i)
	   k=k+1
	   DATCOP(k)=yle(i)
72	   continue
	endif
c now add new lines
	if(nhline.gt.0) then
	   do i=1,nhline
		k=k+1
		DATCOP(k)=xhlb(i)
		k=k+1
		DATCOP(k)=xhle(i)
		k=k+1
		DATCOP(k)=yhline(i)
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
		k=k+1
		DATCOP(k)=xvline(i)
		k=k+1
		DATCOP(k)=yvlb(i)
		k=k+1
		DATCOP(k)=yvle(i)
	   enddo
	endif
      do 60 i=1,30	!keep 1st 30 RLTH always
	   k=k+1
	   DATCOP(k)=RLTH(i)
60	continue
	if(ntext.gt.0) then
	   do 61 i=1,ntext
	   k=k+1
	   DATCOP(k)=RLTH(i+30)
61	   continue
	endif
	if(narrow.gt.0) then
	   do 62 i=1,narrow
	   k=k+1
	   DATCOP(k)=RLTH(i+50)
62	   continue
	endif
	if(nline.gt.0) then
	   do 63 i=1,nline
	   k=k+1
	   DATCOP(k)=RLTH(i+60)
63	   continue
	endif

c end of DATCOP
	k1=k
	nbyte1=4*k
c
c Write newtext as they are -number of nytes need are:
	k2=ntext
	nbyte2=k2*80	!newtext(i) are character*80
c
c and for CNUMX etc
	k=0		!index for CHARCOP
	do 73 i=1,numbx
	k=k+1
	CHARCOP(k)=cnumx(i)
	k=k+1
	CHARCOP(k)=cexpx(i)
73	continue
c
	do 74 i=1,numby
	k=k+1
	CHARCOP(k)=cnumy(i)
	k=k+1
	CHARCOP(k)=cexpy(i)
74	continue
	k3=k
	nbyte3=10*k
c
c make compressed array for int*2 values
	k=0		!index for int2cop
	if(nhline.gt.0) then
	   do i=1,nhline
		k=k+1
		INT2COP(k)=ilhtype(i)
		k=k+1
		INT2COP(k)=(ihlinrel(i))	!ihlinrel actually int*1 !
	   enddo
	endif
	if(nvline.gt.0) then
	   do i=1,nvline
		k=k+1
		INT2COP(k)=ilvtype(i)
		k=k+1
		INT2COP(k)=(ivlinrel(i))	!ivlinrel actually int*1 !
	   enddo
	endif
c
	do 64 i=1,10
	   k=k+1
	   INT2COP(k)=ijus1(i)
	   k=k+1
	   INT2COP(k)=idraw1(i)
64	continue
	if(ntext.gt.0) then
	   do 65 i=1,ntext
	   k=k+1
	   INT2COP(k)=ijus1(i+10)
	   k=k+1
	   INT2COP(k)=idraw1(i+10)
65	   continue
	endif
	do 66 i=1,numbx
	   k=k+1
	   INT2COP(k)=ijus1(i+30)
	   k=k+1
	   INT2COP(k)=idraw1(i+30)
66	continue
	do 67 i=1,numby
	   k=k+1
	   INT2COP(k)=ijus1(i+55)
	   k =k+1
	   INT2COP(k)=idraw1(i+55)
67	continue
c
c Now store ICOL() array (at end if INT2COP, so read only for versions
c that have colour)
	do i=1,30	!keep 1st 30 ICOL always
	   k=k+1
	   INT2COP(k)=icol1(i)
	enddo
	if(ntext.gt.0) then
	    do i=1,ntext
	      k=k+1
	      INT2COP(k)=icol1(i+30)
	    enddo
	endif
	if(narrow.gt.0) then
	    do i=1,narrow
	      k=k+1
	      INT2COP(k)=icol1(i+50)
	    enddo
	endif
	if(nline.gt.0) then
	    do i=1,nline
	      k=k+1
	      INT2COP(k)=icol1(i+60)
	    enddo
	endif
	if(nhline.gt.0) then
	    do i=1,nhline
	      k=k+1
	      INT2COP(k)=icol1(i+71)
	    enddo
	endif
	if(nvline.gt.0) then
	    do i=1,nvline
	      k=k+1
	      INT2COP(k)=icol1(i+81)
	    enddo
	endif
c record background colour
	k=k+1
	INT2COP(k)=icol1(71)
c Add isym,ijoin,iline,icurvw() for VPLOT5
c and add icurvw() for VPLOT5
	do j1=1,ncurvd
	   j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT2COP(k)=(isym(j))
	enddo
	do j1=1,ncurvd
	   j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT2COP(k)=(ijoin(j))
	enddo
c= Best copy whole of icurvw() always, so then need not write
c   weights if icurvw(j)=-1, ie no weights defined
c=	if(isdev.ge.0) then
	   do j1=1,ncurvd
		j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
		k=k+1
	      INT2COP(k)=(icurvw(j))
	   enddo
c=	endif
	do j1=1,ncurvc
	   j=icurvc(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT2COP(k)=(iline(j))
	enddo
c
	k4=k
	nbyte4=k*2	!no of bytes in  INT2COP
c End of int2cop
c Finally integer*4 values: ndat() and ncal() must be integer*4
c (they could be > 32k)
c -number of bytes that these occupy is = 4*ncurvd+4*ncurvc
	k=0		!index for int4cop
	do j1=1,ncurvd
	   j=icurvd(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT4COP(k)=ndat(j)
	enddo
	do j1=1,ncurvc
	   j=icurvc(j1)      !curves renumbered as 1,2,3,... for queue
	   k=k+1
	   INT4COP(k)=ncal(j)
	enddo
c
	k5=k
	nbyte5=4*(ncurvd+ncurvc)
c
c== Total number of records
c Total number of bytes
c First bit actually occupies 552 bytes at present, but leave gap after
c this before writing int2cop etc, in case it is necessary to add other
c things that have to be read first.
c	nbytet=552        !'first record'
	ioffset=768
	nbytet=ioffset    !'first record'
	nbytet=nbytet+nbyte1+nbyte2+nbyte3+nbyte4+nbyte5
c Calculate the number of 1024 byte records that this would occupy if
c fixed record length had been use.
c
	nrect=1 + (nbytet-1)/1024	!for everything
c OPEN DISC FILE
441	continue
	newfil=.false.
	call GBLANK(qfile,40,n1,n2)
	if(n1.eq.0.and.n2.eq.0) idiskq=-1	!qfile not defined
	if(idiskq.ge.1) then	  !qfile already defined
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Queue in '//charnb(qfile)//' O.K.',
     &	   defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'Y') then
		ndev=qfile(1:1)
c=	      ndev=char(idiskq+64)//':'	!see def of idiskq below
	      if(ndev.eq.'A:'.or.ndev.eq.'B:') then
	         ans='Y'
	         call DEFOLTa(ans,defolt)
	         call QDIALOG(1,'Is disc '//ndev//' mounted',
     &	      defolt,ict,cans)
	         call GETINPa(cans,ans)
		   if(ans.eq.'N') goto 25          !specify disc
		endif
		goto 48
	   endif
	endif
c Next bit when qfile not yet defined (or A: not mounted)
	qfile='PLOTS.PLQ'	!default
25	continue
c=	call DISCDIALOG(1,'Specify disc for plot queue file',
c=     &	idiskq,ndev,-1,1,ict)
c=	if(idiskq.eq.-1) goto 99	!if ESC hit then do not queue anything
	call TDIALOG(1,'Give path and name for queue file',
     &  qfile,40,.false.,ict)
	idiskq=ichar(qfile(1:1)) - 64
	if(idiskq.lt.1) idiskq=1  !value not needed now -just signals when qfile set
48	continue
c=	qfile=ndev//filnam
	INQUIRE(file=qfile,exist=present,flen=nlen)
	if(.not.present.or.nlen.eq.0) then
	   ans='Y'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,
     & 'Queue file '//CHARNB(qfile)//' does not exist: create it',
     &    defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') goto 4081
c
	   nplot=0
         iver=1100	!set here in case reset by read from old format queus
	   do 482 i=1,200
	   jstrec(i)=0
482	   lstrec(i)=0
         OPEN(unit=12,file=qfile,status='UNKNOWN',access='TRANSPARENT',
     &   err=302)
c         OPEN(unit=12,file=qfile,status='UNKNOWN',
c     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
	   write(12,rec=1) nplot,jstrec,lstrec,iver
	   newfil=.false.		!file now exists
	else
c         OPEN(unit=12,file=qfile,status='UNKNOWN',
c     &    access='DIRECT',form='UNFORMATTED',recl=1024,err=302)
         OPEN(unit=12,file=qfile,status='UNKNOWN',access='TRANSPARENT',
     &   err=302)
	endif
c Now use first record for nplot etc
c The array jstrec(200) contains in jstrec(nplot) the start record
c for plot #nplot, and in lstrec(nplot) the last record number for it
c ###For floppy at least, add name,jfirst etc as for SCDIR so each person
c can have floppies with own posh plots
	read(12,rec=1) nplot,jstrec,lstrec,iver
	if(iver.ne.1100) then
	   call WDIALOG(1,
     &	'Plot queue has old format: try again',12)
	   close(unit=12)
         goto 25
	endif
	if(.not.newfil) then	!if(newfil) nplot already specified
	 if(nplot.eq.0) then	!initialise to 0 in PLTQINIT or above
	   iplot=1
	   jstrec(1)=2
c	   istrec=2
	   istrec=1025		!for transparent write
	 else
c	   istrec=int4(lstrec(nplot))+1	!start record for current plot
	   istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
	   iplot=nplot+1		!next number
	 endif
	else		!if newfil
c	   istrec=int4(lstrec(nplot)+1)	!start record for current plot
	   istrec=int4(lstrec(nplot))*1024+1  !start record for transparent write
	   iplot=nplot+1		!next number
	endif
4041	continue
	call INTCONV(iplot,cnum1)
	ans='Y'
	call DEFOLTa(ans,defolt)
	if(.not.newfil) then
	   call QDIALOG(1,
     & 'Next queue number = '//CHARNB(cnum1)//' in '//CHARNB(qfile)//
     &   ': O.K.',defolt,ict,cans)
	else if(newfil) then
	   call QDIALOG(1,
     & 'Queue number = '//CHARNB(cnum1)//' in '//CHARNB(qfile)//
     &   ': O.K.',defolt,ict,cans)
	endif
	call GETINPa(cans,ans)
	if(UC(ans).ne.'N') then		!O.K.
	   if(iplot.le.nplot) goto 405	!overwriting existing record
	   nplot1=iplot		!value to be written to 1st record
	   goto 406
	endif
c
c Bit done if want to change (1)plot number,(2)disc or (3)plot queue file name
c
	newfil=.false.
4081	continue
c=	iopt=0	!default
c===Modifs here
	iopt=1
408	call DEFOLTi(iopt,defolt)
	call WDIALOG(1,
     & 'Change (1) the queue number (2) name of queue file',ict)
	call QDIALOG(1,
     & '(3) abandon queueing; (4) O.K. queue now. Option #',
     &  ' ',ict,cans)
	call GETINPi(cans,iopt)
	if(iopt.eq.1) then
	   call INTCONV(iplot,cnum1)
	   call QDIALOG(1,
     &    'Queue number (up to '//CHARNB(cnum1)//')',' ',ict,cans)
	   call GETINPi(cans,i)
	   if(i.ge.1) iplot=i
	else if(iopt.eq.2) then
	   call TDIALOG(1,'Give path and name for queue file',
     &     qfile,40,.false.,ict)
	   newfil=.true.
	   CLOSE(unit=12)
	   goto 48		!Open new file and check if OK now?
	else if(iopt.eq.3) then
	   goto 45		!abandon
	else if(iopt.ne.4) then
	   iopt=4		!new defolt
	   goto 408		!ask again
	endif
c=============================================================
c
c Queue file etc now defined: do next bit if old file is being overwritten
405	continue
	if(iplot.eq.1) jstrec(1)=2	!new start record (rec 1=jstrec etc now)
	istrec=1 + (jstrec(iplot)-1)*1024	!start record transparent
	nplot1=iplot		!value to be written to 1st record
c Allow earlier plot to be overwritten in POSHPLOT.DAT only after query
	if(iplot.lt.nplot) then		!overwriting earlier plot
	   nr1=(lstrec(iplot)-jstrec(iplot))+1
c nr1=no of 1024 byte records for plot to be overwritten
	   read(12,rec=istrec) iptype,ndv12,ndimd2,ndc12,ndimc2,
     &	kwi2,kwj2,kmax2,itit2,title2
	   if(nrect.le.nr1) then
		ans='Y'
	      call DEFOLTa(ans,defolt)
	      call QDIALOG(1,'Will overwrite '//CHARNB(title2)//'  O.K.',
     &	   defolt,ict,cans)
	      call GETINPa(cans,ans)
		if(ans.eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   else
		call BELL(2)
	      call QDIALOG(1,'Will overwrite next plot too: O.K.',
     &	   ' ',ict,cans)
	      call GETINPa(cans,ans)
		if(UC(ans).eq.'N') then
		   idiskq=-1
		   goto 441
		endif
	   endif
	   call INTCONV(nplot,cnum1)
	   call INTCONV(iplot,cnum2)
	   ans='N'
	   call DEFOLTa(ans,defolt)
	   call QDIALOG(1,'Reduce last plot # from '//CHARNB(cnum1)//
     &	' to '//CHARNB(cnum2)//' ',defolt,ict,cans)
	   call GETINPa(cans,ans)
 	   if(ans.eq.'Y') then
		nplot1=iplot  !to write to 1st record
	   else
		nplot1=nplot
	   endif
	endif
c
406	continue
c Check that data will fit in: make max file size=1423 records which is
c most that will fit on 3.5 inch floppy (1457664 bytes usable, and
c 1457664/1024=1423.5   (1457664/2048=711.75)
c	lastr=istrec+nrect-1		!last record for current plot
c	if(lastr.gt.1423) then
	lastr=istrec+nbytet-1		!last record (byte #) for current plot
c To retain compatability with queues written in 1024 byte records, must
c round this up to the nearest integer multiple for 1024 (see nfill, below)
	lastr1=lastr		!rounded up
	n=mod(lastr,1024)
	if(n.ne.0) then
	   nfill=1024-n
	   lastr1=lastr+nfill
	endif
	if(lastr1.gt.1457664.and.ndev.eq.'A:') then
	   call BELL(2)
	   ans='Y'
	   call DEFOLTa(ans,defolt)
     	   call QDIALOG(1,
     & 	'Plot queue will be too big for floppy disc'//' O.K.',
     &	defolt,ict,cans)
	   call GETINPa(cans,ans)
	   if(ans.eq.'N') then
		idiskq=-1
		goto 441
	   endif
	endif
	jstrec(iplot)=1 + ((istrec-1)/1024)
	lstrec(iplot)=jstrec(iplot)+(nrect) - 1
	lstrec(iplot)=1 + ((lastr-1)/1024)	!should be same!
c Update rec #1 -now done AFTER data written so nplot does not get
c altered if there is some problem in writing the data (FIXPLQ1.FOR can
c alter nplot etc)
c	write(12,rec=1) nplot1,jstrec,lstrec
c
c Things to go in 1st record of each plot (Nplot copied into rec 1 also
c so it can be retrieved)
c IPTYPE=1 for VPLOT1
c IPTYPE=11 for VPLOTR
c IPTYPE=12 for VPLOT2
c IPTYPE=14 for VPLOT4
c IPTYPE=15 for VPLOT5
c	=2 for VHIST
c	=3 for PLOTSC
	iptype=15	!for VPLOT5
c
c=	krn=istrec
c
c VPLQ2 version
c	jcol=1002		!signal that colour,interp queued
c	write(12,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
c     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
c     & iscal,doframe,titlex,titley,ilabel,ndimd,ndimc,ncurvd,ndat1,
c     & isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
c     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
c     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
c     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
c     & jcol,mono,interp
c VPLQ4 version -order altered to put dimensions of allocated arrays first!
c	jcol=1002		!signal that colour,interp queued
c	write(12,rec=krn) iptype,ndv1,ndimd,ndc1,ndimc,itit,title1,
c     &xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
c     & csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
c     & ndat1,isym1,ncurvc,ncal1,ijoin1,iline1,syms1,inumx,inumy,sval,
c     & theta,ifitype,ncomp,numbx,numby,ifnt,ntext,
c     & narrow,nline,iltype,isdev,y0,yinf,ntx,nty,itx,ity,k1,k2,k3,k4,
c     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
c     & jcol,mono,interp,nhline,nvline,ntrace,ytsep
c VPLQ5 version: remove isdev,jcol; add kwi,kwj,kmax near start
c and remove ndat1,isym1,ncal1,ijoin1,syms1,/ncal,iline1, and also
c theta (now in int2cop and datcop, except for ndat, ncal which are kept as
c integer*4 and written after int2cop)
c Count the bytes
c                         4     4    4     4     4    4   4   4
c	write(12,rec=krn) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
c	    4    44
c     & itit,title1,
c        4    4   4    4      4     4     4     4    4    4   4    4
c     &xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
c         4     4     4    4      4      40     40      4     4
c     & csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
c          4     4     4    4
c     & ncurvc,inumx,inumy,sval,
c           4     4     4     4     4    4   4  4  4  4  4
c     & ifitype,ncomp,numbx,numby,ifnt,ntext,k1,k2,k3,k4,k5,
c          4     4      4   4   4    4   4   4   4
c     & narrow,nline,iltype,y0,yinf,ntx,nty,itx,ity,
c          4   40   40   4    40  40   4     4    4     4   4     4
c     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
c         4    4       4     4      4      4      4
c     & mono,interp,nhline,nvline,ntrace,ytsep,ioffset
c TOTAL OF ABOVE = 552 BYTES
c Above stuff occupies 552 bytes at presen, but leave gap after it and
c start next write at istrec+768 bytes say.
c Now write NEWTEXT,CHARCOP,INT2COP,INT4COP and DATCOP to disk
	write(12,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     & itit,title1,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,xlo,xhi,ylo,yhi,
     & ifont,ilog,iscal,doframe,titlex,titley,ilabel,ncurvd,
     & ncurvc,inumx,inumy,sval,
     & ifitype,ncomp,numbx,numby,ifnt,ntext,k1,k2,k3,k4,k5,
     & narrow,nline,iltype,y0,yinf,ntx,nty,itx,ity,
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ivplot,
     & mono,interp,nhline,nvline,ntrace,ytsep,ioffset
c
c Now write NEWTEXT,CHARCOP,INT2COP,INT4COP and DATCOP to disk
	istr1=istrec+ioffset
	if(ntext.eq.0) then
	   write(12,rec=istr1) (INT2COP(i),i=1,k4),(INT4COP(i),i=1,k5),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3)
	else
	   write(12,rec=istr1) (INT2COP(i),i=1,k4),(INT4COP(i),i=1,k5),
     &  (DATCOP(i),i=1,k1),(CHARCOP(i),i=1,k3),(NEWTEXT(i),i=1,ntext)
	endif
c NOTE -if this file gets read as fixed length, 1024 byte, records, its
c length must be an integer multiple of 1024 bytes, or attempt to read the
c last record will give an error (see TDAT.FOR) -so now write dummy data
c to make sure that this is so.
	INQUIRE(file=qFILe,flen=nlen)
	n=mod(nlen,1024)
	if(n.ne.0) then
	   nfill=1024-n
	   write(12,rec=nlen+1) (idum(1),i=1,nfill)
	endif
c

c Update rec #1 (after data written safely!)
	write(12,rec=1) nplot1,jstrec,lstrec,iver
c
	call INTCONV(iplot,cnum1)
	call INTCONV(istrec,cnum2)
	call WDIALOG(1,
     & 'Plot queue # '//CHARNB(cnum1)//': start byte # '//
     &	CHARNB(cnum2)//' on disc '//ndev,ict)
	call WDIALOG(1,CHARNB(title1),ict)
c
	if(slock()) write(7,46)iplot,istrec,lastr,qfile,
     & jstrec(iplot),lstrec(iplot),title1
      if(discprt) write(8,46)iplot,istrec,lastr,qfile,
     & jstrec(iplot),lstrec(iplot),title1
46	format(/,44x,'**************************',/,
     & ' Plot queue no ',i3,' in bytes ',i8,' to ',i8,' in ',a40,/,
     & ' (i.e. 1024 byte records numbers ',i5,' to ',i5,')',/,
     & /,1x,a64)
c	if(debug()) pause
	call flush(7)
45	CLOSE(UNIT=12)
99	continue
	DEALLOCATE(datcop,int2cop,int4cop,charcop)
	RETURN		!return to screen coordinates?
c
C ERROR SECTION
302	continue
	call BELL(1)
      call QDIALOG(1,'Error in opening PLOTQ file: try again',
     &   ' ',ict,cans)
      call GETINPa(cans,ans)
	newfil=.false.
	if(ans.eq.'Y') goto 4081
	idest=1551
	DEALLOCATE(datcop,int2cop,charcop)
	RETURN
c
	end

