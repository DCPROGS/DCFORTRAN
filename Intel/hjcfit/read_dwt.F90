
subroutine READ_DWT(n,kfile,pfiles,nset,nfile,nval,irecs,srate,calfacs2,ffilt,nintt,ntot,nmax)
USE DFLIB

	use menu_f90

	TYPE (FILE$INFO) info

	character*33 pfile(20,10)	!path names for .EDE files
	character*60 pfiles(20,10)
	integer kfile(20,10),nfile(10),nintt(10)
	integer nval(20,10),irecs(20,10)
	real*4 calfacs2(20,10),ffilt(10)
    character*1 string1(256)
    character*256 string
	integer*1 ip0,ip1,ip2
	logical discprt,btest,present,clampex
	logical debug1
	common/dp/discprt
	character*8 segment
	character*8 dwells
	character*13 slength
	character*11 start
	character*12 class
	character*11 chart
	integer iseg,idwell(100),istart,in1,in2,in3
	real rn1,rn2
	character datfil*33
	character defolt*30,cans*30		!to hold text & result of QDIALOG
	character*18 ascnum
	character*20 colhead(3)
	character*70 text
	allocatable ix,x
	integer ix(:)
	real x(:)
	logical digchar1
	logical graph,dcfile
	
	allocate(x(200),ix(200))
	
	do j=1,nset
	 nintt(j)=0		!# of obs for set j
	   do i=1,nfile(j)
		CLOSE(unit=18)
		INQUIRE(file=pfiles(i,j),exist=present)
	    ihandle=FILE$FIRST
	    length = GETFILEINFOQQ(pfiles(i,j), info, ihandle)
	    nLEN=info%length 

        ijk=1
		OPEN(unit=18,file=pfiles(i,j),status='UNKNOWN',&
        access='SEQUENTIAL',form='FORMATTED')
8001    format(a256)       
100     continue
        read(18,8001) string
        if(string(1:7).ne.'Segment')goto 99
        len=len_trim(string)
        do iml=1,len
            string1(iml)=string(iml:iml)
        enddo
        i1=1
        
        do m=1,5
        call FINDSEP1(i1,string1,len,isep,iend,inext)
        i1=inext
        jk=inext
        call FINDSEP1(i1,string1,len,isep,iend,inext)
        klm=1
        do kl=i1,iend
        chart(klm:klm)=string1(kl)
        klm=klm+1
        enddo
        call CHTOREAL(chart,rea)
        if(m.eq.1) in1=int(rea)
        if(m.eq.2) in2=int(rea)
        if(m.eq.3) rn1=rea
        if(m.eq.4) in3=int(rea)
        if(m.eq.5) in4=int(rea)
        i1=inext
        jk=inext
      
        enddo
        
       
        idwell(ijk)=in2
        do jm=jkm+1,jkm+in2
            read(18,8001) string
            len=len_trim(string)
            chart=string(1:1)
            
            call CHTOREAL(chart,rea)
            ii=int(rea)
            ix(jm)=ii
            chart=string(3:13)
            
            call CHTOREAL(chart,rea)
            x(jm)=rea
        enddo
        jkm=jm
        ijk=ijk+1
	    goto 100
	    enddo
	 enddo
99	 close(unit=18)
	
	        
end