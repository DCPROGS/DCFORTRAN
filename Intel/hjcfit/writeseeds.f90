subroutine writeseeds(simfile,issims,nset,nlig,k,npar,imod0,ix1sav,iy1sav,iz1sav,conc)
 
 real*4 conc(10,10)
 character*40 simfile
 common/rand/ix1,iy1,iz1
 common/ranlast/ixlast,iylast,izlast
 
 if(issims.eq.1) then
    OPEN(unit=9,file=SIMFILE,status='UNKNOWN',access='APPEND',form='FORMATTED',&
    carriagecontrol='FORTRAN')
	write(9,1) nset,nlig,imod0,k,npar
1   format(' number of sets=',i3,' ligands=',i3,' mech=',i3,' k=',i3,' rates=',i3)
	do i=1,nlig
		do j=1,nset
		   write(9,6) i,j,conc(i,j)*1e6
6		   format(' conc(',i2,',',i2,') = ',g13.6)
		enddo
	 enddo
	write(9,2)
2   format('  Start simulations')
	write(9,7) issims,ix1sav,iy1sav,iz1sav,ix1,iy1,iz1
7	format(i6,' seeds at start= ',3(1x,i8),': at end = ',3(1x,i8))

	CLOSE(unit=9)
else
    OPEN(unit=9,file=SIMFILE,status='UNKNOWN',access='APPEND',form='FORMATTED',&
    carriagecontrol='FORTRAN')

    write(9,7) issims,ix1sav,iy1sav,iz1sav,ix1,iy1,iz1
    CLOSE(unit=9)
endif

end