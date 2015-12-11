	subroutine GETCONC(conc,nset,nlig,ligname,readini,imode)
c To get conc for each set in hjcfit
c Output=conc, rest are input
c imode controls title that is printed
c imode=1 for simulations
c imode=2 for nodata
	real*4 conc(10,10)
	character*20 ligname(10)
	character*1 ans
	logical readini
c
	do j=1,nset
	   if(nlig.gt.0.and.readini) then
		if(imode.eq.1) then
		   print 163,j
163		   format(' SIMULATED DATA FOR SET ',i2,
     &	' Last run used concentration(s) (micromolar):',/)
		else if(imode.eq.2) then
		   print 1631,j
1631		   format(' CONCENTRATION FOR CALCULATE PDFs: SET ',i2,
     &	' Last run used concentration(s) (micromolar):',/)
		endif
c
		do i=1,nlig
		   xs=1.e6*conc(i,j)
		   print 164,i,ligname(i),xs
164		   FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		enddo
		call DCASK('Change the concentration(s)','n',ans)
	   else
		ans='Y'
	   endif
	   if(nlig.gt.0) then
		if(ans.eq.'Y') then
		   do i=1,nlig
			xs=1.e6*conc(i,j)
			print 159,i,ligname(i),xs
159			FORMAT(/,
     &     ' Ligand #',i2,1x,a20,': concentration (muM) [',g13.6,'] = ')
			call INPUTr(xs)
			conc(i,j)=xs*1.0e-6
		   enddo
c		   if(discprt) write(8,108)
108		format(/,'------------------------------------------------')
		   do i=1,nlig
			xs=1.e6*conc(i,j)
c			if(discprt) write(8,164) i,ligname(i),xs
c164		   FORMAT(/,
c     &      ' Ligand #',i2,1x,a20,': concentration (muM) = ',g13.6)
		   enddo
c		   if(discprt) write(8,108)
		endif
	   endif
	enddo
c
	RETURN
	end

