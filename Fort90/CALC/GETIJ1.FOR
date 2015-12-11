	subroutine GETIJ1(IQ,k,i,j,m,kdim)
	dimension IQ(kdim,kdim)
c Finds i,j for given m
c GETIJ1 is version of GETIJ for any size array -has extra argument=kdim
c
c Array IQ defines correspondence between elements of THETA and elements of Q:
c IQ(i,j)=m where theta(m) is the parameter that goes in QT(i,j)
c (though may be multiplied by a statistical factor in QT)- this will
c facilitate definition of V-dependence etc in QDEF1)
	do i=1,k
	   do j=1,k
		if(IQ(i,j).eq.m) RETURN
	   enddo
	enddo

	print 2,m
2	format(
     & ' ERROR (in GETIJ): parameter # ',i3,' not found in Q matrix')
	RETURN

	end







