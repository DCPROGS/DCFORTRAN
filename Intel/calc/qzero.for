	subroutine QZERO(QT,k)
	real*8 QT(k,k)
	DO 54 I=1,K
	DO 54 J=1,K
54	QT(I,J)=0.0D0
	return
	end

