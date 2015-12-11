	subroutine FDIST(VR,n1,n2,P)
c To give P for variance ratio VR with n1,n2 df
c Uses Incomplete Beta function routine as in Numerical Recipes
c For Students t, call with n1=1 and t**2 in place of VR.
c
	en1=float(n1)
	en2=float(n2)
	x=en2/(en2+en1*VR)
	P=BETAI(0.5*en2,0.5*en1,x)
c
	RETURN
	end

