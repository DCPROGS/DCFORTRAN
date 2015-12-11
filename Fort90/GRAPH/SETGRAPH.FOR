	subroutine setgraph(wxlo,wxhi,wylo,wyhi,papxlo,papxhi,
     &      papylo,papyhi,vpapxlo,vpapxhi,vpapylo,vpapyhi,
     &      wxmin,wxmax,wymin,wymax,sqr,ind)

c==========================================================================
c	wxlo,wxhi,wylo,wyhi             : graphboundary in world units
c	papxlo,papxhi,papylo,papyhi     : graphboundary in paper units
c	vpapxlo,vpapxhi,vpapylo,vpapyhi : viewport coordinates in paper units
c     wxmin,wxmax,wymin,wymax         : maximum drawing area in world units
c
c==========================================================================
	logical sqr
	wxmin=((wxhi-wxlo)/(papxhi-papxlo))*(vpapxlo-papxlo)+wxlo
	wxmax=((wxhi-wxlo)/(papxhi-papxlo))*(vpapxhi-papxlo)+wxlo
	wymin=((wyhi-wylo)/(papyhi-papylo))*(vpapylo-papylo)+wylo
	wymax=((wyhi-wylo)/(papyhi-papylo))*(vpapyhi-papylo)+wylo
	if(sqr) then
	   wymin=((sqrt(wyhi)-sqrt(wylo))/(papyhi-papylo))*
     &         (vpapylo-papylo)+sqrt(wylo)
	   wymax=((sqrt(wyhi)-sqrt(wylo))/(papyhi-papylo))*
     &         (vpapyhi-papylo)+sqrt(wylo)
	endif
	call VPTSWI(ind)
	call SETVP2(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,
     &           vpapylo,vpapyhi)

	end

c======================================================================
