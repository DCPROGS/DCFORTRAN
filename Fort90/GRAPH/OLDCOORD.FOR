c=======================================================================
	subroutine wtopap(wx,wy,papx,papy)

	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)

	papx=(wx-wxmin)*(vpapxhi-vpapxlo)/(wxmax-wxmin)+vpapxlo
	papy=(wy-wymin)*(vpapyhi-vpapylo)/(wymax-wymin)+vpapylo

	end

c=======================================================================

	subroutine paptow(papx,papy,wx,wy)

	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)

	wx=(papx-vpapxlo)*(wxmax-wxmin)/(vpapxhi-vpapxlo)+wxmin
	wy=(papy-vpapylo)*(wymax-wymin)/(vpapyhi-vpapylo)+wymin

	end

c=======================================================================

	subroutine pixtopap(ipix,ipiy,papx,papy)

	call papenq(papxmax,papymax,it)
	papx=ipix*papxmax/640.
	papy=ipiy*papymax/480.

	end
c=======================================================================

	subroutine paptopix(papx,papy,ipix,ipiy)

	call papenq(papxmax,papymax,it)
	ipix=papx*640./papxmax
	ipiy=papy*480./papymax


	end
c=======================================================================


	subroutine pixtow(ipix,ipiy,wx,wy)

	call papenq(papxmax,papymax,it)
	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)

	wx=(ipix*papxmax/640.-vpapxlo)*(wxmax-wxmin)/(vpapxhi-vpapxlo)+
     &   wxmin
	wy=(ipiy*papymax/480.-vpapylo)*(wymax-wymin)/(vpapyhi-vpapylo)+
     &   wymin

	end
c=======================================================================

	subroutine wtopix(wx,wy,ipix,ipiy)

	call papenq(papxmax,papymax,it)
	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)

	ipix=(640./papxmax)*((wx-wxmin)*(vpapxhi-vpapxlo)/(wxmax-wxmin)+
     &     vpapxlo)
	ipiy=(480./papymax)*((wy-wymin)*(vpapyhi-vpapylo)/(wymax-wymin)+
     &     vpapylo)

	end
c=======================================================================

	subroutine pixtow1(ipix,ipiy,wx,wy)

	call pixtopap(ipix,ipiy,papx,papy)
	call paptow(papx,papy,wx,wy)

	end
c=======================================================================


	subroutine wtopix1(wx,wy,ipix,ipiy)

	call wtopap(wx,wy,papx,papy)
	call paptopix(papx,papy,ipix,ipiy)

	end
c=======================================================================

	integer function ipixx(wx)
	call papenq(papxmax,papymax,it)
	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)
	ipixx =(640./papxmax)*((wx-wxmin)*(vpapxhi-vpapxlo)/(wxmax-wxmin)+
     &	 vpapxlo)
	end

c=======================================================================
	integer function ipixy(wy)
	call papenq(papxmax,papymax,it)
	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)
	ipixy =(480./papymax)*((wy-wymin)*(vpapyhi-vpapylo)/(wymax-wymin)+
     &	 vpapylo)
	end


c=======================================================================

	real function worldx(ipix)
	call papenq(papxmax,papymax,it)
	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)

	worldx=(ipix*papxmax/640.-vpapxlo)*(wxmax-wxmin)/(vpapxhi-vpapxlo)
     &	 +wxmin
	end

c=======================================================================
	real function worldy(ipiy)
	call papenq(papxmax,papymax,it)
	call vp2enq(wxmin,wxmax,wymin,wymax,vpapxlo,vpapxhi,vpapylo,
     &            vpapyhi)
	worldy=(ipiy*papymax/480.-vpapylo)*(wymax-wymin)/(vpapyhi-vpapylo)
     &       +wymin
	end

c=======================================================================
