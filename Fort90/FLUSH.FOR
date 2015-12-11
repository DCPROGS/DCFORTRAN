	program FLUSH7
      OPEN(unit=7,file='LPT1')             !open printer
	call FLUSH(7)
	CLOSE(7)
	end

