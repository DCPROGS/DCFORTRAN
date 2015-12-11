	character function UC(ch)
c Converts argument to upper case
	character*1 ch
c
      if((ch.ge.'a').and.(ch.le.'z')) ch = char(ichar(ch)-32)
	UC=ch
	return
	end

