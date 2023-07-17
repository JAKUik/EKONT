function kcslov
para m._kc,m._len,m._end
	m._kc=abs(m._kc)
	dime jed(19),des(9),sta(9),tis(9),mil(9)
	jed(1)="jedna"
	jed(2)="dvØ"
	jed(3)="tıi"
	jed(4)="Ÿtyıi"
	jed(5)="pØt"
	jed(6)="çest"
	jed(7)="sedm"
	jed(8)="osm"
	jed(9)="devØt"
	jed(10)="deset"
	jed(11)="jeden ct"
	jed(12)="dvan ct"
	jed(13)="tıin ct"
	jed(14)="Ÿtrn ct"
	jed(15)="patn ct"
	jed(16)="çestn ct"
	jed(17)="sedmn ct"
	jed(18)="osmn ct"
	jed(19)="devaten ct"
	des(2)="dvacet"
	des(3)="tıicet"
	des(4)="Ÿtyıicet"
	des(5)="pades t"
	des(6)="çedes t"
	des(7)="sedmdes t"
	des(8)="osmdes t"
	des(9)="devades t"
	sta(1)="sto"
	sta(2)="dvØstØ"
	sta(3)="tıista"
	sta(4)="Ÿtyıista"
	sta(5)="pØtset"
	sta(6)="çestset"
	sta(7)="sedmset"
	sta(8)="osmset"
	sta(9)="devØtset"
	tis(1)="tis¡c"
	tis(2)="dvatis¡ce"
	tis(3)="tıitis¡ce"
	tis(4)="Ÿtyıitis¡ce"
	tis(5)="pØttis¡c"
	tis(6)="çesttis¡c"
	tis(7)="sedmtis¡c"
	tis(8)="osmtis¡c"
	tis(9)="devØttis¡c"
	mil(1)="mili¢n"
	mil(2)="dvamili¢ny"
	mil(3)="tıimili¢ny"
	mil(4)="Ÿtyıimili¢ny"
	mil(5)="pØtmili¢n…"
	mil(6)="çestmili¢n…"
	mil(7)="sedmmili¢n…"
	mil(8)="osmmili¢n…"
	mil(9)="devØtmili¢n…"

	private m._st,m._skc,m._aa,m._bb,m._st1
	m._st=""
	m._skc=ltrim(str(int(m._kc)))

*** 1-999 ***
	m._aa=val(right(m._skc,2))
	if m._aa<20.and.m._aa>0
		m._st=jed(m._aa)+m._st
	  else
	  	m._bb=10*(m._aa/10-int(m._aa/10))
		if m._bb>0
	  		m._st=jed(m._bb)+m._st
	  		endif
	  	m._bb=int(m._aa/10)
	  	if m._bb>0
			m._st=des(m._bb)+m._st
			endif
		endif
	m._aa=int(val(right(m._skc,3))/100)
	if m._aa>0
		m._st=sta(m._aa)+m._st
		endif

*** 1000-999999 ***
	m._aa=int(val(right(m._skc,5))/1000)
	m._st1=""
	if m._aa>0
		if m._aa<10
			m._st1=tis(m._aa)
		  else
		  	if m._aa<20
				m._st1=jed(m._aa)+"tis¡c"
			  else
			  	m._bb=10*(m._aa/10-int(m._aa/10))
			  	if m._bb>0
				  	m._st1=jed(m._bb)
				  	endif
				m._st1=des(int(m._aa/10))+m._st1+"tis¡c"
				endif
			endif
		endif
	m._aa=int(val(right(m._skc,6))/100000)
	if m._aa>0
		if len(m._st1)=0
			m._st1="tis¡c"
			endif
		m._st1=sta(m._aa)+m._st1
		endif
	m._st=m._st1+m._st

*set step on
*set debug on
*** 1 000 000 - 999 999 999 ***
	m._skc=ltrim(str(int(m._kc/1000)))
	m._aa=int(val(right(m._skc,5))/1000)
	m._st1=""
	if m._aa>0
		if m._aa<10
			m._st1=mil(m._aa)
		  else
		  	if m._aa<20
				m._st1=jed(m._aa)+"mili¢n…"
			  else
			  	m._bb=10*(m._aa/10-int(m._aa/10))
			  	if m._bb>0
				  	m._st1=jed(m._bb)
				  	endif
				m._st1=des(int(m._aa/10))+m._st1+"mili¢n…"
				endif
			endif
		endif
	m._aa=int(val(right(m._skc,6))/100000)
	if m._aa>0
		if len(m._st1)=0
			m._st1="mili¢n…"
			endif
		m._st1=sta(m._aa)+m._st1
		endif
	m._st=m._st1+m._st

	m._st=upper(left(m._st,1))+substr(m._st,2)+m._end+" ---"
return(left(m._st+space(m._len),m._len))
