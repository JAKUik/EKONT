*
*	Vrac¡ vybran‚ znaky velk‚ anglick‚ abecedy
*
*func onlyasc
para m.tx			&&	vstupn¡ ıetØzec
priv i,m.txv,a
m.tx=upper(m.tx)
m.txv=""
for i=1 to len(m.tx)
	a=substr(m.tx,i,1)
	if a>="A".and.a<="Z"
		m.txv=m.txv+a
		endif
	endfor
return m.txv
