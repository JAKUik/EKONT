para m.ink,m.bor,m.zn
private m.i
set talk off
if m.ink="i"
        m.ink="w+"
	endif
if m.bor="i"
	m.bor="b"
	endif
if m.zn="i"
	m.zn="°"
	endif
set color to &ink./&bor
ACTIV SCREE
for m.i=12 to 0 step -.3
	@ m.i,3*m.i,25-m.i,80-3*m.i box repl(m.zn,8) 
	endfor
