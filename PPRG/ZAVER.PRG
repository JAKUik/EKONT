PARA m.ink,m.bor
PRIVATE m.i,set_esc
set_esc=set('ESCAPE')
SET ESCAPE OFF
SET TALK OFF
IF m.ink="i"
	m.ink="w+"
	ENDIF
IF m.bor="i"
	m.bor="b"
	ENDIF
SET COLOR TO &ink./&bor
ACTIVATE SCREEN
FOR m.i=0 TO 12 STEP +.3
	@ m.i,3*m.i,25-m.i,80-3*m.i BOX repl(' ',8)+'°'
	ENDFOR
SET ESCA &set_esc