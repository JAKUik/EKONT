*****************************************
**	NaŸten¡ nastaven¡ z Setupov‚ datab ze
*****************************************
*PROCEDURE setekont
PRIVATE i,f,colors,_path
*	_path=left(fullpath('setekont.prg'),rat('\',fullpath('setekont.prg')))
	_sel=sele()
	SELE 0
	IF type('m.setekopath')='C'
		USE &setekopath.setekont
	  ELSE
		USE setekont
		ENDIF
	_pp=afields(p_setdbf)
	nstation=getenv('NSTATION')
	IF !empty(m.nstation)
		LOCA ALL FOR m.nstation==rtrim(setekont.nstation);
			AND !empty(setekont.nstation)
		IF found()
			FOR i=1 TO _pp
				a=p_setdbf(i,1)
				IF !empty(setekont.&a)
					&a=setekont.&a
					ENDIF
				ENDFOR
			ENDIF
		ENDIF
	DO set_col WITH m.color_type
	USE
	RELE p_setdbf
	SELE (_sel)
RETURN

