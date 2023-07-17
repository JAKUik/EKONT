**	Nastaven¡ barev sch‚mat
*PROCEDURE set_col
PARA _col_type
PRIVATE i,f,colors,_path
*	_path=left(fullpath('set_col.prg'),rat('\',fullpath('set_col.prg')))
	DIME colors(1)
	IF upper(m._col_type)<>'C' AND upper(m._col_type)<>'M'
		f=iif(ISCOLOR(), 'COLOR.CLR', 'MONO.CLR')
	  ELSE
		f=iif(upper(m._col_type)='M', 'MONO.CLR', 'COLOR.CLR')
		ENDIF
	IF file(f)
		RESTORE FROM &f ADDITIVE
		FOR i=1 to 24
			SET COLOR OF SCHEME i TO (colors(i))
			ENDFOR
		ENDIF
	RELEASE COLORS
RETURN
