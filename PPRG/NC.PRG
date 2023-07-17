* PRIVATE ALL

********************
**	Norton Commander
*PROCEDURE norton
PRIVATE oridisk,oridir,oripath,sw
	oridisk=sys(5)
	oridir=curdir()
	oripath=set('path')
	SET PATH TO (getenv('path'))
	sw=file('FOXSWAP.COM')
	SET PATH TO (m.oripath)
	ON ERROR WAIT WIND NOWA "Nedostatek pamÿti pro spuÁtÿn° "+ ;
		"Norton Commandera. ("+strn(memo())+")"
	IF m.sw
		RUN /0 NC
	  ELSE
		RUN NC
		ENDIF
	RUN &oridisk
	ON ERROR &_onerr
	IF !(m.oridisk==sys(5))
		SET DEFAULT TO (m.oridisk)
		ENDIF
	IF !(m.oridir==curdir())
		SET DEFAULT TO (m.oridir)
		ENDIF
RETURN
