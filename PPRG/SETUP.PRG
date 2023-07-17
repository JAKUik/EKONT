SET ESCA OFF
SET COLOR TO w+/b

SET CENT OFF
SET BELL OFF
SET COLOR ON
SET DATE GERMAN
SET MENU OFF
SET ECHO OFF
SET SAFETY OFF
SET SCORE OFF
SET TALK WIND
SET TALK OFF
SET DELE ON
SET CONS ON
SET ODOM TO 50
SET HEAD OFF
SET STEP OFF
SET EXCL OFF

DEAC WIND TRACE
RELE WIND TRACE

* SET SYSMENU OFF
CLEAR MACROS

SET LIBRARY TO foxlib25

*SET COLOR OF SCHEME 15 TO W+/GR,N/BG,N/GR,N/GR,N+/GR,N/G,N/GR,N+/N,G+/GR,N+/GR,+
*SET COLOR OF SCHEME 18 TO ,,G+/W

DO set_col

SET HOURS TO 24
SET CLOCK TO 0,72
SET CLOCK ON

PUBLIC m._onerr,m.ladeni,m.nlq,m.draft,m.dblon,m.dbloff,m.cnon,m.cnoff;
	m.pica,m.elit,m.colorfile,resetprn,prekodtisk
m.resetprn=chr(29)
m.cnon=chr(15)
m.cnoff=chr(18)
m.ecnon=chr(15)+chr(27)+'M'
m.ecnoff=chr(18)+chr(27)+'P'
m.pica=chr(27)+'P'
m.elit=chr(27)+'M'
m.nlq=chr(27)+"x1"
m.draft=chr(27)+"x0"
m.dblon=chr(14)
m.dbloff=chr(20)
prekodtisk="ASCII"

_onerr=[ DO err WITH program(), sys(16), lineno() ]
ON ERROR &_onerr

m.ladeni=.F.
*ON KEY LABEL Ctrl+Tab DO kalkul
ON KEY LABEL Ctrl+F11 susp
ON KEY LABEL Ctrl+F12 DO setlad

RETURN

*******************************
**	INICIALIZA¬NÖ PROCEDURY  **
*******************************
PROCEDURE set_col
PRIVATE i,f,colors

DIME colors(1)
 
f=iif(ISCOLOR() .AND. !file('mono.flg'), 'COLOR.CLR', 'MONO.CLR')
IF file(f)
	RESTORE FROM &f ADDITIVE
	FOR i=1 to 24
		SET COLOR OF SCHEME i TO (colors(i))
	ENDFOR
ENDIF

RELEASE COLORS
RETURN
