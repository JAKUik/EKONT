*   _onerr=[ DO err WITH program(), sys(16), lineno() ]
*	ON ERROR &_onerr



*************************
****  Osetreni chyb  ****
*************************
*PROCEDURE err
PARAM prg, file, line
PRIVATE i, k, y, x, s, v, msg, comm, kdo, dbname, oldsel, prtt, sq
msg=message()
comm=alltrim(strtran(message(1), chr(9), ' '))
kdo=getenv('USERNAME')
*	kdo=iif(type('m.kod')=='C', ' ('+alltrim(m.kod)+')', '')

=numlock(.T.)
=capslock(.F.)
=insmode(.T.)

IF error()=125	&& Printer not ready
	prtt="\Tisk†rna nen° p˝ipravena, opakovat?"
	WAIT CLEAR
	CLEAR TYPEAHEAD
	IF !ask (-1,-1,.T., m.prtt)
		SET PRINTER OFF
		RETURN
	ENDIF
	DO WHILE !printstat()
		IF !ask (-1,-1,.T., m.prtt)
			SET PRINTER OFF
			RETURN
		ENDIF
	ENDDO
	RETRY
ENDIF

sq=alltrim(program(1))
i=2
DO WHILE !empty(program(m.i))
	sq=m.sq+','+alltrim(program(m.i))
	i=m.i+1
ENDDO
i=',ON...,'+upper(alltrim(program()))
IF upper(right(m.sq,len(m.i)))==m.i
	sq=left(m.sq, len(m.sq)-len(m.i))
ENDIF
IF len(m.sq)>250
	sq='...'+right(m.sg,246)
ENDIF

dbname=sys(16,1)
dbname=left(m.dbname, rat('\',m.dbname))+'ERRLOG.DBF'
IF !file(m.dbname)
	oldsel=select()
	SELECT 0
	CREATE DBF (m.dbname) (err N(4), msg C(80), proc C(20), file C(80), ;
			line N(8), comm C(80), date D, time C(5), user C(7), seq C(250))
	USE
	SELECT (m.oldsel)
ENDIF
USE (m.dbname) IN (select(1)) ALIAS errl

* vol†n° mekont p˝i err (vloßit za 'INSERT INTO errl ...') 
IF file("e_err.prg")
	DO e_err WITH error(),m.msg,m.prg,m.file,m.line,m.comm,m.kdo,m.sq
	ENDIF
*

INSERT INTO errl (err, msg, proc, file, line, comm, date, time, user, seq) ;
	VALUES (error(), m.msg, m.prg, m.file, m.line, m.comm, ;
	date(), time(), alltrim(m.kdo), m.sq)
USE IN errl

v=13
s=70
y=int((srows()-m.v)/2)
x=int((scols()-m.s)/2)
DEFINE WINDOW prg_err FROM m.y,m.x TO m.y+m.v-1,m.x+m.s-1 ;
	CLOSE FLOAT MINI ZOOM SHAD COLOR SCHEME 7
ACTIV WIND prg_err NOSHOW
@  0,2 SAY "     "+padc("Kritick† chyba:", m.s-16)
@  1,2 SAY " €€∞ "+padc("ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ", m.s-16)
@  2,2 SAY "€€€€∞   "+ltrim(str(error()))+': '+m.msg SIZE 1,m.s-4
@  3,2 SAY "€€€€∞"
@  4,2 SAY "€€€€∞   PROC: "+m.prg SIZE 1,m.s-4
@  5,2 SAY " €€∞    FILE: "+subst(m.file, rat('\',m.file)+1) SIZE 1,m.s-4
@  6,2 SAY " €€∞    LINE: "+ltrim(str(m.line)) SIZE 1,m.s-4
@  7,2 SAY "        COMM: "+m.comm SIZE 1,m.s-4
@  8,2 SAY " €€∞"
@  9,2 SAY padc("Kontaktujte pros°m dodavatele tohoto programu", m.s-6)
@ 10,2 SAY padc("a sdÿlte mu vÏÁe uvedenÇ informace.", m.s-6)
ACTIV WIND prg_err
WAIT CLEAR
DO WHILE .T.
	CLEAR TYPEAHEAD
	IF ask(m.y+m.v, -1, .T., "\Pokraüovat?")
		RELEASE WIND prg_err
		IF inlist(error(), 1707, 109)
			RETRY
		ENDIF
		RETURN
	ELSE
		IF type('m.LADENI')=='L' AND m.LADENI
			susp
		ENDIF
		IF !m.Esc
			QUIT
		ENDIF
	ENDIF
ENDDO
RETURN







