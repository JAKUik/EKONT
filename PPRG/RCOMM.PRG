SET TALK WIND
SET TALK OFF
SET ESCAPE OFF
WAIT WIND "Welcome to R-Commander" NOWAIT
SET SYSMENU AUTO
SET EXCLUSIVE OFF
SET REFRESH TO 1,1
SET REPROCES TO 10
ON ERROR DO rcomm_err WITH program(), sys(16), lineno()

DO rc_path

DO CASE
CASE ' 2.6'$version() AND file('FOXLIB26.PLB')
	SET LIBRARY TO foxlib26.plb
CASE ' 2.6'$version() AND file('FOXLIB25.PLB')
	SET LIBRARY TO foxlib25.plb
CASE ' 2.5'$version() AND file('FOXLIB25.PLB')
	SET LIBRARY TO foxlib25.plb
CASE ' 2.0'$version() AND file('FOXLIB20.PLB')
	SET LIBRARY TO foxlib20.plb
CASE ' 2.0'$version() AND file('FOXLIB.PLB')
	SET LIBRARY TO foxlib.plb
ENDCASE

IF empty(set('help',1)) AND file('foxhelp.dbf')
	SET HELP TO foxhelp
	SET HELP ON
ENDIF

PRIVATE rc_comm, rc_impl, rc_cr, rc_skip, rc_end, rc_val, rc_retval
PRIVATE rc_log, rc_log1, rc_logn

DIME rc_log[50]
rc_log=''
rc_log1=1
rc_logn=0

PUSH KEY CLEAR
SET TALK ON

rc_impl=''
rc_cr=.F.
DO WHILE .T.
	rc_end=.F.
	rc_val=''
	rc_retval=.F.
	rc_skip=.T.
	rc_comm=padr(m.rc_impl,250)
	rc_impl=''

	DEFINE WINDOW rcomm FROM 1,0 TO 24,79 ;
		TITLE "R - Commander   V1.3" ;
		FOOTER "CTRL+F9 - Show screen     ALT+L - Select     ALT+O - Order     F10 - QUIT" ;
		FLOAT CLOSE SHADOW MINIMIZE SYSTEM COLOR SCHEME 8
	ACTIVATE WINDOW rcomm NOSHOW

	@ 7,1 GET rc_log1 PICTURE "@&N" FROM rc_log RANGE 1, m.rc_logn;
		SIZE 10,76 WHEN !m.rc_skip AND m.rc_logn>0 ;
		VALID rcl1_valid() COLOR SCHEME 9

	@ 17,1 SAY "Command:"
	@ 18,1 GET m.rc_comm SIZE 3,76 WHEN rc_when() VALID rc_valid()

	PUSH KEY CLEAR
	DO rc_keys
	ACTIVATE WINDOW rcomm
	IF !m.rc_cr
		WAIT CLEAR
	ENDIF
	rc_cr=.F.
	READ CYCLE SHOW rc_show()
	POP KEY
	RELEASE WINDOW rcomm

	IF m.rc_end
		EXIT
	ENDIF
	IF m.rc_cr
		DO rc_run
		IF type([rc_log(1)])!='C' OR type([m.rc_logn])!='N' OR !betw(m.rc_logn,0,alen(rc_log))
			DIME rc_log[50]
			rc_log=''
			rc_log1=1
			rc_logn=0
		ENDIF
		IF type([m.rc_impl])!='C'
			rc_impl=''
		ENDIF
		IF type([m.rc_cr])!='L'
			rc_cr=.T.
		ENDIF
	ELSE
		WAIT WIND "Please wait ..." NOWAIT
	ENDIF
ENDDO
WAIT WIND "Good Bye." NOWAIT
POP KEY
ON ERROR
RETURN


*****************
PROCEDURE rc_when
rc_skip=.F.
RETURN .T.


******************
PROCEDURE rc_valid
IF lastkey()!=13 OR empty(m.rc_comm)
	IF lastkey()=5
		RETURN .T.
	ENDIF
	IF inlist(lastkey(), 1,19,26,127)
		KEYBOARD '{HOME}'
	ENDIF
	RETURN 0
ENDIF

rc_comm=alltrim(m.rc_comm)
DO CASE
CASE upper(m.rc_comm)=='Q'
	rc_comm='QUIT'
CASE upper(m.rc_comm)=='CLS'
	rc_comm='CLEAR'
CASE upper(m.rc_comm)=='U'
	rc_comm='USE ?'
CASE upper(left(m.rc_comm,2))=='U ' AND !(left(alltrim(subst(m.rc_comm,2)),1)=='=')
	rc_comm='USE '+alltrim(subst(m.rc_comm,2))
CASE upper(m.rc_comm)=='S'
	rc_comm='SELECT'
CASE 'SELECT'=upper(m.rc_comm)
	rc_comm='SELECT'
CASE upper(m.rc_comm)=='S0'
	rc_comm='SELECT 0'
CASE upper(left(m.rc_comm,2))=='S ' AND !(left(alltrim(subst(m.rc_comm,2)),1)=='=')
	rc_comm='SELECT '+alltrim(subst(m.rc_comm,2))
CASE upper(m.rc_comm)=='B'
	rc_comm='BROWSE'
CASE upper(left(m.rc_comm,2))=='B ' AND !(left(alltrim(subst(m.rc_comm,2)),1)=='=')
	rc_comm='BROWSE '+alltrim(subst(m.rc_comm,2))
CASE upper(m.rc_comm)=='F'
	rc_comm='SET FILTER TO'
CASE upper(left(m.rc_comm,2))=='F ' AND !(left(alltrim(subst(m.rc_comm,2)),1)=='=')
	rc_comm='SET FILTER TO '+alltrim(subst(m.rc_comm,2))
CASE upper(m.rc_comm)=='O'
	rc_comm='SET ORDER TO'
CASE upper(left(m.rc_comm,2))=='O ' AND !(left(alltrim(subst(m.rc_comm,2)),1)=='=')
	rc_comm='SET ORDER TO TAG '+alltrim(subst(m.rc_comm,2))
CASE upper(left(m.rc_comm,2))=='L ' AND !(left(alltrim(subst(m.rc_comm,2)),1)=='=')
	rc_comm='LOCATE ALL FOR '+alltrim(subst(m.rc_comm,2))
CASE upper(m.rc_comm)=='C'
	rc_comm='CONTINUE'
ENDCASE

IF m.rc_logn<1 OR !(rc_log[m.rc_logn]==m.rc_comm)
	rc_logn=m.rc_logn+1
	IF m.rc_logn>alen(rc_log)
		=adel(rc_log,1)
		rc_logn=alen(rc_log)
	ENDIF
	rc_log[m.rc_logn]=m.rc_comm
	rc_log1=m.rc_logn
ENDIF
SHOW GETS

rc_cr=.T.
rc_end=inlist(upper(rc_comm), 'QUIT', 'RETU', 'CANC', 'EXIT')

DO CASE
CASE m.rc_comm=='SELECT'
	DO rc_sele
OTHERWISE
	CLEAR READ
	RETURN
ENDCASE
rc_skip=.T.
rc_cr=.F.
rc_comm=padr('',250)
SHOW GETS
RETURN


****************
PROCEDURE rc_run
rc_val=''
rc_retval=(left(m.rc_comm,1)=='=')
ACTIV SCREEN
POP KEY
IF m.rc_retval
	ON ERROR DO rcomm_err WITH program(), sys(16), lineno(), .T.
	rc_val &rc_comm
	ON ERROR DO rcomm_err WITH program(), sys(16), lineno()
ELSE
	IF left(m.rc_comm,1)=='?'
		ON ERROR DO rcomm_err WITH program(), sys(16), lineno(), .T.
		&rc_comm
		ON ERROR DO rcomm_err WITH program(), sys(16), lineno()
		=inkey(0)
	ELSE
		ON ERROR DO rcomm_err WITH program(), sys(16), lineno(), .T.
		&rc_comm
		ON ERROR DO rcomm_err WITH program(), sys(16), lineno()
	ENDIF
ENDIF
PUSH KEY CLEAR
DO rc_keys

IF type([m.rc_retval])='L' AND m.rc_retval
	DO sh_retv
ENDIF
RETURN



*****************
PROCEDURE sh_retv
PRIVATE rv, t, v, s, l, y, x, x1
rv=m.rc_val
x1=1
v=4
s=74
t=type('m.rv')
DO CASE
	CASE m.t=='C'
		l=int(250/m.s)*m.s
		rv=left('"'+m.rv+'"',m.l)
		v=m.v+int((len(m.rv)-1)/m.s)
		x1=iif(len(m.rv)>=m.s, 0, 1)
	CASE m.t=='L'
		rv=iif(m.rv, '.T.', '.F.')
ENDCASE
s=m.s+2
y=int((srows()-m.v)/2)
x=int((scols()-m.s)/2)
DEFINE WIND sh_retv FROM m.y,m.x TO m.y+m.v-1,m.x+m.s-1 SHADOW COLOR SCHEME 5
ACTIV WIND sh_retv NOSHOW
@ 0,1 SAY "Return value:"
@ 1,m.x1 SAY m.rv COLOR gr+/n
SHOW WIND sh_retv
=inkey(0,'H')
RELEASE WIND sh_retv
RETURN


*****************
PROCEDURE rc_keys
ON KEY
ON KEY LABEL CTRL+F9 DO sh_scrn
ON KEY LABEL F1 DO rc_help
ON KEY LABEL F10 DO rc_stop
ON KEY LABEL CTRL+S DO rc_sele
ON KEY LABEL ALT+L DO rc_sele
ON KEY LABEL ALT+O DO rc_orde
RETURN


*****************
PROCEDURE sh_scrn
PUSH KEY CLEAR
HIDE WIND rcomm
ACTIV SCREEN
READ
ACTIV WIND rcomm
POP KEY
RETURN


*****************
PROCEDURE rc_stop
PUSH KEY CLEAR
rc_end=.T.
CLEAR READ
POP KEY
RETURN


*****************
PROCEDURE rc_help
PUSH KEY CLEAR
PRIVATE ccc
IF varread()=='RC_COMM'
	ccc=alltrim(m.rc_comm)
	HELP &ccc
ELSE
	HELP
ENDIF
POP KEY
RETURN


********************
PROCEDURE rcl1_valid
rc_comm=padr(rc_log[m.rc_log1], len(m.rc_comm))
IF !empty(m.rc_comm)
	KEYBOARD '{END} '
ENDIF
SHOW GETS
RETURN .T.



*****************
PROCEDURE rc_show
IF sys(2016)=='RCOMM' OR sys(2016)='*'
	PRIVATE currwind
	currwind=woutput()
	ACTIVATE WINDOW rcomm SAME

	@ 1,1  SAY "Database:"
	@ 1,12 SAY dbf() SIZE 1,65 COLOR SCHEME 1

	@ 2,1  SAY "Alias:"
	@ 2,12 SAY iif(used(),ltrim(str(select()))+': ','')+alias() SIZE 1,20 COLOR SCHEME 1

	@ 3,1  SAY "Records:"
	@ 3,12 SAY iif(used(),recc(),'') SIZE 1,8 COLOR SCHEME 1

	@ 4,1  SAY "Rec. NO:"
	@ 4,12 SAY iif(used(),recno(),'') SIZE 1,8 COLOR SCHEME 1
	@ 4,22 SAY iif(deleted(), "Deleted", '') SIZE 1,10

	@ 5,1  SAY "Index:"
	@ 5,12 SAY cdx(1) SIZE 1,65 COLOR SCHEME 1

	@ 6,1  SAY "Order:"
	@ 6,12 SAY order() SIZE 1,65 COLOR SCHEME 1

	IF !empty(m.currwind)
		ACTIVATE WINDOW (m.currwind) SAME
	ENDIF
ENDIF
RETURN



*****************
PROCEDURE rc_path
PRIVATE w, p
w=upper(sys(2004))
p=upper(alltrim(set('path')))
IF right(m.w,1)=='\' AND !(right(m.w,2)==':\')
	w=left(m.w, len(m.w)-1)
ENDIF
IF empty(m.p) OR !((m.w+';')$(m.p+';'))
	IF !empty(m.p) AND right(m.p,1)!=';'
		p=m.p+';'
	ENDIF
	SET PATH TO (m.p+m.w)
ENDIF
RETURN




*********************************
****  RC_ASK(y,x,impl,text)  ****
*********************************
PROCEDURE rc_ask
PARAM y, x, imp, txt
PRIVATE len, ws, t, q, k, ask_y, ask_n
PRIVATE tx, bell

IF left(m.txt,1)=='\'
	bell=.T.
	tx=subs(m.txt, 2)
ELSE
	bell=.F.
	tx=m.txt
ENDIF

len=len(m.tx)
ws=m.len+18
x=iif (m.x<0, int((scol()-m.ws)/2), min(m.x, scol()-m.ws))
y=iif (m.y<0, int((srow()-3)/2),    min(m.y, srow()-3))

DEFINE WINDOW ask_wind FROM m.y,m.x TO m.y+2,m.x+m.ws-1 ;
		FLOAT NOCLOSE SHADOW COLOR SCHEME 5
ACTIVATE WIND ask_wind NOSHOW
ask_y=0
ask_n=0
CLEAR PROMPT
@ 0,2 SAY m.tx
IF m.imp
	@ 0,m.len+4  GET m.ask_y PICTURE "@*T \<Ano"
	@ 0,m.len+10 GET m.ask_n PICTURE "@*T \<Ne"
ELSE
	@ 0,m.len+10 GET m.ask_n PICTURE "@*T \<Ne"
	@ 0,m.len+4  GET m.ask_y PICTURE "@*T \<Ano"
ENDIF

SHOW WIND ask_wind
IF m.bell
	?? chr(7)
ENDIF
READ CYCLE MODAL VALID (readkey()%256)!=5
k=readkey()%256
Esc=(m.k=12)
RELEASE WIND ask_wind
RETURN (m.ask_y=1)



*************************
****  Osetreni chyb  ****
*************************
PROCEDURE rcomm_err
PARAM prg, file, line, rc_run
PRIVATE i, k, y, x, s, v, msg, comm, prtt
msg=message()
comm=alltrim(strtran(message(1), chr(9), ' '))
Esc=.F.

IF error()=125	&& Printer not ready
	prtt="\Tisk rna nen¡ pýipravena, opakovat?"
	WAIT CLEAR
	CLEAR TYPEAHEAD
	IF !rc_ask (-1,-1,.T., m.prtt)
		SET PRINTER OFF
		RETURN
	ENDIF
	DO WHILE !printstat()
		IF !rc_ask (-1,-1,.T., m.prtt)
			SET PRINTER OFF
			RETURN
		ENDIF
	ENDDO
	RETRY
ENDIF

IF inlist(subst(m.file, rat('\',m.file)+1), 'RCOMM.FXP', 'RCOMM.APP', 'RCOMM.EXE');
		AND m.prg=='RC_RUN' AND type([m.rc_run])='L' AND m.rc_run
	rc_retval=.F.
	rc_impl=m.rc_comm
	WAIT WIND m.msg NOWAIT
	?? chr(7)+chr(7)
	RETURN
ENDIF

v=13
s=70
y=int((srows()-m.v)/2)
x=int((scols()-m.s)/2)
DEFINE WINDOW rcomm_err FROM m.y,m.x TO m.y+m.v-1,m.x+m.s-1 ;
	CLOSE FLOAT MINI ZOOM SHAD COLOR SCHEME 7
ACTIV WIND rcomm_err NOSHOW
@  0,2 SAY "     "+padc("Kritick  chyba:", m.s-16)
@  1,2 SAY " ÛÛ° "+padc("ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ", m.s-16)
@  2,2 SAY "ÛÛÛÛ°   "+ltrim(str(error()))+': '+m.msg SIZE 1,m.s-4
@  3,2 SAY "ÛÛÛÛ°"
@  4,2 SAY "ÛÛÛÛ°   PROC: "+m.prg SIZE 1,m.s-4
@  5,2 SAY " ÛÛ°    FILE: "+subst(m.file, rat('\',m.file)+1) SIZE 1,m.s-4
@  6,2 SAY " ÛÛ°    LINE: "+ltrim(str(m.line)) SIZE 1,m.s-4
@  7,2 SAY "        COMM: "+m.comm SIZE 1,m.s-4
@  8,2 SAY " ÛÛ°"
@  9,2 SAY padc("Kontaktujte pros¡m dodavatele tohoto programu", m.s-6)
@ 10,2 SAY padc("a sdØlte mu vìçe uveden‚ informace.", m.s-6)
ACTIV WIND rcomm_err
WAIT CLEAR
DO WHILE .T.
	CLEAR TYPEAHEAD
	IF rc_ask(m.y+m.v, -1, .T., "\PokraŸovat?")
		RELEASE WIND rcomm_err
		IF inlist(error(), 1707, 109)
			RETRY
		ENDIF
		RETURN
	ELSE
		IF !m.Esc
			QUIT
		ENDIF
	ENDIF
ENDDO
RETURN


*****************
PROCEDURE rc_sele
PUSH KEY CLEAR
PRIVATE i, n, f, s, d, sch, oe
ACTIVATE SCREEN
n=iif(type([alias(255)])='C', 255, 25)
s=select()
IF used()
	oe=on('error')
	ON ERROR *
	SELECT 0
	ON ERROR &oe
ENDIF
f=select()
SELECT (m.s)
sch=scheme(4)
sch=scheme(4,9)+subst(m.sch, at(',',m.sch))
DEFINE POPUP rc_sele FROM 3,80 MARGIN RELATIVE SCROLL SHADOW COLOR (m.sch)
DEFINE BAR 999 OF rc_sele PROMPT "W.A. Alias         Records  Order      File" SKIP
*---: ---------- ----------  ---------- -------------------------
FOR i=1 TO m.n
	IF used(m.i) OR m.i=m.f
		d=sys(2014, dbf(m.i))
		IF len(m.d)>25
			d='...'+right(m.d,22)
		ENDIF
		DEFINE BAR m.i OF rc_sele PROMPT str(m.i,3)+': '+iif(used(m.i), padr(alias(m.i),11)+str(recc(m.i),10)+'  '+padr(order(m.i),11)+m.d, '')
	ENDIF
ENDFOR
SET MARK OF BAR m.s	OF rc_sele TO .T.
ON SELECT POPUP rc_sele DEACTIVATE POPUP rc_sele
WAIT CLEAR
ACTIVATE POPUP rc_sele BAR m.s
RELEASE POPUP rc_sele
IF bar()>0
	IF lastkey()=10
		rc_comm=padr('SELECT '+lower(alias(bar())), len(m.rc_comm))
	ELSE
		SELECT bar()
	ENDIF
ENDIF
POP KEY
RETURN


*****************
PROCEDURE rc_orde
PUSH KEY CLEAR
PRIVATE n, i, c, cc, xi, x, b, t, ord, ab, atag, acdx, takyof
ACTIVATE SCREEN

xi=type([descending()])='L' AND type([unique()])='L'
x=space(2)

atag=set('order')
acdx=''
ab=0
IF left(m.atag,4)=='TAG '
	atag=alltrim(subst(m.atag,5))
	acdx=alltrim(subst(m.atag,at(' ',m.atag,2)))
	atag=left(m.atag, at(' ',m.atag)-1)
	acdx=left(m.acdx, at(' ',m.acdx+' ')-1)
ENDIF

n=0
DO WHILE !empty(tag(m.n+1))
	n=m.n+1
ENDDO
DIME ord[max(m.n,1)]

takyof=.F.

DEFINE WINDOW rc_ordmsg FROM 4,1 TO 12,51 SHADOW COLOR SCHEME 5
ACTIVATE WINDOW rc_ordmsg
@ 0,0 SAY trim(padc("Index info", wcols()))
@ 1,0 SAY trim(padc("Reading...", wcols()))
ACTIVATE SCREEN
SET MESSAGE WINDOW rc_ordmsg
SET MESSAGE TO 1 LEFT

sch=scheme(4)
sch=scheme(4,9)+subst(m.sch, at(',',m.sch))
DEFINE POPUP rc_orde FROM 3,80 MARGIN RELATIVE SCROLL SHADOW COLOR (m.sch)
DEFINE BAR 990 OF rc_orde PROMPT " <No Order>" MESSAGE padc("No Order", wcols('rc_ordmsg'))
b=0
i=1
DO WHILE !empty(ndx(m.i))
	b=m.b+1
	takyof=.T.
	IF m.xi
		x=chr(iif(descending(ndx(m.i),1),25,24))+iif(unique(ndx(m.i),1),'!',' ')
	ENDIF
	DEFINE BAR m.b OF rc_orde PROMPT m.x+padr(tag(ndx(m.i),1),20) MESSAGE rc_ordmsg(tag(ndx(m.i),1), ndx(m.i), .F.)
	ord[m.b]=lower(tag(ndx(m.i),1))
	IF m.ab=0 AND empty(m.acdx) AND tag(ndx(m.i),1)==m.atag
		ab=m.b
	ENDIF
	i=m.i+1
ENDDO
c=1
DO WHILE !empty(cdx(m.c))
	IF m.c>1
		takyof=.T.
	ENDIF
	cc=fullpath(cdx(m.c))
	cc=subst(m.cc, rat('\',m.cc)+1)
	cc=left(m.cc, at('.',m.cc+'.')-1)
	i=1
	DO WHILE !empty(tag(cdx(m.c),m.i))
		b=m.b+1
		IF m.xi
			x=chr(iif(descending(cdx(m.c),m.i),25,24))+iif(unique(cdx(m.c),m.i),'!',' ')
		ENDIF
		DEFINE BAR m.b OF rc_orde PROMPT m.x+padr(m.cc+':'+tag(cdx(m.c),m.i),20) MESSAGE rc_ordmsg(tag(cdx(m.c),m.i), cdx(m.c), .T.)
		ord[m.b]='TAG '+lower(tag(cdx(m.c),m.i))+' OF '+lower(cdx(m.c))
		IF m.ab=0 AND fullpath(cdx(m.c))==m.acdx AND tag(cdx(m.c),m.i)==m.atag
			ab=m.b
		ENDIF
		i=m.i+1
	ENDDO
	c=m.c+1
ENDDO

IF m.ab>0
	SET MARK OF BAR m.ab OF rc_orde TO .T.
ELSE
	ab=990
ENDIF
ON SELECT POPUP rc_orde DEACTIVATE POPUP rc_orde
WAIT CLEAR
ACTIVATE POPUP rc_orde BAR m.ab
RELEASE POPUP rc_orde
IF bar()>0
	IF lastkey()=10
		IF bar()>=990
			rc_comm=padr('SET ORDER TO', len(m.rc_comm))
		ELSE
			t=ord[bar()]
			IF !m.takyof AND left(m.t,4)=='TAG '
				t=left(m.t, at(' ',m.t+'  ',2)-1)
			ENDIF
			rc_comm=padr('SET ORDER TO '+m.t, len(m.rc_comm))
		ENDIF
	ELSE
		IF bar()>=990
			SET ORDER TO
		ELSE
			SET ORDER TO &ord[bar()]
		ENDIF
	ENDIF
	SHOW GETS OFF
ENDIF
SET MESSAGE WINDOW
SET MESSAGE TO
RELEASE WINDOW rc_ordmsg
POP KEY
RETURN


*******************
PROCEDURE rc_ordmsg
PARAM tag, fname, jecdx
PRIVATE w, r, f, o, tlk

tlk=set('talk')
SET TALK OFF
w=wcols('rc_ordmsg')

o=set('order')
IF m.jecdx
	SET ORDER TO TAG (m.tag) OF (m.fname)
ELSE
	SET ORDER TO (m.tag)
ENDIF
IF type([tagno()])='N'
	PRIVATE b
	b=tagno()
ENDIF

f=sys(2014, m.fname)
IF len(m.f)>(m.w-5)
	f='...'+right(m.f,m.w-8)
ENDIF
r=	 padr("Tag:   "+m.tag, m.w);
	+padr("Expr.: "+sys(14,m.b), m.w);
	+padr("Filt.: "+sys(2021,m.b), m.w);
	+repli('Ä',m.w);
	+padr(iif(m.jecdx,"CDX: ","IDX: ")+m.f,m.w)

SET ORDER TO &o

IF m.tlk='ON'
	SET TALK ON
ENDIF
RETURN m.r
