
*                       Hlavn° program £üetn°ho
* systÇmu EKONTCLOSE ALL
CLEAR ALL
RELEASE ALL

SET ESCA OFF

SET PATH TO
SET PROC TO

**	Vol†n° INI souboru EKONT.ENI
PUBLIC gl_userrem, DSazbaZD, DSazbaZB, DSazbaVPU, DSazbaP, DSazbaPP, MenuCM
PUBLIC DefLadeni
DefLadeni=.F.
DSazbaZD=3400
DSazbaZB=2500
DSazbaVPU=2000
DSazbaP= 400
DSazbaPP= 25
MenuCM="EUR,USD"
=ini("EKONT.ENI")

**	Definice pro p˝eklady
PUBLIC fullver
FULLVER=.T.
MOD_EVIM=.T.
MOD_CEST=.T.
MOD_FAKT=.T.

DO setup
SET LIBR TO ontimer ADDI

ladeni=file("Ekont.Prg") AND m.DefLadeni 

PUSH MENU _msysmenu
sysm=set('sysmenu')
SET SYSMENU TO

SET ODOM TO 100
SET AUTOSAVE ON
SET CLOCK TO 0,0

_onerr=iif(m.ladeni,"",m._onerr)
ON ERROR &_onerr

DEAC WIND trace

=open_dbf(m.gl_data, "SETEKONT", "op_setek", "", .F., .F.)
_pp=afields(p_setdbf)
FOR i=1 TO _pp
	a=p_setdbf(i,1)
	&a=setekont.&a
	IF type(a)='C'
		&a=rtrim(&a)
		ENDIF
	ENDFOR
USE
RELE p_setdbf

expo="EXPO"

DO setekont

**	Screen SAVER
=on_night(200,' ˘*',.F.,.T.,1,0)

SET PROC TO ekont

**********************************************************
**	PromÿnnÇ pro celÏ soubor programÖ - nebudou RELEASNuty
PUBLIC gl_prior, gl_pri_PU, gl_pri_IM, gl_pri_SC, gl_pri_FS, ;
	gl_usernam, gl_verze

shd="Nespr†vnÏ autorizaün° k¢d. Program bude ukonüen."

**	VERZE PROGRAMU	**
gl_verze=4.52
copyrtxt="(C) Jaroslav Kuüera - JAKU  "		&& len=28
copyright=date()


PUBLIC ggf, ggjme
* ggf='ZKOUSKA'
* ggjme="ZkuÁebn° data"
ggf=' '
ggjme=''

**********************************
****	DÖleßitÇ glob†ln° promÿnnÇ - budou ukl†d†ny na disk
* PUBLIC ggdph,ggdtlock,ggjme,gguli,ggmes,ggpsc,ggico,ggdic,ggtel
* PUBLIC ggfax,ggban,gguct,ggdph,ggdenrc,ggdenor,ggdtlock,ggpldph
* PUBLIC ggordciszb, ggkli1, ggkli2, ggkli3

gguli=space(20)
ggmes=space(15)
ggpsc=space(5)
ggico=space(8)
ggdic=space(14)
ggtel=space(30)
ggfax=space(15)
ggban=space(20)
gguct=space(20)
ggdph=343
ggdenrc=1
ggdenor=''
ggdtlock={}		&&	Datum uzamüen° dat
ggpldph='Ano'
gglm=10
ggusr=space(30)
ggrok=year(date())
*	DalÁ° promÿnnÇ pro speci†ln° operace
ggimport=space(40)
ggextprg=space(40)
ggordciszb=""
ggkli1=space(25)
ggkli2=space(25)
ggkli3=space(25)
*	VÏkazy Z=zjednoduÁenÏ, P=plnÏ
ggtp_vyk="Z"


******************
**	PromÿnnÇ
PUBLIC m.esc, kalkfile, kalk_vysl, kalk_kurz

PRIV st_on_dat, st_on_time
STORE "" TO st_on_dat, st_on_time

ok=1
esc=.F.
app=.F.
STORE .F. TO pu, im, sc, fs
kalkfile=""
kalk_vysl='0'

PUBLIC on_list_c, on_list_next
on_list_next=11
DIME on_list_c(m.on_list_next)

************************
****	Zaü†tek programu
DO login

SELE 0
USE users
IF m.gl_usernam='SUPERVISOR' AND recc()=0
	gl_prior=99
	ENDIF
USE

IF !empty(m.gl_usernam)
	kalkfile=m.gl_data+left(m.gl_usernam,8)+'.cal'
  ELSE
	kalkfile="kalk.cal"
	ENDIF

***************************
**	P˝°prava horkÏch kl†ves
IF !file(m.gl_data+'poznamky.dbf')
	CREAT DBF &gl_data.poznamky (datum D, alarm C(1), nazev C(20), text M)
	INDEX ON datum TAG datum
	INDEX ON cs(nazev) TAG nazev
	ENDIF

IF type("m.gl_userrem")='C' AND ;
		!file(m.gl_userrem+m.gl_usernam+"\poznamky.dbf") AND ;
		_isdir(m.gl_userrem+m.gl_usernam)
	CREAT DBF &gl_userrem.&gl_usernam.\poznamky (datum D, alarm C(1), nazev C(20), text M)
	INDEX ON datum TAG datum
	INDEX ON cs(nazev) TAG nazev
	ENDIF

**	ALT+F9 - Nastaven° tiskovÇho vÏstupu
DO tisk_out WITH .F.

****************************************************************
**	Definice HorkÏch kl†ves a jejich definice pro vÁechny LIST_C
**	Definice pole pro LC je p˝ed LOGINem

on_list_c( 1)='#SET=K-W-'
on_list_c( 2)='#P=preftisk.lcp'

*	ALT+K		- Zobrazen° kurzovn°ho l°stku'
ON KEY LABEL Alt+K DO callkurz
on_list_c( 3)='ALT+K:		DO callkurz'

*	CTRL+Tab	- Kalkulaüka'
ON KEY LABEL Ctrl+Tab DO kalkul WITH m.kalkfile, .F.
on_list_c( 4)='Ctrl+Tab:	DO kalkul WITH m.kalkfile, .F.'

*	ALT+F9		- Nastaven° tiskovÇho vÏstupu'
ON KEY LABEL ALT+F9 DO tisk_out WITH .T.
on_list_c( 6)='ALT+F9:		DO tisk_out WITH .T.'

* Definice ALT+U v LISTU_C ü.8 je definov†na v loginu

*	ALT+Z		- Z†pis pozn†mek k programu'
ON KEY LABEL Alt+Z DO poznamky WITH "Poznamky", "Pozn†mky k programu"
on_list_c( 9)='ALT+Z:		DO poznamky WITH "Poznamky","Pozn†mky k programu"'

*	ALT+P		- Z†pis pozn†mek k firmÿ'
ON KEY LABEL Alt+P DO poznamky WITH m.ggf+'Poznamky',"Pozn†mky k firmÿ"
on_list_c(10)='ALT+P:		DO poznamky WITH ggf+"Poznamky","Pozn†mky k firmÿ"'

*	ALT+F1		- Glob†ln° n†povÿda'
ON KEY LABEL ALT+F1 DO poznamky WITH "Helpik", "Manu†l k programu"
on_list_c( 7)='ALT+F1:		DO poznamky WITH "Helpik", "Manu†l k programu"'

IF m.gl_prior>=90
	ON KEY LABEL Alt+N DO call_nc
	ENDIF


*******************
****	Hlavn° MENU
DEFINE POPUP hl_menu FROM 6,45 RELATIVE SHADOW TITLE " Hlavn° menu " ;
	COLOR SCHEME 4
DEFINE BAR  1 OF hl_menu PROMPT " \<Firma k £ütov†n° " ;
	MESS "VÏbÿr firmy k £ütov†n°" ;
	SKIP FOR m.gl_prior=0
DEFINE BAR  9 OF hl_menu PROMPT "\-"
DEFINE BAR 11 OF hl_menu PROMPT " \<PodvojnÇ £üetnictv° " ;
	MESS "Modul podvojnÇho £üetnictv°" ;
	SKIP FOR !(file('UCTO\EKONT-PU.fxp') OR file('UCTO\ekont-pu.prg')) ;
		OR m.ggf=' ' OR !m.pu OR (m.gl_pri_pu<30 AND m.gl_pri_pu!=11)
DEFINE BAR 12 OF hl_menu PROMPT " Evidence \<IM " ;
	MESS "Modul evidence investiün°ho majetku" ;
	SKIP FOR !(file('EVIM\evim.fxp') OR file('EVIM\evim.prg')) ;
		OR m.ggf=' ' OR !m.im OR m.gl_pri_im<30
DEFINE BAR 13 OF hl_menu PROMPT " \<Slußebn° cesty " ;
	MESS "Modul vy£ütov†n° slußebn°ch cest" ;
	SKIP FOR !(file('cesty\cesty.fxp') OR file('CESTY\cesty.prg')) ;
		OR m.ggf=' ' OR !m.sc OR m.gl_pri_sc<30
DEFINE BAR 15 OF hl_menu PROMPT " Fak\<turace a sklad " ;
	MESS "Modul fakturace a veden° skladu" ;
	SKIP FOR !(file('FAKTURY\faktury.fxp') OR file('FAKTURY\faktury.prg')) ;
		OR m.ggf=' ' OR !m.fs OR m.gl_pri_fs<30
DEFINE BAR 19 OF hl_menu PROMPT "\-"	SKIP FOR m.ggf=' '
DEFINE BAR 80 OF hl_menu PROMPT " \<Archivace dat" ;
	MESS "Archivace vÁech dat vybranÇ firmy za z†loßn° mÇdium" ;
	SKIP FOR m.ggf=' ' OR m.gl_pri_pu<50
DEFINE BAR 81 OF hl_menu PROMPT " \<Obnova dat"	;
	MESS "Obnova dat vybranÇ firmy ze z†loßn°ho mÇdia" ;
	SKIP FOR m.ggf=' ' OR m.gl_pri_pu<50
DEFINE BAR 82 OF hl_menu PROMPT " \<RÖznÇ" ;
	MESS "Indexov†n° dat, instalaün° parametry programu, ... ." ;
	SKIP FOR m.gl_prior=0
DEFINE BAR 89 OF hl_menu PROMPT "\-"
DEFINE BAR 98 OF hl_menu PROMPT " \<Znovup˝ihl†Áen°" ;
	MESS "Znovup˝ihl†Áen° do systÇmu."
DEFINE BAR 99 OF hl_menu PROMPT " \<Konec" ;
	MESS "Ukonüen° pr†ce programu."
ON SELE POPU hl_menu DO hl_menu

* DEFI WIND w_lline FROM 24,0 TO 24,80 NONE COLOR SCHEME 14

DO refscr

WAIT WIND NOWA "Manu†l se vyvol† stiskem kl†ves  ALT+F1"

_bar=1
DO WHILE .T.
	ACTIVATE POPUP hl_menu BAR _bar
	IF bar()=0
		_bar=99
		IF m.ladeni
			DO endlad
			EXIT
			ENDIF
		ENDIF
	ENDDO
RETURN
****  OBSLUHA HLAVNIHO MENU
PROCEDURE hl_menu
	DO CASE
		CASE bar()=1
			DO firma
		CASE bar()=11
			HIDE POPUP hl_menu
			SET DEFAULT TO 'UCTO'
			DO ekont-pu
			SET DEFAULT TO '..'
			CLOSE DATA
			SHOW POPUP hl_menu
		CASE bar()=12
			HIDE POPUP hl_menu
			SET DEFAULT TO 'EVIM'
			DO evim
			SET DEFAULT TO '..'
			CLOSE DATA
			SHOW POPUP hl_menu
		CASE bar()=13
			HIDE POPUP hl_menu
			SET DEFAULT TO 'CESTY'
			DO cesty
			SET DEFAULT TO '..'
			CLOSE DATA
			SHOW POPUP hl_menu
		CASE bar()=15
			HIDE POPUP hl_menu
			SET DEFAULT TO 'FAKTURY'
			DO faktury
			SET DEFAULT TO '..'
			CLOSE DATA
			SHOW POPUP hl_menu
		CASE bar()=80 OR bar()=81
			DO zaloha
		CASE bar()=82
			DO ruzne
		CASE bar()=98
			DO login
		CASE bar()=99
*			DO cldata
			HIDE POPUP hl_menu
			QUIT
		ENDCASE
	DO refscr
RETURN
**	P˝ihl†Áen° pod ußivatelskÏm jmÇnem a heslem
PROCEDURE login
	DO refscr

	SET LIBRARY TO crypt ADDI

**	Ovÿ˝en° autorizace programu
	SET TALK OFF
	h=chr(100)
	a=right(sys(2020),8)
	h=h+chr(101)
	f=fopen("ekont.crp")
	h=h+chr(108)

	IF f=-1
		s="xxx"
	  ELSE
		s=left(fgets(f),8)
		=fclose(f)
		ENDIF

	IF empty(s)
		s="xxx"
		ENDIF

	h=h+chr(102)
	r=""
	c=encrypt(a,a)
	h=h+chr(105)
	FOR i=1 TO 8
		r=r+chr((asc(substr(c,i,1))%(64+32-1)) +32)
		ENDFOR
	h=h+chr(110)

*	IF r!=s
*		heslo="        "
*		PRIV _l
*		_l=26
*		h=h+chr(101)
*		DEFINE WINDOW w_ask ;
*			FROM INT((SROW()-5)/2),INT((SCOL()-(m._l-1))/2) ;
*			TO INT((SROW()-5)/2)+4,INT((SCOL()-(m._l-1))/2)+m._l ;
*			NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
*		ACTIVATE WINDOW w_ask SAME
*		@ 0,1 SAY "ID ü°slo: "
*		@ 2,1 SAY "Heslo   : "
*		@ 0,12 SAY str(round(val(a)/1.12,2)*100,10)
*		@ 2,12 GET m.heslo SIZE 1, 8 COLOR ,b/b ;
*			MESS "Zadejte autorizaün° k¢d. Nezobrazuje se."
*		
*		READ MODAL
*		RELEASE WINDOW w_ask
*		h=h+chr(107)
*		SET CONS OFF
*		IF m.heslo=h
*			SET ALTE TO ekont.crp
*			SET ALTE ON
*			??r
*			SET ALTE TO
*		  ELSE
*			WAIT WIND "ZadanÏ autorizaün° k¢d: "+m.heslo
*			IF m.heslo=r
*				SET ALTE TO ekont.crp
*				SET ALTE ON
*				??m.heslo
*				SET ALTE TO
*			  ELSE
*				WAIT WIND m.shd
*				QUIT
*				ENDIF
*			ENDIF
*		ENDIF
	SET CONS ON

	gl_usernam=upper(getenv('USERNAME'))
	=open_dbf(m.gl_data, "USERS", "op_user", "user", .F., .F.)

	heslo="        "
	PRIV _l
	_l=26
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-5)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-5)/2)+4,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Ußivatel: "
	@ 2,1 SAY "Heslo   : "
	@ 0,12 GET m.gl_usernam SIZE 1,12 COLOR SCHEME 4 ;
		MESS "Zadejte vaÁe ußivatelskÇ jmÇno pro tento systÇm."
	@ 2,12 GET m.heslo		SIZE 1, 8 COLOR ,b/b ;
		MESS "Zadejte vaÁe heslo."
	
	READ MODAL
	RELEASE WINDOW w_ask

	gl_usernam=alltrim(upper(m.gl_usernam))
	SEEK m.gl_usernam
	PRIV noname
	noname=!found() OR empty(m.gl_usernam)
	IF m.noname
		WAIT WIND "ZadanÇ jmÇno ußivatele nebylo nalezeno !!!"
		ENDIF

	* heslo=alltrim(m.heslo)
	* heslo=left(repl(5,m.heslo),5)
	IF (empty(users.heslo) OR users.heslo=encrypt(m.heslo,m.heslo)) ;
			AND !m.noname
		IF empty(users.heslo)
			WAIT WIND "Nem†te zad†no heslo. Zadejte si jej v menu RÖznÇ/Zmÿna hesla."
			ENDIF
		gl_prior=users.prior
		gl_pri_pu=users.pr_pu
		gl_pri_im=users.pr_im
		gl_pri_sc=users.pr_sc
		gl_pri_fs=users.pr_fs
	  ELSE
		IF !m.noname
			WAIT WIND NOWA "C H Y B N ê   H E S L O !!!   VaÁe priorita je 0.   P˝ihlaÁte se znovu."
			ENDIF
		STORE 0 TO gl_prior, gl_pri_pu, gl_pri_im, gl_pri_sc, gl_pri_fs
		ENDIF
	RELEASE LIBRARY crypt
	ggf=' '
	ggjme=''
	DO refscr

*	ALT+U		- Definice pozn†mek ußivatele
	ON KEY LABEL Alt+U
	on_list_c( 8)=""
	IF file(m.gl_userrem+m.gl_usernam+"\Poznamky.DBF")
		ON KEY LABEL Alt+U DO poznamky WITH m.gl_userrem+m.gl_usernam+"\Poznamky","Z†pisn°k ußivatele: "+m.gl_usernam
		on_list_c( 8)='ALT+U:	DO poznamky WITH m.gl_userrem+m.gl_usernam+"\Poznamky","Z†pisn°k ußivatele: "+m.gl_usernam'
		ENDIF

	SELE users
	USE
RETURN

**	Obnova obrazovky
PROCEDURE refscr
	=uvod('i','i','i')
	@1,0 SAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
	@2,0 SAY "∫   E K O N T  -  EkonomickÏ systÇm"+iif(m.fullver,space(10)," >>DEMO<< ")+right("V "+ltrim(str(m.gl_verze,5,2)),8)+"  ≥  (C) Kuüera - JAKU 1995 ∫"
	@3,0 SAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"

	ON ERROR err=1
	@ 5,2 FILL TO 11,42 COLOR B/B
	@ 6,3 SAY "N†zev firmy: "
	@ 7,3 SAY "Adres†˝    : "
	@ 9,3 SAY "Ußivatel   : "+left(alltrim(m.gl_usernam)+space(20),19)+"  HP="+str(m.gl_prior,2)
	@10,3 SAY "Priority   : "+ ;
		"PU="+str(m.gl_pri_pu,2)+"  IM="+str(m.gl_pri_im,2)+ ;
		"  SC="+str(m.gl_pri_sc,2)+"  FS="+str(m.gl_pri_fs,2)
	ON ERROR &_onerr

	IF !empty(m.ggf)
		@ 6,16 SAY alltrim(m.ggjme)
		a=alltrim(m.ggf)
		a=left(a,len(a)-1)
		@ 7,16 SAY iif(len(a)<26, a, "... "+right(a,21))
		ENDIF

*	@ 2,scols()-13 SAY "Celkem :"+str(val(sys(1001))/1024,4)
*	@ 3,scols()-13 SAY "Pro RUN:"+str(memory(),4)
*	@ 4,scols()-13 SAY "Alloc. :"+str(val(sys(1016))/1024,4)
RETURN

***************************
**	Z†lohov†n° a obnova dat
PROCEDURE zaloha
	PRIV mech,fi,fie,sw,naz
	naz=substr(m.ggf,rat('\',m.ggf,2)+1)
	naz=left(m.naz,len(m.naz)-1)
	sw=file('FOXSWAP.COM')
	a=at('.',m.naz)
	IF a=0
		a=9
		ENDIF
	fi=left(m.naz,a-1)
	fie=substr(m.naz,a)
	mech=m.floppy
	DO mech.spr
	IF m.esc
		RETURN
		ENDIF
	mech=m.mech+':'
	SET LIBR TO
	SET LIBR TO foxlib25
	SET LIBR TO ontimer ADDI
	IF bar()=80
		a=ask(-1,-1,'N','Opravdu chceÁ zaz†lohovat data firmy: '+m.naz)
		IF a
			ON ERROR WAIT WIND "Nelze provÇst z†lohu dat, "+ ;
				"p˝ihlaÁte se pros°m znovu do programu. ("+strn(memo())+")"
			IF m.sw
				RUN /0 zaloha.bat &mech &fi &fie
			  ELSE
				RUN zaloha.bat &mech &fi &fie
				ENDIF
			ON ERROR &_onerr
			ENDIF
	  ELSE
		IF ask(-1,-1,'N','Opravdu chceÁ obnovit data firmy: '+m.naz)
			ON ERROR WAIT WIND "Nelze provÇst obnovu dat, "+ ;
				"p˝ihlaÁte se pros°m znovu do programu. ("+strn(memo())+")"
			IF m.sw
				RUN /0 obnova.bat &mech &fi &fie
			  ELSE
				RUN obnova.bat &mech &fi &fie
				ENDIF
			ENDIF
			ON ERROR &_onerr
		ENDIF
RETURN

**	Ukonüen° pr†ce programu a n†vrat do FOXPRO
PROCEDURE endlad
	IF !empty(m.ggf)
		SAVE TO (m.ggf+"firma.mem") ALL LIKE gg*
		ENDIF
	RELEASE LIBRARY ontimer
	SET PRINT TO
	SET CLOCK TO 0,scols()-8
	SET PROCEDURE TO
	REST MACROS
	SET FUNC 2 TO 'DO EPS WITH "ucto\doklad";'
	SET FUNC 3 TO 'DO EPS WITH "ucto\TISKY";'
	SET FUNC 4 TO 'DO EPS WITH "ucto\EKONT-PU";'
	SET FUNC 5 TO 'DO EPS WITH "evim\EVIM";'
	SET FUNC 6 TO 'DO EPS WITH "EKONT";'
	SET FUNC 7 TO 'DO EPS WITH "cesty\CESTY";'
	SET FUNC 8 TO 'DO EPS WITH "faktury\FAKTURY";'
	SET FUNC 9 TO 'DO EPS WITH "ucto\tis_vka";'
	SET FUNC 10 TO "quit;"
	ON ERROR
	ON KEY LABEL F1
	ON KEY LABEL ALT+P
	ON KEY LABEL Alt+N
	ON KEY LABEL ALT+U
	ON KEY LABEL ALT+Z
	POP MENU _msysmenu
	SET SYSMENU &sysm
	SET SYSMENU ON
	SET ESCA ON
	CLEAR ALL
	SET TALK ON
	?
	?
	?" 2 ucto\doklad"
	?" 3 ucto\TISKY"
	?" 4 ucto\EKONT-PU"
	?" 5 evim\EVIM"
	?" 6 EKONT"
	?" 7 cesty\CESTY"
	?" 8 faktury\FAKTURY"
	?" 9 ucto\TIS_VKA"
	?"10 QUIT"
RETURN

**	Zad†n° firmy k £ütov†n°
PROCEDURE firma
	PRIV pf,i,tt

* Otev˝en° datab†ze FIRMA
	DO open_dbf WITH m.gl_data, "FIRMA", "op_firma", "NAZ", .F., .F.
	arc=.F.
	IF !m.ladeni
		SET FILT TO naz<>"_SETDBF" AND at('.',naz)=0 AND;
			(AT(m.gl_usernam+',',rtrim(firma.usr)+',')>0 OR ;
			AT('ALL'+',',rtrim(firma.usr)+',')>0)
	  ELSE
		SET FILT TO at('.',naz)=0 AND;
			(AT(m.gl_usernam+',',rtrim(firma.usr)+',')>0 OR ;
			AT('ALL'+',',rtrim(firma.usr)+',')>0)
		ENDIF
	IF int(rand()*10)=1 OR recc()=0
		DO fir_akt
		ENDIF
	GO TOP
	l11=[:naz+' ≥ '+str(rok,4)+' ≥ '+jme+' ≥  '+iif(pu,'˚',' ')+' ≥  '+ ;
			iif(im,'˚',' ')+' ≥  '+iif(sc,'˚',' ')+' ≥  '+iif(fs,'˚',' ')]
	l12="// Adres†˝     ≥ Rok  ≥ N†zev firmy               ≥ PU ≥ IM ≥ SC ≥ FS"

	DO list_c WITH -1,-1,18,0,;
		" VÏbÿr firmy k £ütov†n° ",;
		m.l11, m.l12, ;
		"\ F1-Help  ENTER-Èütov†n°  F7-T˝°dit podle ...",;
		'{naz}',;
		'CR#:	DO fir_ote			## ENTER-Otev˝en° firmy k £ütov†n°',;
		'ALT+A:	DO fir_arc			## ALT+A-Zobrazovat i archivy firem',;
	  iif(m.gl_prior>=90, ;
		'INS:	DO fir_ins			## INS-Vytvo˝en° novÇ firmy',''),;
		'ALT+O:	DO fir_akt			## ALT+O-Obnovit seznam firem',;
	  iif(m.gl_prior>=60, ;
		'ALT+U:	DO fir_app WITH "U"	## ALT+U-Zaloßit u firmy - PodvojnÇ £üetnictv°',''),;
	  iif(m.gl_prior>=60, ;
		'ALT+I:	DO fir_app WITH "I"	## ALT+I-Zaloßit u firmy - Investiün° majetek',''),;
	  iif(m.gl_prior>=60, ;
		'ALT+S:	DO fir_app WITH "C"	## ALT+S-Zaloßit u firmy - Zprac. slußebn°ch cest',''),;
	  iif(m.gl_prior>=60, ;
		'ALT+F:	DO fir_app WITH "F"	## ALT+F-Zaloßit u firmy - Veden° fakturace a skladu ',''),;
	  iif(m.gl_prior>=90, ;
		'ALT+V:	DO fir_vse		## ALT+V-Zobrazovat vÁechny firmy',''),;
	  iif(m.gl_prior>=90, ;
		'F4:	DO firma.spr	## F4-Oprava globaln°ch £dajÖ','')
	IF m.esc
		m.ggf=' '
		ENDIF
	DO refscr
	WAIT CLEAR
	=testflag()
	USE
RETURN
**	Zaloßen° novÇ firmy
PROCEDURE fir_ins 

PRIV nz, jm, fpu, fim, fsc, ffs, rc, fi

	nz=space(8)
	jm=space(25)
	STORE "Ne " TO fpu, fim, fsc, ffs
	
	DO fir_ins.spr
	IF m.esc
		RETURN
		ENDIF

	IF !(m.fpu="Ano" OR m.fim="Ano" OR m.fsc="Ano" OR m.ffs="Ano")
		WAIT WIND "Nebyl zad†n ani jeden modul pro vytvo˝en° datab†z°."
		RETURN
		ENDIF

	nz=upper(alltrim(left(m.nz,8)))
	
	SELE firma
	SET ORDER TO naz

	rc=recno()
	SET EXAC ON
	fi=seek(m.nz)
	SET EXAC OFF
	=gorec(m.rc)
	IF found()
		WAIT WIND "Firma "+m.nz+" jiß existuje."
		RETURN
		ENDIF

	IF !_MKDIR(m.gl_data+m.nz)
		WAIT WIND "ChybnÇ jmÇno adres†˝e, nebo adres†˝ jiß existuje."
		RETURN
		ENDIF

	IF m.fpu="Ano"
		CREATE DBF (m.gl_data+m.nz+"\denik.dbf") (_new_ L)
		USE
		IF SetDbfCP(m.gl_data+m.nz+"\denik.dbf", 852) !=0
			WAIT WIND "Nepoda˝ilo se nastavit p˝°znak datab†ze 852"
		ENDIF
	ENDIF

	IF m.fim="Ano"
		CREATE DBF (m.gl_data+m.nz+"\evim.dbf") (_new_ L)
		USE
		IF SetDbfCP(m.gl_data+m.nz+"\evim.dbf", 852) !=0
			WAIT WIND "Nepoda˝ilo se nastavit p˝°znak datab†ze 852"
		ENDIF
	ENDIF

	IF m.fsc="Ano"
		CREATE DBF (m.gl_data+m.nz+"\cglobal.dbf") (_new_ L)
		USE
		IF SetDbfCP(m.gl_data+m.nz+"\cglobal.dbf", 852) !=0
			WAIT WIND "Nepoda˝ilo se nastavit p˝°znak datab†ze 852"
		ENDIF
	ENDIF

	IF m.ffs="Ano"
		CREATE DBF (m.gl_data+m.nz+"\faktury.dbf") (_new_ L)
		USE
		IF SetDbfCP(m.gl_data+m.nz+"\faktury.dbf", 852) !=0
			WAIT WIND "Nepoda˝ilo se nastavit p˝°znak datab†ze 852"
		ENDIF
	ENDIF

	INSERT INTO firma (naz, jme, usr) VALUES (m.nz, m.jm, m.gl_usernam)

	DO fir_akt

RETURN
**	P˝id†n° datab†z° pro jednotlivÇ podprogramy
PROCEDURE fir_app
PARA _co
	PRIV _se
	_se=sele()
	SELE 0
	SET COMP ON
	DO CASE
	  CASE m._co='U' AND !file(m.gl_data+firma.naz+"\DENIK.DBF")
		CREA DBF (m.gl_data+firma.naz+"\DENIK.DBF") (dat D(8))
		IF file(m.gl_data+"_SETDBF\OSN.DBF")
			COPY FILE (m.gl_data+"_SETDBF\OSN.DBF") TO (m.gl_data+firma.naz+"\OSN.DBF")
			ENDIF
		WAIT WIND NOWA "Ve firmÿ "+alltrim(firma.naz)+" vytvo˝ena oblast pro £üetnictv°."
		DO fir_akt
	  CASE m._co='I' AND !file(m.gl_data+firma.naz+"\EVIM.DBF")
		CREA DBF (m.gl_data+firma.naz+"\EVIM.DBF") (dat D(8))
		WAIT WIND NOWA "Ve firmÿ "+alltrim(firma.naz)+" vytvo˝ena oblast pro evidenci IM."
		DO fir_akt
	  CASE m._co='C' AND !file(m.gl_data+firma.naz+"\cglobal.DBF")
		CREA DBF (m.gl_data+firma.naz+"\cglobal.DBF") (dtu D(8))
		WAIT WIND NOWA "Ve firmÿ '"+alltrim(firma.naz)+"' vytvo˝ena oblast pro zpracov†n° slußebn°ch cest."
		DO fir_akt
	  CASE m._co='F' AND !file(m.gl_data+firma.naz+"\FAKTURY.DBF")
		CREA DBF (m.gl_data+firma.naz+"\FAKTURY.DBF") (ico C(10))
		WAIT WIND NOWA "Ve firmÿ '"+alltrim(firma.naz)+"' vytvo˝ena oblast pro fakturaci a sklad."
		DO fir_akt
		ENDCASE
	SET COMP OFF
	SELE (m._se)
RETURN

**	Aktualizace p˝ehledovÇ datab†ze firem
PROCEDURE fir_akt
	WAIT WIND NOWA "Prov†d°m aktualizaci p˝ehledovÇ datab†ze firem ..."
	PRIV _ff, _rc, _ord, pf, i, tt
	SELE firma
	_rc=recno()
	_ff=filter()
	_ord=order()
	SET FILT TO
	SET ORDER TO naz
	DIME pp(256)
	pf=adir(pp,m.gl_data+"*.*","D")

	SET COMP ON
	FOR i=1 TO pf
		SET EXACT ON
		SEEK (alltrim(pp(i,1)))
		SET EXACT OFF
		IF !found()
			ggf=m.gl_data+pp(i,1)
			IF left(pp(i,1),1)='.' OR at('D',pp(i,5))=0 OR !testflag()
				LOOP
				ENDIF
			tt=m.gl_data+pp(i,1)+"\firma.mem"
			IF !file(tt)
				ggf=pp(i,1)
				SAVE TO (tt) ALL LIKE gg*
				ENDIF
			REST FROM (tt) ADDI
			INSERT INTO firma (naz, jme, usr, rok, pu, im, sc, fs) ;
				VALUES (pp(i,1), m.ggjme, m.ggusr, m.ggrok, ;
					m.pu, m.im, m.sc, m.fs)
			ENDIF
		ENDFOR
	SET COMP OFF
	SCAN ALL
		ggf=m.gl_data+firma.naz
		SCAT MEMVAR
		IF !testflag()
			DELE
		  ELSE
			GATH MEMVAR
			ENDIF
		ENDSCAN
	SET FILT TO &_ff
	SET ORDER TO &_ord
	=gorec(m._rc)
	WAIT CLEAR
	refscr=.T.
RETURN

**	Filtr pro zobrazen° archivn°ch firem
PROCEDURE fir_arc
	m.arc=!m.arc
	IF !m.ladeni
		__fltr="naz<>'_SETDBF' AND "+ ;
			"(AT(m.gl_usernam+',',rtrim(firma.usr)+',')>0 OR"+ ;
			" AT('ALL'+',',rtrim(firma.usr)+',')>0)"+;
			IIF(m.arc,""," AND at('.',naz)=0")
	  ELSE
		__fltr=IIF(m.arc,'',"at('.',naz)=0 AND ")+;
			"(AT(m.gl_usernam+',',rtrim(firma.usr)+',')>0 OR"+ ;
			" AT('ALL'+',',rtrim(firma.usr)+',')>0)"
		ENDIF
	refscr=.T.	
RETURN

**	Filtr pro zobrazen° vÁech firem
PROCEDURE fir_vse
	IF !m.ladeni
		__fltr='naz<>"_SETDBF"'
	  ELSE
		__fltr=''
		ENDIF
	refscr=.T.
RETURN

PROCEDURE fir_ote
	DO wpr
	ggjme=firma.jme
	ggf=m.gl_data+allt(naz)+"\"
	toexit=.T.
	a=m.ggf
	IF file(m.ggf+"firma.mem")
		ggusr=space(30)
		REST FROM (m.ggf+"firma.mem") ADDI
		IF empty(m.ggusr)
			ggusr=firma.usr
			ENDIF
		ENDIF
	ggf=m.a
	DO refscr
RETURN

**	Otestov†n° datab†z° jednotlivÏch systÇmÖ
*	Vrac°:	.T.	- AlespoÂ jedna datab†ze k dispozici
PROCEDURE testflag
	STORE .F. TO pu, im, sc, fs
	SET COMP ON
	IF file(m.ggf+"\denik.dbf")
		pu=.T.
		ENDIF
	IF file(m.ggf+"\evim.dbf")
		im=.T.
		ENDIF
	IF file(m.ggf+"\cglobal.dbf")
		sc=.T.
		ENDIF
	IF file(m.ggf+"\faktury.dbf")
		fs=.T.
		ENDIF
	SET COMP OFF
RETURN m.pu OR m.im OR m.sc OR m.fs

*************
**	R ﬁ Z N ê
PROCEDURE ruzne
	DEFINE POPUP mn_ruzne FROM 12,55 RELATIVE SHADOW ;
		TITLE " R Ö z n Ç " COLOR SCHEME 4
	DEFINE BAR  1 OF mn_ruzne PROMPT " \<Zmÿna hesla " ;
		MESS "Zmÿna vlastn°ho hesla."
	DEFINE BAR 97 OF mn_ruzne PROMPT "\-"
	DEFINE BAR  5 OF mn_ruzne PROMPT " \<Seznam ußivatelÖ " ;
		MESS "Oprava a p˝id†n° ußivatelÖ programu."
	DEFINE BAR  7 OF mn_ruzne PROMPT " Nastaven° stanic " ;
		MESS "Nastaven° systÇmovÏch parametrÖ programu pro jednotlivÇ stanice."
	DEFINE BAR  8 OF mn_ruzne PROMPT " Nastaven° tisk†ren " ;
		MESS "Nastaven° ˝°d°c°ch seqvenc° tisk†ren."
	DEFINE BAR 98 OF mn_ruzne PROMPT "\-"
	DEFINE BAR 80 OF mn_ruzne PROMPT " Stat. dle \<ußivatelÖ " ;
		MESS "Statistika pr†ce jednotlivÏch ußivatelÖ na firm†ch." ;
		SKIP FOR m.gl_prior<90
	DEFINE BAR 81 OF mn_ruzne PROMPT " Stat. dle \<firem " ;
		MESS "Statistika pr†ce ußivatelÖ na jednotlivÏch firm†ch." ;
		SKIP FOR m.gl_prior<90
	DEFINE BAR 99 OF mn_ruzne PROMPT "\-"
	DEFINE BAR 86 OF mn_ruzne PROMPT " \<Novinky" ;
		MESS "Popis £prav v hlavn°m menu systÇmu."
	
	ON SELE POPU mn_ruzne DO mn_ruzne

	ACTI POPUP mn_ruzne

	RELE POPU mn_ruzne
RETURN

PROC mn_ruzne
	DO CASE
		CASE bar()=1	&&	Zmÿna hesla
			=open_dbf(m.gl_data, "USERS", "op_user", "user", .F., .F.)
			SEEK m.gl_usernam
			STORE space(8) TO heslo,aa
			PRIV _l
			_l=26
			DEFINE WINDOW w_ask ;
				FROM INT((SROW()-6)/2),INT((SCOL()-(m._l-1))/2) ;
				TO INT((SROW()-6)/2)+5,INT((SCOL()-(m._l-1))/2)+m._l ;
				NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
			ACTIVATE WINDOW w_ask SAME
			@ 0,1 SAY "Ußivatel: "
			@ 2,1 SAY "Heslo   : "
			@ 3,1 SAY "Znovu   : "
			@ 0,12 SAY m.gl_usernam SIZE 1,12 COLOR SCHEME 5
			@ 2,12 GET m.heslo		SIZE 1, 8 COLOR ,b/b ;
				MESS "Zadejte heslo."
			@ 3,12 GET m.aa			SIZE 1, 8 COLOR ,b/b ;
				MESS "Znovuzad†n° hesla pro kontrolu."
			READ MODAL
*			IF lastkey()!=27
*				a=m.heslo
*				ACTIVATE WINDOW w_ask SAME
*				READ MODAL
*				ENDIF
			RELEASE WINDOW w_ask
			IF lastkey()!=27 AND m.aa==m.heslo
				SET LIBRARY TO crypt ADDI
*				heslo=alltrim(m.heslo)
*				heslo=left(repl(5,m.heslo),5)
				REPL users.heslo WITH ;
					encrypt(m.heslo,m.heslo)
				RELEASE LIBRARY crypt
			  ELSE
				IF !(m.aa==m.heslo)
					WAIT WIND "Nesouhlas° znovunatypovanÇ heslo. Zadejte hesla znovu."
					ENDIF
				ENDIF
			SELE users
			USE

		CASE bar()=5	&&	Seznam ußivatelÖ
			=open_dbf(m.gl_data, "USERS", "op_user", "user", .F., .F.)
			DO list_c WITH -1,-1,15,0,;
				[user_name+' ≥ '+full_name+' ≥ '+str(prior,2)+ ;
					' ≥ '+str(pr_pu,2)+' ≥ '+str(pr_im,2)+' ≥ '+str(pr_sc,2)+' ≥ '+str(pr_fs,2)],;
				" UßivatelÇ programu ",;
				"//  Ußivatel   ≥  PlnÇ jmÇno          ≥ P  ≥ PU ≥ IM ≥ SC ≥ FS ",;
				"\ F1-Help   ENTER-VÏbÿr   INS-P˝id†n° ußivatele   F7-T˝°dit podle ...",;
					'CR#:		DO user_opr WITH .F.	## ENTER-Oprava ußivatele ',;
				iif(m.gl_prior>=90, ;
					'INS:		DO user_opr WITH .T.	## INS-P˝id†n° ußivatele',''),;
				iif(m.gl_prior>=90, ;
					'ALT+D#:	DO user_del				## ALT+D-Smaz†n° ußivatele',''),;
				iif(m.gl_prior>=90, ;
					'ALT+P#:	DO us_prior				## ALT+P-Zmÿna priority','')
			SELE users
			USE
		CASE bar()=7
			=open_dbf(m.gl_data, "SETEKONT", "op_setek", "", .F., .F.)
			GO TOP
			SCATTER MEMVAR
			DO list_c WITH -1,-1,15,0,;
				[nstation+' ≥ '+floppy+' ≥ '+color_type+' ≥ '+str(set132_on,3)+ ;
					' ≥  '+str(set132_off,3)+' ≥'+imp_output+'≥ '+ ;
					left(lpt1,6)+' ≥ '+left(lpt2,6)+' ≥ '+left(lpt3,6)],;
				" Parametry programu ",;
				"// N†zev stanice  ≥Flp≥Typ≥132on≥132off≥ LPT ≥ LPT1   ≥ LPT2   ≥ LPT3",;
				"\ F1-Help   ENTER-Oprava   INS-P˝id†n° stanice   F7-T˝°dit podle ...",;
					'CR#:		DO sete_opr WITH .F.	## ENTER-Oprava stanice ',;
				iif(m.gl_prior>=99, ;
					'INS:		DO sete_opr WITH .T.	## INS-P˝id†n° stanice',''),;
				iif(m.gl_prior>=99, ;
					'ALT+D#:	DO sete_del				## ALT+D-Smaz†n° stanice','')
			USE
			WAIT WIND NOWA "P˝°padnÇ zmÿny parametrÖ se projev° aß p˝i dalÁ°m spuÁtÿn° programu."
		CASE bar()=8
			=open_dbf(m.gl_data, "SETPRINT", "op_setpr", "", .F., .F.)
			DO list_c WITH -1,-1,10,0,;
				[naz+' ≥ '+str(len_pg,3)+' ≥ '+str(margin,5)],;
				" Parametry tisk†ren ",;
				"// N†zev tisk†rny ≥¸†dkÖ≥ L.okraj",;
				"\ F1-Help   ENTER-Oprava   INS-P˝id†n° tisk†rny   F7-T˝°dit podle ...",;
					'CR#:		DO setp_opr WITH .F.	## ENTER-Oprava tisk†rny ',;
				iif(m.gl_prior>=99, ;
					'INS:		DO setp_opr WITH .T.	## INS-P˝id†n° tisk†rny ',''),;
				iif(m.gl_prior>=99, ;
					'ALT+D#:	DO setp_del				## ALT+D-Smaz†n° tisk†rny ','')
			USE
			WAIT WIND NOWA "P˝°padnÇ zmÿny parametrÖ se projev° aß p˝i dalÁ°m spuÁtÿn° programu."
		CASE bar()=80
			=open_dbf(m.gl_data, "STATI", "op_stati", "", .F., .F.)
			DO stat_use
		CASE bar()=81
			=open_dbf(m.gl_data, "STATI", "op_stati", "", .F., .F.)
			DO stat_fir
		CASE bar()=86
			DO novinky
		ENDCASE
RETURN

PROCEDURE setp_opr
PARA app
	IF m.app
		INSERT INTO setprint VALUES ('EPSON',62,15,"ASCII",'0','15','18','15,27,77',;
			'18,27,80','27,80','27,77','27,120,49','27,120,48','14','20')
		ENDIF
	DO setprint.spr
	IF m.esc AND m.app
		DELE
		RETURN
		ENDIF
RETURN

PROCEDURE setp_del
	IF ask(-1,-1,'N',"Opravdu smazat definici tisk†rny "+naz)
		DELE
		ENDIF
RETURN

PROCEDURE sete_opr
PARA app
	IF m.app
		INSERT INTO setekont (nstation) VALUES (getenv('nstation'))
		ENDIF
	DO setekont.spr
	IF m.esc AND m.app
		DELE
		RETURN
		ENDIF
RETURN

PROCEDURE sete_del
	IF ask(-1,-1,'N',"Opravdu smazat definici stanice "+nstation)
		DELE
		ENDIF
RETURN

PROCEDURE us_prior
	SCATTER MEMVAR
	SELE 0
	CREA CURS cu1 (pr N(2), txt C(20))
	INSERT INTO cu1 VALUES (99,"Supervisor")
	INSERT INTO cu1 VALUES (90,"ÊÇf")
	INSERT INTO cu1 VALUES (60,"Povÿ˝enÏ pracovn°k")
	INSERT INTO cu1 VALUES (50,"Pracovn°k")
	INSERT INTO cu1 VALUES (30,"Nahl°ßeü")
	INSERT INTO cu1 VALUES (11,"Fakturant")
	INSERT INTO cu1 VALUES (10,"Host")
	DO prior.spr
	USE
	SELE users
	IF !m.esc OR ask(-1,-1,'A',"Zapsat zmÿny")
		GATHER MEMVAR
		ENDIF
RETURN

PROCEDURE user_opr
PARA app
	IF m.app
		INSERT INTO users (user_name) VALUES (m.gl_usernam)
		ENDIF
	DO users.spr
	IF m.esc AND m.app
		DELE
		RETURN
		ENDIF
RETURN

PROCEDURE user_del
	IF ask(-1,-1,'N',"Opravdu smazat ußivatele "+user_name)
		DELE
		ENDIF
RETURN

PROCEDURE _lstprio
PARA _pr
	LOCATE ALL FOR m._pr=cu1.pr
	DO list_c WITH -1,-1,10,0,;
		[str(cu1.pr,4)+'  - '+txt],;
		" Ußiv: "+users.full_name+" ",;
		"//Prio. - P˝°stupovÇ pr†vo",;
		"\ F1-Help   ENTER-VÏbÿr priority"
	IF !m.esc
		_pr=cu1.pr
		ENDIF
RETURN m._pr
**	Statistika dle firem
PROCEDURE stat_fir

	SET CENT ON
* Dotaz na obdob°
	dtod=ctod("01"+substr(dtoc(gomonth(date(),-1)),3))
	dtdo=gomonth(m.dtod,1)-1

	PRIV _sl, _ra
	_sl=37
	_ra=1
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._ra+2))/2),INT((SCOL()-(m._sl-1))/2) ;
		TO INT((SROW()-(m._ra+2))/2)+m._ra+1,INT((SCOL()-(m._sl-1))/2)+m._sl ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Obdob° od            do "
	@ 0,11 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,25 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	SET CENT OFF
	IF lastkey()=27
		RETURN
		ENDIF

	SET TALK ON
	DO wpr
	SELECT sum(st.log_time) AS log_time, ;
		sum(st.time_user) AS time_user, sum(st.time_cpu) AS time_cpu, ;
		sum(st.time_wait) AS time_wait, sum(st.count_key) AS count_key, ;
		st.firma+st.user AS fius ;
		FROM stati st INTO CURSOR se1 ;
		WHERE betw(st.on_dat, m.dtod, m.dtdo) ;
		GROUP BY fius ORDER BY fius
	SET TALK OFF

	IF empty(m.ggf)
		tskf=m.gl_data+sys(3)+".bak"
	  ELSE
		tskf=m.ggf+'ekont.prt'
		ENDIF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	?space(m.margin+10)+"Statistika dle firem od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?
	?
	_ff='???'
	SCAN ALL
		IF _ff!=left(fius,12)
			IF _ff!='???'
				?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----------+-----------+'
				?space(m.margin/1.6)+'| Celkem:      | '+ ;
					sectotim(_stu+_stc)+' | '+ ;
					sectotim(_stu)+' | '+ ;
					sectotim(_stc)+' | '+ ;
					str(_skl,8)+' | '+ ;
					sectotim(_stw)+' | '+ ;
					sectotim(_stl)+' |'
				?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----------+-----------+'
				?
				ENDIF
			_ff=left(fius,12)
			?space(m.margin/1.6)+'   Firma: '+m._ff
			?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----------+-----------+'
			?space(m.margin/1.6)+'| Ußivatel     | ¨as pr†ce | ¨as ußiv. |  ¨as CPU  |   Èdery  |   ¨ek†n°  |   Celkem  |'
			?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----------+-----------+'
			STORE 0 TO _stu,_stc,_stw,_stl,_skl
			ENDIF
		?space(m.margin/1.6)+'| '+right(fius,12)+' | '+ ;
			sectotim(time_user+time_cpu)+' | '+ ;
			sectotim(time_user)+' | '+ ;
			sectotim(time_cpu)+' | '+ ;
			str(count_key,8)+' | '+ ;
			sectotim(time_wait)+' | '+ ;
			sectotim(log_time)+' |'
		_stu=_stu+time_user
		_stc=_stc+time_cpu
		_stw=_stw+time_wait
		_stl=_stl+log_time
		_skl=_skl+count_key
		ENDSCAN
	?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----------+-----------+'
	?space(m.margin/1.6)+'| Celkem:      | '+ ;
		sectotim(_stu+_stc)+' | '+ ;
		sectotim(_stu)+' | '+ ;
		sectotim(_stc)+' | '+ ;
		str(_skl,8)+' | '+ ;
		sectotim(_stw)+' | '+ ;
		sectotim(_stl)+' |'
	?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----------+-----------+'
	WAIT CLEAR
	SET ALTE TO
	USE
	SELE stati
	USE
	DO tisk
	SET CONS ON
RETURN
**	Statistika dle ußivatelÖ
PROCEDURE stat_use

	SET CENT ON

* Dotaz na obdob°
	dtod=ctod("01"+substr(dtoc(gomonth(date(),-1)),3))
	dtdo=gomonth(m.dtod,1)-1

	PRIV _sl, _ra
	_sl=37
	_ra=1
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._ra+2))/2),INT((SCOL()-(m._sl-1))/2) ;
		TO INT((SROW()-(m._ra+2))/2)+m._ra+1,INT((SCOL()-(m._sl-1))/2)+m._sl ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Obdob° od            do "
	@ 0,11 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,25 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	SET CENT OFF

	IF lastkey()=27
		RETURN
		ENDIF

	SET TALK ON
	DO wpr
	SELECT sum(st.log_time) AS log_time, ;
		sum(st.time_user) AS time_user, sum(st.time_cpu) AS time_cpu, ;
		sum(st.time_wait) AS time_wait, sum(st.count_key) AS count_key, ;
		st.user+st.firma AS fius ;
		FROM stati st INTO CURSOR se1 ;
		WHERE betw(st.on_dat, m.dtod, m.dtdo) ;
		GROUP BY fius ORDER BY fius
	SET TALK OFF

	IF empty(m.ggf)
		tskf=m.gl_data+sys(3)+".bak"
	  ELSE
		tskf=m.ggf+'ekont.prt'
		ENDIF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	?space(m.margin+10)+"Statistika dle ußivatelÖ od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?
	?
	_ff='???'

	PRIV _stu,_stc,_stw,_stl,_skl
	STORE 0 TO _stu,_stc,_stw,_stl,_skl
	SCAN ALL
		IF _ff!=left(fius,8)
			IF _ff!='???'
				?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----+-----------+-----------+'
				?space(m.margin/1.6)+'| Celkem:      | '+ ;
					sectotim(_stu+_stc)+' | '+ ;
					sectotim(_stu)+' | '+ ;
					sectotim(_stc)+' | '+ ;
					str(_skl,8)+' | '+ ;
					str(_skl/(_stu/60),3)+' | '+ ;
					sectotim(_stw)+' | '+ ;
					sectotim(_stl)+' |'
				?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----+-----------+-----------+'
				?
				ENDIF
			_ff=left(fius,8)
			?space(m.margin/1.6)+'   Ußivatel: '+_ff
			?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----+-----------+-----------+'
			?space(m.margin/1.6)+'| Firma        | ¨as pr†ce | ¨as ußiv. |  ¨as CPU  |   Èdery  | È/m |   ¨ek†n°  |   Celkem  |'
			?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----+-----------+-----------+'
			STORE 0 TO _stu,_stc,_stw,_stl,_skl
			ENDIF
		?space(m.margin/1.6)+'| '+right(fius,12)+' | '+ ;
			sectotim(time_user+time_cpu)+' | '+ ;
			sectotim(time_user)+' | '+ ;
			sectotim(time_cpu)+' | '+ ;
			str(count_key,8)+' | '+ ;
			str(count_key/(time_user/60),3)+' | '+ ;
			sectotim(time_wait)+' | '+ ;
			sectotim(log_time)+' |'
		_stu=_stu+time_user
		_stc=_stc+time_cpu
		_stw=_stw+time_wait
		_stl=_stl+log_time
		_skl=_skl+count_key
		ENDSCAN
	?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----+-----------+-----------+'
	?space(m.margin/1.6)+'| Celkem:      | '+ ;
		sectotim(_stu+_stc)+' | '+ ;
		sectotim(_stu)+' | '+ ;
		sectotim(_stc)+' | '+ ;
		str(_skl,8)+' | '+ ;
		str(_skl/(_stu/60),3)+' | '+ ;
		sectotim(_stw)+' | '+ ;
		sectotim(_stl)+' |'
	?space(m.margin/1.6)+'+--------------+-----------+-----------+-----------+----------+-----+-----------+-----------+'
	WAIT CLEAR
	SET ALTE TO
	USE
	SELE stati
	USE
	DO tisk
	SET CONS ON
RETURN
**  Otev˝en° datab†ze nastaven° programu Ekont
PROCEDURE op_setek
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) (nstation C(15), floppy C(1), path_kurzy C(30), ;
			color_type C(1), set132_on N(3), set132_off N(3), ;
			imp_output C(5), lpt1 C(15), lpt2 C(15), lpt3 C(15))
	  CASE m._cfn=2
	  CASE m._cfn=3
		RETURN type("setekont.nstation")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**  Otev˝en° datab†ze nastaven° tiskÖ programu Ekont
PROCEDURE op_setpr
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) (naz C(15), len_pg N(3), margin N(2), prekodtisk C(5), ;
			resetprn C(128), cnon C(70), cnoff C(70), ;
			ecnon C(70), ecnoff C(70), pica C(70), elit C(70), ;
			nlq C(70), draft C(70), dblon C(70), dbloff C(70))
	  CASE m._cfn=2
	  CASE m._cfn=3
		RETURN type("setprint.prekodtisk")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Otev˝en° datab†ze statistiky
PROCEDURE op_stati
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) (firma C(12), user C(8), nstation C(8), program C(8), ;
			on_dat D, on_time C(8), off_dat D, off_time C(8), log_time N(5), ;
			time_user N(5), time_CPU N(5), time_wait N(5), count_key N(5))
	  CASE m._cfn=2
		INDEX ON firma+cs(user) TAG firma
	  CASE m._cfn=3
		RETURN type("stati.nstation")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

**	Otev˝en° p˝ehledovÇ datab†ze firem
PROCEDURE op_firma
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) (naz C(12), jme C(25), rok N(4), usr C(30), prv N(3), ;
			pu L, im L, sc L, fs L)
	  CASE m._cfn=2
		INDEX ON naz TAG naz
		INDEX ON cs(jme) TAG jme
	  CASE m._cfn=3
		RETURN type("FIRMA.fs")="U" OR type("FIRMA.datzal")="D"
	  CASE m._cfn=4
		ENDCASE
RETURN
**  OPEN ußivatelÖ
PROCEDURE op_user 
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) (user_name C(12), full_name C(20), prior N(2), ;
			pr_pu N(2), pr_im N(2), pr_sc N(2), pr_fs N(2), heslo C(8))
	  CASE m._cfn=2
		INDEX ON user_name TAG user
	  CASE m._cfn=3
		RETURN type("USERS.pr_fs")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Uzav˝en° datab†z° s otev˝en°m pot˝ebnÏch datab†z°
PROCEDURE cldata1
	CLOSE DATA

	IF !open_dbf(m.gl_data, "FIRMA", "op_firma", "NAZ", .F., .F.)
		RETURN .F.
	ENDIF

*	SELE 1
*	IF !file(m.gl_data+"firma.dbf")
*		DO crt_fir
*		ENDIF
*	ON ERROR USE &gl_data.firma
*	USE &gl_data.firma
*	ON ERROR &_onerr
*	IF !file(m.gl_data+"firma.cdx")
*		INDEX ON naz TAG naz
*		INDEX ON cs(jme) TAG jme
*		ENDIF
*	SET ORDER TO naz

	IF recc()=0
		DO crt_fir
		ENDIF
	SEEK m.nfirma
RETURN

**	Vytvo˝° p˝ehledovou datab†zi firem
PROCEDURE crt_fir

*	CLOSE DATA
*	CREA DBF &gl_data.firma.dbf (naz C(12), jme C(25), usr C(30), prv N(3))
*	INDEX ON naz TAG naz
*	INDEX ON cs(jme) TAG jme

	DIME pp(256)
	PRIV pf,i,tt
	pf=adir(pp,m.gl_data+"*.","D")
	IF pf<1
		WAIT WIND "Neexistuj° ß†dnÇ datab†zovÇ soubory"
		QUIT
		ENDIF

	SELE 0
	FOR i=1 TO pf
		IF left(pp(i,1),1)='.' OR !file(m.gl_data+pp(i,1)+"\denik.dbf")
			LOOP
			ENDIF
		tt=m.gl_data+pp(i,1)+"\firma.mem"
		IF !file(tt)
			ggf=pp(i,1)
			SAVE TO (tt) ALL LIKE gg*
			ENDIF
		REST FROM (tt) ADDI
		INSERT INTO firma (naz, jme, usr) VALUES (pp(i,1), m.ggjme, 'ALL')
		ENDFOR

	CLOSE DATA
RETURN

*************************************************
**					F U N K C E
*************************************************

**  Zobrazeni podkladu  *
PROCEDURE backgr
PARAM bgr
	PRIVATE bck, ver, pver
	ACTIV SCREEN
	SET MESSAGE TO
	IF srows()=25 AND scols()=80 ;
			AND file(m.bgr+'.BGR')
		RESTORE FROM (m.bgr+'.BGR') ADDITIVE
		REST SCREEN FROM bck
		RELEASE bck
	  ELSE
		CLEAR
		ENDIF
	IF (m.gl_verze!=0)
		ver="Verze: "+alltrim(str(m.gl_verze,10,2))
		pver=''
		DO CASE
		  CASE m.Ladeni
			pver="(Prog.)"
		  CASE type('demo')=='D' AND !empty(m.demo)
			pver="(DEMO)"
		  CASE right(curdir(),5)=='ZKUS\'
			pver="(ZkuÁebn°)"
			ENDCASE
		IF upper(m.bgr)=='LOGIN'
*			pver=iif(empty(m.pver), '', '  '+m.pver)
*			@ 0,scols()-len(m.ver)-len(m.pver) SAY m.ver+m.pver
			@ 0,scols()-len(m.ver)-2 SAY m.ver
			IF !empty(m.pver)
				@ 1,scols()-len(m.pver)-2 SAY m.pver
				ENDIF
		  ELSE
			@ 0,scols()-len(m.ver) SAY m.ver
			IF !empty(m.pver)
				@ 1,scols()-len(m.pver) SAY m.pver
				ENDIF
			ENDIF
		ENDIF
RETURN
********************************
**	Z†pis statistiky do datab†ze
**	p = jmÇno programu v nÿmß se pracovalo
PROCEDURE wrt_stat
PARA p
	IF open_dbf(m.gl_data, "STATI", "op_stati", "Firma", .F., .F.) ;
			AND !empty(m.ggf)
		INSERT INTO stati (firma, user, nstation, program, ;
			on_dat, on_time, off_dat, off_time, ;
			log_time, time_user, time_cpu, time_wait, count_key) ;
		  VALUES (firma.naz, m.gl_usernam, getenv("nstation"), p, ;
			m.st_on_dat, m.st_on_time, date(), time(), ;
			eff_total(), eff_user(), eff_cpu(), eff_wait(), eff_keys())
		ENDIF
RETURN

PROCEDURE sectotim
PARA _sec
RETURN str(int(_sec/3600),3)+':'+strt(str(int(mod(_sec,3600)/60),2)+':'+ ;
			str(mod(_sec,60),2),' ','0')
**	Zobrazen° novinek
PROCEDURE novinky 
	IF file('..\Novinky.Tex')
		DEFINE WIND w_new FROM 5,2 TO 20,75 DOUBLE SHADOW ;
			TITLE " ***  Novinky v programu  *** " COLOR SCHEME 13
		IF m.gl_pri_pu=99
			MODI COMM ..\Novinky.Tex WIND w_new
		  ELSE
			MODI COMM ..\Novinky.Tex NOEDIT WIND w_new
			ENDIF
		RELE WIND w_new
		ENDIF
RETURN

**	Vol†n° obsluhy kurzovn°ch l°stkÖ
PROCEDURE callkurz
	PRIV fk, i, s, a, q
	fk=fopen("calckurz.ini")
	IF fk=-1
		WAIT WIND "Nebyl nalezen soubor s kurzy mÿn (calckurz.ini)"
		RETURN
		ENDIF
	q=5
	DIME ikurz(15,q)
	i=0
	DO WHILE !feof(m.fk) AND i<16
		i=i+1
		s=fgets(m.fk)
		FOR j=1 TO q
			a=iif(at(',',s)=0,len(s),at(',',s)-1)
			ikurz(i,j)=left(s,a)
			s=substr(s,a+2)
			ENDFOR
		IF j<q+1
			i=i-1
			ENDIF
		ENDDO
	a=i
	=fclose(m.fk)

***************************************
****	Dotaz na jednou poloßku    ****
***************************************
	PRIV _l, _r
	_l=77
	_r=8+i
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._r-1))/2), INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-(m._r-1))/2)+m._r, INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME

	j=0
	ON ERROR j=-1
	DO WHILE lastkey()!=27
		c=val(m.kalk_vysl)
		FOR i=1 TO a
			@ 4+i,1 SAY ikurz(i,1)+" "+ikurz(i,2)+" ≥ "+ikurz(i,3)+" ≥ "+ ;
				str(val(ikurz(i,3))*c/val(ikurz(i,1)),10,2)+" ≥≥ "
			IF val(ikurz(i,4))!=0
				??ikurz(i,4)+" ≥"+str(c/val(ikurz(i,4)),9,2)+ ;
					" ≥"+str(c/val(ikurz(i,4))*val(ikurz(i,5)),10,2)+" ≥≥"+ ;
					str(val(ikurz(i,3))*c/val(ikurz(i,1))-c/val(ikurz(i,4))*val(ikurz(i,5)),8,2)
			  ELSE
				??"        ≥          ≥           ≥≥"
				ENDIF
			ENDFOR
		@ 0,1 SAY "¨†stka: "
		@ 2,1 SAY "        ≥    StanovenÏ kurz   ≥≥     P˝epoüet na EURo a Kü      ≥≥ KurzovÏ"
		@ 3,1 SAY " Mÿna   ≥  Kurz  ≥      Kü    ≥≥ koef.   ≥    EUR   ≥     Kü    ≥≥  rozd°l"
		@ 4,1 SAY "ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒ≈≈ƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒ≈≈ƒƒƒƒƒƒƒƒƒ"
		@ 0,10 GET kalk_vysl SIZE 1,12 COLOR SCHEME 4 FUNCTION "K"
		READ MODAL
		ENDDO
	ON ERROR &_onerr
	RELEASE WINDOW w_ask

	RELE ikurz

*	IF type("dat")='D'
*		DO kurzak WITH dat
*	  ELSE
*		DO kurzak WITH gomonth(date(),-1)
*		ENDIF
RETURN
**	Vol†n° Nortona		ALT + C
PROCEDURE call_nc
	DO nc
	DO refscr
RETURN
**	Vyp°Áe do waitu pracuji
PROC wpr
	WAIT WIND NOWA "Pracuji ..."
RETURN
