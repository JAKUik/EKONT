*********************************************************
SET PROC TO cesty

SET CENT ON

**	Tady byly globalni parametry firmy

**	Dal็ก d…leงit glob lnก promุnn
PUBLIC typtisk,pgbeg,pgend,page,nfirma
cis_od=1
cis_do=9999
typtisk="Obojก"		&&	Typ tisku str nek	Obojก,Lich,Sud
pgbeg=1
pgend=9999
page=1
nfirma=''

PRIV t_gl, t_cp, t_vv, t_me, aktarch, flgzmeny, hromada
a=right(sys(3),6)
t_gl="gl"+a
t_cp="cp"+a
t_vv="vv"+a
t_me="me"+a
aktarch=""
flgzmeny=.F.
hromada=.F.

DIME pdow(7)
pdow(1)="Nedุle "
pdow(2)="Pondุlก"
pdow(3)="้ter์  "
pdow(4)="Stýeda "
pdow(5)="ฌtvrtek"
pdow(6)="P tek  "
pdow(7)="Sobota "

*******************
****	Hlavnก MENU
DEFINE POPUP hlm FROM 6,30 RELATIVE SHADOW ;
	TITLE " Hlavnก menu - SC" COLOR SCHEME 4
DEFINE BAR  1 OF hlm PROMPT " \<V์bุr SC k zpracov nก " SKIP FOR !used("cglobal") ;
	MESS "V์bุr cesty ke zpracov nก. Maz nก nepotýebn์ch cest."
DEFINE BAR  9 OF hlm PROMPT "\-"
DEFINE BAR 12 OF hlm PROMPT " \<Glob lnก ฃdaje " SKIP FOR !used(m.t_gl) ;
	MESS "Vyplnุnก hlavnกch ฃdaj… k cestุ."
DEFINE BAR 13 OF hlm PROMPT " \<Cesty a pobyty " SKIP FOR !used(m.t_gl) OR empty(&t_gl..jme) ;
	MESS "Vyplnุnก pr…bุhu cesty s pýesuny a pobyty."
DEFINE BAR 14 OF hlm PROMPT " \<Jin v์daje" SKIP FOR !used(m.t_gl) OR empty(&t_gl..jme) ;
	MESS "Vyplnุnก dal็กch v์daj… souvisejกcกch s cestou."
DEFINE BAR 15 OF hlm PROMPT " Va\<luty" SKIP FOR !used(m.t_gl) OR empty(&t_gl..jme) ;
	MESS "Vyplnุnก kurz… jednotliv์ch mุn, poskytnut์ch z loh a cen pohonn์ch hnot."
DEFINE BAR 21 OF hlm PROMPT " \<Tiskov  sestava " SKIP FOR !used(m.t_cp) OR recc(m.t_cp)=0 ;
	MESS "Tiskov  sestava vlastnกho vyฃtov nก."
DEFINE BAR 19 OF hlm PROMPT "\-"
DEFINE BAR 31 OF hlm PROMPT " ฌก\<selnกky" ;
	MESS "ฌกselnกky voz…, ฃastnกk… cest, historie n hrad a stravnho."
DEFINE BAR 41 OF hlm PROMPT " \<R…zn" ;
	MESS "Glob lnก parametry, indexov nก datab zก a novinky v programu."
*DEFINE BAR 49 OF hlm PROMPT " \<Firma k vyฃtov nก " ;
*	MESS "Zmุna firmy k ฃov nก."
ON SELE POPU hlm DO hlm

DEFI WIND w_lline FROM 24,0 TO 24,80 NONE COLOR SCHEME 14

************************
****	Za tek programu
DO refscr

a=substr(m.ggf,rat('\',m.ggf,2)+1)
a=left(a,len(a)-1)
DO cldata
SELE firma
SEEK a
IF found()
	DO fir_ote
  ELSE
	DO hlm_fir
	ENDIF

fp=fopen('..\Novinky.Tex')
IF fp!=-1
	WAIT WIND NOWA "Poslednก novinka v programu je z "+left(fgets(fp),6)
	=fclose(fp)
	ENDIF

_bar=1
DO WHILE .T.
	ACTIVATE POPUP hlm BAR _bar
	IF bar()=0
		DO hlm_kon
		EXIT
		ENDIF
	ENDDO
RETURN

**	Ukonenก pr ce programu a n vrat do HLAVNIHO SYSTEMU
PROCEDURE hlm_kon

	DO closetmp

	DO wrt_stat WITH "CESTY"
	IF !empty(m.ggf)
		SAVE TO (m.ggf+"firma.mem") ALL LIKE gg*
		ENDIF
	DO cldata
	CLOSE ALL

	SET CENT OFF

	CLEAR FIELDS
	CLEAR GETS
	CLEAR MACROS
	CLEAR MENUS
	CLEAR PROGRAM
	CLEAR PROMPT
	CLEAR TYPEAHEAD
	CLEAR WINDOWS
RETURN
**  Hlavnก podprogramy menu
PROCEDURE hlm
	DO CASE
		CASE bar()=1	&&	V์bุr SC k zpracov nก
			DO OpenSC
		CASE bar()=12	&&	Glob lnก ฃdaje
			DO glob_ud
		CASE bar()=13	&&	Cesty a pobyty
			DO cespobyt
		CASE bar()=14	&&	Vedlej็ก v์daje
			DO vydaje
		CASE bar()=15	&&	Pouงit valuty
			DO meny WITH ""
		CASE bar()=21	&&	Vyฃtov nก
			DO tisk_vyu
		CASE bar()=31	&&	ฌกselnกky
			DO hlm_cis
		CASE bar()=41	&&	R…zn
			DO hlm_ruz
		CASE bar()=49	&&	Firma k vyฃtov nก
			DO Hlm_Fir
		ENDCASE
	DO RefScr
RETURN
**	V์bุr sluงebnก cesty
PROCEDURE OpenSC
	PRIV sel, ord
	sel=SELE()
	DO CloseTmp
	SELE cglobal
	SET ORDER TO dat
	SET FILT TO arch!=repl('9',16)
	GO BOTT
	DO list_c WITH -1,-1,15,0,;
		[iif(flg,'*',' ')+cis+' ณ '+dtoc(dat)+' ณ '+left(popis,22)+' ณ '+left(jme,10)+' ณ '+dtoc(datod)],;
		" Sluงebnก cesty firmy "+m.ggjme,;
		"//   ฌกslo    ณ   Datum    ณ  ้el                  ณ  ้astnกk  ณ Za tek",;
		"\F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
		"{CIS \<ฌกsla, DAT \<Datumu}",;
		'CR#:		DO opsc_edi WITH .F.		## ENTER-Oprava sluงebnก cesty',;
		'INS:		DO opsc_edi WITH .T.		## INS-Nov  sluงebnก cesta',;
		'ALT+M#:	DO opsc_mar					## ALT+M-Oznaenก z znamu',;
		'ALT+O#:	DO opsc_nom					## ALT+O-Zru็enก v็ech oznaenก',;
		'ALT+T#:	DO tisk_all					## ALT+T-Tisk a export oznaen์ch cest ',;
		'ALT+D#:	DO opsc_del					## ALT+D-Vyýazenก sluงebnก cesty'
	SET RELA TO
	SET FILT TO
	SELE (m.sel)
RETURN
**	Natenก sluงebnก cesty pro pýid nก nebo opravu
PROCEDURE opsc_edi
PARA app

* Vytvoýก pr zdn TEMPy
	DO open_tmp	

	IF !m.app
		aktarch=cglobal.arch
		DO DbToTmp WITH cglobal.arch
	  ELSE

* Pýekladov omezenก na 3 cesty pro demo verzi
	IF !m.FULLVER
		IF recc("cglobal")>=3
			WAIT WIND "V DEMO verzi lze zadat pouze 3 vyฃtov nก."
			RETURN
			ENDIF
		ENDIF

		aktarch=dtos(date())+sys(3)
		IF recc("cglobal")!=0 AND ask(-1,-1,'A',"Pýevzกt ฃdaje z aktu lnก SC")
			DO DbToTmp WITH m.aktarch
		  ELSE
			INSERT INTO (m.t_gl) (arch, m_add) VALUES (m.aktarch, dtos(date())+sys(3))
			ENDIF
		ENDIF
	toexit=.T.
RETURN
**	Vymaz nก sluงebnก cesty s vyฃtov nกm a v์daji
PROCEDURE opsc_del
	IF ask(-1,-1,'A',"Opravdu smazat SC . "+cis)
		SELE cvydaje
		REPL ALL FOR cglobal.arch=cvydaje.arch ;
			arch WITH repl('9',16), ;
			m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()

		SELE cvyuct
		REPL ALL FOR cglobal.arch=cvyuct.arch ;
			arch WITH repl('9',16), ;
			m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()

		SELE cglobal
		REPL arch WITH repl('9',16), ;
			m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		ENDIF
RETURN
**	Oznaenก z znamu
PROCEDURE opsc_mar
	REPL flg WITH !flg
	SKIP 1
*	refscr=.T.
RETURN
**	Otevýenก doasn์ch PRตZDNํCH soubor…
PROCEDURE open_tmp
	PRIV sel
	sel=sele()

	SELE cglobal
	=afields(mm)
	SELE 0
	CREA CURS (m.t_gl) FROM ARRAY mm

	SELE cvyuct
	=afields(mm)
	SELE 0
	CREA CURS (m.t_cp) FROM ARRAY mm
	INDEX ON dtos(dat1)+hod1+min1 TAG dat

	SELE cvydaje
	=afields(mm)
	SELE 0
	CREA CURS (m.t_vv) FROM ARRAY mm
	INDEX ON dtos(dat) TAG dat

	SELE cmeny
	=afields(mm)
	SELE 0
	CREA CURS (m.t_me) FROM ARRAY mm
	INDEX ON mena TAG mena

	SELE (m.sel)
RETURN
**	Tisk v็ech oznaen์ch cest
PROCEDURE TISK_ALL
	PRIV ss
	ss=sele()
  * Otevýenก exportnก datab ze
	IF !open_dbf(m.ggf, "CEXPO", "op_expo", "ciscesty", .T., .F.)
		WAIT WIND "Nelze otevýกt exportnก datab zi."
		RETURN
		ENDIF

	SELE (m.ss)
	SCAN ALL FOR flg
		DO open_tmp	
		aktarch=cglobal.arch
		DO DbToTmp WITH cglobal.arch
		DO tisk_vyu
		SELE cexpo
		DELE ALL FOR ciscesty=&t_gl..cis
		SELE (t_me)
		SCAN ALL FOR celkem!=0
			IF kcmd!=0 AND kcdal!=0
				INSERT INTO cexpo VALUES ;
					(&t_gl..dat, &t_me..kctyp, &t_me..kcmd, &t_me..kcmda, &t_me..kcdal, &t_me..kcdaa, ;
					alltrim(&t_me..extxt)+alltrim(&t_gl..cis), ;
					&t_me..celkem*&t_me..kurz/&t_me..koef, &t_gl..cis)
				ENDIF
			IF zmmd!=0 AND zmdal!=0
				INSERT INTO cexpo VALUES ;
					(&t_gl..dat, &t_me..zmtyp, &t_me..zmmd, &t_me..zmmda, &t_me..zmdal, &t_me..zmdaa, ;
					alltrim(&t_me..extxt)+alltrim(&t_gl..cis), ;
					&t_me..celkem, &t_gl..cis)
				ENDIF
			ENDSCAN
		SELE (m.ss)
		ENDSCAN

	SELE cexpo
	USE
RETURN


**	Uzavýenก doasn์ch soubor… a jejich uloงenก
PROCEDURE closetmp

	PRIV a
	a= m.flgzmeny AND used(m.t_gl) ;
		AND (ask(-1,-1,"A","Uloงit aktu lnก vyutov nก") ;
			OR ask(-1,-1,"A","Vyutov nก bude ztraceno. Uloงit ho"))

	IF used(m.t_gl)
		IF a
			DO tmptodb WITH m.t_gl, "cglobal"
			ENDIF
		USE IN (m.t_gl)
		ENDIF

	IF used(m.t_cp)
		IF a
			DO tmptodb WITH m.t_cp, "cvyuct"
			ENDIF
		USE IN (m.t_cp)
		ENDIF

	IF used(m.t_vv)
		IF a
			DO tmptodb WITH m.t_vv, "cvydaje"
			ENDIF
		USE IN (m.t_vv)
		ENDIF

	IF used(m.t_me)
		IF a
			DO tmptodb WITH m.t_me, "cmeny"
			ENDIF
		USE IN (m.t_me)
		ENDIF

	flgzmeny=.F.

RETURN
**	Pýevod z datab ze do TMP souboru
PROCEDURE dbtotmp
PARA newarch
	PRIV sel, archzgl
	sel=sele()

	SELE cglobal
	archzgl=arch

		SCATTER MEMVAR
		IF arch!=m.newarch
			m_add=dtos(date())+sys(3)
			ENDIF
		arch=m.newarch
		INSERT INTO (m.t_gl) FROM MEMVAR


	SELE cvyuct
	SET ORDER TO arch
	=seek(m.archzgl)

	SCAN REST WHILE cvyuct.arch=m.archzgl
		SCATTER MEMVAR
		IF arch!=m.newarch
			m_add=dtos(date())+sys(3)
			ENDIF
		arch=m.newarch
		INSERT INTO (m.t_cp) FROM MEMVAR
		ENDSCAN


	SELE cvydaje
	SET ORDER TO arch
	=seek(m.archzgl)

	SCAN REST WHILE cvydaje.arch=m.archzgl
		SCATTER MEMVAR
		IF arch!=m.newarch
			m_add=dtos(date())+sys(3)
			ENDIF
		arch=m.newarch
		INSERT INTO (m.t_vv) FROM MEMVAR
		ENDSCAN


	SELE cmeny
	SET ORDER TO arch
	=seek(m.archzgl)

	SCAN REST WHILE cmeny.arch=m.archzgl
		SCATTER MEMVAR
		IF arch!=m.newarch
			m_add=dtos(date())+sys(3)
			ENDIF
		arch=m.newarch
		INSERT INTO (m.t_me) FROM MEMVAR
		ENDSCAN

	SELE (m.sel)
RETURN
**	Pýepis TMP souboru do datab zก
PROCEDURE tmptodb 
PARA zdr, cil

	PRIV a, i

*	SELE (m.zdr)
*	a=arch
	SELE (m.cil)
	SET ORDER TO arch

* Vymaงe v็echny poloงky v archivu
	REPL FOR m.aktarch=arch arch WITH repl('9',16)

	SELE (m.zdr)
	SCAN ALL
		SCATTER MEMVAR
		SELE (m.cil)
		IF seek(repl('9',16))
			GATHER MEMVAR
		  ELSE
			INSERT INTO (m.cil) FROM MEMVAR
			ENDIF
		SELE (m.zdr)
		ENDSCAN
RETURN
**	Cesta nebo pobyt
PROCEDURE cespobyt
	SELE (m.t_cp)
	SET ORDER TO dat
	GO TOP
    DO list_c WITH -1,-1,10,0,;
		" Cesty a pobyty ",;
		[:iif(typ="C","Cesta","Pobyt")+'ณ '+dtoc(dat1)+' '+hod1+':'+min1+;
			' ณ '+left(mis1,9)+'ณ '+dtoc(dat2)+' '+hod2+':'+min2+' ณ '+ ;
			left(mis2,9)+'ณ '+left(stat,8)],;
		[|1], ;
		"// Typ ณ  Za tek         ณ  Mกsto   ณ  Konec           ณ  Mกsto   ณ St t",;
		[||1PF=iif(typ='C',"n/w","b/w")],;
		"\ F1-Help   ENTER-Oprava z znamu   F3-Pýid nก cesty   F4-Pýid nก pobytu",;
		'CR#:	DO vyu_edit				## ENTER-Oprava z znamu',;
		'F3:	DO vyu_cedi WITH .T.	## F3-Pýid nก cesty',;
		'F4:	DO vyu_pedi WITH .T.	## F4-Pýid nก pobytu',;
		'ALT+0#:DO vyu_delk				## ALT+0-V์maz kilometr… ',;
		'ALT+S#:DO vyu_setd				## ALT+S-Nastaveni dopravy dle Globalu ',;
		'ALT+D:	DO vyu_del				## ALT+D-Smaz nก z znamu'
	GO TOP
	IF &t_gl..datod!=&t_cp..dat1
		REPL &t_gl..datod WITH &t_cp..dat1
		flgzmeny=.T.
		ENDIF
RETURN

**	Vymaz nก Km v cel sluงebnก cestุ
PROCEDURE vyu_delk
	IF ask(-1,-1,"N","Opravdu vymazat stavy Km u cel sluงebnก cesty")
		REPL ALL kms WITH 0, kmo WITH 0, kmd WITH 0, ;
			m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		refscr=.T.
		flgzmeny=.T.
		GO TOP
		WAIT WIND NOWA "Stavy Km u cel sluงebnก cesty byly vymaz ny."
		ENDIF
RETURN
**	Nastavenก dopravy dle Glob lnกch parametr…
PROCEDURE VYU_SETD
	IF ask(-1,-1,"N","Opravdu nastavit typ dopravy dle Glob lnกch parametr…")
		REPL ALL &t_cp..vla WITH &t_gl..vla, ;
			&t_cp..pri WITH (&t_gl..pri AND &t_gl..vla), ;
			&t_cp..dpp WITH iif(&t_cp..vla,space(12),"Spolujezdec"), ;
			m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		refscr=.T.
		flgzmeny=.T.
		ENDIF
RETURN

PROCEDURE vyu_edit
	IF typ="C"
		DO vyu_cedi WITH .F.
	  ELSE
		DO vyu_pedi WITH .F.
		ENDIF
RETURN
**	Vyฃtov nก cesty
PROCEDURE vyu_cedi
PARA app
	DO cesta.spr
	refscr=.T.
RETURN

**	Vyฃtov nก pobytu
PROCEDURE vyu_pedi
PARA app
	DO pobyt.spr
	refscr=.T.
RETURN

PROCEDURE vyu_del 
	REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DELE
	refscr=.T.
	flgzmeny=.T.
RETURN
**	Zad v nก vedlej็กch v์daj…
PROCEDURE vydaje  

	SELE (m.t_vv)
	SET ORDER TO dat
	DO list_c WITH -1,-1,15,0, ;
		[dtoc(dat)+'ณ '+txt+' ณ'+str(cas,12,2)+' ณ '+mena+' ณ '+ ;
			iif(lim,'L','N')+' '], ;
		" Vedlej็ก v์daje ", ;
		"// Datum    ณ  Popis v์daje        ณ    ฌ stka   ณ ISO ณL/N", ;
		"\F1-Help   F4/Enter-Oprava   INS-Pýid nก   F7-Týกdit podle ...", ;
		"{CIS \<ฌกsla, DAT \<Datumu}", ;
		'CR#:		DO vyd_edi WITH .F.		## ENTER-Oprava v์deje', ;
		'INS:		DO vyd_edi WITH .T.		## INS-Nov์ v์dej', ;
		'ALT+D#:	DO vyd_del				## ALT+D-Smaz nก v์deje'

RETURN
PROCEDURE vyd_edi 
PARA app
	DO vydaje.spr
RETURN
PROCEDURE vyd_del 
	REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DELE
	flgzmeny=.T.
RETURN
	**	Tisk vyฃtov nก
PROCEDURE tisk_vyu

	SET PRINT OFF
	SET TALK OFF
	SET CONS OFF

* Vynulov nก soutov์ch  stek u jednotliv์ch mุn
	SELE (t_me)
	REPL ALL celkem WITH 0

* Nastavenก n hrad k datu vyฃtov nก
	SELE cnahrady
	SET ORDER TO datx
	SET NEAR ON
	=seek(dtos(&t_gl..dat))
	SET NEAR OFF

* Tisk hlaviky
	PRIV sp1, sp2, l
	sp1=space(m.margin/1.6)
	sp2=space(m.margin)

	tskf=m.ggf+"vyuct.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

	SELE (m.t_gl)
	r=lookup(clidi.rodc,&t_gl..kdo,clidi.m_add,'m_add')
	l=4
	?sp1+m.nlq+" Vyฃtov nก pracovnก cesty . "+alltrim(cis)+" ze dne "+dtoc(dat)
	?sp1+repl("~",57)
	?sp1+m.draft+"Vyฃtov nก pro "+;
		iif(vztah,"zamุstnance: ","podnikatele: ")+;
		m.nlq+alltrim(jme)+;
		m.draft+iif(vztah AND !empty(r),"   Rฌ: "+r,"")
	?

	IF vla
		SELE cvozy
		SET ORDER TO m_add
		=seek(&t_gl..vuz)
		SELE (m.t_gl)
		?sp1+"Pouงit vozidlo: "+left(cvozy.popisv,30)+"   SPZ: "+cvozy.spz
		?sp1+"Status vozidla:  "+iif(&t_gl..slu,"Sluงebnก","Soukrom")
		?sp1+"Zp…sob n hrad:   "+iif(&t_gl..pau,"Pau็ lnก","Doloงen")
		?
		l=l+4
		ENDIF

	IF !empty(popis)
		?sp1+"Popis PC: "+popis
		?
		l=l+2
		ENDIF

	SET CENT OFF

* Tisk ฌasovho pr…bุhu cesty
	PRIV dd, mm
	dd=tvyu1()
* Datum za tku cesty

********************************
** V์poet stravnho a kapesnho

	SELE cstravne
	SET ORDER TO stat

	SELE 0
	CREAT CURS cc (stat C(20), dat D, hod N(5,2), str N(12,2), kap N(12,2),;
			 mena C(3))
	INDEX ON stat+dtos(dat) TAG std
	INDEX ON dtos(dat)+cs(stat) TAG dat
	SET ORDER TO std

	SELE (m.t_cp)
	SET RELA TO cs(stat) INTO cstravne

	SCAN ALL
		FOR i=0 TO dat2-dat1
			DO CASE
			  CASE i=0.and.dat2-dat1=0
				a=minusdat(dat1,hod1,min1,dat1,hod2,min2)
			  CASE i=0.and.dat2-dat1>0
				a=minusdat(dat1,hod1,min1,dat1+1,'0','0')
			  CASE i>0.and.i=dat2-dat1
				a=minusdat(dat1+i,'0','0',dat1+i,hod2,min2)
			  OTHE
				a=24*60
				ENDCASE
			mm=mena
			SELE cc
			IF seek(&t_cp..stat+dtos(&t_cp..dat1+i))
				REPL hod WITH hod+a/60
			  ELSE
				INSERT INTO cc (stat, dat, hod, mena) ;
					VALUES (&t_cp..stat, &t_cp..dat1+i,a/60,m.mm)
				ENDIF
			SELE (m.t_cp)
			ENDFOR
		ENDSCAN

	SET RELA TO

	PRIV kapesne
	kapesne=min(cnahrady.kapes, &t_gl..kap)

* Pýepoet hodin na faktick stravn
	SELE cstravne
	SET ORDER TO stravnex
	SET NEAR ON
	SELE cc
	SET ORDER TO dat
	GO TOP

*brow
	IF dtos(dd)>"20000229"
********************************************************
* 			Po novele ZOCN z konem 30/2000

*SET DEBU ON
*SET STEP ON

		PRIV r, r2, h, d, e
		STORE 0 TO h, m, r, r2
		d=dat
		DO WHILE .T.
			e=eof()
			IF e OR dat!=d
				r2=recno()
				=gorec(r)
				=seek(cs(stat)+dtos(&t_gl..dat), 'cstravne')
*wait wind stat+"  "+strn(h)+"  kap="+strn(m.kapesne)
			  	IF h<12
				  	REPL str WITH cstravne.stravne/24*h
				  	REPL kap WITH cstravne.stravne/24*h*(m.kapesne/100)
				  ELSE
				  	REPL str WITH cstravne.stravne
				  	REPL kap WITH cstravne.stravne*(m.kapesne/100)
				  	ENDIF
				IF !e
					=gorec(r2)
				  ELSE
					EXIT
					ENDIF
				STORE 0 TO h, m, r, r2
				d=dat
				ENDIF

			IF eof()
				EXIT
				ENDIF

			IF stat!="Tuzemsko"+space(12)
				h=h+hod
				IF m<hod
					r=recno()
					m=hod
					ENDIF

			  ELSE
* Pro tuzemsko
				IF &t_gl..vztah
					DO CASE 
					  CASE hod<5
						REPL str WITH 0
					  CASE hod<12
					  	REPL str WITH cnahrady.s5_12
					  CASE hod<18
					  	REPL str WITH cnahrady.s12_18
					  OTHE
					  	REPL str WITH cnahrady.snad18
						ENDCASE
				  ELSE
					DO CASE 
					  CASE hod>=18
					  	REPL str WITH cnahrady.snad18
					  CASE hod>=12
					  	REPL str WITH cnahrady.s12_18
						ENDCASE
					ENDIF
	
				IF hod<5
					DO vyuctstr.spr
					ENDIF
	
				ENDIF
			SKIP 1
			ENDDO

	  ELSE
********************************************************
* Pýed novelou platnou pro cesty zapoat pýed 29.2.2000

		SCAN ALL
			IF stat!="Tuzemsko"+space(12)
				=seek(cs(stat)+dtos(&t_gl..dat), 'cstravne')
			  	IF hod<12
				  	REPL str WITH cstravne.stravne/24*hod
				  	REPL kap WITH cstravne.stravne/24*hod*(m.kapesne/100)
				  ELSE
				  	REPL str WITH cstravne.stravne
				  	REPL kap WITH cstravne.stravne*(m.kapesne/100)
				  	ENDIF
			  ELSE
				IF &t_gl..vztah
					DO CASE 
					  CASE hod<5
						REPL str WITH 0
					  CASE hod<12
					  	REPL str WITH cnahrady.s5_12
					  CASE hod<18
					  	REPL str WITH cnahrady.s12_18
					  OTHE
					  	REPL str WITH cnahrady.snad18
						ENDCASE
				  ELSE
					DO CASE 
					  CASE hod>=18
					  	REPL str WITH cnahrady.snad18
					  CASE hod>=12
					  	REPL str WITH cnahrady.s12_18
						ENDCASE
					ENDIF
	
				IF hod<5
					DO vyuctstr.spr
					ENDIF
	
				ENDIF
			ENDSCAN
*****************************  Konec rozli็enก	
		ENDIF

	SET NEAR OFF

*SET STEP OFF
*SET DEBU OFF
************************************
** V์poet kilometr… a jejich n hrad
	SELE (m.t_cp)
	SET RELA TO mena INTO (m.t_me)
	SELE cvozy
	SET ORDER TO m_add
	IF &t_gl..vla AND !seek(&t_gl..vuz)
		WAIT WIND "Nenalezen v…z v seznamu voz…."
		ENDIF

	CREAT CURS km (stat C(20), km N(6), cena N(9,2), cas N(12,2), mena C(3))
	INDEX ON cs(stat) TAG stat
	SET ORDER TO stat

	SELE (m.t_me)
	SET ORDER TO mena

	SELE (m.t_cp)
	SET RELA TO mena INTO (m.t_me)

	IF year(dat)<2007
		a=350
	  ELSE
		a=0
	ENDIF

	SCAN ALL
		x=0
		b=kmd-kmo-kms
		IF alltrim(stat)="Tuzemsko" OR a=0
			x=b
		  ELSE
	  		a=a-b
	  		IF a<0
	  			x=abs(a)
				a=0
				ENDIF
			ENDIF
		IF x!=0
			IF seek(cs(&t_cp..stat),'km')
				REPL km.km WITH km.km+x
			  ELSE
				INSERT INTO km ;
					VALUES (&t_cp..stat, x, &t_me..phm, 0, &t_cp..mena)
				ENDIF
			ENDIF
		ENDSCAN
	SET RELA TO

	SELE km
	IF seek(cs("Tuzemsko"+space(12)))
		REPL km WITH km+350-a
	  ELSE
*		WAIT WIND "Nenalezl jsem tuzemsko."
		ENDIF


* Tisk jกzdnho
	DO tvyu2


* Tisk stravnho a kapesnho
	DO tvyu3


* Tisk n klad… za ubytov nก
	DO tvyu4


* Tisk vedlej็กch v์daj…
	DO tvyu5


* Tisk vedlej็กch v์daj… - nadlimitnกch
	DO tvyu6


* Tisk vyฃtov nก z loh
	DO tvyu7

	SET CENT ON
	SET ALTE TO
	DO tisk
RETURN
**	Tisk ฌasovho pr…bุhu cesty
PROCEDURE tvyu1
	?sp1+m.nlq+"I."
	?sp1+" ฌasov์ pr…bุh sluงebnก cesty"
	?sp1+"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"+m.draft+m.cnon
	?sp2+"ษอออออออหอออออออออออออออออหอออออออออออออออออหอออออออออออออออออหอออออออออออออออออหออออออออออออหออออออออหอออออออออออออป"
	?sp2+"บ  Typ  บ Po tek         บ Mกsto           บ Konec           บ Mกsto           บ St t       บ    Km  บ   Doprava   บ"
	?sp2+"ฬอออออออฮอออออออออออออออออฮอออออออออออออออออฮอออออออออออออออออฮอออออออออออออออออฮออออออออออออฮออออออออฮอออออออออออออน"

	SELE (t_cp)
	SET ORDER TO dat
    SCAN ALL
		?sp2+"บ "+iif(typ="C","Cesta","Pobyt")+' ณ '+dtoc(dat1)+'  '+hod1+':'+min1+' ณ '+;
			mis1+' บ '+dtoc(dat2)+'  '+hod2+':'+min2+' ณ '+ ;
			mis2+' ณ '+left(stat,10)+' ณ '+;
			str(kmd-kmo-kms,6)+' ณ '+iif(vla,"Vlastnก     ",left(dpp,12))+'บ'
		l=l+1
		ENDSCAN
	?sp2+"ศอออออออสอออออออออออออออออสอออออออออออออออออสอออออออออออออออออสอออออออออออออออออสออออออออออออสออออออออสอออออออออออออผ"

	?m.cnoff
	?

	l=l+9

* Vracก datum za tku pracovnก cesty
	GO TOP
RETURN dat1
**	Tabulka jกzdnho
PROCEDURE tvyu2   

	SELE km
	x=recc()
	GO TOP
	IF x=0 OR eof()
		USE
		RETURN
		ENDIF

	l=l+9
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=9
		ENDIF

	?sp1+m.nlq+"II."
	?sp1+"A. Jกzdn"+iif(&t_gl..pau,"",m.cnon+" - slouงก pouze pro v์poet max. doloงen์ch v์daj… na PH"+m.cnoff)
	?sp1+"~~~~~~~~~~"+m.draft+m.cnon
	?sp2+"ษออออออออออออหออออออออออหอออออออออออออหอออออออออออหออออออออออออออออออหออออออออออหอออออออออออออป"
	?sp2+"บ  St t      บ Ujet Km บ P.cena PH/l บ P.l/100Km บ      N hrada Mุnaบ    Kurz  บ Celkem v K บ"	
	?sp2+"ฬออออออออออออฮออออออออออฮอออออออออออออฮอออออออออออฮออออออออออออออออออฮออออออออออฮอออออออออออออน"

	SET RELA TO mena INTO (m.t_me)

	k=0
	c=0
	SCAN ALL
		IF km>0
			x=km/100*&t_me..phm*cvozy.lna100+ ;
				iif(!&t_gl..slu AND &t_gl..vla, ;
					km*cnahrady.auto/round(&t_me..kurz*(1/&t_me..koef),4),0)
			a=x*round(&t_me..kurz*(1/&t_me..koef),4)
			?sp2+'บ '+left(stat,10)+' ณ '+str(km,7)+'  บ '+str(&t_me..phm,9,2)+;
				'   ณ    '+str(cvozy.lna100,5,2)+'  บ '+str(x,12,2)+' '+ ;
				mena+' ณ'+str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+;
				str(a,11,2)+' บ'
			c=c+a
			k=k+km
			IF &t_gl..pau
				REPL &t_me..celkem WITH &t_me..celkem+x
				ENDIF
			l=l+1
			ENDIF
		ENDSCAN
	?sp2+"ศออออออออออออฮออออออออออฮอออออออออออออสอออออออออออสออออออออออออออออออสออออออออออฮอออออออออออออน"
	?sp2+space(13)+'บ '+str(k,7)+'  บ'+space(55)+'บ '+str(c,11,2)+' บ'
	?sp2+space(13)+'ศออออออออออผ'+space(55)+'ศอออออออออออออผ'+m.cnoff

	USE
RETURN
**	Tisk stravnho a kapesnho
PROCEDURE tvyu3

	SELE cc
	x=recc()
	GO TOP
	IF x=0 OR eof()
		USE
		RETURN
		ENDIF

	SET ORDER TO dat

	l=l+7
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=7
		ENDIF

	?m.pica+m.nlq
	?sp1+"B. Stravn a kapesn"
	?sp1+"~~~~~~~~~~~~~~~~~~~~~"+m.draft+m.cnon

	?sp2+"ษออออออออออหออออออออออหออออออออออออหออออออออออออออหออออออออออออออหออออออป"
	?sp2+"บ  Datum   บ  Hodiny  บ St t       บ     Stravn  บ  Kapesn "+str(m.kapesne,2)+"% บ Mุna บ"
	?sp2+"ฬออออออออออฮออออออออออฮออออออออออออฮออออออออออออออฮออออออออออออออฮออออออน"
	SCAN ALL
		?sp2+'บ '+dtoc(dat)+' ณ '+str(hod,8,2)+' ณ '+left(stat,10)+;
			' บ '+str(str,12,2)+' ณ '+str(kap,12,2)+' ณ  '+mena+' บ '
		l=l+1
		ENDSCAN
	?sp2+"ศออออออออออสออออออออออสออออออออออออสออออออออออออออสออออออออออออออสออออออผ"


	SELECT stat, sum(str) AS str, sum(kap) AS kap, mena ;
		FROM cc ;
		INTO CURSOR ss ;
		GROUP BY mena ;
		ORDER BY mena

	SELE (t_me)
	SET ORDER TO mena

	SELE ss

	x=recc()
	GO TOP
	IF x=0 OR eof()
		USE
		RETURN
		ENDIF

	l=l+8
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=8
		ENDIF

	?m.cnoff
	?sp1+" Sumarizace stravnho a kapesnho v jednotliv์ch mุn ch"+m.cnon
	?sp2+"ษอออออออหออออออออออออออหออออออออออออออหออออออออออออออออออออหอออออออออออหอออออออออออออป"
	?sp2+"บ Mุna  บ     Stravn  บ     Kapesn  บ      Celkem   Mุna บ     Kurz  บ Celkem v K บ"
	?sp2+"ฬอออออออฮออออออออออออออฮออออออออออออออฮออออออออออออออออออออฮอออออออออออฮอออออออออออออน"

	SET RELA TO mena INTO (m.t_me)

	PRIV c
	c=0
	SCAN ALL
		IF str+kap>0
    		m=left(mena,1)+' '+substr(mena,2,1)+" "+substr(mena,3,1)
			a=(str+kap)*round(&t_me..kurz*(1/&t_me..koef),4)
			?sp2+'บ '+m+' บ '+str(str,12,2)+' ณ '+;
				str(kap,12,2)+' บ '+str(str+kap,12,2)+'   '+mena+' ณ '+;
				str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+;
				str(a,11,2)+' บ'
			IF &t_me..kurz*&t_me..koef=0
				WAIT WIND "U mุny "+mena+" nenก zad n kurz nebo koeficient."
				ENDIF
			REPL &t_me..celkem WITH &t_me..celkem+str+kap
			c=c+a
			l=l+1
			ENDIF
		ENDSCAN
	?sp2+"ศอออออออสออออออออออออออสออออออออออออออสออออออออออออออออออออสอออออออออออฮอออออออออออออน"
	?sp2+space(71)+'บ '+str(c,11,2)+' บ'
	?sp2+space(71)+'ศอออออออออออออผ'+m.cnoff

	USE

	SELE cc
	USE

RETURN

**	Tabulka ubytov nก
PROCEDURE tvyu4   

	SELE (t_cp)
	COUNT ALL FOR typ='P' AND uby!=0 TO x
	IF x=0
		RETURN
		ENDIF

	l=l+9
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=9
		ENDIF

	?m.pica+m.nlq
	?sp1+"C. V์daje na ubytov nก"
	?sp1+"~~~~~~~~~~~~~~~~~~~~~~~"+m.draft+m.cnon
	?sp2+"ษอออออออออออออออออออออหออออออออออออหออออออออออออออออออหอออออออออออหอออออออออออออป"
	?sp2+"บ      ฌas pobytu     บ St t       บ      V์daje Mุna บ     Kurz  บ Celkem v K บ"
	?sp2+"ฬอออออออออออออออออออออฮออออออออออออฮออออออออออออออออออฮอออออออออออฮอออออออออออออน"

	SELE (t_me)
	SET ORDER TO mena

	SELE (t_cp)
	SET RELA TO mena INTO (t_me)

	SET FILTER TO typ='P'
	c=0
	SCAN ALL
		a=uby*round(&t_me..kurz*(1/&t_me..koef),4)
		?sp2+'บ '+dtoc(dat1)+' - '+dtoc(dat2)+' ณ '+left(stat,10)+;
			' บ '+str(uby,12,2)+' '+mena+' ณ '+;
			str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+;
			str(a,11,2)+' บ'
		l=l+1
		c=c+a
		REPL &t_me..celkem WITH &t_me..celkem+uby
		ENDSCAN
	?sp2+"ศอออออออออออออออออออออสออออออออออออสออออออออออออออออออสอออออออออออฮอออออออออออออน"
	?sp2+space(66)+'บ '+str(c,11,2)+' บ'
	?sp2+space(66)+'ศอออออออออออออผ'+m.cnoff

	SET FILTER TO
	SET RELA TO
RETURN
**	Tisk vedlej็กch v์daj…
PROCEDURE tvyu5

	SELE (m.t_vv)
	COUNT ALL FOR lim TO x
	IF x=0
		RETURN
		ENDIF
	
	SELE (m.t_me)
	SET ORDER TO mena
	SELE (m.t_vv)
	SET ORDER TO dat
	SET FILTER TO lim
	SET RELA TO mena INTO (m.t_me)

	l=l+10
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=10
		ENDIF

	?m.pica+m.nlq
	?sp1+"D. Jin vedlej็ก v์daje"
	?sp1+"~~~~~~~~~~~~~~~~~~~~~~~~"+m.draft+m.cnon
	?sp2+"ษออออออออออหออออออออออออออออออออออหออออออออออออออออออหอออออออออออหอออออออออออออป"
	?sp2+"บ   Datum  บ  Popis v์deje        บ      ฌ stka Mุna บ    Kurz   บ Celkem v K บ"
	?sp2+"ฬออออออออออฮออออออออออออออออออออออฮออออออออออออออออออฮอออออออออออฮอออออออออออออน"

	c=0
	SCAN ALL
		a=cas*&t_me..kurz*(1/&t_me..koef)
		?sp2+'บ '+dtoc(dat)+' ณ '+txt+' ณ '+str(cas,12,2)+' '+mena+' ณ '+;
			str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+;
			str(a,11,2)+' บ'
		c=c+a
		l=l+1
		ENDSCAN
	?sp2+"ศออออออออออสออออออออออออออออออออออสออออออออออออออออออสอออออออออออฮอออออออออออออน"
	?sp2+space(65)+'บ '+str(c,11,2)+' บ'
	?sp2+space(65)+'ศอออออออออออออผ'+m.cnoff
	?

	SET RELA TO
	SET FILT TO

	SELECT sum(cas) AS cas, mena ;
		FROM (m.t_vv) vv ;
		INTO CURSOR ss ;
		WHERE lim ;
		GROUP BY mena ;
		ORDER BY mena

	SET RELA TO mena INTO (m.t_me)

	x=recc()
	l=l+8
	GO TOP
	IF x+l>m.len_pg OR eof()
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=8
		ENDIF

	?sp1+" Sumarizace jin์ch v์daj… v jednotliv์ch mุn ch"+m.cnon
	?sp2+"ษออออออออออออออออออหอออออออออออหอออออออออออออป"
	?sp2+"บ      Celkem mุna บ     Kurz  บ Celkem v K บ"
	?sp2+"ฬออออออออออออออออออฮอออออออออออฮอออออออออออออน"
	c=0
	SCAN ALL
		a=cas*round(&t_me..kurz*(1/&t_me..koef),4)
		if a>0
			?sp2+'บ '+str(cas,12,2)+' '+;
				mena+' ณ '+str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+str(a,11,2)+' บ'
			l=l+1
			REPL &t_me..celkem WITH &t_me..celkem+cas
			c=c+a
			ENDIF
		ENDSCAN
	?sp2+"ศออออออออออออออออออสอออออออออออฮอออออออออออออน"
	?sp2+space(31)+'บ '+str(c,11,2)+' บ'
	?sp2+space(31)+'ศอออออออออออออผ'+m.cnoff
	?

	USE

RETURN

**	Tabulka jin์ch nadlimitnกch v์daj…
PROCEDURE tvyu6   

	SELE (m.t_vv)
	COUNT ALL FOR !lim TO x
	IF x=0
		RETURN
		ENDIF
	
	SELE (t_me)
	SET ORDER TO mena
	SELE (t_vv)
	SET ORDER TO dat
	SET FILTER TO !lim
	SET RELA TO mena INTO &t_me

	l=l+10
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=10
		ENDIF

	?m.pica+m.nlq
	?sp1+"Jin nadlimitnก vedlej็ก v์daje"
	?sp1+"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"+m.draft+m.cnon
	?sp2+"ษออออออออออหออออออออออออออออออออออหออออออออออออออออออหอออออออออออหอออออออออออออป"
	?sp2+"บ   Datum  บ  Popis v์deje        บ      ฌ stka Mุna บ    Kurz   บ Celkem v K บ"
	?sp2+"ฬออออออออออฮออออออออออออออออออออออฮออออออออออออออออออฮอออออออออออฮอออออออออออออน"

	c=0
	SCAN ALL
		a=cas*&t_me..kurz*(1/&t_me..koef)
		?sp2+'บ '+dtoc(dat)+' ณ '+txt+' ณ '+str(cas,12,2)+' '+mena+' ณ '+;
			str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+;
			str(a,11,2)+' บ'
		c=c+a
		l=l+1
		ENDSCAN
	?sp2+"ศออออออออออสออออออออออออออออออออออสออออออออออออออออออสอออออออออออฮอออออออออออออน"
	?sp2+space(65)+'บ '+str(c,11,2)+' บ'
	?sp2+space(65)+'ศอออออออออออออผ'+m.cnoff
	?

	SET RELA TO
	SET FILT TO

	SELECT sum(cas) AS cas, mena ;
		FROM (m.t_vv) vv ;
		INTO CURSOR ss ;
		WHERE !lim ;
		GROUP BY mena ;
		ORDER BY mena

	SET RELA TO mena INTO (m.t_me)

	l=l+8
	x=recc()
	GO TOP
	IF x+l>m.len_pg OR eof()
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=8
		ENDIF

	?sp1+" Sumarizace jin์ch nadlimitnกch v์daj… v jednotliv์ch mุn ch"+m.cnon
	?sp2+"ษออออออออออออออออออหอออออออออออหอออออออออออออป"
	?sp2+"บ      Celkem mุna บ     Kurz  บ Celkem v K บ"
	?sp2+"ฬออออออออออออออออออฮอออออออออออฮอออออออออออออน"
	c=0
	SCAN ALL
		a=cas*round(&t_me..kurz*(1/&t_me..koef),4)
		if a>0
			?sp2+'บ '+str(cas,12,2)+' '+;
				mena+' ณ '+str(&t_me..kurz*(1/&t_me..koef),9,4)+' บ '+str(a,11,2)+' บ'
			l=l+1
			REPL &t_me..celkem WITH &t_me..celkem+cas
			c=c+a
			ENDIF
		ENDSCAN
	?sp2+"ศออออออออออออออออออสอออออออออออฮอออออออออออออน"
	?sp2+space(31)+'บ '+str(c,11,2)+' บ'
	?sp2+space(31)+'ศอออออออออออออผ'+m.cnoff
	?

	USE

RETURN
**	Tabulka celkovho vyฃtov nก z loh
PROCEDURE tvyu7   

	SELE (t_me)
	SET ORDER TO mena

	COUNT ALL FOR celkem!=0 TO x
	l=l+9
	IF x+l>m.len_pg
		??chr(12)+m.cnon+space(125)+alltrim(&t_gl..cis)+m.cnoff
		l=9
		ENDIF

	?m.pica+m.nlq
	?sp1+"Vyฃtov nก z loh"
	?sp1+"~~~~~~~~~~~~~~~~"+m.draft+m.cnon
	?sp2+"ษอออออออหออออออออออออออหออออออออออออออหออออออออออออออหออออออหอออออออออออหอออออออออออออหอออออออออออออป"
	?sp2+"บ Mุna  บ      Z loha  บ     Pouงito  บ    Z…statek  บ Mุna บ     Kurz  บ Pouงito v Kบ Z…statek K บ"
	?sp2+"ฬอออออออฮออออออออออออออฮออออออออออออออฮออออออออออออออฮออออออฮอออออออออออฮอออออออออออออฮอออออออออออออน"

	PRIV m, c, d
	STORE 0 TO c, d
	SCAN ALL
    	IF celkem>0
    		m=left(mena,1)+' '+substr(mena,2,1)+" "+substr(mena,3,1)
			?sp2+'บ '+m+' บ '+str(zaloha,12,2)+' ณ '+;
				str(celkem,12,2)+' ณ '+str(zaloha-celkem,12,2)+' ณ  '+ ;
				mena+' ณ '+str(kurz*(1/koef),9,4)+' บ '+;
				str(celkem*round(kurz*(1/koef),4),11,2)+' บ'+;
				str((zaloha-celkem)*round(kurz*(1/koef),4),12,2)+' บ '
			l=l+1
			c=c+(zaloha-celkem)*round(kurz*(1/koef),4)
			d=d+celkem*round(kurz*(1/koef),4)
			ENDIF
		ENDSCAN

	?sp2+"ศอออออออสออออออออออออออสออออออออออออออสออออออออออออออสออออออสอออออออออออฮอออออออออออออฮอออออออออออออน"
	?sp2+space(72)+'บ '+str(round(d,0),11,2)+' บ '+str(round(c,0),11,2)+' บ'
	?sp2+space(72)+'ศอออออออออออออสอออออออออออออผ'+m.cnoff+chr(12)

RETURN
**	Funkce na odeet datum…
PROCEDURE minusdat
PARA d1,h1,m1,d2,h2,m2
PRIV a
	a=d2-d1
RETURN a*24*60-val(h1)*60-val(m1)+val(h2)*60+val(m2)
PROCEDURE glob_ud 
	SELE (m.t_gl)
	DO global.spr
RETURN
**	ฌึSELNึKY
PROCEDURE hlm_cis
	DEFINE POPUP cis FROM 8,40 RELATIVE SHADOW ;
		TITLE " ฌกselnกky " COLOR SCHEME 4
	DEFINE BAR 10 OF cis PROMPT " \<Dopravnก prostýedky " ;
		MESS "Zad nก ฃdaj… o pouงกvan์ch dopravnกch prostýedcกch."
	DEFINE BAR 15 OF cis PROMPT " ้astnกci sl\<uงebnกch cest" ;
		MESS "Zad nก ฃdaj… o ฃastnกcกch sluงebnกch cest."
	DEFINE BAR 20 OF cis PROMPT "\-"
	DEFINE BAR 50 OF cis PROMPT " \<Stravn " ;
		MESS "Seznam st t… a historie n hrad stravnho v tุchto st tech."
	DEFINE BAR 55 OF cis PROMPT " \<N hrady " ;
		MESS "Historie n hrad za pouงitก dopravnกho prostýedku."
	
	ON SELE POPU cis DO cis
	ACTI POPUP cis
	RELE POPU cis
RETURN

PROC cis
	DO CASE
		CASE bar()=10	&&	Dopravnก prostýedky
			DO vozy WITH ""
		CASE bar()=15	&&	้astnกci sluงebnกch cest
			DO lidi WITH ""
		CASE bar()=50	&&	Stravn
			DO stravne WITH "xxx"
		CASE bar()=55	&&	N hrady
			DO nahrady
		ENDCASE
RETURN
**	LIST_C pro nahrady
PROCEDURE nahrady
	PRIV sel, ord
	sel=SELE()
	SELE cnahrady
	ord=order()
	SET ORDER TO dat
	DO list_c WITH -1,-1,15,0,;
		" Evidence n hrad za vozidla a stravnho v tuzemsku ",;
		[:dtoc(dat)+' ณ '+str(auto,6,2)+' ณ '+str(moto,6,2)+' ณ '+ ;
			str(bus,6,2)+' ณ '+str(nakl,6,2)+' ณ '+str(prives,3)],;
		"//  Datum    ณ  Auto  ณ  Moto  ณ   Bus  ณ  N kl. ณ Pýกvุs",;
		[:dtoc(dat)+' ณ '+str(sdo5,6,2)+' ณ '+str(s5_12,6,2)+' ณ '+ ;
			str(s12_18,6,2)+' ณ '+str(snad18,6,2)+' ณ '+str(kapes,3)],;
		"//  Datum    ณ <5 hod.ณ  5-12  ณ  12-18 ณ 18-24  ณ Kapes.",;
		"\F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
		"{DAT \<Datum, DATX \<Datumx}",;
		'CR:',;
		'F4#:		DO nahr_pri WITH .F.		## F4-Oprava poloงky',;
		'INS:		DO nahr_pri WITH .T.		## INS-Pýid nก poloงky',;
		'ALT+D#:	DO nahr_del					## ALT+D-Smaz nก poloงky'
	SET ORDER TO &ord
	SELE (m.sel)
RETURN

PROCEDURE nahr_pri	&&	Pýid nก n hrad
PARA app
	DO nahrady.spr
RETURN

PROCEDURE nahr_del
	IF ask(-1,-1,'A',"Smazat poloงku")
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
RETURN
**	LIST_C pro stravne
PROCEDURE stravne
PARA p

	PRIV sel, ord
	sel=SELE()
	SELE cstravne
	ord=order()

	IF p="xxx"
		SET ORDER TO stravne
	  ELSE
		SET ORDER TO stat
		ENDIF
	IF !seek(cs(p))
		GO TOP
		ENDIF

	DO list_c WITH -1,-1,15,0,;
	  iif(p="xxx",;
	  	" Evidence stravnho v r…zn์ch st tech ",;
	  	" V์bุr st tu "), ;
		[:stat+' ณ '+mena+' ณ '+dtoc(dat)+' ณ '+str(stravne,10,2)],;
		"//  Stat                    ณ ISO ณ   Datum    ณ Stravne ",;
		"\F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
		"{STRAVNE \<Stravn, STRAVNEX \<Stravnx, STAT S\<t ty}",;
		'F4#:		DO stra_pri WITH .F.		## F4-Oprava poloงky',;
		'INS:		DO stra_pri WITH .T.		## INS-Pýid nก poloงky',;
		'ALT+D#:	DO stra_del					## ALT+D-Smaz nก poloงky'

	SET ORDER TO &ord
	IF p!="xxx" AND lastkey()=13
		SELE (t_me)
		SET ORDER TO mena
		IF !seek(cstravne.mena)
			INSERT INTO (t_me) (mena, kurz, koef, ;
				arch, m_user, m_date, m_time, m_add) ;
			  VALUES (cstravne.mena, iif(rtrim(cstravne.stat)="Tuzemsko",1,0),;
				1, &t_gl..arch, m.gl_usernam, date(), m_time, dtos(date())+sys(3))
			flgzmena=.T.
			ENDIF
		ENDIF

	SELE (m.sel)

RETURN cstravne.stat
*	Pýid nก stravnho
PROCEDURE stra_pri
PARA app
	DO stravne.spr
RETURN

PROCEDURE stra_del
	IF ask(-1,-1,'A',"Smazat poloงku")
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
RETURN
**	LIST_C pro meny
PROCEDURE meny    
PARA _cis
	PRIV sel, ord, rc
	sel=SELE()

	SELE (t_me)
	ord=order()

	SET ORDER TO mena
	IF !seek(m._cis)
		GO TOP
		ENDIF

*	DO list_c WITH -1,-1,15,0,;
*		" Evidence n hrad za vozidla a stravnho v tuzemsku ",;
*		[:dtoc(dat)+' ณ '+str(auto,6,2)+' ณ '+str(moto,6,2)+' ณ '+ ;
*			str(bus,6,2)+' ณ '+str(nakl,6,2)+' ณ '+str(prives,3)],;
*		"//  Datum    ณ  Auto  ณ  Moto  ณ   Bus  ณ  N kl. ณ Pýกvุs",;
*		[:dtoc(dat)+' ณ '+str(sdo5,6,2)+' ณ '+str(s5_12,6,2)+' ณ '+ ;
*			str(s12_18,6,2)+' ณ '+str(snad18,6,2)+' ณ '+str(kapes,3)],;
*		"//  Datum    ณ <5 hod.ณ  5-12  ณ  12-18 ณ 18-24  ณ Kapes.",;
*		"\F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
*		"{DAT \<Datum, DATX \<Datumx}",;

*			str(zmtyp,3)+str(zmmd,3)+str(zmmda,4)+str(zmdal,3)+str(zmdaa,4)+ ;
			str(kctyp,3)+str(kcmd,3)+str(kcmda,4)+str(kcdal,3)+str(kcdaa,4)+ ;
			len(extxt,8)


	DO list_c WITH -1,-1,15,0,;
		" Mุny a jejich kurzy k "+dtoc(&t_gl..dat),;
		[:' '+mena+' ณ '+str(kurz,7,3)+' ณ '+str(koef,5)+' ณ '+ ;
			str(zaloha,12,2)+' ณ '+str(phm,9,2)+' ณ '+m_user],;
		"//Mุna ณ   Kurz  ณ Koef. ณ     Z loha   ณ  Cena PHM ณ  Zadal",;
		[:' '+mena+' ณ '+str(zmtyp,3)+str(zmmd,5)+str(zmmda,5)+str(zmdal,4)+str(zmdaa,5)+" ณ"+ ;
			str(kctyp,4)+str(kcmd,5)+str(kcmda,5)+str(kcdal,4)+str(kcdaa,5)+" ณ "+ ;
			left(extxt,8)],;
		"//Mุna ณ Typ   M  d ti   Dal Kฌ ณ Typ   M  d ti   Dal ZM ณ Txt",;
		"\ F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
		"{mena \<Mุna}",;
	  	'CR#:		EXIT					## Enter-V์bุr mุny', ;
		'F4#:		DO meny_pri WITH .F.	## F4-Oprava ฃdaje',;
		'INS:		DO meny_pri WITH .T.	## INS-Pýid nก mุny',;
		'ALT+D#:	DO meny_del				## ALT+D-Smaz nก mุny'

	SET ORDER TO &ord
	SELE (m.sel)
RETURN &t_me..mena
PROCEDURE meny_pri
PARA app
	DO mena.spr
RETURN
PROCEDURE meny_del
	PRIV rc, v, f

	SELE (m.t_vv)
	
	LOCA FOR mena==&t_me..mena
	
	SELE (m.t_me)
	
	IF !eof((m.t_vv))
		WAIT WIND "Mุna je pouงita ve vedlej็กch v์dajกch. Nelze ji smazat."
	  ELSE
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		flgzmeny=.T.
		refscr=.T.
		ENDIF
RETURN
**	LIST_C pro lidi
PROCEDURE lidi
PARA _cis
	PRIV sel, ord
	sel=sele()
	SELE clidi
	ord=order()

	SET ORDER TO m_add
	IF !seek(m._cis)
		GO TOP
		ENDIF

	SET ORDER TO jme

	DO list_c WITH -1,-1,15,0,;
		[jme+' ณ '+Pomer+' ณ '+RodC],;
		" Evidence ฃastnกk… cest ",;
		"//  Pýกjmenก a jmno        ณ Prac. pomุr ณ Rodn กslo ",;
		"\ F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
		"{JME \<Jmno}",;
	  	'CR#:		EXIT					## Enter-V์bุr vozidla', ;
		'F4#:		DO lidi_pri WITH .F.	## F4-Oprava ฃdaj… ฃastnกka',;
		'INS:		DO lidi_pri WITH .T.	## INS-Pýid nก ฃastnกka',;
		'ALT+D#:	DO lidi_del				## ALT+D-Smaz nก ฃastnกka'
	SET ORDER TO &ord
	SELE (m.sel)
RETURN clidi.m_add

PROCEDURE lidi_pri	&&	Pýid nก ฃastnกka
PARA app
	DO lidi.spr
RETURN

PROCEDURE lidi_del
	PRIV rc, v, f
	DO wpr
	v=clidi.m_add
	SELE cglobal
	rc=recno()
	LOCA FOR kdo=v
	f=found()
	=gorec(m.rc)
	SELE clidi
	WAIT CLEAR
	IF f OR (used(m.t_gl) AND &t_gl..kdo=v)
		WAIT WIND NOWA "้astnกk nalezen ve vyฃtov nก. Nelze jej smazat."
	  ELSE
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()		
		DELE
		ENDIF
RETURN
**	LIST_C pro vozy
PROCEDURE vozy
PARA _cis
	PRIV sel, ord
	sel=SELE()
	SELE cvozy
	ord=order()

	SET ORDER TO m_add
	IF !seek(m._cis)
		GO TOP
		ENDIF

	SET ORDER TO spz

	DO list_c WITH -1,-1,15,0,;
		[spz+' ณ '+str(lna100,5,2)+' ณ '+popisv+' ณ '+typ],;
		" Evidence vozidel ",;
		"//   S P Z  ณ l/100 ณ  Popis vozidla"+space(27)+"ณ Typ vozu",;
		"\ F1-Help   F4-Oprava   INS-Pýid nก   F7-Týกdit podle ...",;
		"{SPZ \<S P Z, popis \<Popis}",;
	  	'CR#:		EXIT					## Enter-V์bุr vozidla', ;
		'F4#:		DO vozy_pri WITH .F.	## F4-Oprava ฃdaj… vozidla',;
		'INS:		DO vozy_pri WITH .T.	## INS-Pýid nก vozidla',;
		'ALT+D#:	DO vozy_del				## ALT+D-Smaz nก vozidla'

	SET ORDER TO &ord
	SELE (m.sel)
RETURN cvozy.m_add

PROCEDURE vozy_pri	&&	Pýid nก vozidla
PARA app
	DO vozy.spr
RETURN

PROCEDURE vozy_del
	PRIV rc, v, f
	DO wpr
	v=cvozy.m_add
	SELE cglobal
	rc=recno()
	LOCA FOR vuz=v
	f=found()
	=gorec(m.rc)
	SELE cvozy
	WAIT CLEAR
	IF f OR (used(m.t_gl) AND &t_gl..vuz=v)
		WAIT WIND NOWA "Vozidlo nalezeno ve vyฃtov nก. Nelze ho smazat."
	  ELSE
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
RETURN
**	LIST_C pro text
PROCEDURE ltext
PARA _cis, _tp

	IF type('m._tp')!='C'
		_tp=""
		ENDIF

	PRIV sel, ord
	sel=SELE()

	SELE ctext
	ord=order()
	SET FILT TO typt=m._tp

	SET ORDER TO text
	IF !seek(m._cis)
		GO TOP
		ENDIF

	DO list_c WITH -1,-1,15,0,;
		[ctext.text],;
		" Popisn texty ",;
		"//  Text",;
		"\ F1-Help   ENTER-V์bุr   F4-Oprava   INS-Pýid nก textu   F7-Týกdit podle ...",;
		'F4#:		DO text_pri WITH .F.		## F4-Oprava textu',;
		'INS:		DO text_pri WITH .T.		## INS-Pýid nก textu',;
		'ALT+D#:	DO text_del					## ALT+D-Smaz nก textu'
	SET ORDER TO &ord
	SET FILT TO
	SELE (m.sel)

RETURN ctext.text
**	Pýid nก text…
PROCEDURE text_pri
PARA app
PRIV mx
*	IF app
*		INSERT INTO ctext (text) VALUES (denik.txt)
*		REPL m_user WITH m.gl_usernam, m_date WITH date(), ;
*			m_time WITH time(), m_add WITH dtos(date())+sys(3)
*		ENDIF
	DO text.spr
*	IF m.app AND m.esc
*		DELE
*		ENDIF
RETURN

PROCEDURE text_del
	REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DELE
RETURN
**	Zad nก firmy k ฃtov nก
PROCEDURE hlm_fir
	PRIV pf,i,tt
	DO wrt_stat WITH "CESTY"
	DO cldata
	SELE firma
	SET ORDER TO naz
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
	GO TOP
	DO list_c WITH -1,-1,18,0,;
		[naz+' ณ '+str(rok,4)+' ณ '+jme],;
		" V์bุr firmy k ฃtov nก ",;
		"// Adres ý     ณ Rok  ณ N zev firmy",;
		"\ F1-Help  ENTER-้tov nก  F7-Týกdit podle ...",;
		'{naz}',;
		'CR#:		DO fir_ote		## ENTER-Otevýenก firmy k ฃtov nก',;
		'ALT+A:		DO fir_arc		## ALT+A-Zobrazovat i archivy firem',;
		iif(m.gl_pri_pu>=60, ;
			'INS:	DO fir_ins		## INS-Nov  firma',''), ;
		iif(m.gl_pri_pu>=90, ;
			'ALT+V:	DO fir_vse		## ALT+V-Zobrazovat v็echny firmy',''), ;
		iif(m.gl_pri_pu>=60, ;
			'F5:	DO fir_copy		## F5-Kopie firmy','')
	IF m.esc
		DO cldata
		m.ggf=' '
		ENDIF
	DO refscr
	DO testflag
RETURN

**	Filtr pro zobrazenก archivnกch firem
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

**	Filtr pro zobrazenก v็ech firem
PROCEDURE fir_vse
	IF !m.ladeni
		__fltr='naz<>"_SETDBF"'
	  ELSE
		__fltr=''
		ENDIF
	refscr=.T.
RETURN

PROCEDURE fir_ote
	erruser=0
	DO wpr
	nfirma=firma.naz
	DO cldata
	ggf=m.gl_data+allt(naz)+"\"
	SELE 0
	ON ERROR erruser=error()
	USE &ggf.cglobal
	ON ERROR &_onerr
	USE
	IF erruser=108
		WAIT WIND "S touto firmou pracuje v์hradnุ jin์ uงivatel. NELZE OTEVüึT."
		ggf=" "
		RETURN
		ENDIF
	SELE firma
	SET FILT TO
	IF !_open(.F., .F.)
		WAIT WIND "Nepodaýilo se otevýกt datab ze."
		ggf=" "
		RETURN
		ENDIF
	m.toexit=.T.
	DO refscr
	ON ERROR &_onerr
RETURN

PROCEDURE fir_ins
PRIV m.nz
	INSERT INTO firma (usr) VALUES (m.gl_usernam)
	DO firmains.spr
	IF m.esc
		DELE
		RETURN
		ENDIF
	nz=alltrim(left(naz,8))
	REPL naz WITH nz
	nz=m.gl_data+nz
	IF file(nz+"\cglobal.dbf")
		WAIT WIND "Zadan์ adres ý jiง existuje na disku a obsahuje datab ze vyฃtov nก cest."
		DELE
		m.toexit=.T.
		RETURN
		ENDIF
	IF !_MKDIR(nz)
		WAIT WIND "Chyba pýi otevกr nก nov firmy."
		m.toexit=.T.
		DELE
		RETURN
		ENDIF
RETURN

*************
**	R Þ Z N 
PROCEDURE hlm_ruz
	DEFINE POPUP spr FROM 8,40 RELATIVE SHADOW ;
		TITLE " R … z n  " COLOR SCHEME 4
	DEFINE BAR 30 OF spr PROMPT " \<Glob lnก parametry" ;
		MESS "Zad nก ฃdaj… o firmุ."
	DEFINE BAR  5 OF spr PROMPT " Oprava inde\<x…" ;
		MESS "Pýeindexov nก databazก."
	DEFINE BAR 20 OF spr PROMPT "\-"
	DEFINE BAR 88 OF spr PROMPT " \<Novinky" ;
		MESS "Popis ฃprav v programu."
	
	ON SELE POPU spr DO spr
	ACTI POPUP spr
	RELE POPU spr
RETURN

PROC spr
	DO CASE
		CASE bar()=5	&&	Oprava index…
			DO closetmp

			DO cldata
			IF !_open(.T., .T.)
				WAIT WIND NOWA "Datab ze nelze indexovat nebo s nimi pracuje jin์ uงivatel."
				ENDIF
			DO cldata
			IF !_open(.F., .F.)
				WAIT WIND NOWA "Nepodaýilo se otevýกt datab ze."
				DO cldata
				ENDIF
		CASE bar()=30	&&	Glob lnก parametry
			SELE firma
			DO ..\firma.spr
		CASE bar()=88	&&	Informace o novink ch
			IF !file('..\Novinky.Tex')
				RETURN
				ENDIF
			DEFINE WIND w_new FROM 5,2 TO 20,75 DOUBLE SHADOW ;
				TITLE " ***  Novinky v programu  *** " COLOR SCHEME 13
			IF m.gl_pri_pu=99
				MODI COMM ..\Novinky.Tex WIND w_new
			  ELSE
				MODI COMM ..\Novinky.Tex NOEDIT WIND w_new
				ENDIF
			RELE WIND w_new
		ENDCASE
RETURN

**	Uzavýenก datab zก
PROCEDURE cldata
	IF file(m.ggf+"cesty.rei")
		ERASE (m.ggf+"cesty.rei")
		ENDIF
	DO cldata1
RETURN
**	Otevýenก datab zก
PROCEDURE _open
PARA exc, rei
PRIV _rt
	_rt=.F.
	IF m.rei OR (file(m.ggf+"cesty.rei") AND ;
			ask(-1,-1,'A',"Obnovit indexy po hav rii ?"))
		rei=.T.
		WAIT WIND NOWAIT "Obnovuji indexov soubory"
	  ELSE
		rei=.F.
		ENDIF
	SET TALK OFF
	SET ALTE TO &ggf.cesty.rei
	SET ALTE ON
	SET ALTE TO

**	Lok lnก datab ze
* Z kladnก ฃdaje pro jednotliv cesty
	_rt=m._rt OR !open_dbf(m.ggf, "cglobal", "op_glob", "CIS", m.exc, m.rei)

* Vlastnก vyฃtov nก
	_rt=m._rt OR !open_dbf(m.ggf, "cvyuct", "op_vyuct", "ARCH", m.exc, m.rei)

* Vedlej็ก v์daje
	_rt=m._rt OR !open_dbf(m.ggf, "cvydaje", "op_vydaj", "ARCH", m.exc, m.rei)

* Kขdy a kurzy mุn
	_rt=m._rt OR !open_dbf(m.ggf, "cmeny", "op_meny", "ARCH", m.exc, m.rei)

**	Lok lnก datab ze pro v็echny cesty
* ฌกselnกk vozidel
	_rt=m._rt OR !open_dbf(m.ggf, "cvozy", "op_vozy", "M_ADD", m.exc, m.rei)

* ฌกselnกk ฃastnกk… cest
	_rt=m._rt OR !open_dbf(m.ggf, "clidi", "op_lidi", "M_ADD", m.exc, m.rei)

* ฌกselnกk text…
	_rt=m._rt OR !open_dbf(m.ggf, "CTEXT", "op_text", "TEXT", m.exc, m.rei)

**	Glob lnก datab ze
* N hrady stravnho a z kladnก n hrady pro tuzemsko
	_rt=m._rt OR !open_dbf(m.gl_data, "cnahrady", "op_nahr", "DAT", m.exc, m.rei)

* Stravn v zahraniก
	_rt=m._rt OR !open_dbf(m.gl_data, "cstravne", "op_strav", "STRAVNE", m.exc, m.rei)

**	Aktivace statistiky
	=eff_start()
	=eff_ktim(90,2)
	st_on_dat=date()
	st_on_time=time()

	a=m.ggf
	IF file(m.ggf+"firma.mem")
		REST FROM (m.ggf+"firma.mem") ADDI
		ENDIF
	ggf=a
	IF !empty(m.ggjme)
		SELE firma
		REPL jme WITH m.ggjme
		ENDIF
	SELE cglobal
	WAIT CLEAR
RETURN !m._rt
**	Open CGLOBAL 	- Glob lnก parametry pro cestu
PROCEDURE op_glob
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(10), dat D, datod D, popis C(40), kdo C(16), ;
			jme C(25), vuz C(16), vla L, pri L, vztah L, kap N(3), ;
			slu L, pau L, flg L, arch C(16), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(cis) TAG cis
		INDEX ON cs(jme) TAG jme
		INDEX ON dtos(dat)+cs(cis) TAG dat
		INDEX ON arch TAG arch
	  CASE m._cfn=3
		RETURN type("cglobal.datod")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CVYUCT	- Vlastnก vyฃtov nก
PROCEDURE op_vyuct
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (dat1 D, hod1 C(2), min1 C(2), mis1 C(15), ;
			dat2 D, hod2 C(2), min2 C(2), mis2 C(15), popis C(40), ;
			stat C(20), vla L, dpp C(12), pri L, upn C(3), ;
			kmo N(6), kmd N(6), uby N(12,2), mena C(3), kms N(6), typ C(1),;
			arch C(16), m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON arch TAG arch
	  CASE m._cfn=3
		RETURN type("cvyuct.mena")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CVYDAJE	- V์daje bุhem cesty
PROCEDURE op_vydaj
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (dat D, txt C(20), cas N(12,2), ;
			stat C(20), mena C(3), lim L, arch C(16), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON arch TAG arch
	  CASE m._cfn=3
		RETURN type("cvydaje.arch")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CMENY		- Kขdy mุn, kurzy, z lohy, ceny PHM
PROCEDURE op_meny
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (mena C(3), kurz N(7,3), koef N(5), ;
			zaloha N(12,2), phm N(9,2), celkem N(12,2), flg L, ;
			zmtyp N(3), zmmd N(3), zmmda N(4), zmdal N(3), zmdaa N(4), ;
			kctyp N(3), kcmd N(3), kcmda N(4), kcdal N(3), kcdaa N(4), ;
			extxt C(40), ;
			arch C(16), m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON mena TAG mena
		INDEX ON arch TAG arch
	  CASE m._cfn=3
		RETURN type("cmeny.extxt")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN


**	Open CVOZY		- ฌกselnกk voz…
PROCEDURE op_vozy
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (spz C(9), lna100 N(5,2), popisv C(40), typ C(8), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON spz TAG spz
		INDEX ON cs(popisv) TAG popis
		INDEX ON m_add TAG m_add
	  CASE m._cfn=3
		RETURN type("cvozy.popisv")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CLIDI		- ฌกselnกk lidก
PROCEDURE op_lidi
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (jme C(25), pomer C(11), rodc C(11), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(jme) TAG jme
		INDEX ON m_add TAG m_add
	  CASE m._cfn=3
		RETURN type("clidi.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CTEXT		- ฌกselnกk text…
PROCEDURE op_text
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (text C(40), typt C(1), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(text) TAG text
	  CASE m._cfn=3
		RETURN type("ctext.typt")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CNAHRADY	- ฌกselnกk n hrad a stravnho v tuzemsku
PROCEDURE op_nahr
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (dat D, auto N(6,2), moto N(6,2), bus N(6,2), ;
			nakl N(6,2), prives N(3), sdo5 N(6,2), s5_12 N(6,2), ;
			s12_18 N(6,2), snad18 N(6,2), kapes N(3), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON dtos(dat) TAG dat
		INDEX ON dtos(dat) TAG datx DESC
	  CASE m._cfn=3
		RETURN type("cnahrady.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CSTRAVNE	- ฌกselnกk stravnho v cizinุ
PROCEDURE op_strav
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (dat D, stat C(25), mena C(3), stravne N(10,2), ;
			m_date D, m_time C(5), m_user C(10), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(left(stat,20))+dtos(dat) TAG stat UNIQ DESC
		INDEX ON cs(left(stat,20))+dtos(dat) TAG stravne
		INDEX ON cs(left(stat,20))+dtos(dat) TAG stravnex DESC
	  CASE m._cfn=3
		RETURN type("cstravne.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CEXPO		- Exportnก datab ze
PROCEDURE op_expo 
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (dat D, typ N(3), md N(3), mda N(4), ;
			dal N(3), daa N(4), txt C(40), cas N(12,2), ciscesty C(10))
	  CASE m._cfn=2
		INDEX ON ciscesty TAG ciscesty
	  CASE m._cfn=3
		RETURN type("cexpo.cas")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN


**	Obnova obrazovky
PROCEDURE refscr
	DO backgr WITH 'main'
	IF !empty(m.ggf)
		PRIVATE t
		t="้tovan  firma: "+alltrim(m.ggjme)
		@ 0,int((scols()-len(m.t))/2) SAY m.t
		ENDIF
*	@ 2,scols()-13 SAY "Celkem :"+str(val(sys(1001))/1024,4)
*	@ 3,scols()-13 SAY "Pro RUN:"+str(memory(),4)
*	@ 4,scols()-13 SAY "Alloc. :"+str(val(sys(1016))/1024,4)
	IF used(m.t_gl)
		PRIV a
		a=&t_gl..cis+'  '+dtoc(&t_gl..dat)+'  '+&t_gl..popis
		@2,1 SAY "ฌกslo: "+ltrim(&t_gl..cis)
		@3,1 SAY "Datum: "+dtoc(&t_gl..dat)
		@4,1 SAY "Popis: "+rtrim(&t_gl..popis)
		ENDIF
RETURN
