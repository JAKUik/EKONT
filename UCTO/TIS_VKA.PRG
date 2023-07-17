*******************************
**	Uz vØrkov‚ vìkazy a sestavy
* PROCEDURE tis_vka
	PRIV tdto, tdtd, _naz, _uli, _mes, _ico, newadr
	_naz=m.ggjme
	_uli=m.gguli
	_mes=m.ggmes
	_ico=m.ggico
	tdto=ctod("01.01."+str(year(date()-1)))
	tdtd=ctod("31.12."+str(year(date()-1)))

	PRIV tp_vyk, _flxx
	tp_vyk=iif(m.ggtp_vyk="Z","VZR_","VPR_")
	_flxx=m.gl_data+m.tp_vyk+str(m.ggrok,4)+".dbf"

	DEFINE POPUP vyk FROM 10,40 RELATIVE SHADOW ;
		TITLE " PüEHLEDY " COLOR SCHEME 4
	DEFINE BAR 20 OF vyk PROMPT " \<VìpoŸet vìkazu" ;
		MESS "VìpoŸet vìkazu."
	DEFINE BAR 22 OF vyk PROMPT " \<Prohl¡§en¡ vìkazu " ;
		SKIP FOR !file(m.ggf+"vykaz.dbf") ;
		MESS "Prohl¡§en¡ vìkazu."
	DEFINE BAR 25 OF vyk PROMPT " \<Tisk vìkazu " ;
		SKIP FOR !file(m.ggf+"vykaz.dbf") ;
		MESS "Tisk vìkazu."

	DEFINE BAR 29 OF vyk PROMPT "\-"
	DEFINE BAR 60 OF vyk PROMPT " Vìkazy v PR do 2002" ;
		MESS "Tisk vìkaz… v pln‚m rozsahu."
	DEFINE BAR 62 OF vyk PROMPT " Vìkazy ve ZR do 2002" ;
		MESS "Tisk vìkaz… ve zkr cen‚m rozsahu."
	DEFINE BAR 89 OF vyk PROMPT "\-"
	DEFINE BAR 90 OF vyk PROMPT " Posledn¡ \<sestava" ;
		SKIP FOR !file(m.ggf+"vykazy.prt") ;
		MESS "éŸtovì rozvrh pou§itìch £Ÿt…."
	DEFINE BAR 99 OF vyk PROMPT " \<Definice vìkaz…" ;
		SKIP FOR m.gl_pri_pu<99 OR !file(m._flxx) ;
		MESS "Definov n¡ £Ÿetn¡ch vìkaz…."
	ON SELE POPU vyk DO vyk

	ACTI POPUP vyk

	RELE POPU vyk
RETURN

PROC vyk
	DO CASE
		CASE bar()=20			&&	Vìkazy vìpoŸet
			DO vyk_vyp
		CASE bar()=22			&&	Vìkazy prohl¡§en¡
			DO vyk_view
		CASE bar()=25			&&	Vìkazy tisk
			DO vyk_tisk

		CASE bar()=60			&&	Vìkazy PR do 2002
			DO vykaz WITH .T.
		CASE bar()=62			&&	Vìkazy ZR do 2002
			DO vykaz WITH .F.
		CASE bar()=90			&&	Posledn¡ sestava
			tskf=m.ggf+"vykazy.prt"
			DO tisk WITH 6,.T.
		CASE bar()=99			&&	Definov n¡ vìkaz…
			DO vyk_def
		ENDCASE
RETURN

**	VìpoŸet vìkazu
PROCEDURE VYK_VYP  
	PRIV y, xx

	PRIV newadr
	newadr=left(alltrim(firma.naz)+space(12),12)

	SELE denik
	o=order()
	SET ORDER TO dat
	GO TOP
	tdto=dat
	GO BOTT
	tdtd=dat
	SET ORDER TO &o

	SET CENT ON
	DO vykazV.spr
	SET CENT OFF

	IF m.esc
		RETURN
		ENDIF

	PRIV xx

	IF year(m.tdtd)>=2003
		xx=m.tp_vyk+str(year(m.tdtd),4)+".dbf"
	  ELSE
		WAIT WIND "Pro roky 2002 a ni§ç¡ pou§ijte volbu z menu."
	ENDIF

	IF !file(m.xx)
		WAIT WIND "Nenalezena definiŸn¡ datab ze pro vìkazy pro zadanì rok."
		SELE vykaz
		USE
		RETURN
	ENDIF

	IF !file(m.gl_data+m.xx)
		WAIT WIND "Neexistuje datab ze £daj… pro vìpoŸet vìkaz…."
		SELE vykaz
		USE
		RETURN
	ENDIF

	DELE FILE (m.ggf+"vykaz.dbf")
	DELE FILE (m.ggf+"vykaz.cdx")

	COPY FILE (m.gl_data+m.xx) TO (m.ggf+"vykaz.dbf")

	SELE 0
	USE m.ggf+"vykaz.dbf" EXCL
	INDEX ON radek TAG radek
	SET ORDER TO radek

* NaplnØn¡ z…statk… £Ÿt…

	SELE osn
	SET ORDER TO uct
	WAIT WIND NOWA "Pracuji ş"
	REPL ALL cmd WITH 0, cdal WITH 0

	f="!del AND !(md>701 OR dal>701) AND betw(dat,m.tdto,m.tdtd)"

	SELE denik
	WAIT WIND NOWA "Pracuji şş"
	SET RELA TO str(md,3)+str(mda,4) INTO osn
	REPL ALL osn.cmd WITH osn.cmd+denik.cas FOR &f

	WAIT WIND NOWA "Pracuji şşş"
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	REPL ALL osn.cdal WITH osn.cdal+denik.cas FOR &f

* Kontrola pıedvyplnØn¡ ı dk… u vìkaz…
	WAIT WIND NOWA "Pracuji şşşş"
	SELE osn
	SCAN ALL FOR (cdal != cmd)
		IF empty(rad)
			IF uct=431 AND ;
					ask(-1,-1,"A","Z…statek na £Ÿtu Ÿ.: "+str(uct,3)+". Pıeruçit ")
				SELE vykaz
				USE
				RETURN
			ENDIF
			IF uct!=701 AND uct!=702 AND ;
					ask(-1,-1,"A","Nenalezena definice ı dku u £Ÿtu Ÿ.: "+str(uct,3)+". Pıeruçit ")
				SELE vykaz
				USE
				RETURN
			ENDIF
		ENDIF
	ENDSCAN

	SELE osn
	SET RELA TO rad INTO vykaz

	SCAN ALL FOR uct!=0

		DO CASE
			CASE roz='A'
				cc=cmd-cdal
				IF slo=" "
					cc=cdal-cmd
				ENDIF
			CASE roz='P'
				cc=cdal-cmd
				IF slo!=" "
					cc=cmd-cdal
				ENDIF
			CASE roz='-' AND betw(uct,500,599)
				cc=cmd-cdal
			CASE roz='-' AND betw(uct,600,699)
				cc=cdal-cmd
			OTHER
				IF cmd!=0 OR cdal!=0
					WAIT WIND "U £Ÿtu Ÿ."+strn(osn.uct)+'.'+strn(osn.ana)+" je zad no pochybn‚ zaıazen¡."
					SELE vykaz
					USE
					RETURN
				ENDIF
		ENDCASE
		
		DO CASE
			CASE slo="1"
				REPL vykaz.sl1 WITH vykaz.sl1+cc
			CASE slo="2"
				REPL vykaz.sl2 WITH vykaz.sl2+cc
			CASE slo=" "
				REPL vykaz.sl WITH vykaz.sl+cc
			OTHER
				WAIT WIND "U £Ÿtu Ÿ."+strn(osn.uct)+'.'+strn(osn.ana)+" je zad n chybnì sloupec."
				SELE vykaz
				USE
				RETURN
		ENDCASE

	ENDSCAN
	SET RELA TO

	SELE vykaz
	SET ORDER TO

* Pıevod na tis¡ce
	REPL ALL tsl WITH round(sl/1000,0), tsl1 WITH round(sl1/1000,0), tsl2 WITH round(sl2/1000,0)

* PıepoŸty vzoreŸk…
	DO rcalcnew WITH ""
	DO rcalcnew WITH "T"

*************************
* NaŸten¡ minul‚ho obdob¡
*************************
	IF !empty(m.newadr) AND file(m.gl_data+m.newadr+"\vykaz.dbf")
		WAIT WIND NOWA "NaŸ¡t m minul‚ £Ÿetn¡ obdob¡."
		SELE 0
		_err=0
		ON ERROR _err=error()
		USE (m.gl_data+m.newadr+"\vykaz.dbf") ALIAS vykmo
		SET ORDER TO radek
		ON ERROR &_onerr
		DO CASE
		  CASE m._err=0
			DO _load_mo
			SELE vykaz
		  CASE m._err=3
			WAIT WIND NOWA "Soubor je pou§¡v n. Nelze naŸ¡st MO."
		  OTHER
			WAIT WIND NOWA "Pıi pokusu o naŸten¡ dat vznikla chyba Ÿ."+strn(m._err)
			ENDCASE
	  ELSE
		WAIT WIND NOWA "Data z minul‚ho obdob¡ nebyla nalezena."
		ENDIF

	SELE vykaz
	USE
	WAIT CLEAR

RETURN

PROCEDURE _load_mo
	SELE vykaz
	REPL ALL tmo WITH 0

	SCAN ALL FOR !empty(vtmo)
		_r=left(vtmo,4)
		SELE vykmo
		IF !seek(m._r)
			WAIT WIND "Nenalezen ı dek "+_r+" v minul‚m obdob¡."
			USE
			RETURN
		ENDIF
		SELE vykaz
		WAIT WIND NOWA str(vykmo.tsl)
		REPL tmo WITH vykmo.tsl
	ENDSCAN

	SELE vykmo
	USE

RETURN
**	Prohl¡§en¡ vìkazu 
PROCEDURE vyk_view

	SELE 0
	USE m.ggf+"vykaz"

	DO list_c WITH -1,-1,12,0, " Vìkaz ", ;
		[:DEF=Vykaz], ;
		"\ F1-Help    ENTER-Oprava  F7-Tı¡dit podle ...",;
		'{radek}',;
		'CR#:		DO vyk_edit		## ENTER-Oprava zaokrouhlen¡', ;
		'ALT+P#:	DO rcalclis		## ALT+P-PıepoŸ¡t n¡ vìkazu'

	USE

RETURN


PROCEDURE rcalclis
	PRIV rc
	rc=recno()
	DO rcalcnew WITH "T"	
	refscr=.T.
	=gorec(m.rc)
RETURN

PROCEDURE vyk_edit

	app=.F.
	vdef=.F.

	DO CASE
	  CASE empty(radek) OR left(vsl1,1)="+" OR left(vsl,1)="+"
		WAIT WIND NOWA "ü dek obsahuje poŸ¡tan‚ hodnoty, zaokrouhlete zdrojovì ı dek."		
	  CASE sl=0 AND sl1=0 AND sl2=0
		WAIT WIND NOWA "Na ı dku nejsou § dn‚ hodnoty, zaokrouhlete jinì ı dek."
	  OTHE
		DO vyk_def.spr
	ENDCASE

RETURN

**	Tisk vìkaz…
PROCEDURE vyk_tisk

	SET CENT ON

	DO vykazT.spr

	tskf=m.ggf+"vykazy.prt"
	SET TALK OFF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	WAIT CLEAR

	SELE 0
	USE m.ggf+"vykaz"

	PRIV sethead, ispage
	sethead="_headakt"
	ispage=.F.

	SCAN ALL
		DO CASE
			CASE left(text,5)="#AKTI"
				DO _t_akti
				DO _headakt WITH "H"
			CASE left(text,5)="#PASI"
				IF !m.ispage
					DO &sethead WITH "E"
				ENDIF
				DO _headpas WITH "H"
			CASE left(text,5)="#VYSL"
				DO _t_vysl
				DO _headvys WITH "H"
			CASE left(text,5)="#PAGE"
				DO &sethead WITH "E"
				??chr(12)
				ispage=.T.
				LOOP
			CASE left(text,5)="#HEAD"
				IF !m.ispage
					DO &sethead WITH "E"
				ENDIF
				DO &sethead WITH "H"
			CASE left(text,5)="#LINE"
				IF !m.ispage
					DO &sethead WITH "E"
				ENDIF
				?
				ispage=.T.
				LOOP
			CASE left(text,1)="-" OR left(text,1)="=" 
				DO &sethead WITH left(text,1)
			OTHE
				IF !empty(radek)
					DO &sethead WITH "R"
				ENDIF	
		ENDCASE
		ispage=.F.
	ENDSCAN
	IF !m.ispage
		DO &sethead WITH "E"
	ENDIF
	DO _t_end

	SET CENT OFF
	SET ALTE TO
	SET CONS ON
	DO tisk
	USE	

RETURN

**	ZaŸ tek sestavy HLAVA SESTAVY ROZVAHY
PROCEDURE _t_akti

	IF m.ggtp_vyk="Z"
		?m.nlq+ ;
		space(10)+ "    R O Z V A H A     "+space(10)+"N zev a s¡dlo £Ÿetn¡ jednotky"
		?space(10)+" ve zkr cen‚m rozsahu "+space(10)+m._naz
		?space( 8)+"od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+	space(7) +m._uli
		?space(10)+"(v celìch tis¡c¡ch KŸ)"+space(10)+m._mes
		?space( 8)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "
		?space( 8)+"³ Rok  ³ MØs¡c ³  I ¬ O   ³    "
		?space( 8)+"³ "+str(year(m.tdtd),4)+" ³  "+str(month(m.tdtd),2)+"   ³ "+m._ico+" ³    "
		?space( 8)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "
		?
		?m.draft+m.cnon
	  ELSE
		?m.nlq+ ;
		space(10)+ "    R O Z V A H A     "+space(10)+"N zev a s¡dlo £Ÿetn¡ jednotky"
		?space(10)+"   v pln‚m rozsahu    "+space(10)+m._naz
		?space( 8)+"od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+	space(7) +m._uli
		?space(10)+"(v celìch tis¡c¡ch KŸ)"+space(10)+m._mes
		?space( 8)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "
		?space( 8)+"³ Rok  ³ MØs¡c ³  I ¬ O   ³    "
		?space( 8)+"³ "+str(year(m.tdtd),4)+" ³  "+str(month(m.tdtd),2)+"   ³ "+m._ico+" ³    "
		?space( 8)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "
		?
		?m.draft+m.cnon
	ENDIF

RETURN

**	ZaŸ tek sestavy HLAVA SESTAVY VíSLEDOVKY
PROCEDURE _t_vysl

	IF m.ggtp_vyk="Z"
		?m.cnoff+m.nlq+ ;
		space(10)+ " VíKAZ ZISKŞ A ZTRµT  "+space(10)+"N zev a s¡dlo £Ÿetn¡ jednotky"
		?space(10)+" ve zkr cen‚m rozsahu "+space(10)+m._naz
		?space( 8)+"od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+	space(7) +m._uli
		?space(10)+"(v celìch tis¡c¡ch KŸ)"+space(10)+m._mes
		?space( 8)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "
		?space( 8)+"³ Rok  ³ MØs¡c ³  I ¬ O   ³    "
		?space( 8)+"³ "+str(year(m.tdtd),4)+" ³  "+str(month(m.tdtd),2)+"   ³ "+m._ico+" ³    "
		?space( 8)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "
		?
		?m.draft+m.cnon
	  ELSE
		?m.cnoff+m.nlq+ ;
		space(10)+ " VíKAZ ZISKŞ A ZTRµT  "+space(10)+"N zev a s¡dlo £Ÿetn¡ jednotky"
		?space(10)+"   v pln‚m rozsahu    "+space(10)+m._naz
		?space( 8)+"od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+	space(7) +m._uli
		?space(10)+"(v celìch tis¡c¡ch KŸ)"+space(10)+m._mes
		?space( 8)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "
		?space( 8)+"³ Rok  ³ MØs¡c ³  I ¬ O   ³    "
		?space( 8)+"³ "+str(year(m.tdtd),4)+" ³  "+str(month(m.tdtd),2)+"   ³ "+m._ico+" ³    "
		?space( 8)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "
		?
		?m.draft+m.cnon
	ENDIF

RETURN

**	UkonŸen¡ sestavy
PROCEDURE _t_end
	?
	?
	?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(7)+"³ Odesl no dne:     ³ Podpis statut rn¡ho org nu ³ Osoba zodpovØdn  za £Ÿetnictv¡ ³ Osoba zodpovØdn  za £Ÿetn¡ ³"
	?space(7)+"³                   ³ nebo fyzick‚ osoby, kter   ³ (jm‚no a podpis)               ³ z vØrku (jm‚no a podpis)   ³"
	?space(7)+"³                   ³ je £Ÿetn¡ jednotkou        ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³ Tel.:                      ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	??m.cnoff+m.draft+chr(12)
RETURN


**	Tabulka AKTIVA
PROCEDURE _headakt
PARA x
	DO CASE
	  CASE x="R"
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl1,7)+" ³ "+str0(tsl2,7)+" ³ "+str0(tsl,7)+" ³ "+str0(tmo,7)+" ³"
	  CASE x="H"
		?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ¿"
		?space(7)+"³ OznaŸen¡ ³                           A K T I V A                             ³ü dek³     BØ§n‚ £Ÿetn¡ obdob¡     ³Minul‚ éO³"
		?space(7)+"³          ³                                                                   ³     ÃÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ´         ³"
		?space(7)+"³    a     ³                                b                                  ³  c  ³ Brutto  ³ Korekce ³  Netto  ³  Netto  ³"
		?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
	  CASE x="-"
		?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
	  CASE x="="
		?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
	  CASE x="E"
		?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
	ENDCASE
	sethead="_headakt"
RETURN

**	Tabulka PASIVA
PROCEDURE _headpas
PARA x
	DO CASE
	  CASE x="R"
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl,9)+" ³ "+str0(tmo,9)+" ³"
	  CASE x="H"
		?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
		?space(7)+"³ OznaŸen¡ ³                           P A S I V A                             ³ü dek³  Stav v   ³  Stav v   ³"
		?space(7)+"³          ³                                                                   ³     ³ bØ§n‚m éO ³ minul. éO ³"
		?space(7)+"³    a     ³                                b                                  ³  c  ³     5     ³     6     ³"
		?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	  CASE x="-"
		?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	  CASE x="="
		?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	  CASE x="E"
		?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	ENDCASE
	sethead="_headpas"
RETURN

**	Tabulka VíSLEDOVKU
PROCEDURE _headvys
PARA x
	DO CASE
	  CASE x="R"
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl,10)+" ³ "+str0(tmo,10)+" ³"
	  CASE x="H"
		?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		?space(7)+"³ OznaŸen¡ ³                             T E X T                               ³ü dek³ SkuteŸnost v £Ÿ. obdob¡ ³"
		?space(7)+"³          ³                                                                   ³     ÃÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ´"
		?space(7)+"³    a     ³                                b                                  ³  c  ³ sledovan‚m ³  minul‚m   ³"
		?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
	  CASE x="-"
		?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
	  CASE x="="
		?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
	  CASE x="E"
		?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	ENDCASE
	sethead="_headvys"
RETURN
**	PıepoŸ¡t n¡ tabulky
PROCEDURE rcalcnew
PARA fl

	SELE vykaz
	SET ORDER TO

	SCAN ALL WHILE left(text,5)!="#PASI"
		IF left(text,1)="#" OR left(text,1)="-" OR left(text,1)="="
			LOOP
		ENDIF
		a=alltrim(radek)
		b=a+"_1"
		c=a+"_2"
		PRIV &a, &b, &c
		&b=&fl.sl1
		&c=&fl.sl2
		&a=&b+&c
		REPL &fl.sl1 WITH &b, &fl.sl2 WITH &c, &fl.sl WITH &a
	ENDSCAN
	SCAN REST FOR !(left(text,1)="#" OR left(text,1)="-" OR left(text,1)="=")
		a=alltrim(radek)
		PRIV &a
		&a=&fl.sl
		REPL &fl.sl WITH &a
	ENDSCAN

	PRIV x, ppp
	x=6
	FOR i=1 TO x
		WAIT WIND NOWA "PıepoŸet vìkaz… "+ ;
				repl('ş',i)+repl('',x-i)
		ppp=.T.
		SCAN ALL
			IF left(text,5)="#PASI"
				ppp=.F.
				LOOP
			  ELSE
				IF left(text,1)="#" OR left(text,1)="-" OR left(text,1)="="
					LOOP
				ENDIF
			ENDIF
			a=alltrim(radek)
			IF m.ppp
				b=a+"_1"
				c=a+"_2"
				IF left(vsl1,1)='+'
					REPL &fl.sl1 WITH eval(vsl1)
					IF left(vsl2,1)='+'
						REPL &fl.sl2 WITH eval(vsl2)
					ENDIF
					&b=&fl.sl1
					&c=&fl.sl2
				ENDIF
				REPL &fl.sl WITH &fl.sl1+&fl.sl2
			  ELSE
				IF left(vsl,1)='+'
					REPL &fl.sl WITH eval(vsl)
				ENDIF
			ENDIF
			&a=&fl.sl
		ENDSCAN
	ENDFOR
	WAIT CLEAR
RETURN

**	Definov n¡ vìkaz…
PROCEDURE vyk_def 

	SELE 0
***************  Nechat zad vat rok vìkazu
	USE m.gl_data+m.tp_vyk+"2004" EXCL

	DO list_c WITH -1,-1,12,0, " Vìkaz ", ;
		[:DEF=Vykaz], ;
		"\ F1-Help    ENTER-Oprava  F7-Tı¡dit podle ...",;
		'{rad}',;
		'CR#:		DO vyk_defe WITH .F.	## ENTER-Oprava definic',;
		'ALT+D#:	DO vyk_dele				## ALT+D-Vymaz n¡ ı dku',;
		'INS:		DO vyk_defe WITH .T.	## ENTER-Oprava definic'

	USE

RETURN
PROCEDURE vyk_defe
PARA app

	IF m.app
		INSERT BLANK
		app=.F.
	ENDIF

	vdef=.T.
	DO vyk_def.spr

RETURN
PROCEDURE vyk_dele
*	IF ask(-1,-1,"A","Vymazat polo§ku ?")
		DELE
*	ENDIF
RETURN
**	Vìkazy - T/F - Plnì/Zkr cenì rozsah
PROCEDURE vykaz
PARA typs
	SET ESCA ON
	ON ESCA DO stoptisk
	PRIV pass, o, f
	pass=0

	IF used("vykaz_pr")
		SELE vykaz_pr
		USE
		ENDIF
	IF used("vykaz_zr")
		SELE vykaz_zr
		USE
		ENDIF

	SELE denik
	o=order()
	SET ORDER TO dat
	GO TOP
	tdto=dat
	GO BOTT
	tdtd=dat
	SET ORDER TO &o
	
	DO t-priz.spr
	IF m.esc
		RETURN
		ENDIF

	SELE osn
	SET ORDER TO uct
	WAIT WIND NOWA "Pracuji ş"
	REPL ALL cmd WITH 0, cdal WITH 0

	f="!del AND !(md>701 OR dal>701) AND betw(dat,m.tdto,m.tdtd)"

	SELE denik
	WAIT WIND NOWA "Pracuji şş"
	SET RELA TO str(md,3)+str(mda,4) INTO osn
	REPL ALL osn.cmd WITH osn.cmd+denik.cas FOR &f

	WAIT WIND NOWA "Pracuji şşş"
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	REPL ALL osn.cdal WITH osn.cdal+denik.cas FOR &f
	WAIT WIND NOWA "Pracuji şşşş"

	SELE osn
	REPL ALL cmd WITH 0, cdal WITH 0 ;
		FOR inlist(uct, 336, 341, 342, 343, 345, 346, 347, 371)

	SELE denik
	SET RELA TO str(md,3) INTO osn
	REPL ALL osn.cmd WITH osn.cmd+denik.cas FOR &f ;
		AND inlist(md, 336, 341, 342, 343, 345, 346, 347, 371)
	WAIT WIND NOWA "Pracuji şşşşş"

	SET RELA TO str(dal,3) INTO osn
	REPL ALL osn.cdal WITH osn.cdal+denik.cas FOR &f ;
		AND inlist(dal, 336, 341, 342, 343, 345, 346, 347, 371)
	SET RELA TO

	SELE osn
	SCAN ALL FOR (cdal != cmd)
		IF empty(rad)
			IF uct=431 AND cdal!=cmd AND ;
					ask(-1,-1,"A","Z…statek na £Ÿtu Ÿ.: "+str(uct,3)+". Pıeruçit ")
				RETURN
				ENDIF
			IF uct!=701 AND uct!=702 AND ;
					ask(-1,-1,"A","Nenalezena definice ı dku u £Ÿtu Ÿ.: "+str(uct,3)+". Pıeruçit ")
				RETURN
				ENDIF
			ENDIF
		ENDSCAN

	SELE 0
	CREA CURS cu1 (rad C(4), sl N(14,2), sl1 N(14,2), sl2 N(14,2))
	FOR i=1 TO 111
		INSERT INTO cu1 (rad) VALUES ("R"+alltrim(str(i)))
		ENDFOR
	FOR i=1 TO 63
		INSERT INTO cu1 (rad) VALUES ("V"+alltrim(str(i)))
		ENDFOR
	INDEX ON rad TAG rad
	SET ORDER TO rad

	SELE osn
	PRIV cc
	SCAN ALL
		IF ana!=0 AND inlist(osn.uct, 336, 341, 342, 343, 345, 346, 347, 371)
			LOOP
			ENDIF
		cc=0
		WAIT WIND NOWA "Zpracov n¡ £Ÿtu Ÿ.: "+str(uct,3)
		DO CASE
			CASE roz='A' AND !betw(uct,500,699)
				cc=cmd-cdal
			CASE roz='P' AND !betw(uct,500,699)
				cc=cdal-cmd
			CASE roz='-' AND betw(uct,500,599)
				cc=cmd-cdal
			CASE roz='-' AND betw(uct,600,699)
				cc=cdal-cmd
			OTHER
				IF cmd!=0 OR cdal!=0
					WAIT WIND "U £Ÿtu Ÿ."+strn(osn.uct)+'.'+strn(osn.ana)+" je zad no pochybn‚ zaıazen¡."
					SELE cu1
					USE
					RETURN
					ENDIF
				LOOP
			ENDCASE
		SELE cu1
		SEEK alltrim(osn.rad)
		IF !found()
			WAIT WIND "ü dek u £Ÿtu Ÿ.: "+strn(osn.uct)+'.'+strn(osn.ana)+" nen¡ ve vìkazech."
			USE
			RETURN
			ENDIF
		IF at(osn.slo," 12")=0
			WAIT WIND "Sloupec u £Ÿtu Ÿ.: "+strn(osn.uct)+'.'+strn(osn.ana)+" nevyhovuje."
			USE
			RETURN
			ENDIF
		a=osn.slo
		IF m.cc<0 AND inlist(osn.uct, 336, 341, 342, 343, 345, 346, 347, 371)
*			WAIT WIND "éŸet Ÿ¡slo "+strn(osn.uct)+"."+strn(osn.ana)+" m  z pornou hodnotu, upravte zaıazen¡."
*			DO CASE
*				CASE osn.rad="R45 "
*					SEEK "R95"
*					a=" "
*				CASE osn.rad="R46 "
*					SEEK "R96"
*					a=" "
*				CASE osn.rad="R47 "
*					SEEK "R97"
*					a=" "
*				CASE osn.rad="R95 "
*					SEEK "R45"
*					a="1"
*				CASE osn.rad="R96 "
*					SEEK "R46"
*					a="1"
*				CASE osn.rad="R97 "
*					SEEK "R47"
*					a="1"
*				OTHE
*					WAIT WIND "æpatn‚ ı dky u £Ÿt…: 341, 342, 343, 345, 346, 347, 371"
*					USE
*					RETURN
*				ENDCASE
*			cc=m.cc*-1
			ENDIF
		REPL sl&a WITH sl&a.+m.cc
		SELE osn
		ENDSCAN

	SELE cu1
	SET ORDER TO
	SCAN ALL WHILE val(substr(cu1.rad,2))<62
		REPL sl WITH sl1+sl2
		a=alltrim(cu1.rad)
		b=a+"_1"
		c=a+"_2"
		PRIV &a, &b, &c
		&a=cu1.sl
		&b=cu1.sl1
		&c=cu1.sl2
		ENDSCAN
	SCAN REST
		a=alltrim(cu1.rad)
		PRIV &a
		&a=cu1.sl
		ENDSCAN
	USE

	PRIV xx
	DO CASE
		CASE year(m.tdtd)<=2000
			xx="vpr_2000.dbf"
		CASE year(m.tdtd)=2001
			xx="vpr_2001.dbf"
		CASE year(m.tdtd)>=2002
			xx="vpr_2002.dbf"
		ENDCASE

	IF !file(m.gl_data+m.xx)
		WAIT WIND "Neexistuje datab ze £daj… pro vìpoŸet vìkaz…."
		RETURN
		ENDIF

*WAIT WIND m.gl_data+m.xx+" TO "+m.ggf+"vykaz_pr.dbf"

	COPY FILE (m.gl_data+m.xx) TO (m.ggf+"vykaz_pr.dbf")



**	NaplnØn¡ datab z¡ vìkaz… £daji v KŸ a propoŸet vzorc…
	SELE 0
	USE m.ggf+"vykaz_pr"
	
	DO rcalcvyk WITH ""

	PRIV _aa, _bb
	_aa=round(m.V62/1000,0)

	SCAN ALL WHILE val(substr(radek,2))<62
		a=alltrim(radek)
		b=a+"_1"
		c=a+"_2"
		PRIV &a, &b, &c
		&b=round(sl1/1000,0)
		&c=round(sl2/1000,0)
		&a=&b+&c
		REPL tsl1 WITH &b, tsl2 WITH &c, tsl WITH &a
		ENDSCAN
	SCAN REST
		a=alltrim(radek)
		PRIV &a
		&a=round(sl/1000,0)
		REPL tsl WITH &a
		ENDSCAN

	DO rcalcvyk WITH "t"

	_bb=m.V62-m._aa
* WAIT WIND strn(v62)+"  "+strn(v53)+"  "+strn(v54)+"  "+strn(_aa)+"  "+strn(_bb)
	IF m._bb<0
		LOCA FOR radek="V53 " ALL
		REPL tsl WITH tsl+abs(m._bb)
		V53=tsl
	  ELSE
		LOCA FOR radek="V54 " ALL
		REPL tsl WITH tsl+abs(m._bb)
		V54=tsl
		ENDIF

	DO rcalcvyk WITH "t"
* brow

**	Dorovn n¡ zaokrouhlovac¡ch rozd¡l…
	DO WHILE m.R80 != m.V62 AND lastkey()!=27
* WAIT WIND "R80: "+strn(m.R80)+"   - V62: "+strn(m.V62)
		DO dor_pas WITH m.R80>m.V62
		DO rcalcvyk WITH "t"
		IF pass>5
			SET ESCA OFF
			PRIV _l
			_l=50
			DEFINE WINDOW w_ask ;
				FROM INT((SROW()-8)/2),INT((SCOL()-(m._l-1))/2) ;
				TO INT((SROW()-8)/2)+5,INT((SCOL()-(m._l-1))/2)+m._l ;
				NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
			ACTIVATE WINDOW w_ask SAME
			@ 1,1 SAY "Akt/Pas   :  R 1: "+str(m.R1,10)+ " ³ R62: "+str(m.R62,10)
			@ 3,1 SAY "HV Rozvaha:  R80: "+str(m.R80,10)+" ³ V62: "+str(m.V62,10)
			WAIT WIND
			RELEASE WINDOW w_ask
			SET ESCA ON
			ENDIF
		ENDDO

*	WAIT WIND "!!! Vìkazy nejsou vypoŸteny spr vnØ !!!"

	SET ESCA OFF
	ON ESCA

	tskf=m.ggf+"vykazy.prt"
	SET TALK OFF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	WAIT CLEAR


	IF m.typs
		IF ask(-1,-1,'A',"Vytvoıit pouze pracovn¡ vìkaz v KŸ a tis. KŸ pro kontrolu")
			DO vk_pr_p
		  ELSE
			DO vk_pr_o
			ENDIF
	  ELSE
		DO vk_zr
		ENDIF

	SET ALTE TO
	SET CONS ON
	DO tisk
	USE
RETURN

**	Vìkazy ve zkr cen‚m rozsahu
PROCEDURE vk_zr

	PRIV xx
	DO CASE
		CASE year(m.tdtd)<=2000
			xx="vzr_2000.dbf"
		CASE year(m.tdtd)=2001
			xx="vzr_2001.dbf"
		CASE year(m.tdtd)>=2002
			xx="vzr_2002.dbf"
		ENDCASE

	USE
	IF !file(m.gl_data+m.xx)
		WAIT WIND "Neexistuje datab ze £daj… pro vìpoŸet vìkaz…."
		RETURN
		ENDIF
	COPY FILE (m.gl_data+m.xx) TO (m.ggf+"vykaz_zr.dbf")



	WAIT WIND NOWA "Konvertuji vìkazy z pln‚ho rozsahu do zkr cen‚ho."
	USE (m.ggf+"vykaz_zr.dbf")
	SCAN ALL
		DO CASE
			CASE !empty(vsl1) AND left(vsl1,1)!='+'
				REPL tsl1 WITH eval(vsl1)
				IF left(vsl1,1)='+'
					REPL tsl2 WITH eval(vsl2)
				ENDIF
				REPL tsl WITH tsl1+tsl2
			CASE !empty(vsl) AND left(vsl,1)!='+'
				REPL tsl1 WITH 0, tsl2 WITH 0
				REPL tsl WITH eval(vsl)
			OTHE
				REPL tsl1 WITH 0, tsl2 WITH 0, tsl WITH 0
			ENDCASE
		ENDSCAN

	SCAN ALL WHILE radek!="R999"
		a=alltrim(radek)
		b=a+"_1"
		c=a+"_2"
		&a=tsl
		&b=tsl1
		&c=tsl2
		ENDSCAN
	SCAN REST
		a=alltrim(radek)
		&a=tsl
		ENDSCAN

	DO rcalcvyk WITH "t"
	SELE denik
	GO BOTT
*	dtdo=ctod("31.12."+str(year(denik.dat),4))
	SELE vykaz_zr
	SET CENT ON
	_naz=m.ggjme
	_uli=m.gguli
	_mes=m.ggmes
	_ico=m.ggico

	PRIV newadr
	newadr=left(alltrim(firma.naz)+space(12),12)
	DO vykaz.spr
	SET COMP ON
	IF !empty(m.newadr) AND file(m.gl_data+m.newadr+"\vykaz_zr.dbf")
		WAIT WIND NOWA "NaŸ¡t m minul‚ £Ÿetn¡ obdob¡."
		SELE 0
		_err=0
		ON ERROR _err=error()
		USE (m.gl_data+m.newadr+"\vykaz_zr.dbf") ALIAS vykmo
		ON ERROR &_onerr
		DO CASE
		  CASE m._err=3
			WAIT WIND NOWA "Soubor je pou§¡v n. Nelze naŸ¡st MO."
		  CASE m._err=0
			REPL ALL tmo WITH 0
			IF recc()=recc('vykaz_zr')
				SCAN ALL
					_rc=recno()
					SELE vykaz_zr
					=gorec(m._rc)
					REPL tmo WITH vykmo.tsl
					SELE vykmo
					ENDSCAN
				WAIT CLEAR
			  ELSE
				WAIT WIND NOWA "Struktura dat z minul‚ho obdob¡ je jin  ne§ z bØ§n‚ho obdob¡."
				ENDIF
			USE
		  OTHER
			WAIT WIND NOWA "Pıi pokusu o naŸten¡ dat vznikla chyba Ÿ."+strn(m._err)
			ENDCASE
	  ELSE
		WAIT WIND NOWA "Data z minul‚ho obdob¡ nebyla nalezena."
		ENDIF
	SET COMP OFF
	SET CONS OFF
	SELE vykaz_zr
	?m.nlq+space(20)+"    R O Z V A H A     "+space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(20)+" ve zkr cen‚m rozsahu "+space(10)+"³  Rozvaha éŸ PODZ 1-01  ³"
	?space(18)+"od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?space(20)+"(v celìch tis¡c¡ch KŸ)"
	?space(7)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "+m.cnon+"N zev a s¡dlo £Ÿetn¡ jednotky"+m.cnoff
	?space(7)+"³¬¡s.ı.³    I K F    ³ Rok  ³ MØs¡c ³  I ¬ O   ³    "+m.cnon+m._naz+m.cnoff
	?space(7)+"³  01  ³ 8 0 2 0 9 5 ³ "+str(year(m.tdtd),4)+" ³  "+str(month(m.tdtd),2)+"   ³ "+m._ico+" ³    "+m.cnon+m._uli+m.cnoff
	?space(7)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "+m.cnon+m._mes+m.cnoff
	?
	?m.draft+m.cnon
	SCAN ALL WHILE substr(radek,2)<"999"
		IF radek="R1  " OR radek="R23 "
			IF radek="R23 "
				?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
				??chr(12)
				ENDIF
			?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ¿"
			?space(7)+"³ OznaŸen¡ ³                           A K T I V A                             ³ü dek³     BØ§n‚ £Ÿetn¡ obdob¡     ³Minul‚ éO³"
			?space(7)+"³          ³                                                                   ³     ÃÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ´         ³"
			?space(7)+"³    a     ³                                b                                  ³  c  ³ Brutto  ³ Korekce ³  Netto  ³  Netto  ³"
			?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			ENDIF
		IF substr(radek,2)!="999"
			?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
				" ³ "+str0(tsl1,7)+" ³ "+str0(tsl2,7)+" ³ "+str0(tsl,7)+" ³ "+str0(tmo,7)+" ³"
		ENDIF
		DO CASE
			CASE year(m.tdtd)>=2002 AND inlist(radek, "R1  ", "R2  ", "R3  ", "R4  ", "R12 ", "R13 ", "R14 ", "R23 ", "R30 ", "R36 ", "R44 ", "R49 ", "R53 ")
					?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)>=2002 AND inlist(radek, "R5  ", "R15 ", "R24 ", "R31 ", "R37 ", "R45 ", "R50 ")
					?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)< 2002 AND inlist(radek, "R1  ", "R2  ", "R3  ", "R4  ", "R13 ", "R14 ", "R21 ", "R28 ", "R34 ", "R43 ", "R47 ", "R48 ", "R52 ")
					?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)< 2002 AND inlist(radek, "R5  ", "R15 ", "R22 ", "R29 ", "R35 ", "R44 ", "R49 ")
					?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
		ENDCASE
	ENDSCAN
	IF substr(radek,2)!="999"
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl1,7)+" ³ "+str0(tsl2,7)+" ³ "+str0(tsl,7)+" ³ "+str0(tmo,7)+" ³"
	ENDIF
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
	SKIP 1
	??chr(12)
	SCAN REST WHILE substr(radek,2)<"999"
		IF radek="R55 " OR radek="R84 "
			IF radek="R84 "
				?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
				??chr(12)
				ENDIF
			?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(7)+"³ OznaŸen¡ ³                           P A S I V A                             ³ü dek³  Stav v   ³  Stav v   ³"
			?space(7)+"³          ³                                                                   ³     ³ bØ§n‚m éO ³ minul. éO ³"
			?space(7)+"³    a     ³                                b                                  ³  c  ³     5     ³     6     ³"
			?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
		ENDIF
		IF substr(radek,2)!="999"
			?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
				" ³ "+str0(tsl,9)+" ³ "+str0(tmo,9)+" ³"
		ENDIF
		DO CASE
			CASE year(m.tdtd)>=2002 AND inlist(radek, "R55 ", "R56 ", "R58 ", "R63 ", "R67 ", "R70 ", "R71 ", "R76 ", "R92 ", "R96 ", "R97 ", "R100")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)>=2002 AND inlist(radek, "R59 ", "R64 ", "R68 ", "R72 ", "R77 ", "R84 ", "R93 ", "R98 ")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)< 2002 AND inlist(radek, "R54 ", "R55 ", "R58 ", "R63 ", "R67 ", "R70 ", "R71 ", "R72 ", "R76 ", "R93 ", "R97 ", "R98 ", "R102", "R103")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)< 2002 AND inlist(radek, "R56 ", "R59 ", "R64 ", "R68 ", "R73 ", "R77 ", "R84 ", "R94 ", "R99 ")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDCASE
		ENDSCAN
	IF substr(radek,2)!="999"
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl,9)+" ³ "+str0(tmo,9)+" ³"
	ENDIF
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	?
	?
	?
	?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(7)+"³ Odesl no dne:   ³ Podpis statut rn¡ho org nu ³ Osoba zodpovØdn  za £Ÿetnictv¡ ³ Osoba zodpovØdn  za £Ÿetn¡ ³"
	?space(7)+"³                 ³ nebo fyzick‚ osoby, kter   ³ (jm‚no a podpis)               ³ z vØrku (jm‚no a podpis)   ³"
	?space(7)+"³                 ³ je £Ÿetn¡ jednotkou        ³                                ³                            ³"
	?space(7)+"³                 ³                            ³                                ³                            ³"
	?space(7)+"³                 ³                            ³                                ³ Tel.:                      ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.cnoff
	SKIP 1
	??chr(12)
	?m.nlq+space(20)+		 " VíKAZ ZISKŞ A ZTRµT  "+space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(20)+		 " ve zkr cen‚m rozsahu "+space(10)+"³ Vìsledovka éŸ PODZ 2-01 ³"
	?space(18)+"od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?space(20)+		 "(v celìch tis¡c¡ch KŸ)"
	?space(7)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "+m.cnon+"N zev a s¡dlo £Ÿetn¡ jednotky"+m.cnoff
	?space(7)+"³¬¡s.ı.³    I K F    ³ Rok  ³ MØs¡c ³  I ¬ O   ³    "+m.cnon+m._naz+m.cnoff
	?space(7)+"³  01  ³ 8 0 4 0 9 5 ³ "+str(year(m.tdtd),4)+" ³  "+str(month(m.tdtd),2)+"   ³ "+m._ico+" ³    "+m.cnon+m._uli+m.cnoff
	?space(7)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "+m.cnon+m._mes+m.cnoff
	?
	?m.draft+m.cnon
	SCAN REST
		IF radek="V1  " OR radek="V24 "
			IF radek="V24 "
				?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
				??chr(12)
				ENDIF
			?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(7)+"³ OznaŸen¡ ³                             T E X T                               ³ü dek³ SkuteŸnost v £Ÿ. obdob¡ ³"
			?space(7)+"³          ³                                                                   ³     ÃÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ´"
			?space(7)+"³    a     ³                                b                                  ³  c  ³ sledovan‚m ³  minul‚m   ³"
			?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDIF
		IF substr(radek,2)!="999"
			?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
				" ³ "+str0(tsl,10)+" ³ "+str0(tmo,10)+" ³"
		ENDIF
		DO CASE
			CASE year(m.tdtd)>=2002 AND inlist(radek, "V1  ", "V2  ", "V3  ", "V7  ", "V10 ", "V11 ", "V22 ", "V31 ", "V32 ", "V35 ", "V36 ", "V38 ", "V41 ", "V42 ", "V43 ", "V44 ")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)>=2002 AND inlist(radek, "V4  ", "V8  ", "V33 ", "V39 ")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)< 2002 AND inlist(radek, "V1  ", "V2  ", "V3  ", "V7  ", "V10 ", "V11 ", "V22 ", "V31 ", "V32 ", "V35 ", "V36 ", "V37 ", "V39 ", "V42 ", "V43 ", "V44 ", "V45 ", "V46 ")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE year(m.tdtd)< 2002 AND inlist(radek, "V4  ", "V8  ", "V33 ", "V40 ")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDCASE
		ENDSCAN
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?
	?
	?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(7)+"³ Odesl no dne:     ³ Podpis statut rn¡ho org nu ³ Osoba zodpovØdn  za £Ÿetnictv¡ ³ Osoba zodpovØdn  za £Ÿetn¡ ³"
	?space(7)+"³                   ³ nebo fyzick‚ osoby, kter   ³ (jm‚no a podpis)               ³ z vØrku (jm‚no a podpis)   ³"
	?space(7)+"³                   ³ je £Ÿetn¡ jednotkou        ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³ Tel.:                      ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	??m.cnoff+m.draft+chr(12)
	SET CENT OFF
	USE
RETURN

**	Ostrì vìkaz v pln‚m rozsahu
PROCEDURE vk_pr_o
	SELE denik
	GO BOTT
	dtdo=ctod("31.12."+str(year(denik.dat),4))
	SELE vykaz_pr
	SET CENT ON
	_naz=m.ggjme
	_uli=m.gguli
	_mes=m.ggmes
	_ico=m.ggico

	PRIV newadr
	newadr=left(alltrim(firma.naz)+space(12),12)
	DO vykaz.spr
	SET COMP ON
	IF !empty(m.newadr) AND file(m.gl_data+m.newadr+"\vykaz_pr.dbf")
		WAIT WIND NOWA "NaŸ¡t m minul‚ £Ÿetn¡ obdob¡."
		SELE 0
		_err=0
		ON ERROR _err=error()
		USE (m.gl_data+m.newadr+"\vykaz_pr.dbf") ALIAS vykmo
		ON ERROR &_onerr
		DO CASE
		  CASE m._err=3
			WAIT WIND NOWA "Soubor je pou§¡v n. Nelze naŸ¡st MO."
		  CASE m._err=0
			REPL ALL tmo WITH 0
			IF recc()=recc('vykaz_pr')
				SCAN ALL
					_rc=recno()
					SELE vykaz_pr
					=gorec(m._rc)
					REPL tmo WITH vykmo.tsl
					SELE vykmo
					ENDSCAN
				WAIT CLEAR
			  ELSE
				WAIT WIND NOWA "Struktura dat z minul‚ho obdob¡ je jin  ne§ z bØ§n‚ho obdob¡."
				ENDIF
			USE
		  OTHER
			WAIT WIND NOWA "Pıi pokusu o naŸten¡ dat vznikla chyba Ÿ."+strn(m._err)
			ENDCASE
	  ELSE
		WAIT WIND NOWA "Data z minul‚ho obdob¡ nebyla nalezena."
		ENDIF
	SET COMP OFF
	SET CONS OFF
	SELE vykaz_pr
	?m.nlq+space(20)+"    R O Z V A H A     "+space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(20)+"   v pln‚m rozsahu    "+space(10)+"³  Rozvaha éŸ POD 1-01  ³"
	?space(20)+"    k "+dtoc(m.dtdo)+"      "+space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?space(20)+"(v celìch tis¡c¡ch KŸ)"
	?space(7)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "+m.cnon+"N zev a s¡dlo £Ÿetn¡ jednotky"+m.cnoff
	?space(7)+"³¬¡s.ı.³    I K F    ³ Rok  ³ MØs¡c ³  I ¬ O   ³    "+m.cnon+m._naz+m.cnoff
	?space(7)+"³  01  ³ 8 0 1 0 9 5 ³ "+str(year(m.dtdo),4)+" ³  12   ³ "+m._ico+" ³    "+m.cnon+m._uli+m.cnoff
	?space(7)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "+m.cnon+m._mes+m.cnoff
	?
	?m.draft+m.cnon
	SCAN ALL WHILE substr(radek,2)<"999"
		IF radek="R1  " OR radek="R30 "
			IF radek="R30 "
				?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
				??chr(12)
				ENDIF
			?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ¿"
			?space(7)+"³ OznaŸen¡ ³                           A K T I V A                             ³ü dek³     BØ§n‚ £Ÿetn¡ obdob¡     ³Minul‚ éO³"
			?space(7)+"³          ³                                                                   ³     ÃÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄ´         ³"
			?space(7)+"³    a     ³                                b                                  ³  c  ³ Brutto  ³ Korekce ³  Netto  ³  Netto  ³"
			?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			ENDIF
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl1,7)+" ³ "+str0(tsl2,7)+" ³ "+str0(tsl,7)+" ³ "+str0(tmo,7)+" ³"
		DO CASE
			CASE inlist(radek, "R1  ", "R2  ", "R3  ", "R11 ", "R21 ", "R28 ", "R35 ", "R41 ", "R50 ", "R54 ", "R55 ", "R59 ", "R60 ")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			CASE inlist(radek, "R4  ", "R12 ", "R22 ", "R29 ", "R36 ", "R42 ", "R51 ", "R56 ")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄ´"
			ENDCASE
		ENDSCAN
	?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
		" ³ "+str0(tsl1,7)+" ³ "+str0(tsl2,7)+" ³ "+str0(tsl,7)+" ³ "+str0(tmo,7)+" ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙ"
	SKIP 1
	??chr(12)
	SCAN REST WHILE substr(radek,2)<"999"
		IF radek="R62 " OR radek="R94 "
			IF radek="R94 "
				?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
				??chr(12)
				ENDIF
			?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(7)+"³ OznaŸen¡ ³                           P A S I V A                             ³ü dek³  Stav v   ³  Stav v   ³"
			?space(7)+"³          ³                                                                   ³     ³ bØ§n‚m éO ³ minul. éO ³"
			?space(7)+"³    a     ³                                b                                  ³  c  ³     5     ³     6     ³"
			?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDIF
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl,9)+" ³ "+str0(tmo,9)+" ³"
		DO CASE
			CASE inlist(radek, "R61 ", "R62 ", "R65 ", "R70 ", "R74 ", "R77 ", "R78 ", "R79 ", "R83 ", "R100", "R104", "R105", "R109", "R110")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE inlist(radek, "R63 ", "R66 ", "R71 ", "R75 ", "R80 ", "R84 ", "R91 ", "R101", "R106")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDCASE
		ENDSCAN
	?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
		" ³ "+str0(tsl,9)+" ³ "+str0(tmo,9)+" ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	?
	?
	?
	?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(7)+"³ Odesl no dne:   ³ Podpis statut rn¡ho org nu ³ Osoba zodpovØdn  za £Ÿetnictv¡ ³ Osoba zodpovØdn  za £Ÿetn¡ ³"
	?space(7)+"³                 ³ nebo fyzick‚ osoby, kter   ³ (jm‚no a podpis)               ³ z vØrku (jm‚no a podpis)   ³"
	?space(7)+"³                 ³ je £Ÿetn¡ jednotkou        ³                                ³                            ³"
	?space(7)+"³                 ³                            ³                                ³                            ³"
	?space(7)+"³                 ³                            ³                                ³ Tel.:                      ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.cnoff
	SKIP 1
	??chr(12)
	?m.nlq+space(20)+		 " VíKAZ ZISKŞ A ZTRµT  "+space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(20)+		 "   v pln‚m rozsahu    "+space(10)+"³ Vìsledovka éŸ POD 2-01 ³"
	?space(20)+"    k "+dtoc(m.dtdo)+"      "+space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?space(20)+		 "(v celìch tis¡c¡ch KŸ)"
	?space(7)+"ÚÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿    "+m.cnon+"N zev a s¡dlo £Ÿetn¡ jednotky"+m.cnoff
	?space(7)+"³¬¡s.ı.³    I K F    ³ Rok  ³ MØs¡c ³  I ¬ O   ³    "+m.cnon+m._naz+m.cnoff
	?space(7)+"³  01  ³ 8 0 3 0 9 5 ³ "+str(year(m.dtdo),4)+" ³  12   ³ "+m._ico+" ³    "+m.cnon+m._uli+m.cnoff
	?space(7)+"ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ    "+m.cnon+m._mes+m.cnoff
	?
	?m.draft+m.cnon
	SCAN REST
		IF radek="V1  " OR radek="V30 "
			IF radek="V30 "
				?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
				??chr(12)
				ENDIF
			?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(7)+"³ OznaŸen¡ ³                             T E X T                               ³ü dek³ SkuteŸnost v £Ÿ. obdob¡ ³"
			?space(7)+"³          ³                                                                   ³     ÃÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ´"
			?space(7)+"³    a     ³                                b                                  ³  c  ³ sledovan‚m ³  minul‚m   ³"
			?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDIF
		?space(7)+"³"+oznaceni+"³ "+text+" ³ "+right("00"+alltrim(substr(radek,2)),3)+ ;
			" ³ "+str0(tsl,10)+" ³ "+str0(tmo,10)+" ³"
		DO CASE
			CASE inlist(radek, "V1  ", "V2  ", "V3  ", "V7  ", "V10 ", "V11 ", "V16 ", "V28 ", "V31 ", "V35 ", "V46 ", "V47 ", "V51 ", "V52 ", "V54 ", "V57 ", "V58 ", "V59 ", "V60 ", "V61 ")
				?space(7)+"ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			CASE inlist(radek, "V4  ", "V8  ", "V12 ", "V32 ", "V48 ", "V55 ")
				?space(7)+"³          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			ENDCASE
		ENDSCAN
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?
	?
	?space(7)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(7)+"³ Odesl no dne:     ³ Podpis statut rn¡ho org nu ³ Osoba zodpovØdn  za £Ÿetnictv¡ ³ Osoba zodpovØdn  za £Ÿetn¡ ³"
	?space(7)+"³                   ³ nebo fyzick‚ osoby, kter   ³ (jm‚no a podpis)               ³ z vØrku (jm‚no a podpis)   ³"
	?space(7)+"³                   ³ je £Ÿetn¡ jednotkou        ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³                            ³"
	?space(7)+"³                   ³                            ³                                ³ Tel.:                      ³"
	?space(7)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	??m.cnoff+m.draft+chr(12)
	SET CENT OFF
	USE
RETURN

**	Pracovn¡ vìkaz na kontrolu
PROCEDURE vk_pr_p
	?"     A K T I V A"
	?"    ~~~~~~~~~~~~~"+m.draft+m.cnon
	SCAN ALL WHILE substr(radek,2)<"999"
		?oznaceni+"³ "+left(text,40)+"³"+alignr(substr(radek,2))+"³"+str(sl1,13,2)+" ³"+ ;
			str(sl2,13,2)+" ³"+str(sl,13,2)+" ³³"+ ;
			str(tsl1,7)+" ³"+str(tsl2,7)+" ³"+str(tsl,7)+" ³"
		ENDSCAN
	?oznaceni+"³ "+left(text,40)+"³"+alignr(substr(radek,2))+"³"+str(sl1,13,2)+" ³"+ ;
		str(sl2,13,2)+" ³"+str(sl,13,2)+" ³³"+ ;
		str(tsl1,7)+" ³"+str(tsl2,7)+" ³"+str(tsl,7)+" ³"
	SKIP 1
	?chr(12)
	?"     P A S I V A"
	?"    ~~~~~~~~~~~~~"
	SCAN REST WHILE substr(radek,2)<"999"
		?oznaceni+"³ "+left(text,40)+"³"+alignr(substr(radek,2))+"³"+str(sl,13,2)+" ³³"+ ;
			str(tsl,7)+" ³"
		ENDSCAN
	?oznaceni+"³ "+left(text,40)+"³"+alignr(substr(radek,2))+"³"+str(sl,13,2)+" ³³"+ ;
		str(tsl,7)+" ³"
	SKIP 1
	?chr(12)
	?"     V í S L E D O V K A"
	?"    ~~~~~~~~~~~~~~~~~~~~~"
	SCAN REST
		?oznaceni+"³ "+left(text,40)+"³"+alignr(substr(radek,2))+"³"+str(sl,13,2)+" ³³"+ ;
			str(tsl,7)+" ³"
		ENDSCAN
	?m.cnoff+chr(12)
	USE
RETURN

**	Dorovn vac¡ funkce pro PASIVA
PROCEDURE dor_pas
PARA plus
	b="x"
	c=iif(m.plus,-999999,999999)
	LOCA ALL FOR radek=="R95 "
	SCAN REST
		IF radek="R102"
			EXIT
			ENDIF
		IF sl=0
			LOOP
			ENDIF
		IF m.plus
			IF sl-tsl*1000 > c
				c=sl-tsl*1000
				b=radek
				ENDIF
		  ELSE
			IF sl-tsl*1000 < c
				c=sl-tsl*1000
				b=radek
				ENDIF
			ENDIF
		ENDSCAN
	IF b="x"
		WAIT WIND "Neum¡m zaokrouhlit vìkazy, neboœ nejsou vyu§ity ı dky R95-P102"
		RETURN
		ENDIF
	LOCA ALL FOR radek==b
	REPL tsl WITH iif(m.plus,tsl+1,tsl-1)
	&b=tsl
RETURN

**	PıepoŸ¡t n¡ tabulky
PROCEDURE rcalcvyk
PARA fl

	PRIV ssl1, ssl2, ssl, x, ppp
	pass=m.pass+1
	x=6
	FOR i=1 TO x
		WAIT WIND NOWA "PıepoŸet vìkaz… ("+strn(m.pass)+")  "+ ;
				repl('ş',i)+repl('',x-i)
		STORE 0 TO ssl1, ssl2, ssl
		ppp=.T.
		SCAN ALL
			IF substr(radek,2)="999"
				REPL &fl.sl WITH m.ssl, &fl.sl1 WITH m.ssl1, &fl.sl2 WITH m.ssl2
				STORE 0 TO ssl1, ssl2, ssl
				ppp=.F.
				LOOP
				ENDIF
			a=alltrim(radek)
			IF m.ppp
				b=a+"_1"
				c=a+"_2"

				IF empty(vsl1) OR left(vsl1,1)!='+'
					REPL &fl.sl1 WITH &b, &fl.sl2 WITH &c
				  ELSE
					REPL &fl.sl1 WITH eval(vsl1)
					IF left(vsl2,1)='+'
						REPL &fl.sl2 WITH eval(vsl2)
					ENDIF
					&b=&fl.sl1
					&c=&fl.sl2
					ENDIF
				REPL &fl.sl WITH &fl.sl1+&fl.sl2
			  ELSE
				IF empty(vsl) OR left(vsl,1)!='+'
					REPL &fl.sl WITH &a
				  ELSE
					REPL &fl.sl WITH eval(vsl)
					ENDIF
				ENDIF
			&a=&fl.sl
			ssl=m.ssl+&fl.sl
			ssl1=m.ssl1+&fl.sl1
			ssl2=m.ssl2+&fl.sl2
			ENDSCAN
		ENDFOR
	WAIT CLEAR
RETURN

PROCEDURE str0
PARA _nn, _cc, _dd
	IF type("m._dd")="L"
		_dd=0
		ENDIF
RETURN iif(m._nn=0, space(m._cc), str(m._nn, m._cc, m._dd))
