
*                    PROGRAM NA PODVOJNê
* È¨ETNICTV÷DEAC WIND trace

**	Definice horkÏch kl†ves

SET PROC TO ekont-pu

**	Tady byly globalni parametry firmy

* username=upper(getenv('USERNAME'))

PUBLIC llhere, tjazyk, kodkam, kodwin, commchar
llhere=.F.
tjazyk="¨eskÏ   "		&&	"AnglickÏ","NÿmeckÏ "

* K¢dov†n° pro WINDOWS
commchar=""
FOR i=0 TO 31
	commchar=m.commchar+chr(i)
	ENDFOR

*kodkam=[ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄]
kodlat=[¨ÅÇ‘Ñ“õüÿ∑ë÷ííéµêß¶ìî‡ÖÈÏôöÊïÌ¸ú†°¢£Â’ﬁ‚Á˝ÍË¨≠ÆØ∞±≤||||++||++++++--|-+||++==|=|=-=-++++||++]
kodwin=[»¸ÈÔ‰œçËÏÃ≈Âææƒ¡…ûéÙˆ”˘⁄˝÷‹äº›ÿù·ÌÛ˙Ú“˘‘ö¯‡¿¨≠ÆØ∞±≤||||++||++++++--|-+||++==|=|=-=-++++||++]

**	DalÁ° dÖleßitÇ glob†ln° promÿnnÇ
PUBLIC tstr,ttyp,tdto,tdtd,ttis,typtisk,pgbeg,pgend,page,addfaktur,filfaktur
PUBLIC nfirma, TypRad, tcis
STORE 0 TO tstr,ttyp, mimo, mod
mimo=space(80)
mod=1
tdto={}
tdtd={}
cis_od=1
cis_do=99999
tcis=1
uzucty="Ano"
ttis="Datumu"
typtisk="Oboj°"		&&	Typ tisku str†nek	Oboj°,LichÇ,SudÇ
pgbeg=1
pgend=9999
page=1
addfaktur=.F.
filfaktur=""
TypRad="FV,FP,PO,PV,BA,BD,VU"

DIME pdow(7)
pdow(2)="Pondÿl°"
pdow(3)="ÈterÏ  "
pdow(4)="St˝eda "
pdow(5)="¨tvrtek"
pdow(6)="P†tek  "
pdow(7)="Sobota "
pdow(1)="Nedÿle "

******************
**	DalÁ° promÿnnÇ
nfirma=''
end=.F.
masku="???"
maska="????"
yana="Ne "
vall="Ne "
DIME pstr(1),ptyp(1)
PUBLIC l_pol		&&	Obsah n° poloßky
l_pol=1
tuct=211			&&	Pro saldokonto
tana=0

***  PromÿnnÇ pro p˝izn†n° k DPH po 1.5.2004
PRIV DodatDPH, K550, K560, R560

DodatDPH="N"
K550=1
K560=1
R560=0
	
*****	Tisk dokladÖ - definice   *****
stn=1
cty=30
kli=space(25)
imzau={}
cod=1
cdo=99999

DO refscr

*******************
****	Hlavn° MENU
DEFINE POPUP hlm from 6,30 relative shadow title " Hlavn° menu - PÈ " color scheme 4
DEFINE BAR  2 OF hlm PROMPT " Èüetn° \<den°k "	SKIP FOR m.ggf=' ' OR m.gl_pri_pu=11 ;
	MESS "Èütov†n° a opravy v £üetn°m den°ku"
DEFINE BAR  3 OF hlm PROMPT " F\<aktury" ;
	SKIP FOR m.ggf=' ' OR (m.gl_pri_pu<=30 AND m.gl_pri_pu!=11) ;
	MESS "P˝ehled faktur."
DEFINE BAR  5 OF hlm PROMPT " Spe\<ci†ln° operace"	;
	SKIP FOR m.ggf=' ' OR m.gl_pri_pu<=30 ;
	MESS "Speci†ln° operace (ü°slov†n° dokladÖ, uz†vÿrky, importy, ...)."
DEFINE BAR 10 OF hlm PROMPT "\-"	SKIP FOR m.ggf=' '
DEFINE BAR 29 OF hlm PROMPT " ¨°\<seln°ky"	;
	SKIP FOR m.ggf=' ' OR m.gl_pri_pu<=30 ;
	MESS "Èütov† osnova, ü°seln°ky typÖ, st˝edisek, DPH."
DEFINE BAR 30 OF hlm PROMPT " \<TiskovÇ sestavy "	SKIP FOR m.ggf=' ' OR m.gl_pri_pu=11 ;
	MESS "TiskovÇ sestavy. Hlavn° kniha, £üetn° den°k, rozvaha, ..."
DEFINE BAR 60 OF hlm PROMPT "\-"	SKIP FOR m.ggf=' '
DEFINE BAR 90 OF hlm PROMPT " \<R Ö z n Ç"	SKIP FOR m.ggf=' ' OR m.gl_pri_pu=11 ;
	MESS "Oprava indexÖ datab†z°. Informace o £prav†ch programu."
DEFINE BAR 22 OF hlm PROMPT " \<Minul† obdob° " SKIP FOR m.ggf=' ' OR m.gl_pri_pu=11 ;
	MESS "Volba jinÇho roku £ütov†n° firmy"
DEFINE BAR 20 OF hlm PROMPT " \<Firma k £ütov†n° " SKIP FOR m.gl_pri_pu=11 ;
	MESS "Volba jinÇ firmy k £ütov†n°"
ON SELE POPU hlm DO hlm

DEFI WIND w_lline FROM 24,0 TO 24,80 NONE COLOR SCHEME 14

************************
****	Zaü†tek programu
a=substr(m.ggf,rat('\',m.ggf,2)+1)
a=left(a,len(a)-1)
IF !cldata1()
	WAIT WIND "BAD open"
	DO hlm_fir
ENDIF
	
SELE firma
SEEK a
IF found() 
	IF !fir_ote()
		DO hlm_fir
	ENDIF
  ELSE
	DO hlm_fir
ENDIF

*fp=fopen('..\Novinky.Tex')
*IF fp!=-1
*	WAIT WIND NOWA "Posledn° novinka v programu je z "+left(fgets(fp),6)
*	=fclose(fp)
*	ENDIF

_bar=1
DO WHILE .T.
	ACTIVATE POPUP hlm BAR _bar
	IF bar()=0
		DO hlm_kon
		EXIT
		ENDIF
	ENDDO
RETURN
**	Obnova obrazovky
PROCEDURE refscr
*	=uvod('i','i','i')
	DO backgr WITH 'main'
*	@1,0 SAY "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª"
*	@2,0 SAY "∫   PodvojnÇ £üetnictv° "+space(27)+right("    V "+ltrim(str(m.gl_verze,5,2)),8)+"  ≥  (C) JAKU 1993  ∫"
*	@3,0 SAY "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº"
	IF !empty(m.ggf)
		PRIVATE t
		t="Èütovan† firma: "+alltrim(m.ggjme)+" {"+str(m.ggrok,4)+"}"
		@ 0,int((scols()-len(m.t))/2) SAY m.t
		ENDIF
*	@ 2,scols()-13 SAY "Celkem :"+str(val(sys(1001))/1024,4)
*	@ 3,scols()-13 SAY "Pro RUN:"+str(memory(),4)
*	@ 4,scols()-13 SAY "Alloc. :"+str(val(sys(1016))/1024,4)
RETURN

**	Ukonüen° pr†ce programu a n†vrat do HLAVNIHO SYSTEMU
PROCEDURE hlm_kon
	DO wrt_stat WITH "EKONT"
	IF !empty(m.ggf)
		SAVE TO (m.ggf+"firma.mem") ALL LIKE gg*
		ENDIF
	DO cldata
	CLOSE ALL

	CLEAR FIELDS
	CLEAR GETS
	CLEAR MACROS
	CLEAR MENUS
	CLEAR PROGRAM
	CLEAR PROMPT
	CLEAR TYPEAHEAD
	CLEAR WINDOWS
RETURN

*****************************
****  HLAVNI PODPROGRAMY MENU
PROCEDURE hlm
	hl="hlm_"+left(onlya_z(prompt()),3)
	DO &hl
	DO refscr
RETURN

************************
**	Extern° tisky sestav
PROCEDURE hlm_tis
	DO tisky
RETURN
**	Èütov†n° do £üetn°ho den°ku
PROCEDURE hlm_etn
	LastTypDPH=0
	m.mmd='???'
	m.mmda='????'
	m.mdal='???'
	m.mdaa='????'
	m.mask='AND'
*	tdto={}
*	tdtd={}
	fdel=.T.
	SELE denik
	SET RELA TO denik.dph INTO 'dph'
	SET ORDER TO &ggdenor
	SET FILT TO !del
	_err=0
	ON ERROR _err=error()
	IF ggdenrc<>0
		GO ggdenrc
		ENDIF
	IF _err=5
		GO BOTT
	  ELSE
		IF _err<>0
			DO err WITH program(), _err, lineno()-5
			ENDIF
		ENDIF
	ON ERROR &_onerr
	_ln1=[:iif(del,'>',' ')+iif(flg,'*',' ')+dtoc(dat)+'≥'+str(typ,3)+'/'+str(cis,5)+'≥'+;
			right("00"+ltrim(str(md,3)),3)+str(mda,5)+'≥'+;
			right("00"+ltrim(str(dal,3)),3)+str(daa,5)+;
			'≥'+left(txt,14)+'≥'+str(cas,12,2)+'≥'+str(str,3)+'≥'+str(dph,4)]
	_ln11="//    Datum ≥ Doklad  ≥ M† d†ti≥  Dal   ≥  Text        ≥    ¨†stka  ≥Str≥ DPH"
	_ln2=[:iif(del,'>',' ')+iif(flg,'*',' ')+dtoc(dat)+'≥'+str(typ,3)+'/'+str(cis,5)+'≥'+;
			right("00"+ltrim(str(md,3)),3)+str(mda,5)+'≥'+;
			right("00"+ltrim(str(dal,3)),3)+str(daa,5)+;
			'≥'+left(txt,36)]
	_ln21="//    Datum ≥ Doklad  ≥ M† d†ti≥  Dal   ≥  Text"
	_ln3=[:iif(del,'>',' ')+iif(flg,'*',' ')+dtoc(dat)+'≥'+str(typ,3)+'/'+str(cis,5)+;
			'≥'+left(txt,40)+'≥'+str(cas,12,2)]
	_ln31="//    Datum ≥ Doklad  ≥  Text                                  ≥    ¨†stka"
	_ln4=[:iif(del,'>',' ')+iif(flg,'*',' ')+dtoc(dat)+'≥'+str(typ,3)+'/'+str(cis,5)+;
			'≥'+left(txt,16)+'≥'+str(cas,12,2)+' ≥ '+dtoc(adat)+" "+atime+" "+auser]
	_ln41="//    Datum ≥ Doklad  ≥  Text          ≥     ¨†stka  ≥ Za£ütoval"
	_ln5=[:iif(del,'>',' ')+iif(flg,'*',' ')+dtoc(dat)+'≥'+str(typ,3)+'/'+str(cis,5)+;
			'≥'+left(txt,12)+'≥'+str(cas,12,2)+' ≥'+mena+'≥'+str(cascm,12,2)+'≥'+str(kurz,12,4)]
	_ln51="//    Datum ≥ Doklad  ≥  Text      ≥     ¨†stka  ≥Mÿn≥ ¨†stka CM  ≥      Kurz"

	on_list_c(m.on_list_next)='#SET=K+'
	STORE '' TO _ln101,_ln102,_ln103,_ln104,_ln105,_ln106,_ln110
	IF m.gl_pri_pu>30
		_ln101='INS:		DO hlm_tov			## INS-NovÇ poloßky den°ku'
		_ln102='CR#:		DO opr_den WITH .T.	## ENTER-Oprava poloßky den°ku'
		_ln103='ALT+D#:		DO opr_del			## ALT+D-Smaz†n°/Obnova poloßky den°ku'
		_ln104='ALT+Del:	DO dele_all			## ALT+Del-Smaz†n° vÁech z†znamÖ'
		_ln105='ALT+O:		DO hlm_otoc			## ALT+O-Otoüen° MD <--> DAL'
		_ln106='F3#:		DO opr_den WITH .F.	## F3-Prohl°ßen° poloßky den°ku'
		_ln107='ALT+N#:		DO opr_FlMa			## ALT+N-Vyfiltrov†n° oznaüenÏch poloßek'
	  ELSE
		_ln102='CR#:		DO opr_den WITH .F.	## ENTER-Prohl°ßen° den°ku'
		ENDIF
	IF m.gl_pri_pu>=60
		_ln110='ALT+Q:	DO opr_chan			## ALT+Q-Hromadn† oprava £dajÖ'
		ENDIF
	_ln111='ALT+V:	DO opr_uma			## ALT+V-Odznaüen° vÁech z†znamÖ'
	_ln112='F5:		DO opr_cop1			## F5-Kop°rov†n° jednoho z†znamu'
	DO list_c WITH 1,-1,18,0," Opravy £üetn°ho den°ku ",;
		_ln1, _ln11, [|1],;
		_ln2, _ln21, [|1],;
		_ln3, _ln31, [|1],;
		_ln4, _ln41, [|1],;
		_ln5, _ln51, [|1],;
		"\ ** F1 - H E L P **    ENTER-Oprava    F7-T˝°dit podle ...",;
		"{DAT \<Datum, TYP \<Typ, MD \<M† d†ti, DAL D\<al, CAS=1 \<¨†stka, STR=1 \<St˝edisko, DPH=1 Ty\<p DPH}",;
		_ln101,_ln102,_ln106,_ln103,;
		_ln104,;
		'F2:		DO opr_fdel			## F2-Zobr./Nezobraz. smazanÇ poloßky',;
		'?#:		DO opr_find			## ?-Vyhled†n° nejasnost° ( ? )',;
		'ALT+F:		DO opr_filt			## ALT+F-Filtrov†n° den°ku',;
		'ALT+C#:	DO opr_kont			## ALT+C-Kontroly den°ku',;
		'ALT+A:		DO opr_sal			## ALT+A-Saldo konta',;
		'ALT+M:		DO opr_mark			## ALT+M-Oznaüen°/odznaüen° z†znamu',;
		_ln111,_ln112,_ln105,_ln110,_ln107,;
		[||1PF=iif('?'$txt,scheme(14,2),0)]

	@srow()-1,0 CLEAR
	SET FILT TO !del
	ggdenor=order()
	ggdenrc=recno()
	on_list_c(m.on_list_next)=.F.
RETURN
**	NovÇ z†znamy do den°ku
PROCEDURE hlm_tov
	SELE denik

* P˝ekladovÇ omezen° na 100 z†znamÖ pro demo verzi
	IF !m.FULLVER
		IF recc()>=100
			WAIT WIND "V DEMO verzi lze zadat pouze 100 z†znamÖ."
			RETURN
			ENDIF
		ENDIF

	SET RELA TO denik.dph INTO 'dph'
	SET ORDER TO
	SET FILT TO
	m.esc=.F.
	m.ok=1
	m.kalk_vysl=" "
	DO WHILE !m.esc AND ok=1
		DO uctden
		ENDDO
RETURN
**	P˝°prava z†znamu pro £ütov†n°
PROCEDURE uctden
	last_curobj=1
	ddSazbaDPH=0
	IF eof()
		GO BOTT
		IF eof()
			m.cis=1
			m.dat=date()
			m.datnakl=date()
			m.dtdph={}
			STORE 0 TO str,typ,md,mda,dal,daa,kurz,cascm
			m.mena=''
			m.txt=''
			m.zdph=0		&&	Vypoüte se do n° z†klad DPH
			m.ddph=0		&&	Vypoüte se do n° daÂ DPH
			m.dpmd=0		&&	Èüet MD pro z†pis dalÁ°ho z†znamu
			m.dpda=0		&&		 DAL
			m.dpam=0		&&	Analytika MD pro dalÁ° z†znam
			m.dpad=0		&&			  DAL
			ENDIF
		ENDIF
	IF !eof()
		IF md=m.ggdph .or. dal=m.ggdph
			SKIP -1
			ENDIF
		m.str=str
		m.typ=typ
		m.cis=cis
		m.dat=dat
		m.dtdph={}
		m.md=md
		m.mda=mda
		m.dal=dal
		m.daa=daa
		m.txt=txt
		m.mena=mena
		m.kurz=kurz
		m.zdph=0		&&	Vypoüte se do n° z†klad DPH
		m.ddph=0		&&	Vypoüte se do n° daÂ DPH
		m.dpmd=0		&&	Èüet MD pro z†pis dalÁ°ho z†znamu
		m.dpda=0		&&		 DAL
		m.dpam=0		&&	Analytika MD pro dalÁ° z†znam
		m.dpad=0		&&			  DAL
		ENDIF
	INSERT INTO denik (str,typ,cis,dat,md,mda,dal,daa,txt,mena,kurz,;
			del,adat,atime,auser) ;
		VALUES (m.str,m.typ,m.cis,m.dat,m.md,m.mda,m.dal,;
			m.daa,m.txt,m.mena,m.kurz,;
			.F.,date(),left(time(),5),m.gl_usernam)
	app=.T.
	edt=.T.
	esc=.T.
*	last_curobj=1
	DO while m.esc
		RELE WIND w_denik
		DO denik.spr
		IF m.esc
			IF m.ok!=2 AND ask(-1,-1,'A','Ukonüit zad†v†n° bez z†pisu tohoto z†znamu')
				DELE
				GO BOTT
				RELE WIND w_denik
				m.ok=0
				RETURN
			  ELSE
				IF m.ok=2
					RELEASE WIND w_denik
					EXIT
					ENDIF
				ok=1
				esc=.T.
				ENDIF
			ENDIF
		ENDDO
	kalk_vysl=str(cas,12,2)

**	Doplnÿn° z†znamu s DPH
	GO recno()
*	IF dph.dph>0 .AND. md!=m.ggdph .AND. dal!=m.ggdph
	IF m.ddSazbaDPH>0 .AND. md!=m.ggdph .AND. dal!=m.ggdph
		PRIV _dph
		_dph=dph
		REPL cas WITH zdph, dph WITH 0
		INSERT INTO denik (str,typ,cis,dat,;
				md,mda,dal,daa,txt,;
				cas,dph,zad,del,adat,atime,auser) ;
			VALUES (denik.str,denik.typ,denik.cis,m.dtdph,;
				m.dpmd,m.dpam,m.dpda,m.dpad,denik.txt,;
				m.ddph,m._dph,m.zdph,.F.,date(),left(time(),5),m.gl_usernam)
		LastTypDPH=m._dph
	ENDIF
RETURN

PROCEDURE opr_del
	IF del
		WAIT WIND NOWA "SmazanÏ z†znam byl obnoven."
	  ELSE
		WAIT WIND NOWA "Z†znam byl smaz†n."
		=beep(1400,150)
		=beep(3200,120)
		ENDIF
	REPL del WITH !del
RETURN
** Smaz†n° vÁech vybranÏch z†znamÖ
PROCEDURE dele_all
	IF ask(-1,-1,'N',"Jsou vybr†ny spr†vnÇ £daje k smaz†n°") AND ;
			ask(-1,-1,'N',"Opravdu smazat vybranÇ £daje")
		REPL ALL sou WITH 0
		REPL ALL del WITH .T., sou WITH 99
		ENDIF
RETURN
**	Otoüen° MD <--> DAL
PROCEDURE hlm_otoc
	IF !ask(-1,-1,'A',"Je opravdu spr†vnÿ nastaven vÏbÿr poloßek pro otoüen°")
		RETURN
		ENDIF
	PRIV _mmd, _mma, _order
	SELE denik
	_order=order()
	SET ORDER TO
	GO TOP
	SCAN ALL
		_mmd=md
		_mma=mda
		REPL md WITH dal, mda WITH daa, dal WITH m._mmd, daa WITH m._mma
		ENDSCAN
	SET ORDER TO &_order
RETURN

**	Oprava z†znamÖ den°ku
PROCEDURE opr_den
PARA _fedt		&&	P˝°znak prohl°ßen° nebo opravy
	last_curobj=1
	app=.F.
	ok=1
	esc=.F.
	edt= m._fedt AND denik.dat>m.ggdtlock
	IF m.edt
		WAIT WIND NOWA " OPRAVA ZµZNAMU V DEN÷KU "
		=beep(1400,150)
		=beep(3200,120)
		ENDIF
	DO WHILE abs(ok)=1 AND !m.esc AND !eof() AND !bof()
		edt= _fedt AND denik.dat>m.ggdtlock
		DO denik.spr
		IF abs(ok)=1 AND !m.esc
			SKIP ok
			ENDIF
		ENDDO
	RELE WIND w_denik
RETURN
**	Filtrov†n° den°ku
PROCEDURE opr_filt
	SET ORDER TO dat
	SET FILT TO !del
	DO opr_fil.spr
	IF esc
		RETURN
		ENDIF
	m.fi='!del'+iif(m.ttyp<>0,'.and.typ=m.ttyp','')+;
		iif(m.tstr<>0,'.and.str=m.tstr','')+;
		'.and.(like(m.mmd,str(md,3)).and.like(m.mmda,str(mda,4)) '+m.mask+;
		' like(m.mdal,str(dal,3)).and.like(m.mdaa,str(daa,4)))'+;
		'.and.dat>=m.tdto.and.dat<=m.tdtd'
	SET FILT TO &fi
	GO TOP
	__fltr=filter()
	m.refscr=.T.
RETURN
**	Smaz†n° z†znamu
PROCEDURE opr_fdel
	IF m.fdel
		SET FILT TO
	  ELSE
		SET FILT TO !del
		ENDIF
	__fltr=filter()		&&	Nastaven° vnit˝n° promÿnnÇ LIST_C
	fdel=!m.fdel
	refscr=.T.
RETURN

PROCEDURE opr_find
	IF at('?',txt)>0
		SKIP 1
		ENDIF
	DO wpr
	LOCA NEXT 999999 FOR at('?',txt)>0
	WAIT CLEAR
RETURN
**	Kontrola den°ku - p˝i˝azen° analytik
PROCEDURE opr_kont
	DO wpr
	SELE osn
	SET ORDER TO uct
*	Nastaven° flagu v OSN
	REPL flg1 WITH .F. ALL
	GO TOP
	DO WHILE !eof()
		IF ana=0
			_uc=uct
			SKIP 1
			IF _uc=uct
				SKIP -1
				REPL flg1 WITH .T.
			  ELSE
			  	SKIP -1
				ENDIF
			ENDIF
		SKIP 1
		ENDDO

	WAIT WIND NOWA "Prohled†v†m chybnÇ analytiky na  Mµ DµTI  (1)"
	SELE denik
	SET ORDER TO md
	SELE osn
	SET RELA TO str(uct,3)+'   0' INTO denik
	GO TOP
	SCAN ALL FOR flg1
		IF !eof('denik')
			WAIT WIND NOWA "Èüet  Mµ DµTI  m† v°ce analytik"
			SET RELA TO
			SELE denik
			SET ORDER TO
			RETURN
			ENDIF
		ENDSCAN

	WAIT WIND NOWA "Prohled†v†m chybnÇ analytiky na  DAL   (1)"
	SELE denik
	SET ORDER TO dal
	SELE osn
	SET RELA TO str(uct,3)+'   0' INTO denik
	GO TOP
	SCAN ALL FOR flg1
		IF !eof('denik')
			WAIT WIND NOWA "Èüet  DAL  m† v°ce analytik"
			SET RELA TO
			SELE denik
			SET ORDER TO
			RETURN
			ENDIF
		ENDSCAN

	SET RELA TO
	SELE denik
	SET ORDER TO
	WAIT WIND NOWA "Prohled†v†m chybnÇ analytiky na  Mµ DµTI  (2)"
	LOCA ALL FOR !seek(str(md,3)+str(mda,4),'osn')
	IF found()
		WAIT WIND NOWA "Nalezena chyba v analytice na  Mµ DµTI"
		RETURN
		ENDIF

	WAIT WIND NOWA "Prohled†v†m chybnÇ analytiky na  DAL      (2)"
	LOCA ALL FOR !seek(str(dal,3)+str(daa,4),'osn')
	IF found()
		WAIT WIND NOWA "Nalezena chyba v analytice na  DAL"
		RETURN
		ENDIF

	WAIT WIND NOWA "Nenalezena chyba."
RETURN
**	Hromadn† oprava £dajÖ
PROCEDURE opr_chan
	udaj="STR"
	obsah=""
	DO opr_chan.spr
*	?type('udaj'),m.udaj
*	?type('obsah'),m.obsah
	IF esc OR !ask(-1,-1,"N","Opravdu provÇst hromadnou zmÿnu")
		RETURN
		ENDIF
	WAIT WIND NOWA "Z†lohuji pÖvodn° vybran† data do DENIK_CH.BAK"
	COPY TO &ggf.\denik_ch.bak
	REPL &udaj WITH obsah ALL
	WAIT WIND NOWA "Hromadn† zmÿna provedena !"
	refscr=.T.
	GO TOP
RETURN
**	Oznaüen° z†znamu
PROCEDURE Opr_mark
	REPL flg WITH !flg
	IF !eof()
		SKIP 1
		ENDIF
	refscr=.T.
RETURN
**	Kop°rov†n° jednoho z†znamu s jeho oznaüen°m
PROCEDURE OPR_COP1
	PRIV r

	SCAT MEMVAR

	flg=.T.
	adat=date()
	atime=left(time(),5)
	auser=m.gl_usernam

	r=recno()
	INSERT INTO denik FROM MEMVAR
	=gorec(r)

	IF !eof()
		SKIP 1
		ENDIF
RETURN
**	Odznaüen° vÁech z†znamÖ
PROCEDURE Opr_Uma 
	r=recno()
	REPL ALL flg WITH .F.
	=gorec(r)
	refscr=.T.
RETURN
**	VÏpis zÖstatku £ütu - SALDO KONTO *3*
PROCEDURE opr_sal 
	DO saldo.spr
	IF m.esc
		RETURN
	ENDIF

	PRIV scmd, scdal, scmdcm, scdalcm 

	CREATE CURSOR saldo (uct C(3), ana C(4), txt C(55), ;
		cmd N(12,2), cdal N(12,2), cmdcm N(12,2), cdalcm N(12,2), ;
		zmd N(12,2), zdal N(12,2), zmdcm N(12,2), zdalcm N(12,2))
	INDEX ON uct+ana TAG uct
	INDEX ON cs(txt) TAG txt
	SET ORDER TO uct

	SELE osn
	SCAN ALL FOR uct=m.tuct 
		INSERT INTO saldo (uct, ana, txt) ;
			VALUES (str(osn.uct,3), str(osn.ana,4), osn.txt)
	ENDSCAN

	SELE denik
	SET FILT TO !del &_ff
	_rc=recno()
	_or=order()
	_rel=set('relation')

	SET RELA TO str(md,3)+str(mda,4) INTO saldo
	SET ORDER TO md
	GO TOP

	SCAN ALL FOR md=m.tuct
		IF dat<=m.tdtd
			REPL saldo.cmd WITH saldo.cmd+cas, saldo.cmdcm WITH saldo.cmdcm+cascm
		ENDIF
	ENDSCAN

	SET RELA TO str(dal,3)+str(daa,4) INTO saldo
	SET ORDER TO dal
	GO BOTT

	SELE saldo
	GO TOP
	SELE denik

	SCAN ALL FOR dal=m.tuct
		IF dat<=m.tdtd
			REPL saldo.cdal WITH saldo.cdal+cas, saldo.cdalcm WITH saldo.cdalcm+cascm
		ENDIF
	ENDSCAN
	
	SET ORDER TO &_or
	SET RELA TO &_rel
	=gorec(_rc)

	SELE saldo
	SUM ALL cmd, cdal, cmdcm, cdalcm TO scmd, scdal, scmdcm, scdalcm 
	SET ORDER TO uct
	GO TOP
	IF ana="   0"
		REPL cmd WITH scmd, cdal WITH scdal, cmdcm WITH scmdcm, ;
			cdalcm WITH scdalcm 
	ENDIF

	SCAN ALL
		IF cmd>cdal
			REPL zmd WITH cmd-cdal
		  ELSE
			REPL zdal WITH cdal-cmd
		ENDIF
		IF cmdcm>cdalcm
			REPL zmdcm WITH cmdcm-cdalcm
		  ELSE
			REPL zdalcm WITH cdalcm-cmdcm
		ENDIF
	ENDSCAN






	_ln1=[:ana+' ≥ '+left(txt,15)+'≥'+;
			str(cmd,12,2)+' ≥'+str(cdal,12,2)+' ≥'+;
			iif(cmd-cdal>0, str(cmd-cdal,12,2)+' ≥'+space(12),;
				space(12)+' ≥'+str(cdal-cmd,12,2))]
	_ln11="//  AU ≥ Text           ≥  Obrat  MD  ≥  Obrat DAL  ≥  ZÖst. MD   ≥  ZÖst. DAL "
	_ln2=[:ana+' ≥ '+left(txt,43)+'≥'+;
			iif(cmd-cdal>0, str(cmd-cdal,12,2)+' ≥'+space(12),;
				space(12)+' ≥'+str(cdal-cmd,12,2))]
	_ln21="//  AU ≥ Text                                       ≥  ZÖst. MD   ≥  ZÖst. DAL "
*		_ln1,_ln11,_ln2,_ln21



	DO list_c WITH -1,-1,10,13,;
		" Èüet "+str(m.tuct,4)+" k "+dtoc(m.tdtd)+' '+ ;
			iif(m.tstr=0,"","St˝edisko: "+str(m.tstr,3)),;
		":DEF=Saldo",;
		'F8#: #FILTER', ;
		'#FLT= NenulovÏ zÖstatek: cmd-cdal<>0, '+;
			'NulovÏ zÖstatek: cmd-cdal=0, NulovÏ obrat: cmd=0 AND cdal=0, '+;
			'NenulovÏ obrat: cmd!=0 OR cdal!=0 '

	USE
	SELE denik
	SET FILT TO !del
	WAIT CLEAR

RETURN
**	VÏpis zÖstatku £ütu - SALDO KONTO
PROCEDURE opr_sal1
	DO saldo.spr
	IF m.esc
		RETURN
		ENDIF

	PRIV smd, sdal, _rc, _se, _or, _ord1, _tx, _ln, _rel, _ff
	SELE osn
	_ord1=order()
	SET ORDER TO uct
	SET FILT TO
	REPL ALL cmd WITH 0, cdal WITH 0 FOR uct=m.tuct
	_ff=iif(m.tstr=0,"","AND str=m.tstr")
	SELE denik
	SET FILT TO !del &_ff
	_rc=recno()
	_or=order()
	_rel=set('relation')
	SET CONS OFF
	DO wpr
	SET RELA TO str(md,3)+str(mda,4) INTO osn
	SET ORDER TO md
	SEEK str(m.tuct,3)
	smd=0
	DO WHILE md=m.tuct
		IF dat<=m.tdtd
			REPL osn.cmd WITH osn.cmd+cas
			smd=m.smd+cas
			ENDIF
		SKIP 1
		ENDDO
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	SET ORDER TO dal
	SEEK str(m.tuct,3)
	sdal=0
	DO WHILE dal=m.tuct
		IF dat<=m.tdtd
			REPL osn.cdal WITH osn.cdal+cas
			sdal=m.sdal+cas
			ENDIF
		SKIP 1
		ENDDO
	SET ORDER TO &_or
	SET RELA TO &_rel
	=gorec(_rc)

	SELE osn
	SET FILT TO uct=m.tuct
	GO TOP
	REPL cmd WITH smd, cdal WITH sdal
	SET CONS ON
	WAIT CLEAR
	_ln1=[:str(ana,4)+' ≥ '+left(txt,15)+'≥'+;
			str(cmd,12,2)+' ≥'+str(cdal,12,2)+' ≥'+;
			iif(cmd-cdal>0, str(cmd-cdal,12,2)+' ≥'+space(12),;
				space(12)+' ≥'+str(cdal-cmd,12,2))]
	_ln11="//  AU ≥ Text           ≥  Obrat  MD  ≥  Obrat DAL  ≥  ZÖst. MD   ≥  ZÖst. DAL "
	_ln2=[:str(ana,4)+' ≥ '+left(txt,43)+'≥'+;
			iif(cmd-cdal>0, str(cmd-cdal,12,2)+' ≥'+space(12),;
				space(12)+' ≥'+str(cdal-cmd,12,2))]
	_ln21="//  AU ≥ Text                                       ≥  ZÖst. MD   ≥  ZÖst. DAL "

	DO list_c WITH -1,-1,10,13,;
		" Èüet "+str(m.tuct,4)+" k "+dtoc(m.tdtd)+' '+ ;
			iif(m.tstr=0,"","St˝edisko: "+str(m.tstr,3)),;
		_ln1,_ln11,_ln2,_ln21, ;
		'F8#: #FILTER', ;
		'#FLT= NenulovÏ zÖstatek: cmd-cdal<>0, '+;
			'NulovÏ zÖstatek: cmd-cdal=0, NulovÏ obrat: cmd=0 AND cdal=0, '+;
			'NenulovÏ obrat: cmd!=0 OR cdal!=0 '

	SET FILT TO
	SET ORDER TO &_ord1
	SELE denik
	SET FILT TO !del
	WAIT CLEAR
RETURN
**	VÏpis zÖstatku £ütu - SALDO KONTO *2*
PROCEDURE opr_sal2
	DO saldo.spr
	IF m.esc
		RETURN
	ENDIF

	PRIV smd, sdal, smdcm, sdalcm, _rc, _se, _or, _ord1, _tx, _ln, _rel, _ff
	STORE 0 TO smd, sdal, smdcm, sdalcm

	SET CONS OFF
	DO wpr

	SELE 0
	CREATE CURSOR saldo (uct N(3), ana N(4), txt C(55), ;
		cmd N(15,2), cdal N(15,2), cmdcm N(15,2), cdalcm N(15,2))
	INDEX ON str(uct,3)+str(ana,4) TAG uct
	INDEX ON cs(txt) TAG txt
	SET ORDER TO uct

	_ff=iif(m.tstr=0,"","AND str=m.tstr")

	SELE osn
	_ord1=order()
	SET ORDER TO uct
	SET FILT TO

	SELE denik
	SET FILT TO !del &_ff
	_rc=recno()
	_or=order()
	_rel=set('relation')

	SET RELA TO str(md,3)+str(mda,4) INTO saldo
	SET RELA TO str(md,3)+str(mda,4) INTO osn

*	SELE saldo
*	SET RELA TO str(saldo.uct,3)+str(saldo.ana,4) INTO osn ADDI
*	GO TOP
	
	SELE denik
	SET ORDER TO md
*	SEEK str(m.tuct,3)
	GO TOP

	SCAN ALL FOR md=m.tuct
		IF dat<=m.tdtd
*			IF !seek(str(md,3)+str(mda,4), "saldo")
			IF eof("saldo")
				INSERT INTO saldo (uct, ana, txt, cmd, cmdcm) VALUES ;
					(osn.uct, osn.ana, osn.txt, denik.cas, denik.cascm)
			  ELSE
				REPL saldo.cmd WITH saldo.cmd+cas, saldo.cmdcm WITH saldo.cmdcm+cascm
			ENDIF
			smd=m.smd+cas
			smdcm=m.smdcm+cascm
		ENDIF
	ENDSCAN

	SET RELA TO str(dal,3)+str(daa,4) INTO saldo
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	SET ORDER TO dal
*	SEEK str(m.tuct,3)
	GO BOTT

	SELE saldo
*	SET RELA TO str(saldo.uct,3)+str(saldo.ana,4) INTO osn ADDI
	GO TOP
	SELE denik

	SCAN ALL FOR dal=m.tuct
		IF dat<=m.tdtd
*			IF !seek(str(dal,3)+str(daa,4), "saldo")
			IF eof("saldo")
*				WAIT WIND str(dal,3)+str(daa,4)
				INSERT INTO saldo (uct, ana, txt, cdal, cdalcm) VALUES ;
					(osn.uct, osn.ana, osn.txt, denik.cas, denik.cascm)
			  ELSE
				REPL saldo.cdal WITH saldo.cdal+cas, saldo.cdalcm WITH saldo.cdalcm+cascm
			ENDIF
			sdal=m.sdal+cas
			sdalcm=m.sdalcm+cascm
		ENDIF
	ENDSCAN

	SET ORDER TO &_or
	SET RELA TO &_rel
	=gorec(_rc)

	SELE saldo
	GO TOP
	IF ana!=0
		SELE osn
		=seek(str(m.tuct,3)+str(0,4))
		SELE saldo
		INSERT INTO saldo (uct, ana, txt) VALUES (m.tuct, 0, osn.txt)
		GO TOP
	ENDIF
	REPL saldo.cdal WITH saldo.cdal+m.sdal, saldo.cdalcm WITH saldo.cdalcm+m.sdalcm, ;
		saldo.cmd WITH saldo.cmd+m.smd, saldo.cmdcm WITH saldo.cmdcm+m.smdcm	

BROW
USE

RETURN


	PRIV smd, sdal, _rc, _se, _or, _ord1, _tx, _ln, _rel, _ff
	SELE osn
	_ord1=order()
	SET ORDER TO uct
	SET FILT TO
	REPL ALL cmd WITH 0, cdal WITH 0 FOR uct=m.tuct
	_ff=iif(m.tstr=0,"","AND str=m.tstr")
	SELE denik
	SET FILT TO !del &_ff
	_rc=recno()
	_or=order()
	_rel=set('relation')
	SET CONS OFF
	DO wpr
	SET RELA TO str(md,3)+str(mda,4) INTO osn
	SET ORDER TO md
	SEEK str(m.tuct,3)
	smd=0
	DO WHILE md=m.tuct
		IF dat<=m.tdtd
			REPL osn.cmd WITH osn.cmd+cas
			smd=m.smd+cas
			ENDIF
		SKIP 1
		ENDDO
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	SET ORDER TO dal
	SEEK str(m.tuct,3)
	sdal=0
	DO WHILE dal=m.tuct
		IF dat<=m.tdtd
			REPL osn.cdal WITH osn.cdal+cas
			sdal=m.sdal+cas
			ENDIF
		SKIP 1
		ENDDO
	SET ORDER TO &_or
	SET RELA TO &_rel
	=gorec(_rc)

	SELE osn
	SET FILT TO uct=m.tuct
	GO TOP
	REPL cmd WITH smd, cdal WITH sdal
	SET CONS ON
	WAIT CLEAR
	_ln1=[:str(ana,4)+' ≥ '+left(txt,15)+'≥'+;
			str(cmd,12,2)+' ≥'+str(cdal,12,2)+' ≥'+;
			iif(cmd-cdal>0, str(cmd-cdal,12,2)+' ≥'+space(12),;
				space(12)+' ≥'+str(cdal-cmd,12,2))]
	_ln11="//  AU ≥ Text           ≥  Obrat  MD  ≥  Obrat DAL  ≥  ZÖst. MD   ≥  ZÖst. DAL "
	_ln2=[:str(ana,4)+' ≥ '+left(txt,43)+'≥'+;
			iif(cmd-cdal>0, str(cmd-cdal,12,2)+' ≥'+space(12),;
				space(12)+' ≥'+str(cdal-cmd,12,2))]
	_ln21="//  AU ≥ Text                                       ≥  ZÖst. MD   ≥  ZÖst. DAL "

	DO list_c WITH -1,-1,10,13,;
		" Èüet "+str(m.tuct,4)+" k "+dtoc(m.tdtd)+' '+ ;
			iif(m.tstr=0,"","St˝edisko: "+str(m.tstr,3)),;
		_ln1,_ln11,_ln2,_ln21, ;
		'F8#: #FILTER', ;
		'#FLT= NenulovÏ zÖstatek: cmd-cdal<>0, '+;
			'NulovÏ zÖstatek: cmd-cdal=0, NulovÏ obrat: cmd=0 AND cdal=0, '+;
			'NenulovÏ obrat: cmd!=0 OR cdal!=0 '

	SET FILT TO
	SET ORDER TO &_ord1
	SELE denik
	SET FILT TO !del
	WAIT CLEAR
RETURN
**	Vyfiltrov†n° oznaüenÏch z†znamÖ den°ku
PROCEDURE OPR_FLMA
	SET ORDER TO dat
	SET FILT TO !del AND flg
	GO TOP
	__fltr=filter()
	m.refscr=.T.
RETURN
****	¨°seln°ky MENU
PROCEDURE hlm_sel
	DEFINE POPUP cis FROM 8,35 RELATIVE SHADOW ;
		TITLE " ¨ ÷ S E L N ÷ K Y " COLOR SCHEME 4
	DEFINE BAR 20 OF cis PROMPT " \<Aktualizace £ütÖ " ;
		MESS "Vytvo˝en° a oprava £ütovÇho rozvrhu."
	DEFINE BAR 30 OF cis PROMPT " \<Nastaven° DPH pro rok 2012" ;
		MESS "Nastaven° sazeb DPH na 20% a 14%."
	DEFINE BAR 40 OF cis PROMPT " \<Typy dokladÖ " ;
		MESS "Vytvo˝en° a oprava ü°seln°ku typÖ dokladÖ."
	DEFINE BAR 50 OF cis PROMPT " \<St˝ediska " ;
		MESS "Vytvo˝en° a oprava ü°seln°ku st˝edisek."
	ON SELE POPU cis DO cis

	ACTI POPUP cis

	RELE POPU cis
RETURN

PROC cis
	hl="cis_"+left(onlya_z(prompt()),3)
	DO &hl
RETURN

**********************************
****	P˝id†n° a oprava st˝edisek
PROCEDURE cis_ste
	SELE str
	SET ORDER TO cis
	GO TOP
	DO list_c WITH -1,-1,12,0,;
		[str(str.cis,4)+'  ≥ '+str.txt],;
		" St˝ediska ",;
		"//¨°slo ≥ N†zev",;
		"\ F1-Help    ENTER-Oprava  INS-P˝id†n° st˝ediska   F7-T˝°dit podle ...",;
		'{cis,txt}',;
		'CR#:		DO str_pri WITH .F.		## ENTER-Oprava st˝ediska',;
		'INS:		DO str_pri WITH .T.		## INS-P˝id†n° st˝ediska',;
		'ALT+D#:	DO str_del				## ALT+D-Smaz†n° st˝ediska'
RETURN

PROCEDURE str_pri
PARA app
PRIV mx
	IF app
 		INSERT INTO str (cis) VALUES (cis+1)
		ENDIF
	DO str.spr
	IF m.app AND m.esc
		DELE
		PACK
		ENDIF
RETURN

PROCEDURE str_del
	DO wpr
	SELE denik
	SET FILT TO !del
	LOCA ALL FOR str.cis=str
	SELE str
	IF found('denik')
		WAIT WIND "St˝edisko nalezeno v den°ku. NELZE VYPUSTIT."
	  ELSE
		IF ask(-1,-1,'N',"Smazat st˝edisko ü. "+str(cis))
			DELE
			WAIT WIND NOWA 'St˝edisko vymaz†no'
			PACK
			m.refscr=.T.
			ENDIF
		ENDIF
RETURN
**	¨°seln°k typÖ dokladÖ
PROCEDURE cis_typ
	SELE typ
	SET ORDER TO cis
	GO TOP
	DO list_c WITH -1,-1,12,0, " Typy dokladÖ ", ;
		[:DEF=Typ], ;
		"\ F1-Help    ENTER-Oprava  INS-P˝id†n° typu   F7-T˝°dit podle ...",;
		'{cis,txt}',;
		'CR#:		DO typ_pri WITH .F.	## ENTER-Oprava typu',;
		'CR#:		DO typ_pri WITH .F.	## ENTER-Oprava typu',;
		'INS:		DO typ_pri WITH .T.	## INS-P˝id†n° typu',;
		'F6#:		DO typ_ren			## F6-P˝eü°slov†n° typu',;
		'ALT+D#:	DO typ_del			## ALT+D-Smaz†n° typu',;
		'ALT+C#:	DO typ_rad			## ALT+C-Kontrola posloupnosti ˝ady'
 

*	DO list_c WITH -1,-1,12,0,;
*		[str(typ.cis,4)+'  ≥ '+typ.txt+' ≥'+str(typ.puct,4)+' ≥ '+typ.ptxt],;
*		" Typy dokladÖ ",;
*		"//¨°slo ≥ Popis                          ≥ Uct ≥ Popis platby/£hrady",;
*		"\ F1-Help    ENTER-Oprava  INS-P˝id†n° typu   F7-T˝°dit podle ...",;
*		'{cis,txt}',;
*		'CR#:		DO typ_pri WITH .F.	## ENTER-Oprava typu',;
*		'INS:		DO typ_pri WITH .T.	## INS-P˝id†n° typu',;
*		'F6#:		DO typ_ren			## F6-P˝eü°slov†n° typu',;
*		'ALT+D#:	DO typ_del			## ALT+D-Smaz†n° typu',;
*		'ALT+C#:	DO typ_rad			## ALT+C-Kontrola posloupnosti ˝ady'
RETURN

PROCEDURE typ_rad
	PRIV _tp
	DO wpr
	_tp=typ.cis
	SELE 0
	CREA CURS rada (rada N(5), txt C(25))
	SELE denik
	_ord=order()
	_rc=IIF(eof(),recc(),recno())
	SET ORDER TO typ
	SEEK str(_tp,3)
	_min=cis
	INSERT INTO rada VALUES (_min, ">>> NejmenÁ° ü°slo <<<")
	_max=cis
	DO WHILE typ=_tp AND !eof()
		FOR i=_max+1 TO denik.cis-1
			INSERT INTO rada VALUES (i, "Chybÿj°c° ü°slo")
			ENDFOR
		_max=cis
		SKIP 1
		ENDDO
	INSERT INTO rada VALUES (_max, ">>> NejvÿtÁ° ü°slo <<<")
	WAIT CLEAR
	SELE rada
	DO list_c WITH -1,-1,12,0,;
		[str(rada,6)+' ≥ '+txt],;
		" ¸ada ü."+str(_tp,4)+" ",;
		"// ¨°slo ≥  Popis"
	SELE denik
	SET ORDER TO &_ord
	=gorec(m._rc)
	SELE rada
	USE
RETURN
PROCEDURE typ_pri
PARA app
PRIV mx
	IF app
		INSERT INTO typ (cis,txt) VALUES (cis+1,txt)
		ENDIF
	DO typ.spr
	IF m.app.and.m.esc
		DELE
		PACK
		ENDIF
RETURN

PROCEDURE typ_ren
	PRIV _ra
	_rt=cis
	DO typ_ren.spr
	IF m.esc
		RETURN
		ENDIF
	SET TALK ON
	SELE denik
	SET RELA TO
	SET FILT TO
	REPL ALL typ WITH m._rt FOR typ=typ.cis
	SET FILT TO !del
	SELE ufaktury
	REPL ALL typ WITH m._rt FOR typ=typ.cis
	SELE typ
	REPL cis WITH m._rt
	SET TALK OFF
	WAIT CLEAR
RETURN

PROCEDURE typ_del
	DO wpr
	SELE denik
	SET FILT TO !del
	SET ORDER TO typ
*	LOCA ALL FOR typ.cis=typ
	SEEK str(typ.cis,3)
	SELE typ
	IF found('denik')
		WAIT WIND "Typ dokladu nalezen v den°ku. NELZE VYPUSTIT."
	  ELSE
		IF ask(-1,-1,'N',"Smazat typ dokladu ü. "+str(cis))
			DELE
			WAIT WIND NOWA 'Typ vymaz†n'
			PACK
			m.refscr=.T.
			ENDIF
		ENDIF
RETURN
**	Nastaven° sn°ßenÇ sazby DPH
PROCEDURE CIS_NAS 
* sazbaDPH N 2
* 2150-2152
* 2250, 2350, 2450, 2550, 2650, 2750, 2850
* 3150-3151
* 3250-3251
* 3350-3351
* 3450-3451
* 3550-3551
* 3650-3651
* 3850
* 9950

SELE osn
SET FILTER TO uct=343
REPL ALL sazbadph WITH 14 ;
	FOR at(str(ana,4),"2150,2151,2152,2250,2350,2450,2550,2650,2750,2850,"+;
		"3150-3151,3250-3251,3350-3351,3450-3451,3550-3551,3650-3651,3850,"+;
		"9950")>0

REPL ALL sazbadph WITH 20 ;
	FOR at(str(ana,4),"2100,2101,2102,2200,2300,2400,2500,2600,2700,2800,"+;
		"3100-3101,3200-3201,3300-3301,3400-3401,3500-3501,3600-3601,3800,"+;
		"9900")>0

WAIT WIND NOWA "Sazby p˝enastaveny na rok 2012 (20% a 14%)"

SET FILT TO
	
RETURN
**	¨°seln°k DPH
PROCEDURE cis_dru
	SELE dph
	SET order TO cis
	SELE osn
	SET ORDER TO uct
	SET FILTER TO uct=m.ggdph .and. ana>0
	SET RELA TO ana INTO dph
	GO TOP
	DO list_c WITH -1,-1,12,0,;
		[str(osn.uct,3)+str(osn.ana,5)+' ≥ '+left(osn.txt,40)+' ≥ '+str(dph.dph,5,2)+" %"],;
		" Druhy  D P H ",;
		"// SÈ  AÈ  ≥ Text"+space(37)+"≥ Sazba",;
		"\ F1-Help    ENTER-Oprava sazby DPH    F7-T˝°dit podle ...",;
		'CR#:		DO dph_opr		## ENTER-Oprava sazby DPH'
	SET FILT TO
	SET RELA TO
RETURN

PROCEDURE dph_opr
	IF eof('dph')
		INSERT INTO dph (cis) VALUES (osn.ana)
		ENDIF
	SELE dph
	DO dph.spr
	SELE osn
RETURN
**	Èüetn° osnova
PROCEDURE cis_akt
	SELE osn
	SET ORDER TO uct
	m.fre=""
	GO TOP

	_ln10="// SÈ   AÈ  ≥  Text - ¨esky"+space(27)+" ≥DaÂ≥DPH≥Roz≥VÏkaz"
	_ln11=[:' '+right("000"+ltrim(str(uct,4)),3)+' '+;
			iif(ana=0,"    ",str(ana,4))+' ≥ '+left(txt,40)+;
			' ≥ '+dan+' ≥ '+dph+' ≥ '+roz+' ≥ '+rad+' '+slo+' ']
	_ln15="// SÈ   AÈ  ≥  Pozn†mka"+space(50)
	_ln16=[:' '+right("000"+ltrim(str(uct,4)),3)+' '+;
			iif(ana=0,"    ",str(ana,4))+' ≥ '+Poznamka]
	_ln20="// SÈ   AÈ  ≥  Text - Anglicky"+space(24)+" ≥DaÂ≥DPH≥Roz≥VÏkaz"
	_ln21=[:' '+right("000"+ltrim(str(uct,4)),3)+' '+;
			iif(ana=0,"    ",str(ana,4))+' ≥ '+left(txtA,40)+;
			' ≥ '+dan+' ≥ '+dph+' ≥ '+roz+' ≥ '+rad+' '+slo+' ']
	_ln30="// SÈ   AÈ  ≥  Text - Nÿmecky"+space(25)+" ≥DaÂ≥DPH≥Roz≥VÏkaz"
	_ln31=[:' '+right("000"+ltrim(str(uct,4)),3)+' '+;
			iif(ana=0,"    ",str(ana,4))+' ≥ '+left(txtN,40)+;
			' ≥ '+dan+' ≥ '+dph+' ≥ '+roz+' ≥ '+rad+' '+slo+' ']

	DO list_c WITH -1,-1,12,0,;
		" Seznam syntetickÏch £ütÖ ",;
		_ln11, _ln10, _ln16, _ln15, _ln21, _ln20, _ln31, _ln30, ; 
		"\ F1-Help  ENTER-Oprava  INS-P˝id†n° £ütu  F7-T˝°dit podle ...",;
		'CR#:		DO osn_pri WITH .F.		## ENTER-Oprava £ütu',;
		'INS:		DO osn_pri WITH .T.		## INS-P˝id†n° £ütu',;
		'F5#:		DO osn_brf				## F5-Souhr. oprava 1 £daje £ütÖ',;
		'F6#:		DO osn_ren				## F6-P˝eü°slov†n° analytiky',;
		'ALT+D#:	DO osn_del				## ALT+D-Smaz†n° £ütu'
RETURN

***	Èüetn° osnova
*PROCEDURE cis_akt
*	SELE osn
*	SET ORDER TO uct
*	m.fre=""
*	GO TOP
*	DO list_c WITH -1,-1,12,0,;
*		[' '+right("000"+ltrim(str(uct,4)),3)+' '+;
*			iif(ana=0,"    ",str(ana,4))+' ≥ '+left(txt,40)+;
*			' ≥ '+dan+' ≥ '+dph+' ≥ '+roz+' ≥ '+rad+' '+slo+' '],;
*		" Seznam syntetickÏch £ütÖ ",;
*		"// SÈ   AÈ  ≥  Text"+space(35)+" ≥DaÂ≥DPH≥Roz≥VÏkaz",;
*		"\ F1-Help  ENTER-Oprava  INS-P˝id†n° £ütu  F7-T˝°dit podle ...",;
*		'CR#:		DO osn_pri WITH .F.		## ENTER-Oprava £ütu',;
*		'INS:		DO osn_pri WITH .T.		## INS-P˝id†n° £ütu',;
*		'F5#:		DO osn_brf				## F5-Souhr. oprava 1 £daje £ütÖ',;
*		'F6#:		DO osn_ren				## F6-P˝eü°slov†n° analytiky',;
*		'ALT+D#:	DO osn_del				## ALT+D-Smaz†n° £ütu'
*RETURN

**	P˝id†n° £ütu do osnovy
PROCEDURE osn_pri
PARA app
	PRIV m._uct,m._dan,m._dph,m._roz
	IF app
		INSERT INTO osn (uct, ana, txt, txtA, txtN, dan, dph, roz, flg1, rad, slo) ;
			VALUES (uct,ana+1,txt,txtA, txtN, dan,dph,roz,.F.,rad,slo)
		ENDIF
	DO osnova.spr
	IF m.esc .AND. m.app
		DELE
		ENDIF
RETURN

PROCEDURE osn_brf
	DO osn_brf.spr
	IF m.ok<3
		WAIT WIND NOWA "Nelze hromadnÿ mÿnit ü°slo £ütu a analytiku"
		RETURN
		ENDIF
	=afield(pp)
	m.fre="free "+pp(m.ok,1)
	BROW FIELDS uct:H='SÈ',ana:H='AÈ',txt:45:H='Text',dan:P='@M A,N':H='DaÂ',dph:P='@M A,N':H='DPH',roz:P='@M A,P,-':H='Roz' &fre COLOR SCHEME 10
RETURN
** P˝eü°slov†n° analytiky
PROCEDURE osn_ren
	IF ana=0
		WAIT WIND NOWA "Nelze p˝eü°slov†vat z†kladn° £üet osnovy"
		RETURN
		ENDIF
	PRIV _ra,_pa
	_ra=ana
	DO osn_ren.spr
	IF m.esc
		RETURN
		ENDIF
	SET TALK ON
	SELE denik
	SET RELA TO
	SET FILT TO
	REPL ALL mda WITH m._ra FOR md=osn.uct AND mda=osn.ana
	REPL ALL daa WITH m._ra FOR dal=osn.uct AND daa=osn.ana
	SET FILT TO !del
	SELE ufaktury
	REPL ALL ana WITH m._ra FOR uct=osn.uct AND ana=osn.ana
	SELE osn
	REPL ana WITH m._ra
	SET TALK OFF
	WAIT CLEAR
RETURN

PROCEDURE osn_del
	DO wpr
	IF ana=0
		PRIV _uc,_rc
		_uc=uct
		_rc=recno()
		COUNT ALL FOR _uc=uct TO _uc
		=gorec(m._rc)
		IF _uc>1
			WAIT WIND "Èüet obsahuje analytiky. NELZE VYPUSTIT Z OSNOVY"
			RETURN
			ENDIF
		ENDIF
	SELE denik
	_ord=order()
	SET FILT TO !del
*	LOCA ALL FOR (md=osn.uct.AND.mda=osn.ana) .OR.;
*			(dal=osn.uct.AND.daa=osn.ana)
	SET ORDER TO md
	SEEK str(osn.uct,3)+str(osn.ana,4)
	_fom=found()
	SET ORDER TO dal
	SEEK str(osn.uct,3)+str(osn.ana,4)
	_fod=found()
	SET ORDER TO &_ord
	SELE OSN
	IF _fod OR _fom
		WAIT WIND "Èüet nalezen v den°ku. NELZE VYPUSTIT Z OSNOVY"
	  ELSE
		IF ask(-1,-1,'N',"Smazat £üet ü. "+;
				right('00'+ltrim(str(uct)),3)+str(ana,5))
			DELE
			WAIT WIND NOWA 'Èüet vymaz†n'
			m.refscr=.T.
			ENDIF
		ENDIF
RETURN
**	LIST_C pro text
PROCEDURE ltext
	PRIV m.sel
	sel=SELE()
	SELE text
	SET ORDER TO text
	DO list_c WITH -1,-1,15,0,;
		[text.text],;
		" PopisnÇ texty ",;
		"//  Text",;
		"\ F1-Help   ENTER-VÏbÿr   F4-Oprava   INS-P˝id†n° textu   F7-T˝°dit podle ...",;
		'F4#:		DO text_pri WITH .F.		## F4-VÏbÿr textu',;
		'INS:		DO text_pri WITH .T.		## INS-P˝id†n° textu',;
		'ALT+D#:	DO text_del					## ALT+D-Smaz†n° textu'
	SELE (m.sel)
	t=text.text
	a=at("##",t)
	IF a>0
		t=rtrim(left(t,a-1)+alltrim(str(denik.cis))+substr(t,a+2))
		ENDIF
RETURN t
**	P˝id†n° textÖ
PROCEDURE text_pri
PARA app
PRIV mx
	IF app
		INSERT INTO text (text) VALUES (denik.txt)
		ENDIF
	DO text.spr
	IF m.app.and.m.esc
		DELE
		ENDIF
RETURN

PROCEDURE text_del
	DELE
RETURN
**	LIST_C pro nab°dku datumÖ
*	t - .T. - poü†teün° datumy, .F. - koncovÇ datumy
PROCEDURE ldate   
PARA p, t
	PRIV r, o, s, fd, ld, d1, t1, d2, t2
	s=sele()

	SELE denik
	r=recno()
	o=order()
	SET ORDER TO dat
	GO TOP
	fd=dat
	GO BOTT
	ld=dat
	=gorec(r)
	SET ORDER TO &o

	SELE 0

	CREAT CURS ldate (ldatum D, popisdat C(28))

	IF !t
		INSERT INTO ldate VALUES (date(), "DneÁn° datum")
		ENDIF

	INSERT INTO ldate VALUES (iif(t,m.fd,m.ld), ;
			iif(t,"Prvn° datum v den°ku","Posledn° datum v den°ku"))

	d1=gomonth(ctod("01."+right('0'+strn(int((month(date())-1)/3)*3+1),2)+;
		"."+strn(year(date()))),-3)
	d2=gomonth(m.d1,3)-1
	INSERT INTO ldate VALUES (iif(t,m.d1,m.d2), ;
			iif(t,"Zaü†tek minulÇho ütvrtlet°","Konec minulÇho ütvrtlet°"))

	d1=ctod("01.01."+strn(year(date())))
	d2=ctod("31.12."+strn(year(date())))
	INSERT INTO ldate VALUES (iif(t,m.d1,m.d2), ;
			iif(t,"Prvn° den roku","Posledn° den roku"))

	INSERT INTO ldate VALUES (p, "PÖvodn° £daj")

	d1=ctod("01."+substr(dtos(date()),5,2)+'.'+strn(year(date())))
	d2=gomonth(d1,1)-1
	INSERT INTO ldate VALUES (iif(t,m.d1,m.d2), ;
			iif(t,"Prvn° den v mÿs°ci","Posledn° den v mÿs°ci"))

	GO TOP

	DO list_c WITH -1,-1,10,0,;
		[dtoc(ldatum)+' ≥ '+popisdat],;
		" VÏbÿr obdob° "+iif(t,"OD","DO"),;
		"//  Datum    ≥ Popis",;
		"\ F1-Help    ENTER-VÏbÿr datumu",;
		'CR#:	EXIT			## ENTER-VÏbÿr datumu'
	p=ldatum
	USE
	SELE (s)
RETURN p
**	LIST_C pro St˝edisko
PROCEDURE lstr
	PARA ud
	PRIV m.sel
	sel=SELE()
	SELE str
	SET ORDER TO cis
	SEEK m.ud
	DO list_c WITH -1,-1,10,0,;
		[str(str.cis,4)+'  ≥ '+str.txt],;
		" St˝ediska ",;
		"//¨°slo ≥ N†zev",;
		"\ F1-Help    ENTER-VÏbÿr st˝ediska    F7-T˝°dit podle ...",;
		'{cis,txt}',;
		'CR#:	m.toexit=.T.			## ENTER-VÏbÿr st˝ediska',;
		'F4#:	DO str_pri WITH .F.		## F4-Oprava st˝ediska',;
		'INS:	DO str_pri WITH .T.		## INS-P˝id†n° st˝ediska'
	SELE (m.sel)
RETURN str.cis
**	LIST_C pro Typ
PROCEDURE ltyp
	PARA ud
	PRIV m.sel
	sel=SELE()
	SELE typ
	SET ORDER TO cis
	SEEK m.ud
	DO list_c WITH -1,-1,10,0,;
		[str(typ.cis,4)+'  ≥ '+typ.txt],;
		" Typy dokladÖ ",;
		"//¨°slo ≥ N†zev",;
		"\ F1-Help    ENTER-VÏbÿr typu dokladu    F7-T˝°dit podle ...",;
		'{cis,txt}',;
		'CR#:		toexit=.T.			## ENTER-VÏbÿr typu',;
		'INS:		DO typ_pri WITH .T.	## INS-P˝id†n° typu',;
		'F4#:		DO typ_pri WITH .F.	## F4-Oprava typu',;
		'ALT+K#:	DO typ_rad			## ALT+K-Kontrola posloupnosti ˝ady'
	SELE (m.sel)
RETURN typ.cis

******************
**	LIST_C pro DPH
PROCEDURE ldph
	PARA ud
	PRIV m.sel
	sel=sele()
	SELE dph
	SET ORDER TO cis
	SELE osn
	SET ORDER TO uct
	SET FILT TO uct=m.ggdph .and. ana>0
	SET RELA TO ana INTO dph
 	LOCATE FOR osn.ana=m.ud
	DO list_c WITH -1,-1,12,0,;
		[str(osn.ana,4)+' ≥ '+left(osn.txt,40)+' ≥ '+str(dph.dph,5,2)+" %"],;
		" Druhy  D P H ",;
		"//¨°s. ≥ Text"+space(37)+"≥ Sazba",;
		"\ F1-Help    ENTER-VÏbÿr typu DPH    F7-T˝°dit podle ..."
	SET FILT TO
	SET RELA TO
	SELE (m.sel)
RETURN osn.ana
**	LIST_C pro n†zev £ütu a analytiku
*	uct - ü°slo £ütu
*	flg	- .T.=vrac° ü°slo £ütu,  .F.=vrac° analytiku
PROCEDURE luct
	PARA uct,flg
	PRIV m.sel, ord
	sel=sele()
	SELE osn
	ord=order()
	IF m.flg
		SET FILT TO osn.ana=0
		SET ORDER TO uct
		SEEK str(m.uct,3)+'   0'
	  ELSE
		SET FILT TO m.uct=osn.uct AND osn.ana!=0
		GO TOP
		IF eof()
			SET FILT TO m.uct=osn.uct
			ENDIF
		ENDIF

	IF m.flg
		_top=" ¨°sla £ütÖ "
		_ln11=[:str(osn.uct,4)+' ≥ '+osn.txt]
		_ln10="//Èüet ≥ Text"
		_ln16=[:str(osn.uct,4)+' ≥ '+osn.poznamka]
		_ln15="//Èüet ≥ Poznamka"
		_hlp="\ F1-Help    ENTER-VÏbÿr £ütu   F7-T˝°dit podle ..."
	ELSE
		_top=" Èüet ü."+str(m.uct,3)+' '
		_ln11=[:str(osn.ana,4)+' ≥ '+osn.txt]
		_ln10="//Èüet ≥ Text"
		_ln16=[:str(osn.ana,4)+' ≥ '+osn.poznamka]
		_ln15="//Èüet ≥ Poznamka"
		_hlp="\ F1-Help    ENTER-VÏbÿr analytiky   F7-T˝°dit podle ..."
	ENDIF

	DO list_c WITH -1,-1,12,0,;
		m._top, ;
		m._ln11, m._ln10, m._ln16, m._ln15, ; 
		m._hlp, ;
		iif(m.flg,'','INS:	DO luct_pri		## INS-P˝id†n° £ütu'),;
		'F4#:	DO osn_pri WITH .F.			## F4-Oprava £ütu',;
		'CR#:	m.toexit=.T.				## ENTER-VÏbÿr '+;
			iif(m.flg,'£ütu','analytiky')
	SET FILT TO
	IF flg
		SET ORDER TO &ord
		ENDIF
	SELE (m.sel)
RETURN iif(m.flg,osn.uct,osn.ana)
**	P˝id†n° £ütu do osnovy
PROCEDURE luct_pri
	PRIV _uct,_ana,_rc, _fi
	_uct=uct
	_ord=order()
	_rc=recno()
	_fi=filter()
	SELE 0
	CREA CURS rada (rada N(5), txt C(30))
	SELE osn
	SET FILT TO
	SET ORDER TO uct
	SEEK str(_uct,3)+'   0'
	_min=1
	_max=0
	DO WHILE uct=_uct AND !eof()
		FOR i=_max+1 TO osn.ana-1
			INSERT INTO rada VALUES (i, "VolnÇ ü°slo")
			ENDFOR
		_max=ana
		SKIP 1
		ENDDO
	INSERT INTO rada VALUES (_max+1, ">>> Posledn° volnÇ ü°slo <<<")
	SELE rada
	GO TOP
	REPL txt WITH ">>> Prvn° volnÇ ü°slo <<<"
	DO list_c WITH -1,-1,12,0,;
		[str(rada,6)+' ≥ '+txt],;
		" VolnÇ analytiky £ütu"+str(_uct,4)+" ",;
		"// ¨°slo ≥  Popis"
	_ana=rada
	USE
	SELE osn
	SET ORDER TO &_ord
	SET FILT TO &_fi
	=gorec(_rc)
	IF esc
		RETURN
		ENDIF
	INSERT INTO osn (uct, ana, txt, dan, dph, roz, rad, slo, flg1) ;
		VALUES (uct, _ana, txt, dan, dph, roz, rad, slo, .F.)
	DO osnova.spr
	IF m.esc
		DELE
		ENDIF
	esc=.F.
RETURN
**	KNIHA FAKTUR
PROCEDURE HLM_FAK

	DEFINE POPUP khf FROM 8,40 RELATIVE SHADOW ;
		TITLE " Kniha z†vazkÖ a pohled†vek " COLOR SCHEME 4
	DEFINE BAR 10 OF khf PROMPT " \<Seznam faktur" ;
		MESS "Seznam z†vazkÖ a pohled†vek - oprava, maz†n°, p˝id†v†n°."
	DEFINE BAR 14 OF khf PROMPT "\-"
	DEFINE BAR 30 OF khf PROMPT " \<Tisky " ;
		MESS "TiskovÇ seznamy z knihy faktur."


	ON SELE POPU khf DO khf

	ACTI POPUP khf

	RELE POPU khf
RETURN
**	Roz˝azen° menu Knihy z†vazkÖ a pohled†vek
PROCEDURE KHF     
	DO CASE
		CASE bar()=10	&&	Kniha faktur
			DO fak_sez

		CASE bar()=30	&&	Tisky z knihy faktur
			DO l_ftisk
*			DO fak_tisk

		CASE bar()=88	&&	Naüten° dat z p˝enosovÇho souboru
			IF ask(-1,-1,"N", "Opravdu p˝evÇst importn° datab†ze do den°ku.") ;
					AND ask(-1,-1,"N", "Do den°ku budou hromadnÿ p˝id†na data. Opravdu pokraüovat.")
				HIDE POPUP ALL
				DO (alltrim(m.ggimport)+"import.fxp") WITH alltrim(m.ggimport)
				SHOW POPUP hlm, spe
				WAIT CLEAR
				DO refscr
				ENDIF

		ENDCASE

RETURN
**	Kniha faktur - opravy
PROCEDURE fak_sez
	PRIV _se,_ce
	_ce=SYS(2001, "CENTURY")
	_se=sele()
	SET CENT OFF
	SELE ufaktury
	SET FILT TO &filfaktur
*	SET NEAR ON
	SEEK str(denik.typ,3)+str(denik.cis,5)
	IF eof()
		GO BOTT
		ENDIF
*	SET NEAR OFF

	_ln1=[:str(typ,3)+' ≥'+str(cis,5)+'≥ '+dtoc(vys)+' ≥ '+dtoc(spl)+' ≥ '+ ;
			dtoc(zap)+' ≥ '+var+'≥ '+kos+' ≥ '+str(cas,14,2)+' ']
	_ln2=[:str(typ,3)+' ≥'+str(cis,5)+'≥ '+ ;
			dtoc(zap)+' ≥ '+var+'≥'+str(cas,14,2)+' ≥ '+left(odb,24)]
	_ln3=[:str(typ,3)+' ≥'+str(cis,5)+'≥ '+dtoc(vys)+' ≥ '+dtoc(spl)+' ≥ '+ ;
			dtoc(zap)+' ≥ '+var+'≥ '+kos+' ≥ '+str(cascm,14,2)+' ']

	DO list_c WITH -1,-1,12,0,;
		" F a k t u r y ",;
		m._ln1,;
		"//Typ ≥ ¨is.≥ Vystavena≥ Splatn†  ≥ Zaplacena≥ Var. symbol ≥KoÁ≥ ¨†stka",;
		m._ln2,;
		"//Typ ≥ ¨is.≥ Zaplacena≥ Var. symbol ≥   ¨†stka      ≥ Odbÿratel/Dodavatel     ",;
		m._ln3,;
		"//Typ ≥ ¨is.≥ Vystavena≥ Splatn†  ≥ Zaplacena≥ Var. symbol ≥KoÁ≥ ¨†s CM",;
		"\ F1-Help   INS-P˝id†n°   ENTER-Oprava   ALT+P-Proplacen° faktury",;
		'F8#: #FILTER', ;
		'#FLT= NezaplacenÇ faktury: empty(zap) ', ;
		'INS:		DO l_fopr WITH .T.		## INS-P˝id†n° faktury',;
		'CR#:		DO l_fopr WITH .F.		## ENTER-Oprava faktury',;
		'ALT+A:		DO l_aktodb				## ALT+A-Doplnÿn° n†zvÖ odb./dodav.',;
		'ALT+F:		DO l_filf				## ALT+F-Filtr zaplacen°',;
		'ALT+P#:	DO l_fzap				## ALT+P-Proplacen° faktury',;
		'TAB#:		DO l_fkos				## Tab-Nastaven°/zruÁen° koÁilky',;
		'ALT+D#:	DO l_fvym				## ALT+D-Vymaz†n° faktury',;
		'CTRL+CR:	EXIT					## Ctrl+ENTER-Ukonüen°',;
		'ALT+T#:	DO l_ftisk				## Tisk sestavy z knihy faktur'

*	SET ORDER TO typcis
	SELE (_se)
	SET CENT &_ce
	SHOW GETS
RETURN

**	Opravy v knize faktur
PROCEDURE l_fopr
* app=.T. - automatickÇ p˝id†n° faktury
PARA app
PRIV u, a, v, i, x, w
STORE 0 TO a,u,x
	IF app
		IF typ.puct=denik.md
			u=denik.md
			a=denik.mda
			ENDIF
		IF typ.puct=denik.dal
			u=denik.dal
			a=denik.daa
			ENDIF
* Generov†n° variabiln°ho symbolu dle masky
		w=gen_vs(typ.cr_vs, denik.cis)

		PRIV _odb
		_odb=''
		IF ufaktury.ana>0
			_odb=lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct')
		ENDIF

		INSERT INTO ufaktury (typ, cis, uct, ana, odb, vys, spl, cas, var, ;
				cascm, mena, kurz) ;
			VALUES (denik.typ, denik.cis, u, a, m._odb, denik.dat, denik.dat+14, ;
				denik.cas, m.w, denik.cascm, denik.mena, denik.kurz)
				
		ENDIF
	DO faktury.spr
	IF esc AND app AND ask(-1,-1,'A',"Opravdu zruÁit z†pis faktury")
		DELE
		RETURN
		ENDIF
	PRIV _rc, _co, _tp, _ci
	_rc=recno()
	_tp=typ
	_ci=cis
	_vys=year(vys)
	CALC cnt() TO _co FOR typ=_tp AND cis=_ci AND year(vys)=_vys
	=gorec(_rc)
	IF _co>1
		WAIT WIND NOWA "Faktura je uvedena v knize jiß "+strn(_co)+" x"
		ENDIF
	refscr=.T.
RETURN
**	Generov†n° variabiln°ho symbolu dle masky
PROCEDURE GEN_VS  
PARA v, cisfak
PRIV w, i, x
	x=0
	v=v+" "
	w=""
	FOR i=1 TO len(v)-1
		IF substr(v,i,1)="#"
			x=x+1
			LOOP
			ENDIF
*			#-ky skonüily
		IF x>1
			w=w+right(repl("0",20)+strn(m.cisfak),x)
			x=0
			ENDIF
		IF x=1
			w=w+strn(m.cisfak)
			x=0
			ENDIF
		IF substr(v,i,1)=" "
			LOOP
			ENDIF
		w=w+substr(v,i,1)
		ENDFOR

RETURN w
**	Proplacen° faktury
PROCEDURE l_fzap
IF m.gl_pri_pu=11
	WAIT WIND NOWA "Pro tuto funkci nem†te dostateünou prioritu."
	RETURN
ENDIF

PRIV oricast, oricastCM, oriKurz
	oricast=ufaktury.cas
	oricastCM=ufaktury.CasCM
	orikurz=ufaktury.kurz
	REPL zap WITH denik.dat
	DO faktury.spr
	IF ask(-1,-1,"A","Vyplnit fakturu do den°ku")
		PRIV t
		t=""

		IF seek(ufaktury.typ,"typ")
			t=typ.ptxt
			ENDIF

		DO CASE
		  CASE denik.md=ufaktury.uct
			REPL denik.mda WITH ufaktury.ana
			REPL denik.txt WITH rtrim(m.t)+strn(ufaktury.cis)
		  CASE denik.dal=ufaktury.uct
			REPL denik.daa WITH ufaktury.ana
			REPL denik.txt WITH rtrim(m.t)+strn(ufaktury.cis)
			ENDCASE
		REPL denik.rtyp WITH ufaktury.typ
		REPL denik.rcis WITH ufaktury.cis
		IF ufaktury.casCM=0
			REPL denik.cas WITH ufaktury.cas
		  ELSE
			REPL denik.cas WITH m.orikurz*ufaktury.casCM
		ENDIF
		REPL denik.casCM WITH ufaktury.casCM
		REPL denik.mena WITH ufaktury.mena
		REPL denik.kurz WITH m.orikurz

		IF ufaktury.cas != m.oricast
			IF ufaktury.casCM=0
				REPL ufaktury.cas WITH m.oricast-ufaktury.cas
			  ELSE
				REPL ufaktury.cas WITH m.orikurz*(m.oricastCM-ufaktury.casCM)
			ENDIF
			REPL ufaktury.casCM WITH m.oricastCM-ufaktury.casCM
			REPL zap WITH {  .  .  }
		ENDIF
	ELSE
		REPL ufaktury.zap WITH {  .  .  }, ufaktury.cas WITH m.oricast, ;
			ufaktury.casCM WITH m.oricastCM
		ENDIF
	REPL ufaktury.Kurz WITH m.oriKurz
	refscr=.T.
	toexit=.T.
RETURN
**	Doplnÿn° n†zvÖ analytik do knihy faktur
PROCEDURE l_aktodb

WAIT WIND NOWA "DoplÂuji n†zvy analytik do knihy faktur"

PRIV _rc, _fi
SELE ufaktury

_rc=recno()
_fi=filter()
SET FILT TO

SCAN ALL

	**	Doplnÿn° n†zvu analytiky
	IF ufaktury.ana>0 AND empty(ufaktury.odb)
		PRIV _odb
		_odb=lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct')
		REPL ufaktury.odb WITH m._odb
	ENDIF

ENDSCAN	

SET FILT TO &_fi
=gorec(m._rc)

refscr=.T.

WAIT CLEAR

RETURN


**	Nastaven°/zruÁen° koÁilky
PROCEDURE l_fkos
	REPL kos WITH iif(kos=' ','˚',' ')
	SKIP 1
*	refscr=.T.
RETURN
PROCEDURE l_filf  
	IF empty(m.filfaktur)
		filfaktur="empty(zap)"
	  ELSE
		filfaktur=""
		ENDIF
	SET FILT TO &filfaktur
	GO TOP
	__fltr=filter()
	m.refscr=.T.
RETURN

**	ZruÁen° faktury
PROCEDURE l_fvym
IF m.gl_pri_pu=11
	WAIT WIND NOWA "Pro tuto funkci nem†te dostateünou prioritu."
	RETURN
ENDIF

	DELE
	refscr=.T.
RETURN
**	Tisky z knihy faktur
PROCEDURE FAK_TISK

	PRIV _se,_ce
	_ce=SYS(2001, "CENTURY")
	_se=sele()
	SET CENT OFF
	SELE ufaktury
	SET FILT TO &filfaktur
*	SET NEAR ON
	SEEK str(denik.typ,3)+str(denik.cis,5)
	IF eof()
		GO BOTT
		ENDIF
*	SET NEAR OFF

*	DO list_c WITH -1,-1,12,0, " Typy dokladÖ ", ;
*		[:DEF=Typ], ;
*		"\ F1-Help    ENTER-Oprava  INS-P˝id†n° typu   F7-T˝°dit podle ...",;
*		'{cis,txt}',;
*		'CR#:		DO typ_pri WITH .F.	## ENTER-Oprava typu',;
*		'CR#:		DO typ_pri WITH .F.	## ENTER-Oprava typu',;
*		'INS:		DO typ_pri WITH .T.	## INS-P˝id†n° typu',;
*

*		[str(typ,3)+' ≥'+str(cis,5)+'≥ '+dtoc(vys)+' ≥ '+dtoc(spl)+' ≥ '+ ;
*			dtoc(zap)+' ≥ '+var+'≥ '+kos+' ≥ '+str(cas,14,2)+' '],;
*		"//Typ ≥ ¨is.≥ Vystavena≥ Splatn†  ≥ Zaplacena≥ Var. symbol ≥KoÁ≥ ¨†stka",;
*
	DO list_c WITH -1,-1,12,0," F a k t u r y ",;
		[:DEF=Faktury], ;
		"\ F1-Help   INS-P˝id†n°   ENTER-Oprava   ALT+P-Proplacen° faktury",;
		'F8#: #FILTER', ;
		'#FLT= NezaplacenÇ faktury: empty(zap) ', ;
		'INS:		DO l_fopr WITH .T.		## INS-P˝id†n° faktury',;
		'CR#:		DO l_fopr WITH .F.		## ENTER-Oprava faktury',;
		'ALT+A:		DO l_addf				## ALT+A-AutomatickÇ p˝id†v†n°',;
		'ALT+F:		DO l_filf				## ALT+F-Filtr zaplacen°',;
		'ALT+P#:	DO l_fzap				## ALT+P-Proplacen° faktury',;
		'TAB#:		DO l_fkos				## Tab-Nastaven°/zruÁen° koÁilky',;
		'ALT+D#:	DO l_fvym				## ALT+D-Vymaz†n° faktury',;
		'CTRL+CR:	EXIT					## Ctrl+ENTER-Ukonüen°'



*		'ALT+T#:	DO l_ftisk				## Tisk sestavy z knihy faktur'

*	SET ORDER TO typcis

	SELE (_se)
	SET CENT &_ce
	SHOW GETS
RETURN
**	Tisk spoupisu faktur
PROCEDURE L_FTISK 
	PRIV fi
	lmarg=m.margin
	llhere=.F.
	DO t-faktur.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF

	SELE ufaktury

*	SET ESCA ON
*	ON ESCA DO stoptisk

	m.fi=iif(m.ttyp<>0,'.and.typ=m.ttyp','')
	SET FILT TO vys>=m.tdto AND vys<=m.tdtd &fi AND empty(zap)

	SET CONS OFF
	DO wpr
	tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll,pp
	headprog="l_thfak"


	SET ORDER TO uctana

	GO TOP
	PRIV tpd,cid,sa,sc
	tpd=uct
	cid=ana
	sa=0
	sc=0

	DO head
	?m.nlq+space(m.lmarg/1.6)+strn(ana,5)+" / "+iif(ana>0,lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct'),lookup(osn.txt,str(uct,3)+"   0",osn.uct,'uct'))+m.draft
	?space(m.lmarg/1.6)+"+------------+----------------+----------+--------------+"
	?space(m.lmarg/1.6)+"| Typ  ¨°slo |       ¨†stka   | Vystaveno|  Var.symbol  |"
	?space(m.lmarg/1.6)+"+------------+----------------+----------+--------------+"
	ll=ll+4
	SCAN ALL
		IF uct<>m.tpd OR ana<>m.cid
			=tstpgend(3)
			?space(m.lmarg/1.6)+"+============+================+==========+==============+"
			?space(m.lmarg/1.6)+"|    Celkem: | "+str(m.sa,14,2)+" |"
			?space(m.lmarg/1.6)+"+============+================+"
			tpd=uct
			cid=ana
			ll=ll+3
			sa=0
			=tstpgend(4)
			?m.nlq+space(m.lmarg/1.6)+strn(ana,5)+" / "+iif(ana>0,lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct'),lookup(osn.txt,str(uct,3)+"   0",osn.uct,'uct'))+m.draft
			?space(m.lmarg/1.6)+"+------------+----------------+----------+--------------+"
			?space(m.lmarg/1.6)+"| Typ  ¨°slo |       ¨†stka   | Vystaveno|  Var.symbol  |"
			?space(m.lmarg/1.6)+"+------------+----------------+----------+--------------+"
			ll=ll+4
			ENDIF
		=tstpgend(2)
		?space(m.lmarg/1.6)+"| "+str(typ,3)+str(cis,7)+" | "+;
			str(cas,14,2)+" | "+dtoc(vys)+" | "+var+" |"
		sa=sa+cas
		sc=sc+cas
		ll=ll+1
		ENDSCAN
	=tstpgend(3)
	?space(m.lmarg/1.6)+"+============+================+==========+==============+"
	?space(m.lmarg/1.6)+"|    Celkem: | "+str(m.sa,14,2)+" |"
	?space(m.lmarg/1.6)+"+============+================+"
	ll=ll+3
	
	=tstpgend(3)
	?space(m.lmarg/1.6)+"+************+****************+"
	?space(m.lmarg/1.6)+"|    Celkem: | "+str(m.sa,14,2)+" |"
	?space(m.lmarg/1.6)+"+************+****************+"
	ll=ll+3
	
	SET ALTE ON
	??chr(12)
	ll=m.ll+1
	WAIT CLEAR
*	SET ESCA OFF
*	ON ESCA
	SET ALTE TO
	DO tisk WITH m.margin/1.6
	SET CONS ON
	SET TALK OFF
	SET FILT TO

RETURN
PROCEDURE l_thfak  
	?m.cnon+space(m.lmarg)+"Kniha faktru"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(91), ;
			space(24)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(25))+ ;
		"Strana:",m.page PICT '@L 9999'
	?m.cnoff
	ll=2
	IF m.page=1
		?m.nlq+space(m.margin/1.6)+alignc("Kniha neproplacenÏch faktur"+space(40))
		?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		?m.draft
		ll=ll+3
		ENDIF

*	?m.nlq+space(m.lmarg/1.6)+strn(ana,5)+" / "+iif(ana>0,lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct'),lookup(osn.txt,str(uct,3)+"   0",osn.uct,'uct'))+m.draft
*	?space(m.lmarg/1.6)+"+------------+----------------+----------+--------------+"
*	?space(m.lmarg/1.6)+"| Typ  ¨°slo |       ¨†stka   | Vystaveno|  Var.symbol  |"
*	?space(m.lmarg/1.6)+"+------------+----------------+----------+--------------+"
*	ll=ll+4
RETURN

**	Speci†ln° operace
PROCEDURE hlm_spe
	utyp=0
	uc=132
	nuc='5??'
	an=9
	mes='Ne '
	ana=0
	rok=year(denik.dat)

	DEFINE POPUP spe FROM 5,40 RELATIVE SHADOW ;
		TITLE " Speci†ln° operace " COLOR SCHEME 4
	DEFINE BAR  5 OF spe PROMPT " \<P˝eü°slov†n° dokladÖ po dnech" ;
		MESS "U vÁech poloßek typu dokladu p˝i˝ad° v novÇm dni dalÁ° vyÁÁ° ü°slo." ;
		SKIP FOR recc('denik')=0
	DEFINE BAR  6 OF spe PROMPT " \<Zmÿna ü°sel dokladÖ" ;
		MESS "Zmÿna ü°sel OD,DO,NA a to i v textu, je-li ü°slo za '/'." ;
		SKIP FOR recc('denik')=0
	DEFINE BAR  7 OF spe PROMPT " Import dat ze slußebn°ch cest          {60}" ;
		MESS "Provede spuÁtÿn° programu pro import dat." ;
		SKIP FOR !file(alltrim(m.ggf)+"cexpo.dbf") OR m.gl_pri_pu<60
	DEFINE BAR 10 OF spe PROMPT " \<Uzamüen° obdob°" ;
		MESS "Uzamkne vÁechny z†znamy £ütovanÇ do uvedenÇho data."
	DEFINE BAR 14 OF spe PROMPT "\-"
	DEFINE BAR 15 OF spe PROMPT " VedlejÁ° po˝izovac° n†klady            {50}" ;
		MESS "RozpouÁt° vedlejÁ° po˝izovac° n†klady do n†kladÖ." ;
		SKIP FOR recc('denik')=0 OR m.gl_pri_pu<50
	DEFINE BAR 18 OF spe PROMPT " Import dat (z extern°ch programÖ)      {90}" ;
		MESS "Provede spuÁtÿn° programu pro import dat." ;
		SKIP FOR !file(alltrim(m.ggimport)+"import.fxp") OR m.gl_pri_pu<90
	DEFINE BAR 19 OF spe PROMPT "\-"
*	DEFINE BAR 20 OF spe PROMPT " P˝ednastaven° vÏkazÖ do osnovy <=2001  {50}" ;
*		MESS "P˝ednastav° ˝†dky a sloupce vÏkazÖ do £üetn° osnovy." ;
*		SKIP FOR m.gl_pri_pu<60
*	DEFINE BAR 21 OF spe PROMPT " P˝ednastaven° vÏkazÖ do osnovy rok 2002  {50}" ;
*		MESS "P˝ednastav° ˝†dky a sloupce vÏkazÖ do £üetn° osnovy." ;
*		SKIP FOR m.gl_pri_pu<60
	DEFINE BAR 20 OF spe PROMPT " P˝ednast.vÏkazÖ do \<osnovy na rok "+strn(m.ggrok)+"  {50}" ;
		MESS "P˝ednastav° ˝†dky a sloupce vÏkazÖ do £üetn° osnovy." ;
		SKIP FOR m.gl_pri_pu<60
	DEFINE BAR 22 OF spe PROMPT " Import z†kladn°ch £dajÖ z jinÇ firmy   {90}" ;
		MESS "P˝evezme £üetn° osnovu, druhy DPH, typy dokladÖ a texty z jinÇ firmy." ;
		SKIP FOR m.gl_pri_pu<90
	DEFINE BAR 221 OF spe PROMPT " Import £üetn° osnovy pro rok 2001      {90}" ;
		MESS "Importuje p˝ednastavenÇ syntetickÇ £üty platnÇ pro rok 2001." ;
		SKIP FOR m.gl_pri_pu<90
	DEFINE BAR 23 OF spe PROMPT " Proveden° uz†vÿrkovÏch operac°         {60}" ;
		MESS "P˝evede zÖstatky vÁech £ütÖ na £üet 702." ;
		SKIP FOR recc('denik')=0 OR m.gl_pri_pu<60
	DEFINE BAR 25 OF spe PROMPT " P˝eveden° uz†vÿrky na poü†teün° stavy  {60}" ;
		MESS "P˝evod z†vÿrkovÏch £ütÖ na zaü†tek dalÁ°ho £üetn°ho obdob°." ;
		SKIP FOR recc('denik')=0 OR m.gl_pri_pu<60
	DEFINE BAR 35 OF spe PROMPT " Odstranÿn° smazanÏch z†znamÖ           {60}" ;
		MESS "Odstran° z den°ku vÁechny obnovitelnÇ smazanÇ z†znamy ( NENµVRATN∑ !!! )." ;
		SKIP FOR recc('denik')=0 OR m.gl_pri_pu<60

	ON SELE POPU spe DO spe

	ACTI POPUP spe

	RELE POPU spe
RETURN

PROCEDURE spe
	DO CASE
		CASE bar()=5	&&	P˝eü°slov†n° dokladÖ po dnech
			DO spe_prec
		CASE bar()=6	&&	Zmÿna ü°sel dokladÖ OD,DO,NA
			DO spe_zmci
		CASE bar()=7	&&	Import £dajÖ ze slußebn°ch cest
			DO spe_imsc
		CASE bar()=10	&&	Uzamüen° obdob°
			DO spe_lock
		CASE bar()=15	&&	VedlejÁ° po˝izovac° n†klady
			DO spe_vpn
		CASE bar()=18	&&	Naüten° dat z p˝enosovÇho souboru
			IF ask(-1,-1,"N", "Opravdu p˝evÇst importn° datab†ze do den°ku.") ;
					AND ask(-1,-1,"N", "Do den°ku budou hromadnÿ p˝id†na data. Opravdu pokraüovat.")
				HIDE POPUP ALL
				DO (alltrim(m.ggimport)+"import.fxp") WITH alltrim(m.ggimport)
				SHOW POPUP hlm, spe
				WAIT CLEAR
				DO refscr
				ENDIF
		CASE bar()=20 	&&	P˝ednastaven° vÏkazÖ
			DO spe_vyko
*		CASE bar()=21 	&&	P˝ednastaven° vÏkazÖ
*			DO spe_vyk
		CASE bar()=22 	&&	Import £üetn° osnovy
			DO spe_iosn
		CASE bar()=221 	&&	Import £üetn° osnovy
			IF file("osn2001.DBF")
				SELE osn
				DELE ALL for uct<711 AND ana=0
				APPEND FROM osn2001.DBF
				WAIT WIND "Èüetn° osnova pro rok 2001 byla nastavena."
			  ELSE
				WAIT WIND "Datab†ze £üetn° osnovy 2001 nenalezena."
				ENDIF
		CASE bar()=23 	&&	Uz†vÿrkovÇ operace
			DO spe_uzav
		CASE bar()=25 	&&	P˝evod z†vÿrkovÏch £ütÖ
			DO spe_prev
		CASE bar()=35 	&&	Odstranÿn° smazanÏch z†znamÖ
			DO spe_smaz
		ENDCASE
RETURN
**	Import £üetn° osnovy
PROCEDURE spe_iosn

	PRIV rc, fi, fj

	SELE osn
	IF recc()!=0 AND !ask(-1,-1,"N","Souüasn† £üetn° osnova nen° pr†zdn†, m† se nen†vratnÿ p˝epsat")
		RETURN
		ENDIF

	SELE firma
	rc=recno()
	SET FILT TO pu

	l11=[:naz+' ≥ '+str(rok,4)+' ≥ '+jme]
	l12="// Adres†˝     ≥ Rok  ≥ N†zev firmy"

	DO list_c WITH -1,-1,10,0,;
		" VÏbÿr firmy k importu z†kladn°ch dat ",;
		m.l11, m.l12, ;
		"\ F1-Help  ENTER-VÏbÿr",;
		'{naz}',;
		'CR#:	EXIT		## ENTER-Otev˝en° firmy k £ütov†n°'

	fi=firma.naz
	fj=firma.jme
	SET FILT TO
	=gorec(m.rc)

	IF m.esc OR !ask(-1,-1,"N","Opravdu importovat data z firmy "+rtrim(m.fj))
		RETURN
		ENDIF

	SELE osn
	SET TALK ON
	DELE ALL
	PACK
	APPEND FROM (m.gl_data+m.fi+"\osn")

	SELE dph
	DELE ALL
	PACK
	APPEND FROM (m.gl_data+m.fi+"\dph")
	
	SELE typ
	DELE ALL
	PACK
	APPEND FROM (m.gl_data+m.fi+"\typ")
	
	SELE text
	DELE ALL
	PACK
	APPEND FROM (m.gl_data+m.fi+"\text")
	
	SET TALK OFF
	WAIT CLEAR
RETURN
**	Import vÏstupu ze slußebn°ch cest
PROCEDURE SPE_IMSC
	IF !open_dbf(m.ggf, "CEXPO", "op_expo", "ciscesty", .T., .F.)
		WAIT WIND "Nelze otev˝°t importn° datab†zi ze slußebn°ch cest."
		RETURN
		ENDIF

	PRIV i
	i=0
	SELE cexpo
	SCAN ALL
		INSERT INTO denik (str, typ, cis, dat, md, mda, dal, ;
			daa, txt, cas, dph, zad, sou, del, ;
			adat, atime, auser) ;
		  VALUES (0, cexpo.typ, 0, cexpo.dat, ;
			cexpo.md, cexpo.mda, cexpo.dal, cexpo.daa, cexpo.txt, ;
			cexpo.cas, 0, 0, 0, .F., date(), time(), "CESTY")
		i=i+1
		WAIT WIND NOWA "Bylo importov†no "+strn(i)+" z†znamÖ."
		ENDSCAN
	DELE ALL
	PACK
	USE
RETURN
**	OdstraÂuje z den°ku vÁechny smazanÇ poloßky
PROCEDURE spe_smaz
	IF !ask(-1,-1,"N", "Odstranit vÁechny smazanÇ poloßky z den°ku") ;
			OR !ask(-1,-1,"N", "Tato operace je nevratn†. Opravdu pokraüovat.")
		RETURN
		ENDIF
	SELE denik
	SET FILT TO
	SET TALK ON
	DELE ALL FOR denik.del
	PACK
	SET TALK OFF
	SET FILT TO !del
	WAIT WIND NOWA "VÁechny smazanÇ z†znamy odstranÿny. P˝°padnÿ prove‘te opravu indexÖ."
RETURN

**	P˝evod koncovÏch stavÖ na poü†teün°
PROCEDURE spe_prev
	SELE denik
	DO wpr
	SET FILT TO !del
	SET ORDER TO
	SET TALK ON
	COPY TO &ggf.prevod.bak FOR dal=702 OR md=702
	SELE 0
	USE &ggf.prevod.bak
	REPL dal WITH md, daa WITH mda, md WITH 701, mda WITH 0 ALL FOR dal=702 AND md!=710
	REPL md WITH dal, mda WITH daa, dal WITH 701, daa WITH 0 ALL FOR md=702 AND dal!=710
	REPL ALL dat WITH (dat+1), cis WITH 1
	REPL ALL md WITH 431, dal WITH 701 FOR dal=710
	REPL ALL dal WITH 431, md WITH 701 FOR md=710
	USE
	SET TALK OFF
	PRIV _l, newadr
	newadr=left(alltrim(firma.naz)+space(12),12)
	_l=33
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "P˝evÇst do firmy: "
	@ 0,19 GET m.newadr SIZE 1,12 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF
	IF !file(m.gl_data+m.newadr+"\denik.dbf")
		WAIT WIND NOWA "V adres†˝i '"+m.newadr+"' nenalezen soubor DENIK.DBF pro p˝evod."
		RETURN
		ENDIF
	SELE 0
	ON ERROR USE (m.gl_data+m.newadr+"\denik.dbf")
	USE (m.gl_data+m.newadr+"\denik.dbf")
	ON ERROR &_onerr
	IF !used()
		WAIT WIND NOWA "Nelze otev˝°t c°lovou datab†zi."
		RETURN
		ENDIF
	WAIT WIND NOWA "ZjiÁúuji p˝°tomnost poü†teün°ch stavÖ v c°lovÇm den°ku."
	LOCATE ALL FOR (md=701 OR dal=701) AND !del
	IF found()
		WAIT WIND "V c°lovÇm den°ku byly nalezeny poü†teün° stavy. P˝evod nen° proveden !!!"
		RETURN
		ENDIF
	APPEND FROM &ggf.prevod.bak
	USE
	WAIT WIND "P˝evod poü†teün°ch stavÖ do firmy '"+m.newadr+"' proveden !!!"
RETURN
**	P˝eü°slov†n° typu
PROCEDURE spe_zmci
	PRIV ttyp, odcis, docis, nacis, okolik
	ttyp=denik.typ
	odcis=denik.cis
	STORE 0 TO docis, nacis

	DO zmcis.spr

	IF m.esc OR !ask(-1,-1,"A", "P˝eü°slov†n° typu "+strn(m.ttyp)+" od "+strn(m.odcis)+" do "+strn(m.docis)+" na "+strn(m.nacis)+". V po˝†dku") ;
			OR !ask(-1,-1,"N", "Tato operace je nevratn†. Opravdu pokraüovat.")
		RETURN
		ENDIF

	DO wpr
	SELE denik
	SET FILT TO !del
	_rc=recno()
	_ord=order()
	SET ORDER TO
	okolik=m.nacis-m.odcis
	SCAN ALL FOR denik.typ=m.ttyp AND betw(denik.cis, m.odcis, m.docis)
		c=denik.cis
		REPL denik.cis WITH m.c+m.okolik, ;
			txt WITH strt(denik.txt, "/"+strn(m.c), "/"+strn(m.c+m.okolik))
		ENDSCAN
	=gorec(m._rc)
	SET ORDER TO &_ord
	WAIT CLEAR
RETURN
**	P˝ednastaven° £üetn° osnovy pro vÏkazy do roku 2002
PROCEDURE SPE_VYKO
DO CASE
  CASE m.ggrok<2002
	IF !ask(-1,-1,'N',"Opravdu smazat pÖvodn° nastaven° a pouß°t implicitn°")
		RETURN
	ENDIF
	SELE osn
	SET FILT TO
	REPL ALL rad WITH "    ", slo WITH " "
* Aktiva - Sloupec 1
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛"
	REPL ALL rad WITH "R2 ", slo WITH "1" FOR uct=353
	REPL ALL rad WITH "R5 ", slo WITH "1" FOR uct=11
	REPL ALL rad WITH "R6 ", slo WITH "1" FOR uct=12
	REPL ALL rad WITH "R7 ", slo WITH "1" FOR uct=13
	REPL ALL rad WITH "R8 ", slo WITH "1" FOR uct=14
	REPL ALL rad WITH "R9 ", slo WITH "1" FOR uct=18
	REPL ALL rad WITH "R9 ", slo WITH "1" FOR uct=19
	REPL ALL rad WITH "R10 ", slo WITH "1" FOR uct=41
	REPL ALL rad WITH "R11 ", slo WITH "1" FOR uct=51
	REPL ALL rad WITH "R13 ", slo WITH "1" FOR uct=31
	REPL ALL rad WITH "R14 ", slo WITH "1" FOR uct=21
	REPL ALL rad WITH "R15 ", slo WITH "1" FOR uct=22
	REPL ALL rad WITH "R16 ", slo WITH "1" FOR uct=25
	REPL ALL rad WITH "R17 ", slo WITH "1" FOR uct=26
	REPL ALL rad WITH "R18 ", slo WITH "1" FOR uct=28
	REPL ALL rad WITH "R18 ", slo WITH "1" FOR uct=29
	REPL ALL rad WITH "R18 ", slo WITH "1" FOR uct=32
	REPL ALL rad WITH "R19 ", slo WITH "1" FOR uct=42
	REPL ALL rad WITH "R20 ", slo WITH "1" FOR uct=52
	REPL ALL rad WITH "R21 ", slo WITH "1" FOR uct=97
	REPL ALL rad WITH "R23 ", slo WITH "1" FOR uct=61
	REPL ALL rad WITH "R24 ", slo WITH "1" FOR uct=62
	REPL ALL rad WITH "R25 ", slo WITH "1" FOR uct=63
	REPL ALL rad WITH "R26 ", slo WITH "1" FOR uct=66
	REPL ALL rad WITH "R27 ", slo WITH "1" FOR uct=67
	REPL ALL rad WITH "R27 ", slo WITH "1" FOR uct=69
	REPL ALL rad WITH "R30 ", slo WITH "1" FOR uct=112
	REPL ALL rad WITH "R30 ", slo WITH "1" FOR uct=119
	REPL ALL rad WITH "R31 ", slo WITH "1" FOR uct=121
	REPL ALL rad WITH "R31 ", slo WITH "1" FOR uct=122
	REPL ALL rad WITH "R32 ", slo WITH "1" FOR uct=123
	REPL ALL rad WITH "R33 ", slo WITH "1" FOR uct=124
	REPL ALL rad WITH "R34 ", slo WITH "1" FOR uct=132
	REPL ALL rad WITH "R34 ", slo WITH "1" FOR uct=139
	REPL ALL rad WITH "R43 ", slo WITH "1" FOR betw(uct,311,315)
	REPL ALL rad WITH "R44 ", slo WITH "1" FOR uct=354
	REPL ALL rad WITH "R44 ", slo WITH "1" FOR uct=355
	REPL ALL rad WITH "R44 ", slo WITH "1" FOR uct=358
	REPL ALL rad WITH "R44 ", slo WITH "1" FOR uct=398
	REPL ALL rad WITH "R48 ", slo WITH "1" FOR uct=351
	REPL ALL rad WITH "R50 ", slo WITH "1" FOR uct=335
	REPL ALL rad WITH "R50 ", slo WITH "1" FOR uct=375
	REPL ALL rad WITH "R50 ", slo WITH "1" FOR uct=378
	REPL ALL rad WITH "R52 ", slo WITH "1" FOR uct=211
	REPL ALL rad WITH "R52 ", slo WITH "1" FOR uct=213
	REPL ALL rad WITH "R52 ", slo WITH "1" FOR uct=261
	REPL ALL rad WITH "R53 ", slo WITH "1" FOR uct=221
	REPL ALL rad WITH "R54 ", slo WITH "1" FOR uct=251
	REPL ALL rad WITH "R54 ", slo WITH "1" FOR uct=253
	REPL ALL rad WITH "R57 ", slo WITH "1" FOR uct=381
	REPL ALL rad WITH "R57 ", slo WITH "1" FOR uct=382
	REPL ALL rad WITH "R58 ", slo WITH "1" FOR uct=385
	REPL ALL rad WITH "R59 ", slo WITH "1" FOR uct=386
	REPL ALL rad WITH "R60 ", slo WITH "1" FOR uct=388
* Aktiva - Sloupec 2
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛˛"
	REPL ALL rad WITH "R5 ", slo WITH "2" FOR uct=71
	REPL ALL rad WITH "R6 ", slo WITH "2" FOR uct=72
	REPL ALL rad WITH "R7 ", slo WITH "2" FOR uct=73
	REPL ALL rad WITH "R8 ", slo WITH "2" FOR uct=74
	REPL ALL rad WITH "R9 ", slo WITH "2" FOR uct=78
	REPL ALL rad WITH "R9 ", slo WITH "2" FOR uct=79
	REPL ALL rad WITH "R10 ", slo WITH "2" FOR uct=93
	REPL ALL rad WITH "R14 ", slo WITH "2" FOR uct=81
	REPL ALL rad WITH "R15 ", slo WITH "2" FOR uct=82
	REPL ALL rad WITH "R16 ", slo WITH "2" FOR uct=85
	REPL ALL rad WITH "R17 ", slo WITH "2" FOR uct=86
	REPL ALL rad WITH "R18 ", slo WITH "2" FOR uct=88
	REPL ALL rad WITH "R18 ", slo WITH "2" FOR uct=89
	REPL ALL rad WITH "R19 ", slo WITH "2" FOR uct=94
	REPL ALL rad WITH "R20 ", slo WITH "2" FOR uct=95
	REPL ALL rad WITH "R21 ", slo WITH "2" FOR uct=98
	REPL ALL rad WITH "R30 ", slo WITH "2" FOR uct=191
	REPL ALL rad WITH "R31 ", slo WITH "2" FOR uct=192
	REPL ALL rad WITH "R31 ", slo WITH "2" FOR uct=193
	REPL ALL rad WITH "R32 ", slo WITH "2" FOR uct=194
	REPL ALL rad WITH "R33 ", slo WITH "2" FOR uct=195
	REPL ALL rad WITH "R34 ", slo WITH "2" FOR uct=196
	REPL ALL rad WITH "R54 ", slo WITH "2" FOR uct=291
	REPL ALL rad WITH "R54 ", slo WITH "2" FOR uct=293
* Pasiva
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛˛˛"
	REPL ALL rad WITH "R64 " FOR uct=411
	REPL ALL rad WITH "R64 " FOR uct=491
	REPL ALL rad WITH "R65 " FOR uct=252
	REPL ALL rad WITH "R67 " FOR uct=412
	REPL ALL rad WITH "R68 " FOR uct=413
	REPL ALL rad WITH "R69 " FOR uct=414
	REPL ALL rad WITH "R70 " FOR uct=415
	REPL ALL rad WITH "R72 " FOR uct=421
	REPL ALL rad WITH "R73 " FOR uct=422
	REPL ALL rad WITH "R74 " FOR uct=423
	REPL ALL rad WITH "R74 " FOR uct=427
	REPL ALL rad WITH "R76 " FOR uct=428
	REPL ALL rad WITH "R77 " FOR uct=429
	REPL ALL rad WITH "R81 " FOR uct=451
	REPL ALL rad WITH "R82 " FOR uct=454
	REPL ALL rad WITH "R83 " FOR uct=459
	REPL ALL rad WITH "R88 " FOR uct=255
	REPL ALL rad WITH "R88 " FOR uct=473
	REPL ALL rad WITH "R90 " FOR uct=479
	REPL ALL rad WITH "R92 " FOR uct=321
	REPL ALL rad WITH "R92 " FOR uct=322
	REPL ALL rad WITH "R92 " FOR uct=324
	REPL ALL rad WITH "R92 " FOR uct=325
	REPL ALL rad WITH "R92 " FOR uct=475
	REPL ALL rad WITH "R92 " FOR uct=478
	REPL ALL rad WITH "R93 " FOR uct=364
	REPL ALL rad WITH "R93 " FOR uct=365
	REPL ALL rad WITH "R93 " FOR uct=366
	REPL ALL rad WITH "R93 " FOR uct=367
	REPL ALL rad WITH "R93 " FOR uct=368
	REPL ALL rad WITH "R94 " FOR uct=331
	REPL ALL rad WITH "R94 " FOR uct=333
	REPL ALL rad WITH "R95 " FOR uct=336
	REPL ALL rad WITH "R96 " FOR uct=341
	REPL ALL rad WITH "R96 " FOR uct=342
	REPL ALL rad WITH "R96 " FOR uct=343
	REPL ALL rad WITH "R96 " FOR uct=345
	REPL ALL rad WITH "R96 " FOR uct=346
	REPL ALL rad WITH "R96 " FOR uct=347
	REPL ALL rad WITH "R97 " FOR uct=371
	REPL ALL rad WITH "R98 " FOR uct=361
	REPL ALL rad WITH "R98 " FOR uct=471
	REPL ALL rad WITH "R100" FOR uct=379
	REPL ALL rad WITH "R100" FOR uct=474
	REPL ALL rad WITH "R103" FOR uct=231
	REPL ALL rad WITH "R103" FOR uct=232
	REPL ALL rad WITH "R103" FOR uct=461
	REPL ALL rad WITH "R104" FOR uct=241
	REPL ALL rad WITH "R104" FOR uct=249
	REPL ALL rad WITH "R107" FOR uct=383
	REPL ALL rad WITH "R108" FOR uct=384
	REPL ALL rad WITH "R109" FOR uct=387
	REPL ALL rad WITH "R110" FOR uct=389
* VÏsledovka
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛˛˛˛"
	REPL ALL rad WITH "V1 " FOR uct=604
	REPL ALL rad WITH "V2 " FOR uct=504
	REPL ALL rad WITH "V5 " FOR uct=601
	REPL ALL rad WITH "V5 " FOR uct=602
	REPL ALL rad WITH "V6 " FOR betw(uct,610,619)
	REPL ALL rad WITH "V7 " FOR betw(uct,620,629)
	REPL ALL rad WITH "V9 " FOR uct=501
	REPL ALL rad WITH "V9 " FOR uct=502
	REPL ALL rad WITH "V9 " FOR uct=503
	REPL ALL rad WITH "V10 " FOR betw(uct,510,519)
	REPL ALL rad WITH "V13 " FOR uct=521
	REPL ALL rad WITH "V13 " FOR uct=522
	REPL ALL rad WITH "V14 " FOR uct=523
	REPL ALL rad WITH "V15 " FOR uct=524
	REPL ALL rad WITH "V15 " FOR uct=525
	REPL ALL rad WITH "V15 " FOR uct=526
	REPL ALL rad WITH "V16 " FOR uct=527
	REPL ALL rad WITH "V16 " FOR uct=528
	REPL ALL rad WITH "V17 " FOR betw(uct,530,539)
	REPL ALL rad WITH "V18 " FOR uct=551
	REPL ALL rad WITH "V19 " FOR uct=641
	REPL ALL rad WITH "V19 " FOR uct=642
	REPL ALL rad WITH "V20 " FOR uct=541
	REPL ALL rad WITH "V20 " FOR uct=542
	REPL ALL rad WITH "V21 " FOR uct=652
	REPL ALL rad WITH "V21 " FOR uct=654
	REPL ALL rad WITH "V21 " FOR uct=655
	REPL ALL rad WITH "V22 " FOR uct=552
	REPL ALL rad WITH "V22 " FOR uct=554
	REPL ALL rad WITH "V22 " FOR uct=555
	REPL ALL rad WITH "V23 " FOR uct=657
	REPL ALL rad WITH "V23 " FOR uct=659
	REPL ALL rad WITH "V24 " FOR uct=557
	REPL ALL rad WITH "V24 " FOR uct=559
	REPL ALL rad WITH "V25 " FOR uct=644
	REPL ALL rad WITH "V25 " FOR uct=646
	REPL ALL rad WITH "V25 " FOR uct=648
	REPL ALL rad WITH "V26 " FOR uct=543
	REPL ALL rad WITH "V26 " FOR uct=544
	REPL ALL rad WITH "V26 " FOR uct=545
	REPL ALL rad WITH "V26 " FOR uct=546
	REPL ALL rad WITH "V26 " FOR uct=548
	REPL ALL rad WITH "V27 " FOR uct=697
	REPL ALL rad WITH "V28 " FOR uct=597
	REPL ALL rad WITH "V30 " FOR uct=661
	REPL ALL rad WITH "V31 " FOR uct=561
	REPL ALL rad WITH "V35 " FOR uct=665
	REPL ALL rad WITH "V36 " FOR uct=666
	REPL ALL rad WITH "V37 " FOR uct=674
	REPL ALL rad WITH "V38 " FOR uct=574
	REPL ALL rad WITH "V39 " FOR uct=679
	REPL ALL rad WITH "V40 " FOR uct=579
	REPL ALL rad WITH "V41 " FOR uct=662
	REPL ALL rad WITH "V42 " FOR uct=562
	REPL ALL rad WITH "V43 " FOR uct=663
	REPL ALL rad WITH "V43 " FOR uct=668
	REPL ALL rad WITH "V44 " FOR uct=563
	REPL ALL rad WITH "V44 " FOR uct=568
	REPL ALL rad WITH "V45 " FOR uct=698
	REPL ALL rad WITH "V46 " FOR uct=598
	REPL ALL rad WITH "V49 " FOR uct=591
	REPL ALL rad WITH "V49 " FOR uct=595
	REPL ALL rad WITH "V50 " FOR uct=592
	REPL ALL rad WITH "V53 " FOR betw(uct,680,689)
	REPL ALL rad WITH "V54 " FOR betw(uct,580,589)
	REPL ALL rad WITH "V56 " FOR uct=593
	REPL ALL rad WITH "V57 " FOR uct=594
	REPL ALL rad WITH "V59 " FOR uct=596
	WAIT CLEAR
CASE m.ggrok=2002
	IF !ask(-1,-1,'N',"Opravdu smazat pÖvodn° nastaven° a pouß°t implicitn°")
		RETURN
		ENDIF
	SELE osn
	SET FILT TO
	REPL ALL rad WITH "    ", slo WITH " "
* Aktiva - Sloupec 1
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛"
	REPL ALL rad WITH "R2 ", slo WITH "1" FOR uct=353
	REPL ALL rad WITH "R5 ", slo WITH "1" FOR uct=11
	REPL ALL rad WITH "R6 ", slo WITH "1" FOR uct=12
	REPL ALL rad WITH "R7 ", slo WITH "1" FOR uct=13
	REPL ALL rad WITH "R8 ", slo WITH "1" FOR uct=14
	REPL ALL rad WITH "R9 ", slo WITH "1" FOR uct=19
	REPL ALL rad WITH "R10 ", slo WITH "1" FOR uct=41
	REPL ALL rad WITH "R11 ", slo WITH "1" FOR uct=51
	REPL ALL rad WITH "R13 ", slo WITH "1" FOR uct=31
	REPL ALL rad WITH "R14 ", slo WITH "1" FOR uct=21
	REPL ALL rad WITH "R15 ", slo WITH "1" FOR uct=22
	REPL ALL rad WITH "R16 ", slo WITH "1" FOR uct=25
	REPL ALL rad WITH "R17 ", slo WITH "1" FOR uct=26
	REPL ALL rad WITH "R18 ", slo WITH "1" FOR uct=29 OR uct=32
	REPL ALL rad WITH "R19 ", slo WITH "1" FOR uct=42
	REPL ALL rad WITH "R20 ", slo WITH "1" FOR uct=52
	REPL ALL rad WITH "R21 ", slo WITH "1" FOR uct=97
	REPL ALL rad WITH "R23 ", slo WITH "1" FOR uct=61
	REPL ALL rad WITH "R24 ", slo WITH "1" FOR uct=62
	REPL ALL rad WITH "R25 ", slo WITH "1" FOR uct=63 OR uct=65
	REPL ALL rad WITH "R26 ", slo WITH "1" FOR uct=66
	REPL ALL rad WITH "R27 ", slo WITH "1" FOR uct=67 OR uct=69
	REPL ALL rad WITH "R28 ", slo WITH "1" FOR uct=43
	REPL ALL rad WITH "R29 ", slo WITH "1" FOR uct=53

	REPL ALL rad WITH "R32 ", slo WITH "1" FOR uct=119 OR uct=112
	REPL ALL rad WITH "R33 ", slo WITH "1" FOR uct=121 OR uct=122
	REPL ALL rad WITH "R34 ", slo WITH "1" FOR uct=123
	REPL ALL rad WITH "R35 ", slo WITH "1" FOR uct=124
	REPL ALL rad WITH "R36 ", slo WITH "1" FOR uct=132 OR uct=139

	REPL ALL rad WITH "R45 ", slo WITH "1" FOR betw(uct,311,315)
	REPL ALL rad WITH "R46 ", slo WITH "1" FOR uct=354 OR uct=355 OR uct=358 OR uct=398

	REPL ALL rad WITH "R51 ", slo WITH "1" FOR uct=335 OR uct=371 OR betw(uct,373,376) OR uct=378

	REPL ALL rad WITH "R53 ", slo WITH "1" FOR uct=211 OR uct=213 OR uct=261
	REPL ALL rad WITH "R54 ", slo WITH "1" FOR uct=221
	REPL ALL rad WITH "R55 ", slo WITH "1" FOR uct=251 OR uct=253 OR uct=256 OR uct=257
	REPL ALL rad WITH "R56 ", slo WITH "1" FOR uct=259

	REPL ALL rad WITH "R59 ", slo WITH "1" FOR uct=381 OR uct=382
	REPL ALL rad WITH "R60 ", slo WITH "1" FOR uct=385
	REPL ALL rad WITH "R61 ", slo WITH "1" FOR uct=388

* Aktiva - Sloupec 2
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛˛"
	REPL ALL rad WITH "R5 ", slo WITH "2" FOR uct=71
	REPL ALL rad WITH "R6 ", slo WITH "2" FOR uct=72
	REPL ALL rad WITH "R7 ", slo WITH "2" FOR uct=73
	REPL ALL rad WITH "R8 ", slo WITH "2" FOR uct=74
	REPL ALL rad WITH "R9 ", slo WITH "2" FOR uct=78
	REPL ALL rad WITH "R9 ", slo WITH "2" FOR uct=79
	REPL ALL rad WITH "R10 ", slo WITH "2" FOR uct=93
	REPL ALL rad WITH "R14 ", slo WITH "2" FOR uct=81
	REPL ALL rad WITH "R15 ", slo WITH "2" FOR uct=82
	REPL ALL rad WITH "R16 ", slo WITH "2" FOR uct=85
	REPL ALL rad WITH "R17 ", slo WITH "2" FOR uct=86
	REPL ALL rad WITH "R18 ", slo WITH "2" FOR uct=89
	REPL ALL rad WITH "R19 ", slo WITH "2" FOR uct=94
	REPL ALL rad WITH "R20 ", slo WITH "2" FOR uct=95
	REPL ALL rad WITH "R21 ", slo WITH "2" FOR uct=98
	REPL ALL rad WITH "R32 ", slo WITH "2" FOR uct=191
	REPL ALL rad WITH "R33 ", slo WITH "2" FOR uct=192 OR uct=193
	REPL ALL rad WITH "R34 ", slo WITH "2" FOR uct=194
	REPL ALL rad WITH "R35 ", slo WITH "2" FOR uct=195
	REPL ALL rad WITH "R36 ", slo WITH "2" FOR uct=196
	REPL ALL rad WITH "R55 ", slo WITH "2" FOR uct=291

* Pasiva
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛˛˛"
	REPL ALL rad WITH "R65 " FOR uct=411 OR uct=491
	REPL ALL rad WITH "R66 " FOR uct=252
	REPL ALL rad WITH "R67 " FOR uct=419
	REPL ALL rad WITH "R69 " FOR uct=412
	REPL ALL rad WITH "R70 " FOR uct=413
	REPL ALL rad WITH "R71 " FOR uct=414
	REPL ALL rad WITH "R72 " FOR uct=418
	REPL ALL rad WITH "R74 " FOR uct=421
	REPL ALL rad WITH "R75 " FOR uct=422
	REPL ALL rad WITH "R76 " FOR uct=423 OR uct=427
	REPL ALL rad WITH "R78 " FOR uct=428
	REPL ALL rad WITH "R79 " FOR uct=429
	REPL ALL rad WITH "R83 " FOR uct=451
	REPL ALL rad WITH "R86 " FOR uct=481

	REPL ALL rad WITH "R95 " FOR uct=321 OR uct=322 OR uct=324 OR uct=325
	REPL ALL rad WITH "R96 " FOR uct=336
	REPL ALL rad WITH "R96 " FOR betw(uct,364,368)
	REPL ALL rad WITH "R97 " FOR uct=331 OR uct=333
	REPL ALL rad WITH "R99 " FOR betw(uct,341,343) OR betw(uct,345,347)
	REPL ALL rad WITH "R102" FOR uct=379

	REPL ALL rad WITH "R105" FOR uct=231 OR uct=232
	REPL ALL rad WITH "R106" FOR uct=241 OR uct=249
	REPL ALL rad WITH "R109" FOR uct=383
	REPL ALL rad WITH "R110" FOR uct=384
	REPL ALL rad WITH "R111" FOR uct=389

* VÏsledovka
	WAIT WIND NOWA "P˝ednastavuji £üetn° osnovu pro vÏkazy. Moment ˛˛˛˛"
	REPL ALL rad WITH "V1 " FOR uct=604
	REPL ALL rad WITH "V2 " FOR uct=504
	REPL ALL rad WITH "V5 " FOR uct=601 OR uct=602
	REPL ALL rad WITH "V6 " FOR betw(uct,610,619)
	REPL ALL rad WITH "V7 " FOR betw(uct,620,629)

	REPL ALL rad WITH "V9 " FOR uct=501 OR uct=502 OR uct=503
	REPL ALL rad WITH "V10 " FOR betw(uct,510,519)
	REPL ALL rad WITH "V13 " FOR uct=521 OR uct=522
	REPL ALL rad WITH "V14 " FOR uct=523
	REPL ALL rad WITH "V15 " FOR uct=524 OR uct=525 OR uct=526
	REPL ALL rad WITH "V16 " FOR uct=527 OR uct=528
	REPL ALL rad WITH "V17 " FOR betw(uct,530,539)
	REPL ALL rad WITH "V18 " FOR uct=551
	REPL ALL rad WITH "V19 " FOR uct=641 OR uct=642
	REPL ALL rad WITH "V20 " FOR uct=541 OR uct=542
	REPL ALL rad WITH "V21 " FOR uct=652 OR uct=654 OR uct=655
	REPL ALL rad WITH "V22 " FOR uct=552 OR uct=554 OR uct=555
	REPL ALL rad WITH "V23 " FOR uct=657 OR uct=658 OR uct=659
	REPL ALL rad WITH "V24 " FOR uct=557 OR uct=558 OR uct=559
	REPL ALL rad WITH "V25 " FOR uct=644 OR uct=646 OR uct=648
	REPL ALL rad WITH "V26 " FOR betw(uct,543,546) OR uct=548 OR uct=549
	REPL ALL rad WITH "V27 " FOR uct=697
	REPL ALL rad WITH "V28 " FOR uct=597

	REPL ALL rad WITH "V30 " FOR uct=661
	REPL ALL rad WITH "V31 " FOR uct=561
	REPL ALL rad WITH "V36 " FOR uct=666
	REPL ALL rad WITH "V37 " FOR uct=566
	REPL ALL rad WITH "V38 " FOR uct=664
	REPL ALL rad WITH "V39 " FOR uct=564
	REPL ALL rad WITH "V40 " FOR uct=674
	REPL ALL rad WITH "V41 " FOR uct=574
	REPL ALL rad WITH "V42 " FOR uct=679
	REPL ALL rad WITH "V43 " FOR uct=579
	REPL ALL rad WITH "V44 " FOR uct=662
	REPL ALL rad WITH "V45 " FOR uct=562

	REPL ALL rad WITH "V46 " FOR uct=663 OR uct=667 OR uct=668
	REPL ALL rad WITH "V47 " FOR uct=563 OR uct=567 OR uct=568 OR uct=569
	REPL ALL rad WITH "V48 " FOR uct=698
	REPL ALL rad WITH "V49 " FOR uct=598
	REPL ALL rad WITH "V52 " FOR uct=591 OR uct=595
	REPL ALL rad WITH "V53 " FOR uct=592
	REPL ALL rad WITH "V55 " FOR betw(uct,680,689)
	REPL ALL rad WITH "V56 " FOR betw(uct,580,589)
	REPL ALL rad WITH "V58 " FOR uct=593
	REPL ALL rad WITH "V59 " FOR uct=594
	REPL ALL rad WITH "V61 " FOR uct=596
	WAIT CLEAR
  OTHER
	DO spe_vykS
ENDCASE

RETURN
**	P˝ednastaven° £üetn° osnovy pro vÏkazy od roku 2003
PROCEDURE spe_vykS
	PRIV tp_vyk, selvyk
	tp_vyk=iif(m.ggtp_vyk="Z","VZR_","VPR_")

	IF !ask(-1,-1,'N',"Opravdu smazat pÖvodn° nastaven° a pouß°t implicitn°")
		RETURN
	ENDIF
	SELE osn
	SET FILT TO
	REPL ALL rad WITH "    ", slo WITH " "

	xx=m.gl_data+m.tp_vyk+str(m.ggrok,4)+".dbf"

	IF !file(m.xx)
		WAIT WIND "Neexistuje datab†ze £dajÖ pro vÏpoüet vÏkazÖ."
		RETURN
	ENDIF

	SELE osn
	SET ORDER TO uct

	SELE 0		
	USE (m.xx)
	selvyk=sele()

	PRIV _tx, issl
	issl=.T.
	SCAN ALL
		IF left(text,5)="#PASI"
			issl=.F.
		ENDIF
		IF left(vsl1,1)="+" OR left(vsl2,1)="+" OR left(vsl,1)="+"
			LOOP
		ENDIF
		IF empty(vsl1) AND empty(vsl2) AND empty(vsl)
			LOOP
		ENDIF

WAIT WIND NOWA "Zpracov†n° ˝†dku: "+radek

		IF m.issl
* Nastavov†n° sloupce ü.1
			_tx=alltrim(vsl1)+","
			DO WHILE !empty(m._tx)
*				IF !ask(-1,-1,"A","¸†dek: "+_tx+"  Pokraüovat")
*					USE
*					RETURN
*				ENDIF
				_tx=xdekrad(m._tx,radek,"1")
				IF inkey()=27
					RETURN
				ENDIF
			ENDDO
* Nastavov†n° sloupce ü.2
			_tx=alltrim(vsl2)+","
			DO WHILE !empty(m._tx)
				_tx=xdekrad(m._tx,radek,"2")
				IF inkey()=27
					RETURN
				ENDIF
			ENDDO
		  ELSE
* Nastavov†n° neoü°slovanÇho sloupce
			_tx=alltrim(vsl)+","
			DO WHILE !empty(m._tx)
				_tx=xdekrad(m._tx,radek," ")
				IF inkey()=27
					RETURN
				ENDIF
			ENDDO
		ENDIF
	ENDSCAN

	USE
	WAIT CLEAR
RETURN

*capsstru

PROCEDURE xdekrad
PARA _t,_ra,_sl
	PRIV a,b,s,u
	a=at(",",_t)
	b=at("-",_t)
	s=""
	DO CASE
	  CASE a!=0 AND (a<b OR b=0)
		s=left(_t,a-1)
		_t=subst(_t,a+1)
	  CASE b!=0
		s=left(_t,a-1)
		u=val(s)
		s1=val(subst(_t,b+1,a-b-1))
		IF s1>u
			_t=strn(u+1)+substr(_t,b)
		  ELSE
			_t=substr(_t,a+1)
		ENDIF
	  OTHER
		s=_t
		_t=""
	ENDCASE
	SELE osn
	u=val(s)
	REPL ALL rad WITH _ra, slo WITH _sl FOR uct=u
	SELE (m.selvyk)
		
RETURN _t

**	VedlejÁ° po˝izovac° n†klady
PROCEDURE spe_vpn
	SELE denik
	SET FILT TO !del
	GO TOP
*	ttyp=0
*	mod=1
*	mimo=space(80)
	DO vpn.spr
	IF m.esc
		RETURN
		ENDIF
	DO wpr

	PRIV d, smd, sdal, x, y, koe, _rc, _stru
	mimo=iif(empty(m.mimo),'.T.',alltrim(m.mimo))
	SELE denik
	_stru=capstru(.T.)

	WAIT WIND NOWA "Zpracov†v†m mÿs°c ü.: "+strn(m.mod)+ ;
		"  ¨ekejte pros°m..."
	SELE 0
	CREA CURS cu1 (&_stru)
	SELE osn
	SET ORDER TO uct
	REPL cmd WITH 0, cdal WITH 0 FOR m.uc=uct
	SELE denik
	d=ctod('01.'+str(m.mod,2)+'.'+str(m.rok,4))
	IF m.mod>1
		d=d-1
		ENDIF
	SET RELA TO str(md,3)+str(mda,4) INTO osn
	SET ORDER TO md
	SEEK str(m.uc,3)
	REPL osn.cmd WITH osn.cmd+cas REST ;
		FOR iif(m.mod=1, dal=701, .T.) AND dat<=d

	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	SET ORDER TO dal
	SEEK str(m.uc,3)
	REPL osn.cdal WITH osn.cdal+cas REST ;
		FOR iif(m.mod=1, md=701, .T.) AND dat<=d
	SET RELA TO

	SELE osn
	SET ORDER TO uct
	SEEK str(m.uc,3)
	SUM cmd, cdal WHILE m.uc=uct FOR ana=m.an TO smd, sdal
	x=m.smd-m.sdal
	SEEK str(m.uc,3)
	SUM cmd, cdal WHILE m.uc=uct FOR m.an!=ana AND &mimo TO smd, sdal
	y=m.smd-m.sdal
	koe=round(x/y,4)
	SELE denik
	SET ORDER TO md
	SEEK '5'

	SCAN WHILE like(m.nuc,str(md,3)) FOR month(dat)=m.mod AND m.uc=dal ;
			AND m.an!=daa AND iif(m.ttyp=0,.T.,m.ttyp=typ)
		ana=denik.daa
		IF &mimo
			_rc=recno()
			INSERT INTO cu1 (str, typ, cis, dat, md, mda, dal, ;
				daa, txt, cas, dph, zad, sou, del, ;
				adat, atime, auser) ;
			  VALUES (denik.str, iif(m.utyp!=0, m.utyp, denik.typ),;
				iif(m.utyp!=0, 0, denik.cis), denik.dat, denik.md, ;
				denik.mda, m.uc, m.an, "RozpuÁtÿn° VPN - koef. "+strn(m.koe), ;
				denik.cas*m.koe, 0, 0, 11, .F., date(), ;
				left(time(),5), m.gl_usernam)
			=gorec(m._rc)
			ENDIF
		ENDSCAN

	SELE denik
	s=capstru(.F.)
	IF m.mes='Ano'
		SELECT str, typ, cis, dat, md, mda, dal, ;
				daa, txt, sum(cas) AS cas, dph, zad, sou, del, ;
				adat, atime, auser, str(md,3)+str(mda,4) AS gr ;
			FROM cu1 INTO CURS se1 GROUP BY gr
		d=ctod('01.'+str(m.mod,2)+'.'+str(m.rok,4))
		d=gomonth(m.d,1)-1
		SCAN ALL
			INSERT INTO denik (str, typ, cis, dat, md, mda, dal, ;
				daa, txt, cas, dph, zad, sou, del, ;
				adat, atime, auser) ;
			  VALUES (se1.str, se1.typ, se1.cis, ;
				m.d, se1.md, se1.mda, se1.dal, se1.daa, se1.txt, ;
				se1.cas, se1.dph, se1.zad, se1.sou, se1.del, ;
				se1.adat, se1.atime, se1.auser)
			ENDSCAN
		USE
	  ELSE
		SELE cu1
		SCAN ALL
			INSERT INTO denik (str, typ, cis, dat, md, mda, dal, ;
				daa, txt, cas, dph, zad, sou, del, ;
				adat, atime, auser) ;
			  VALUES (cu1.str, cu1.typ, cu1.cis, ;
				cu1.dat, cu1.md, cu1.mda, cu1.dal, cu1.daa, cu1.txt, ;
				cu1.cas, cu1.dph, cu1.zad, cu1.sou, cu1.del, ;
				cu1.adat, cu1.atime, cu1.auser)
			ENDSCAN
		ENDIF
	SELE cu1
	USE

	SELE denik
	WAIT CLEAR
	=beep(1000,300)
	=beep(2000,300)
RETURN
**	P˝eü°slov†n° dokladÖ
PROCEDURE spe_prec
	DO typ_prec.spr
	_tcis=m.tcis
	IF m.esc
		RETURN
		ENDIF
	IF m.ttyp=0
		WAIT WIND "Nebylo by jistÿ vhodnÇ p˝eü°slovat £plnÿ vÁechny typy dokladÖ !!!"
		WAIT CLEAR
		RETURN
		ENDIF
	DO wpr
	SELE denik
	INDEX ON dtos(dat) TAG _idat
	SET ORDER TO _idat
	m.fi='!del AND typ=m.ttyp'+;
		iif(m.tstr<>0,' AND str=m.tstr','')+ ;
			' AND dat>=m.tdto AND dat<=m.tdtd'
	SET FILT TO &fi
	COUNT ALL TO rc
	GO TOP
	dt=dat
	pp=1
	DO WHILE !eof()
		IF dt<>dat
			=proc(rc,pp)
			dt=dat
			_tcis=m._tcis+1
			ENDIF
		REPL cis WITH m._tcis
		pp=pp+1
		SKIP 1
		ENDDO
	SET ORDER TO
	DELE TAG _idat
	SET FILTER TO !del
	WAIT WIND "P˝eü°slov†n° dokladÖ ukonüeno."
RETURN
**	Uzamüen° z†znamÖ proti p˝episu
PROCEDURE spe_lock
	PRIV _l
	_l=25
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Z†mek dat do: "
	@ 0,15 GET m.ggdtlock SIZE 1,8 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
RETURN
**	Uz†vÿrkovÇ operace
PROCEDURE spe_uzav
	PRIV zisk, tdat, ttyp, tcis
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	GO BOTT
	ttyp=99
	tcis=0
	tdat=dat

	DO uzaver.spr
	IF m.esc
		RETURN
		ENDIF

	DO wpr
	SELE osn
	SET ORDER TO uct
	SET TALK ON
	REPL ALL cmd WITH 0, cdal WITH 0

	SELE denik
	SET FILT TO dat<=m.tdat AND !del

	SET RELA TO str(md,3)+str(mda,4) INTO osn
	REPL ALL osn.cmd WITH osn.cmd+denik.cas
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	REPL ALL osn.cdal WITH osn.cdal+denik.cas
	SET RELA TO

	SELE OSN
	SET ORDER TO uct

	SET FILT TO uct!=0
	PRIV m5, d5, m6, d6

*	VÏpoüet £üetn°ho zisku
	SET CONS OFF
	SET TALK OFF
	CALC sum(cmd), sum(cdal) TO m5, d5 FOR betw(uct,500,599)
	CALC sum(cmd), sum(cdal) TO m6, d6 FOR betw(uct,600,699)
	zisk=d6-m6-(m5-d5)
	PRIV cc, u7, tx, uc
	cc=0
	SCAN ALL
		DO CASE
			CASE betw(uct,500,599)
				cc=cmd-cdal
				u7=.T.
				tx="P˝evod n†kladovÏch £ütÖ"
				uc=710
			CASE betw(uct,600,699)
				cc=cdal-cmd
				u7=.F.
				tx="P˝evod vÏnosovÏch £ütÖ"
				uc=710
			CASE roz='A'
				cc=cmd-cdal
				u7=.T.
				tx="P˝evod aktivn°ch £ütÖ"
				uc=702
			CASE roz='P'
				cc=cdal-cmd
				u7=.F.
				tx="P˝evod pasivn°ch £ütÖ"
				uc=702
			CASE betw(uct,711,799)
				cc=cmd-cdal
				u7=.T.
				tx="P˝evod podrozvahovÏch £ütÖ"
				uc=702
			CASE betw(uct,800,999)
				cc=cdal-cmd
				u7=.T.
				tx="P˝evod vnitropodnikovÏch £ütÖ"
				uc=702
			ENDCASE

		IF m.cc!=0
			IF u7
				INSERT INTO denik (typ,cis,dat,md,mda,dal,daa,txt,cas,adat,atime,auser) ;
					VALUES (m.ttyp,m.tcis,m.tdat,m.uc,0,osn.uct,osn.ana,m.tx,m.cc,date(),left(time(),5),m.gl_usernam)
			  ELSE
				INSERT INTO denik (typ,cis,dat,md,mda,dal,daa,txt,cas,adat,atime,auser) ;
					VALUES (m.ttyp,m.tcis,m.tdat,osn.uct,osn.ana,m.uc,0,m.tx,m.cc,date(),left(time(),5),m.gl_usernam)
				ENDIF
			ENDIF
		ENDSCAN
	INSERT INTO denik (typ,cis,dat,md,mda,dal,daa,txt,cas,adat,atime,auser) ;
		VALUES (m.ttyp,m.tcis,m.tdat,710,0,702,0, ;
		"P˝evod hospod†˝skÇho vÏsledku",m.zisk,date(),left(time(),5),m.gl_usernam)
	SET FILT TO
	WAIT CLEAR
RETURN

**	Smaz†n° vÁech z†znamÖ a p˝eveden° 702 na 701
PROCEDURE spe_obra
	SELE denik
	IF !ask(-1,-1,'N',"Opravdu smazat vÁechny neuz†vÿrkovÇ £üty") OR ;
			!ask(-1,-1,'N',"Tato operace je nevratn†, opravdu pokraüovat")
		RETURN
		ENDIF
	DO wpr
	PRIV mmd,mma
	SET FILT TO
	DELE ALL FOR !(md=702 OR dal=702)
	DELE ALL FOR (md=710 OR dal=710) AND !((md=710 AND dal=702)OR(md=702 AND dal=710))
	PACK
	SET FILT TO !del
	SET ORDER TO
	SET TALK OFF
	SCAN ALL FOR md=702 OR dal=702
		mmd=denik.md
		mma=denik.mda
		REPL md WITH dal, mda WITH daa, dal WITH m.mmd, daa WITH m.mma
		ENDSCAN
	REPL ALL dat WITH (dat+1) FOR (md=702 OR dal=702)
	* REPL ALL dtdph WITH (dtdph+1) FOR (md=702 OR dal=702)
	REPL ALL md WITH 701 FOR md=702
	REPL ALL dal WITH 701 FOR dal=702
	REPL ALL md WITH 431 FOR md=710
	REPL ALL dal WITH 431 FOR dal=710
	SET FILT TO !del
	SET TALK ON
	WAIT WIND "P˝evr†cen° £ütÖ ukonüeno"
RETURN
**	R ﬁ Z N ê
PROCEDURE hlm_rzn
	DEFINE POPUP spr FROM 8,40 RELATIVE SHADOW ;
		TITLE " R Ö z n Ç " COLOR SCHEME 4
	DEFINE BAR 30 OF spr PROMPT " \<Glob†ln° parametry" ;
		MESS "Zad†n° £dajÖ o firmÿ."
	DEFINE BAR 31 OF spr PROMPT " \<Extern° parametry" ;
		MESS "DodateünÇ systÇmovÇ parametry."
	DEFINE BAR  5 OF spr PROMPT " Oprava inde\<xÖ" ;
		MESS "P˝eindexov†n° databaz°."
	DEFINE BAR 20 OF spr PROMPT "\-"
	DEFINE BAR  1 OF spr PROMPT " \<Novinky" ;
		MESS "Popis £prav v programu."
	
	ON SELE POPU spr DO spr

	ACTI POPUP spr

	RELE POPU spr
RETURN

PROC spr
	DO CASE
		CASE bar()=1	&&	Informace o novink†ch
			DO novinky
		CASE bar()=5	&&	Oprava indexÖ
*			HIDE POPU ALL
			DO cldata
			=_open(.T.)
*			SHOW POPU ALL
		CASE bar()=30	&&	Glob†ln° parametry
			SELE firma
			DO ..\firma.spr
			IF !empty(m.ggf)
				SAVE TO (m.ggf+"firma.mem") ALL LIKE gg*
				ENDIF
		CASE bar()=31	&&	Extern° parametry
			SELE firma
			DO firma-e.spr
		CASE bar()=40
			DO statisti
		ENDCASE
RETURN
**	Zad†n° firmy k £ütov†n°
PROCEDURE hlm_fir
	PRIV pf,i,tt
	DO wrt_stat WITH "EKONT"
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
		[naz+' ≥ '+str(rok,4)+' ≥ '+jme],;
		" VÏbÿr firmy k £ütov†n° ",;
		"// Adres†˝     ≥ Rok  ≥ N†zev firmy",;
		"\ F1-Help  ENTER-Èütov†n°  F7-T˝°dit podle ...",;
		'{naz}',;
		'CR#:		DO fir_ote		## ENTER-Otev˝en° firmy k £ütov†n°',;
		'ALT+A:		DO fir_arc		## ALT+A-Zobrazovat i archivy firem',;
		iif(m.gl_pri_pu>=90, ;
			'ALT+V:	DO fir_vse		## ALT+V-Zobrazovat vÁechny firmy',''), ;
		iif(m.gl_pri_pu>=60, ;
			'F5:	DO fir_copy		## F5-Kopie firmy','')

*		iif(m.gl_pri_pu>=60, ;
*			'INS:	DO fir_ins		## INS-Nov† firma',''), ;

	IF m.esc
		DO cldata
		m.ggf=' '
		ENDIF
	DO refscr
	DO testflag
RETURN
**	Minul† obdob° £ütov†n° firmy
PROCEDURE HLM_MIN
	PRIV pf,i,tt,xnaz,xa
	
	xa=at('.',firma.naz)
	xnaz=iif(m.xa!=0,left(firma.naz,m.xa-1),alltrim(firma.naz))
	xa=len(m.xnaz)

	DO wrt_stat WITH "EKONT"
	DO cldata
	SELE firma
	SET ORDER TO naz
	arc=.F.
	SET FILT TO naz<>"_SETDBF" AND left(naz,m.xa)=m.xnaz AND;
		(at(m.gl_usernam+',',rtrim(firma.usr)+',')>0 OR ;
		at('ALL'+',',rtrim(firma.usr)+',')>0)

	GO TOP
	DO list_c WITH -1,-1,18,0,;
		[naz+' ≥ '+str(rok,4)+' ≥ '+jme],;
		" VÏbÿr firmy k £ütov†n° ",;
		"// Adres†˝     ≥ Rok  ≥ N†zev firmy",;
		"\ F1-Help  ENTER-Èütov†n°  F7-T˝°dit podle ...",;
		'{naz}',;
		'CR#:		DO fir_ote		## ENTER-Otev˝en° firmy k £ütov†n°',;
		'ALT+A:		DO fir_arc		## ALT+A-Zobrazovat i archivy firem',;
		iif(m.gl_pri_pu>=90, ;
			'ALT+V:	DO fir_vse		## ALT+V-Zobrazovat vÁechny firmy','')

	IF m.esc
		DO fir_ote
		ENDIF
	DO refscr
RETURN
**	Provede zkop°rov†n° firmy do novÇho adres†˝e
PROCEDURE fir_copy
	PRIV newadr, oldadr
	oldadr=alltrim(firma.naz)
	newadr=left(m.oldadr+space(12),12)

	PRIV _l
	_l=30
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "NovÏ adres†˝: "
	@ 0,16 GET m.newadr SIZE 1,12 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF
	DO wpr
	IF !_mkdir(m.gl_data+m.newadr)
		WAIT WIND NOWA "Nelze vytvo˝it novÏ adres†˝ '"+m.newadr+"'"
		RETURN
	ENDIF
	PRIV _cntpol
	_cntpol=8
	DIME pp(m._cntpol)
	pp(1)="DENIK"
	pp(2)="TYP"
	pp(3)="OSN"
	pp(4)="STR"
	pp(5)="DPH"
	pp(6)="ufaktury"
	pp(7)="TEXT"
	pp(8)="VYKAZ"
	FOR i=1 TO m._cntpol
		IF file(m.gl_data+m.newadr+"\"+pp(i)+".dbf")
			WAIT WIND NOWA "V novÇm adres†˝i jiß existuj° kop°rovanÇ soubory. Kopie neprovedena."
			RETURN
		ENDIF
	ENDFOR
	INSERT INTO firma (usr, naz, jme) ;
		VALUES (firma.usr, m.newadr, firma.jme)
	DO cldata
	FOR i=1 TO m._cntpol
		IF file(m.gl_data+m.oldadr+"\"+pp(i)+".dbf")
			COPY FILE (m.gl_data+m.oldadr+"\"+pp(i)+".dbf") TO ;
					(m.gl_data+m.newadr+"\"+pp(i)+".dbf")
		ENDIF
		IF file(m.gl_data+m.oldadr+"\"+pp(i)+".cdx")
			COPY FILE (m.gl_data+m.oldadr+"\"+pp(i)+".cdx") TO ;
					(m.gl_data+m.newadr+"\"+pp(i)+".cdx")
		ENDIF
	ENDFOR
	IF file(m.gl_data+m.oldadr+"\poznamky.dbf")
		COPY FILE (m.gl_data+m.oldadr+"\poznamky.dbf") TO ;
			(m.gl_data+m.newadr+"\poznamky.dbf")
		COPY FILE (m.gl_data+m.oldadr+"\poznamky.fpt") TO ;
			(m.gl_data+m.newadr+"\poznamky.fpt")
	ENDIF
	IF file(m.gl_data+m.oldadr+"\firma.mem")
		COPY FILE (m.gl_data+m.oldadr+"\firma.mem") TO ;
			(m.gl_data+m.newadr+"\firma.mem")
	ENDIF
	WAIT WIND NOWA "Kopie firmy byla £spÿÁnÿ provedena."
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
**	Otev˝en° vÁech datab†z° - vrac° .F. kdyß nÿjak† chyba
PROCEDURE fir_ote
	erruser=0
	DO wpr
	nfirma=firma.naz

* Otestuje, jsou-li otev˝eny datab†ze
	PRIV errordenik
	errordenik=.T.
	ON ERROR errordenik=.F.
	SELE denik
	ON ERROR &_onerr

	IF m.errordenik
		DO cldata
	  ELSE
		DO cldata1
		ENDIF

	ggf=m.gl_data+allt(naz)+"\"
	SELE 0
	ON ERROR erruser=error()
	USE &ggf.denik EXCLUSIVE
	ON ERROR &_onerr
	USE

*Wait wind "GGF:"+m.ggf

	IF erruser=108 OR erruser=1705
		WAIT WIND "S touto firmou pracuje jinÏ ußivatel. NELZE OTEV¸÷T."
		ggf=" "
		RETURN .F.
	ENDIF

*Wait wind "Uß jsem to p˝ebÿhl - Chyba: "+strn(m.erruser)

	SELE firma
	SET FILT TO
	IF !_open(.F.)
		WAIT WIND "S touto firmou pracuje jinÏ ußivatel. NELZE OTEV¸÷T."
		ggf=" "
		RETURN .F.
	ENDIF

	SELE DENIK
	toexit=.T.
	STORE 0 TO tstr,ttyp, mimo, mod
	DO refscr

	SET FILT TO !del
	ON ERROR GO BOTT
	SET ORDER TO ggdenor
	=gorec(m.ggdenrc)
	ON ERROR &_onerr
	ggdenrc=recno()
	ggdenor=order()
RETURN
**	Otev˝en° datab†z° vybranÇ firmy
PROCEDURE _open
PARA rei
	IF (m.rei OR file(m.ggf+"ekont.rei") AND ;
			ask(-1,-1,'A',"Obnovit indexy po hav†rii"))
		rei=.T.
		WAIT WIND NOWAIT "Obnovuji indexovÇ soubory"
	  ELSE
		rei=.F.
		ENDIF

	SET TALK OFF
	SET ALTE TO &ggf.ekont.rei
	SET ALTE ON
	SET ALTE TO

**	Aktivace statistiky
	=eff_start()
	=eff_ktim(90,2)
	st_on_dat=date()
	st_on_time=time()

*IF getdbfcp(m.ggf+"Denik.Dbf")!=852 ;
*		AND ask(-1,-1,"N","Datab†ze jsou ve starÇm k¢dov†n°. P˝evÇst na Latin2")
*
*	WAIT WIND "Datab†ze budou nyn° p˝evedeny do k¢dov†n° Latin2"
*
*	DO KamToLat WITH m.ggf
*	
*ENDIF


* DENIK
	IF !open_dbf(m.ggf, "DENIK", "op_denik", "DAT", .T., m.rei)
		RETURN .F.
	ENDIF
	IF m.rei
*		PACK
		ENDIF
	SET FILT TO !del
	GO TOP
	tdto=denik.dat
	GO BOTT
	tdtd=denik.dat
	SET ORDER TO dat

* DPH
	IF !open_dbf(m.ggf, "DPH", "op_dph", "CIS", .T., m.rei)
		RETURN .F.
	ENDIF

* OSN
	IF !open_dbf(m.ggf, "OSN", "op_osn", "UCT", .T., m.rei)
		RETURN .F.
	ENDIF
	IF !seek("343  81")
		INSERT INTO osn (uct, ana, txt, dan, dph, roz, rad) VALUES ;
		  (343,81,"Vr†cenÇ DPH zahraniün°m osob†m -  5%","N","N","P","R96")
		INSERT INTO osn (uct, ana, txt, dan, dph, roz, rad) VALUES ;
		  (343,82,"Vr†cenÇ DPH zahraniün°m osob†m - 22%","N","N","P","R96")
		INSERT INTO dph (cis, dph) VALUES (81, 5)
		INSERT INTO dph (cis, dph) VALUES (82,22)
	ENDIF

	IF !seek("343 302")
		WAIT WIND "Do £üetn° osnovy budou p˝id†ny novÇ analytiky pro DPH."
		IF file("N_OSNDPH.DBF")
			APPEND FROM N_OSNDPH.DBF
			SELE dph
			APPEND FROM N_CISDPH.DBF
			SELE osn
		  ELSE
			WAIT WIND "Nenalezen soubor pro p˝id†n° novÏch analytik DPH"
			ENDIF
	  ELSE
		IF osn.SazbaDPH=0
			REPL osn.SazbaDPH WITH  5 FOR at(str(osn.ana,4),"  21  31  51  61  72  81 302 304 322 324 332 334 444 534")>0
			REPL osn.SazbaDPH WITH 22 FOR at(str(osn.ana,4),"  22  32  52  62  73  82 303 305 323 325 333 335 445 535")>0
		ENDIF
	ENDIF

	IF !seek("3436000")
		WAIT WIND "Do £üetn° osnovy budou p˝id†ny novÇ analytiky pro DPH."
		IF file("N_DPH04.DBF")
			DELE ALL FOR uct=343 AND ana>1000
			APPEND FROM N_DPH04.DBF
		  ELSE
			WAIT WIND "Nenalezen soubor pro p˝id†n° novÏch analytik DPH"
		ENDIF
	ENDIF

	IF !seek("3432800")
		WAIT WIND "Do £üetn° osnovy budou p˝id†ny novÇ analytiky pro DPH pro rok 2012."
		IF file("N_DPH12.DBF")
*			DELE ALL FOR uct=343 AND ana>1000
			APPEND FROM N_DPH12.DBF
		  ELSE
			WAIT WIND "Nenalezen soubor pro p˝id†n° novÏch analytik DPH pro rok 2012"
		ENDIF
	ENDIF

* TYP
	IF !open_dbf(m.ggf, "TYP", "op_typ", "CIS", .T., m.rei)
		RETURN .F.
	ENDIF

** Nastaven° implicitn°ch textÖ a £ütÖ kvÖli propl†cen° faktur
	SCAN ALL
		IF betw(cis,10,19) AND empty(ptxt) AND puct=0
			REPL puct WITH 311, ptxt WITH "Èhrada V/"
			ENDIF
		IF betw(cis,20,29) AND empty(ptxt) AND puct=0
			REPL puct WITH 321, ptxt WITH "Platba P/"
			ENDIF
		ENDSCAN

* STR
	IF !open_dbf(m.ggf, "STR", "op_str", "CIS", .T., m.rei)
		RETURN .F.
	ENDIF
* TEXT
	IF !open_dbf(m.ggf, "TEXT", "op_text", "TEXT", .T., m.rei)
		RETURN .F.
	ENDIF
* FAKTURY
	PRIV vys
	vys=''

	IF !open_dbf(m.ggf, "UFAKTURY", "op_fakt", "TYPCIS", .T., m.rei)
		RETURN .F.
	ENDIF
* UCURS
	IF !open_dbf(m.ggf, "UCURS", "op_ucurs", "DAT", .T., m.rei)
		RETURN .F.
	ENDIF

	a=m.ggf
	IF file(m.ggf+"firma.mem")
		REST FROM (m.ggf+"firma.mem") ADDI
		ENDIF
	ggf=a
	IF !empty(m.ggjme)
		SELE firma
		REPL jme WITH m.ggjme
		ENDIF
	SELE denik
	SET RELA TO denik.dph INTO 'dph'
	WAIT CLEAR
RETURN
PROCEDURE op_denik
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (str N(3), typ N(3), cis N(5), dat D, datnakl D, ;
			md N(3), mda N(4), dal N(3), daa N(4), txt C(40), ;
			rtyp N(3), rcis N(5), cas N(12,2), dph N(4), zad N(12,2), ;
			cascm N(12,2), mena C(3), kurz N(12,4), ;
			sou N(8), del L, flg L, adat D, atime C(5), auser C(10), ;
			mdat D, mtime C(5), muser C(10))
		USE
		IF SetDbfCP(m._nn, 852) !=0
			WAIT WIND "Nepoda˝ilo se nastavit p˝°znak datab†ze 852"
		ENDIF
		USE (m._nn) EXCL
	  CASE m._cfn=2
		INDEX ON dtos(dat)+str(typ,3)+str(cis,5) TAG dat
		INDEX ON str(typ,3)+str(cis,5)+dtos(dat) TAG typ
		INDEX ON str(md,3)+str(mda,4)+dtos(dat) TAG md
		INDEX ON str(dal,3)+str(daa,4)+dtos(dat) TAG dal
		INDEX ON cas TAG cas
		INDEX ON str(dph,4)+dtos(dat)+str(typ,3)+str(cis,5) TAG dph
*		INDEX ON str(str,3)+dtos(dat) TAG str
*		INDEX ON dtos(dtdph)+str(typ,3)+str(cis,5) TAG dtdph
		SET EXCL ON
*		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("kurz")="U"
*		RETURN fsize('cis')!=5
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_osn
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (uct N(3), ana N(4), sazbaDPH N(2), txt C(55), txtA C(55), ;
			txtN C(55), poznamka C(60), dan C(1), dph C(1), roz C(1), flg1 L, cmd N(15,2), ;
			cdal N(15,2), rad C(4), slo C(1))
	  CASE m._cfn=2
		INDEX ON str(uct,3)+str(ana,4) TAG uct
		INDEX ON cs(txt) TAG txt
		INDEX ON cs(Poznamka) TAG Poznamka
		SET EXCL ON
*		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("OSN.SazbaDPH")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_typ
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis N(3), txt C(30), puct N(3), ptxt C(25), ;
			is_dt_nak C(1), is_mena C(1), is_kniha C(1), cr_vs C(12), ;
			TypRady C(2))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON cs(txt) TAG txt
		SET EXCL ON
*		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("typ.TypRady")="U"
	  CASE m._cfn=4
		REPL ALL is_dt_nak WITH "N", is_mena WITH "N", ;
			is_kniha WITH "N" FOR is_dt_nak=" "
		ENDCASE
RETURN

PROCEDURE op_str
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis N(3), txt C(30))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON cs(txt) TAG txt
		SET EXCL ON
*		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("str.txt")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_dph
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis N(4), dph N(5,2), txt C(35), new c(1))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON cs(txt) TAG txt
		SET EXCL ON
*		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("dph.new")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_text
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (text C(40))
	  CASE m._cfn=2
		INDEX ON cs(text) TAG text
		SET EXCL ON
*		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("text.text")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_fakt
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (typ N(3), cis N(5), uct N(3), ana N(5), odb C(55), ;
			vys D, spl D, zap D, var C(12), cas N(14,2), kos C(1), ;
			cascm N(12,2), mena C(3), kurz N(12,4))
	  CASE m._cfn=2
		INDEX ON str(typ,3)+str(cis,5)+right("0"+strn(year(vys),4),2) TAG typcis
		INDEX ON var TAG var
		INDEX ON cas TAG cas
		INDEX ON cascm TAG cascm
		INDEX ON str(uct,3)+str(ana,5) TAG uctana
		INDEX ON cs(odb) TAG odb
		SET EXCL ON
		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN type("ufaktury.odb")="U"
*		RETURN fsize('cis')!=5
	  CASE m._cfn=4
		ENDCASE
RETURN
**	Open CEXPO		- Exportn° datab†ze
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

**	Datab†ze opakuj°c°ch se plateb a vÏbÿrÖ
PROCEDURE OP_UCURS
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (str N(3), typ N(3), cis N(5), dat D, ;
			md N(3), mda N(4), dal N(3), daa N(4), txt C(40), ;
			rtyp N(3), rcis N(5), cas N(12,2), dph N(4), zad N(12,2), ;
			sou N(8), flg L, del L, adat D, atime C(5), auser C(10), ;
			mdat D, mtime C(5), muser C(10), jmeno C(8))
	  CASE m._cfn=2
		INDEX ON dtos(dat)+str(typ,3)+str(cis,5) TAG dat
		INDEX ON str(typ,3)+str(cis,5)+dtos(dat) TAG typ
		INDEX ON str(md,3)+str(mda,4)+dtos(dat) TAG md
		INDEX ON str(dal,3)+str(daa,4)+dtos(dat) TAG dal
		INDEX ON cas TAG cas
		INDEX ON str(dph,4)+dtos(dat)+str(typ,3)+str(cis,5) TAG dph
		INDEX ON cs(jmeno) TAG jmeno
		SET EXCL ON
		PACK
		SET EXCL OFF
	  CASE m._cfn=3
		RETURN fsize('cis')!=5
	  CASE m._cfn=4
		ENDCASE
RETURN
**  Uzav˝en° datab†z°
PROCEDURE cldata
	IF file(m.ggf+"ekont.rei")
		ERASE (m.ggf+"ekont.rei")
		ENDIF
	DO cldata1
RETURN
**	Nov† str†nka s testem jej°ho tisku a tisku hlaviüky
PROCEDURE head
	IF between(m.page,m.pgbeg,m.pgend).AND.tststr(m.page)
		SET ALTE ON
		IF m.ff
			??chr(12)
		  ELSE
			m.ff=.T.
			ENDIF
	  ELSE
		SET ALTE OFF
		ENDIF
	DO &headprog

*	IF type("llhere") = "U"
*		llhere=.F.
*		ENDIF

	IF m.llhere
*			WAIT WIND NOWA "je nastaven llhere"
      ELSE
*			WAIT WIND NOWA "neni nastaven llhere"
		ENDIF

*IF !m.llhere
	page=m.page+1
*	ENDIF	
llhere=.F.
RETURN
**	Testuje pot˝ebu odstr†nkov†n°
PROCEDURE tstpgend
PARA _ll
	IF m.ll+m._ll > m.len_pg
		DO head
		ENDIF
RETURN .T.
**	Test tisku sudÇ a lichÇ str†nky
PROCEDURE tststr
PARA _np
	IF m.typtisk<>"Oboj°"
		IF m.typtisk<>"LichÇ"
			RETURN mod(m._np,2)=0		&&	Pro sudÇ
		  ELSE
			RETURN mod(m._np,2)<>0		&&	Pro lichÇ
			ENDIF
		ENDIF
RETURN .T.
**	Vyp°Áe do waitu pracuji
PROC wpr
	WAIT WIND NOWA "Pracuji ..."
RETURN
**	Vrac° poüet analytik danÇho £ütu
PROCEDURE countana
PARA _uc
PRIV _se,_rc,_pp
	_se=sele()
	SELE osn
	_rc=recno()
	COUNT FOR osn.uct=_uc TO _pp
	=gorec(_rc)
	SELE (_se)
RETURN _pp
**	ASKN - Funkce ASK, bez moßnosti ESC
PROCEDURE askn
	PARA y, x, m.imp, m.txt
	PRIV m.ret
	m.esc=.T.
	DO WHILE esc
		m.ret=ask(y,x,m.imp,m.txt)
		ENDDO
RETURN (m.ret)
**	Otev˝en° tiskovÇho souboru
PROCEDURE opfile
	PUBLIC m.tskf
	m.tskf=ggf+"ekont.prt"
	SET ALTE TO (m.tskf)
	SET ALTE ON
	SET CONS OFF
	??m.resetprn
RETURN
**	VÏpoüet maxim†ln°ho ü°sla v typu dokladu
PROC mxcisdok
PARA _td
	PRIV _rc,_sl,_pp
	_sl=select()
	SELE denik
	_rc=recno()
	CALC max(cis) FOR m._td=typ .AND. m._rc!=recno() TO _pp
	=gorec(_rc)
	SELE (_sl)
RETURN _pp
**	Zobrazen° pro funkci sumdokl, mxcisdok
PROC viewsumy
	ON KEY LABEL ALT+S
	PRIVATE _rc, _or, _tp, _ci
	_rc=recno()
	_or=order()
	_fi=filter()
	_tp=typ
	_ci=cis
*	COUNT TO sumrec
	a='"'+str(_tp,3)+str(_ci,5)+'"'
*	CALC sum(cas) TO sumdok ;
*		FOR str(typ,3)+str(cis,5)= &a
	SET ORDER TO typ
	SET FILT TO !del
	sumdok=0
	SEEK str(_tp,3)+str(_ci,5)
	SCAN REST WHILE _tp=typ AND _ci=cis
		sumdok=m.sumdok+cas
		ENDSCAN
	SET NEAR ON
	SEEK str(_tp,3)+str(99999,4)
	mxcisdok=0
	IF !bof()
		SKIP -1
		ENDIF
	DO WHILE !bof() AND typ=_tp AND cis=_ci
		SKIP -1
		ENDDO
	IF !bof() AND _tp=typ
		mxcisdok=cis
		ENDIF
	SET NEAR OFF
	SET FILT TO &_fi
	SET ORDER TO &_or
	=gorec(_rc)
*	mxcisdok=mxcisdok(denik.typ)
*	SHOW GETS OFF
	ON KEY LABEL ALT+S SHOW GETS
RETURN sumdok

PROCEDURE capstru
PARA typesize
	PRIVATE i, ww, t, n, ff
	n=afield(ff)
	ww=''
	FOR i=1 TO m.n
		ww=m.ww+', '+lower(alltrim(ff[m.i,1]))
		IF m.typesize
			t=upper(ff[m.i,2])
			ww=m.ww+' '+m.t
			DO CASE
				CASE m.t='D'
				CASE m.t='L'
				CASE m.t='M'
				CASE ff[m.i,4]=0
					ww=m.ww+'('+ltrim(str(ff[m.i,3]))+')'
				OTHER
					ww=m.ww+'('+ltrim(str(ff[m.i,3]))+','+;
						ltrim(str(ff[m.i,4]))+')'
				ENDCASE
			ENDIF
		ENDFOR
RETURN alltrim(subst(m.ww,2))
