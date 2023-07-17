SET CLOCK TO 0,0
SET CENT ON

SET PROC TO evim

**********************************
****	D…leงit glob lnก promุnn - budou ukl d ny na disk

**	Dal็ก d…leงit glob lnก promุnn
PUBLIC typtisk, pgbeg, pgend, page, nfirma
typtisk="Obojก"		&&	Typ tisku str nek	Obojก,Lich,Sud
pgbeg=1
pgend=9999
page=1
kalk_vysl='0'
nfirma=''

******************
**	Dal็ก promุnn
public m.esc
end=.F.

DO refscr

*******************
****	Hlavnก MENU
DEFINE POPUP hlm_e from 6,30 relative shadow title " Hlavnก menu - IM " color scheme 4
DEFINE BAR  2 OF hlm_e PROMPT " \<Evidence majetku "	SKIP FOR m.ggf=' ' ;
	MESS "Evidence majetku."
DEFINE BAR  4 OF hlm_e PROMPT " \<Pýenos do P้ "	SKIP FOR m.ggf=' ' ;
	MESS "Pýenos odpis… do ฃetnictvก na z kladุ pýedฃtovacกho pýedpisu."
DEFINE BAR 10 OF hlm_e PROMPT "\-"	SKIP FOR m.ggf=' '
DEFINE BAR 29 OF hlm_e PROMPT " ฌก\<selnกky " SKIP FOR m.ggf=' ' ;
	MESS "Stýediska, odpisov sazby, a jin tabulky."
DEFINE BAR 30 OF hlm_e PROMPT " \<Tiskov sestavy " SKIP FOR m.ggf=' ' ;
	MESS "Tiskov sestavy."
DEFINE BAR 60 OF hlm_e PROMPT "\-"	SKIP FOR m.ggf=' '
DEFINE BAR 20 OF hlm_e PROMPT " \<Firma" ;
	MESS "Volba jin firmy."
DEFINE BAR 90 OF hlm_e PROMPT " \<R … z n "	SKIP FOR m.ggf=' ' ;
	MESS "Z lohov nก, obnova a indexov nก dat. Informace o ฃprav ch programu."
ON SELE POPU hlm_e DO hlm_e

DEFI WIND w_lline FROM 24,0 TO 24,80 NONE COLOR SCHEME 14

*********************************
**	ZAฌตTEK HLAVNึ ฌตSTI PROGRAMU

a=substr(m.ggf,rat('\',m.ggf,2)+1)
a=left(a,len(a)-1)

DO cldata1
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

	ACTIVATE POPUP hlm_e BAR _bar

	IF bar()=0
		DO hlm_kon
		EXIT
		ENDIF
	ENDDO
RETURN

**	Obnova obrazovky
PROCEDURE refscr
	DO backgr WITH 'main'
	IF !empty(m.ggf)
		PRIVATE t
		t="้tovan  firma: "+alltrim(m.ggjme)
		@ 0,int((scols()-len(m.t))/2) SAY m.t
	ENDIF
RETURN

**	KONEC HLAVNึ ฌตSTI PROGRAMU
*******************************

**	Ukonenก pr ce programu a n vrat do HLAVNIHO SYSTEMU
PROCEDURE hlm_kon
	DO wrt_stat WITH "EVIM"
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

	SET CENT OFF
RETURN

*****************************
****  HLAVNI PODPROGRAMY MENU
PROCEDURE hlm_e
	hl="hlm_"+left(onlya_z(prompt()),3)
	DO &hl
RETURN
**	Evidence majetku
PROCEDURE hlm_evi
	SELE evim
	SET FILT TO

	_ln10=[:ozn+' '+invc+' ณ '+dtoc(dat)+' ณ '+str(porcena,12,2)+' ณ '+left(naz,27)+' ณ'+str(evim.str,3)]
	_ln11="//  Inv. กslo ณ Datum poý. ณ   Poý. cena  ณ N zev"+space(22)+" ณStr"
	_ln20=[:ozn+' '+invc+' ณ '+left(naz,60)]
	_ln21="//  Inv. กslo ณ N zev"
	_ln30=[:ozn+' '+invc+' ณ '+dtoc(dat)+' ณ '+str(porcena,12,2)+' ณ '+left(naz,20)+' ณ '+skp]
	_ln31="//  Inv. กslo ณ Datum poý. ณ   Poý. cena  ณ N zev                ณ S K P"
	_ln35=[:ozn+' '+invc+' ณ '+dtoc(dtzrus)+' ณ '+str(porcena,12,2)+' ณ '+left(naz,20)+' ณ '+skp]
	_ln36="//  Inv. กslo ณ Datum vyý. ณ   Poý. cena  ณ N zev                ณ S K P"
	_ln40=[:ozn+' '+invc+' ณ '+str(odp_md,3)+str(odp_mda,3)+' / '+ ;
		str(odp_dal,3)+str(odp_daa,3)+' ณ '+ ;
		str(nls_md,3)+str(nls_mda,3)+' / '+ ;
		str(nls_dal,3)+str(nls_daa,3)+' ณ '+ ;
		str(opr_md,3)+str(opr_mda,3)+' / '+ ;
		str(opr_dal,3)+str(opr_daa,3)]
	_ln41="//  Inv. กslo ณ Pýedpis - odpis ณ Pýedpis - NLS   ณ Opravn  poloงka "
	DO list_c WITH -1,-1,12,0,;
		" Evidence investinกho majetku ",;
		m._ln10, m._ln11, m._ln20, m._ln21, m._ln30, m._ln31, m._ln35, m._ln36, m._ln40, m._ln41, ;
		"\ F1-Help   ENTER-Odpisy   F4-Oprava   INS-Pýid nก majetku",;
		'INS:		DO maj_pri WITH .T.		## INS-Pýid nก majetku',;
		'F4#:		DO maj_pri WITH .F.		## F4-Oprava majetku',;
		'ENTER#:	DO maj_odp				## ENTER-Odpisy majetku',;
		'ALT+T#:	DO maj_tz				## ALT+T-Technick zhodnocenก',;
		'ALT+D#:	DO maj_del				## ALT+D-Smaz nก majetku',;
		"SPACE#:	DO maj_ozn				## SPACE-Inverze oznaenก 1 z znamu " ,;
		"ALT+B#:	DO maj_oznf				## ALT+B-Oznaenก z znam… dle v์bุru ", ;
		"ALT+A#:	DO maj_inva				## ALT+A-Inverze oznaenก v็ech z znam… ", ;
		"ALT+I#:	DO maj_ik				## ALT+I-Tisk vybran์ch inv. karet ", ;
		"F6#:		DO maj_zmic				## F6-Zmุna invent rnกho กsla ", ;
		"ALT+K#:	DO maj_kop				## ALT+K-Zkopกrov nก ฃ.pýedpisu "
		
*		"ALT+O#:	DO maj_seto				## ALT+O-Nastavenก odpisov์ch skupin do odpis… "

RETURN

PROCEDURE maj_pri
PARA app

* Pýekladov omezenก na 5 z znam… pro demo verzi
	IF !m.FULLVER
		IF recc()>=5 AND m.app
			WAIT WIND "V DEMO verzi lze zadat pouze 5 z znam…."
			RETURN
			ENDIF
		ENDIF

	DO evim.spr
RETURN

PROCEDURE maj_del
	IF ask(-1,-1,'N',"Opravdu smazat vybran์ IM i s jeho odpisy")
		SELE evodp
		DELE ALL FOR evim.invc=evodp.invc
		SELE evim
		DELE
		ENDIF
RETURN

***************************
**	Zkopกrov nก ฃ.pýedpisu
PROCEDURE maj_kop
PRIV _rc
	_rc=recno()
	SKIP -1
	IF bof()
		=gorec(m._rc)
	  ELSE
		SCATTER FIELD odp_md, odp_mda, odp_dal, odp_daa, ;
			nls_md, nls_mda, nls_dal, nls_daa, ;
			opr_md, opr_mda, opr_dal, opr_daa MEMVAR
		=gorec(m._rc)
		GATHER MEMVAR FIELD odp_md, odp_mda, odp_dal, odp_daa, ;
			nls_md, nls_mda, nls_dal, nls_daa, ;
			opr_md, opr_mda, opr_dal, opr_daa
		SKIP 1
		ENDIF
	refscr=.T.
RETURN

********************
**	Oznaenก z znamu dle v์bุru
PROCEDURE maj_oznf
PRIV _rc, _l, _fi
	_rc=recno()
	_fi=space(len(evim.invc))
	_l=27+len(m._fi)
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "ฌ st invent rnกho กsla: "
	@ 0,25 GET m._fi SIZE 1,len(m._fi) COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	REPL ozn WITH '๛' ALL FOR at(alltrim(m._fi),evim.invc)>0
	=gorec(m._rc)
	refscr=.T.
RETURN

********************
**	Oznaenก z znamu
PROCEDURE maj_ozn
	REPL ozn WITH iif(ozn='๛',' ','๛')
	SKIP 1
RETURN

****************************
**	Zmุna invent rnกho กsla
PROCEDURE maj_zmic
	PRIV _l, nic
	nic=space(len(evim.invc)-len(rtrim(invc)))+rtrim(invc)
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Nov invent rnก กslo: "
	@ 0,24 GET m.nic SIZE 1,10 PICTURE '@JK' COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF
	_rc=recno()
	nic=space(len(evim.invc)-len(rtrim(nic)))+rtrim(nic)
	SET ORDER TO invc
	IF seek(cs(m.nic))
		WAIT WIND "Invent rnก กslo '"+nic+"' jiง existuje"
		=gorec(m._rc)
		RETURN
		ENDIF
	=gorec(m._rc)
	SELE evtz
	REPL invc WITH m.nic FOR invc=evim.invc ALL
	SELE evodp
	REPL invc WITH m.nic FOR invc=evim.invc ALL
	SELE evim
	REPL invc WITH m.nic
	WAIT WIND NOWA "Zmุna invent rnกho กsla provedena"
RETURN
**	Tisk invent rnกch karet pro vybran์ majetek
PROCEDURE maj_ik
	PRIV _rc, dt, pc, lmarg
	lmarg=8
	WAIT WIND NOWA 'Tiskov์ v์stup: '+imp_output+' - '+rtrim(m.&imp_output)
	SET CONS OFF
	DO opfile
	SELE evim
	_rc=recno()
	SET FILT TO ozn='๛'
	GO TOP
	IF eof()
		SET FILT TO recno()=_rc
		GO TOP
		ENDIF
	DO WHILE !eof()
		ll=9
		?space(lmarg)+m.nlq+"             INVENTตRNึ KARTA INVESTIฌNึHO MAJETKU"
		?space(lmarg)+"            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		?
		?space(lmarg)+"Invent rnก กslo            : "+evim.invc+'   '+m.draft
		?space(lmarg)+"Vymezenก IM a jeho zatýกdุnก: "+evim.naz
		?space(lmarg)+"Druh majetku                : "+evim.druh
		?space(lmarg)+"Datum uvedenก do uงกv nก    : "+dtoc(evim.dat)
		?space(lmarg)+"Poýizovacก cena             : "+str(evim.porcena,12,2)
		?
		SELE evtz
		SET FILT TO evim.invc=invc
		SET ORDER TO dttz
		GO TOP
		IF !eof()
			a=0
			IF ll+5>=m.len_pg
				??chr(12)
				ll=0
				ENDIF
			?space(lmarg)+" Technick zhodnocenก:"+m.cnon
			?space(lmarg)+"ฺฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
			?space(lmarg)+"ณ    Datum   ณ     ฌ stka   ณ  Text                     ณ"
			?space(lmarg)+"รฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
			SCAN ALL
				IF ll+8>=m.len_pg
					?chr(12)
					?m.cnoff+space(lmarg)+" Technick zhodnocenก:  (pokraov nก ...)"+m.cnon
					?space(lmarg)+"ฺฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
					?space(lmarg)+"ณ    Datum   ณ     ฌ stka   ณ  Text                     ณ"
					?space(lmarg)+"รฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
					ll=4
					ENDIF
				?space(lmarg)+"ณ "+dtoc(dttz)+" ณ "+str(cas,12,2)+" ณ "+txt+" ณ "
				a=a+cas
				ll=ll+1
				ENDSCAN
			?space(lmarg)+"ภฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
			?space(lmarg)+space(13)+"ณ "+str(a,12,2)+" ณ"
			?space(lmarg)+space(13)+"ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
			?
			GO BOTT
			?space(lmarg)+" Zv์็en  poýizovacก cena:"+str(porcena(evim.invc,evtz.dttz),12,2)
			?
			ll=ll+6
			ENDIF
		SET FILT TO
		SELE evim
		IF ll+7>=m.len_pg
			?chr(12)
			ll=0
			ENDIF
		?m.cnoff+space(lmarg)+" ้ฌETNึ ODPISOVตNึ"+m.cnon
		?space(lmarg)+"Zp…sob odpisov nก: "+evim.zpodpisu
		?space(lmarg)+"ฺฤฤฤฤฤฤฤยฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
		?space(lmarg)+"ณ       ณ    ณ              ณ              ณ              ณ            Opravn   poloงka            ณ"
		?space(lmarg)+"ณ  Rok  ณMุs.ณ     Odpis    ณ Mimoý. odpis ณ  Z…st. cena  ณ    Vznik        Hodnota       Zru็enก  ณ"
		?space(lmarg)+"รฤฤฤฤฤฤฤลฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤด"
		ll=ll+6
		a=0
		SELE evodp
		SET FILT TO evim.invc=invc
		SET ORDER TO rokodp
		SCAN ALL
			IF ll+4>=m.len_pg
				??chr(12)
				?m.cnoff+space(lmarg)+" ้ฌETNึ ODPISOVตNึ  (pokraov nก ...)"+m.cnon
				?space(lmarg)+"ฺฤฤฤฤฤฤฤยฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
				?space(lmarg)+"ณ       ณ    ณ              ณ              ณ              ณ            Opravn   poloงka            ณ"
				?space(lmarg)+"ณ  Rok  ณMุs.ณ     Odpis    ณ Mimoý. odpis ณ  Z…st. cena  ณ    Vznik        Hodnota       Zru็enก  ณ"
				?space(lmarg)+"รฤฤฤฤฤฤฤลฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤด"
				ll=5
				ENDIF
			?space(lmarg)+'ณ '+str(year(evim.dat)+evodp.dorokodp-1,5)+' ณ '+ ;
				str(uopocmes,2)+' ณ '+str(uoodpis,12,2)+' ณ '+str(uomimor,12,2)+' ณ '+ ;
				str(uozc,12,2)+' ณ '+iif(oprpol>0,dtoc(dtoprpol),space(10))+' ณ '+ ;
				str(oprpol,12,2)+' ณ '+iif(oprpol<0,dtoc(dtoprpol),space(10))+' ณ '
			a=a+uoodpis+uomimor
			ll=ll+1
			ENDSCAN
		?space(lmarg)+"ภฤฤฤฤฤฤฤมฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤู"
		?space(lmarg)+space(13)+"ณ   ---------->> "+str(a,12,2)+" ณ"
		?space(lmarg)+space(13)+"ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
		ll=ll+3
		IF ll+7>=m.len_pg
			?chr(12)
			ll=0
		  ELSE
			?
			ll=ll+1
			ENDIF
		?m.cnoff+space(lmarg)+" DAีOV ODPISOVตNึ"+m.cnon
		?space(lmarg)+"Metoda odpisov nก: "+evim.domet
		?space(lmarg)+"ฺฤฤฤฤฤฤฤยฤฤฤฤยฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
		?space(lmarg)+"ณ Obdobกณ RO ณ  OS ณ     Odpis    ณ  Z…st. cena  ณ"
		?space(lmarg)+"รฤฤฤฤฤฤฤมฤฤฤฤลฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
		ll=ll+5
		a=0
		SCAN ALL
			IF ll+4>=m.len_pg
				??chr(12)
				?m.cnoff+space(lmarg)+" DAีOV ODPISOVตNึ  (pokraov nก ...)"+m.cnon
				?space(lmarg)+"ฺฤฤฤฤฤฤฤยฤฤฤฤยฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
				?space(lmarg)+"ณ Obdobกณ RO ณ  OS ณ     Odpis    ณ  Z…st. cena  ณ"
				?space(lmarg)+"รฤฤฤฤฤฤฤลฤฤฤฤลฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
				ll=4
				ENDIF
			?space(lmarg)+'ณ '+str(year(evim.dat)+evodp.dorokodp-1,5)+' ณ '+ ;
				str(dorokodp,2)+' ณ '+str(doskup,3)+' ณ '+str(doodpis,12,2)+' ณ '+ ;
				str(dozc,12,2)+' ณ'
			a=a+doodpis
			ll=ll+1
			ENDSCAN		
		?space(lmarg)+"ภฤฤฤฤฤฤฤมฤฤฤฤมฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
		?space(lmarg)+space(19)+'ณ '+str(a,12,2)+' ณ'
		?space(lmarg)+space(19)+"ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
		ll=ll+3
		IF ll+6>=m.len_pg
			?chr(12)
			ll=0
		  ELSE
			ll=ll+1
			?
			ENDIF
		?m.cnoff+space(lmarg)+" OSTATNึ ้DAJE"+m.cnon
		?space(lmarg)+"ฺฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
		?space(lmarg)+"ณ Obdobกณ  Nadl.sloงka ณ Odpo.par.34 ณ"
		?space(lmarg)+"รฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
		ll=ll+4
		a=0
		SCAN ALL
			IF ll+6>=m.len_pg
				??chr(12)
				?m.cnoff+space(lmarg)+" OSTATNึ ้DAJE  (pokraov nก ...)"+m.cnon
				?space(lmarg)+"ฺฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
				?space(lmarg)+"ณ Obdobกณ  Nadl.sloงka ณ Odpo.par.34 ณ"
				?space(lmarg)+"รฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
				ll=4
				ENDIF
			?space(lmarg)+'ณ '+str(year(evim.dat)+evodp.dorokodp-1,5)+ ;
				' ณ '+str(nadlsl,12,2)+' ณ '+str(odppar34,12,2)+' ณ '
			a=a+nadlsl
			ll=ll+1
			ENDSCAN
		?space(lmarg)+"ภฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
		?space(lmarg)+space(8)+'ณ '+str(a,12,2)+' ณ'
		?space(lmarg)+space(8)+"ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
		?
		SET FILT TO
		SELE evim
		?space(lmarg)+"Datum vyýazenก: "+dtoc(dtzrus)+"     Zp…sob vyýazenก: "+zpzrus+"     Schv lil:"
		??m.cnoff+chr(12)
		SKIP 1
		ENDDO
	SET ALTE TO
	SET CONS ON
	SELE evim
	DO tisk WITH lmarg,.T.
RETURN
**	Inverze oznaenก
PROCEDURE maj_inva
	PRIV a
	a=ozn
	REPL ozn WITH iif(a='๛',' ','๛') ALL
	GO TOP
	refscr=.T.
RETURN
**	Odpisy majetku
PROCEDURE maj_odp
	SELE evodp
	SET ORDER TO rokodp
	SET FILT TO invc=evim.invc
	DO list_c WITH -1,-1,12,0,;
		" Odpisy IM inv..: "+left(alltrim(evim.invc)+" - "+alltrim(evim.naz),40)+' ',;
		[:str(dorokodp,2)+' ณ'+str(doodpis,12,2)+' ณ'+str(dozc,12,2)+ ;
			' ณ'+str(uoodpis+uomimor,12,2)+' ณ'+str(uozc,12,2)+' ณ '+left(popodp,14)],;
		"//Rokณ DO  ฌ stka  | Z…st. cena  ณ ้O  ฌ stka  | Z…st. cena  ณ Pozn mka",;
		"\ F1-Help    ENTER-Oprava   INS-Pýid nก odpisu   F7-Týกdit podle ...",;
		'CR#:		DO odp_pri WITH .F.		## ENTER-Oprava ',;
		'INS:		DO odp_pri WITH .T.		## INS-Pýid nก odpisu',;
		'ALT+D#:	DO odp_del				## ALT+D-Smaz nก poslednกho odpisu'
RETURN

PROCEDURE odp_pri
PARA app
	IF app
		GO BOTT
		SCATTER MEMVAR
		IF eof()
			invc=evim.invc
			zcmouc=evim.porcena
			zcmodan=evim.porcena
			dorokodp=1
			doskup=evim.doskup
		  ELSE
			zcmouc=evodp.uozc
			zcmodan=evodp.dozc
			uoodpis=0
			uozc=0
			dopolsaz='Ne '
			dorokodp=evodp.dorokodp+1
			odppar34=0
			ENDIF
		IF !empty(evim.dtzrus) AND year(evim.dtzrus)<year(evim.dat)+m.dorokodp-1
			WAIT WIND NOWA "Majetek je jiง vyýazen"
			RETURN
			ENDIF
	  ELSE
		SCATTER MEMVAR
		IF m.dorokodp>1
			SKIP -1
			zcmouc=evodp.uozc
			zcmodan=evodp.dozc
			SKIP 1
			ENDIF
		ENDIF

	zvporc=porcena(evim.invc,ctod('31.12.'+str(year(evim.dat)+m.dorokodp-1,4)))

	DO load_odp

	DO evodp.spr
	IF !(m.esc AND !ask(-1,-1,'A',"Zapsat zmุny")))
		IF m.app
			APPEND BLANK
			ENDIF
		GATHER MEMVAR
		ENDIF
	IF !m.app
		SKIP 1
		IF eof()
			GO BOTT
			ENDIF
		ENDIF
RETURN

PROCEDURE odp_del
	GO BOTT
	IF ask(-1,-1,'A',"Opravdu zru็it odpis roku "+str(year(evim.dat)+dorokodp-1,4))
		DELE
		ENDIF
RETURN

************************
**	Technick zhodnocenก
PROCEDURE maj_tz
	SELE evtz
	SET ORDER TO dttz
	SET FILT TO invc=evim.invc
	DO list_c WITH -1,-1,12,0, ;
		[dtoc(dttz)+' ณ '+str(cas,12,2)+' ณ '+txt], ;
		" TZ IM inv..: "+alltrim(evim.invc)+" - "+alltrim(evim.naz)+' ', ;
		"// Datum     ณ     ฌ stka   ณ  Text", ;
		"\ F1-Help    ENTER-Oprava   INS-Pýid nก TZ   F7-Týกdit podle ...", ;
		'CR#:		DO tz_pri WITH .F.		## ENTER-Oprava TZ', ;
		'INS:		DO tz_pri WITH .T.		## INS-Pýid nก TZ', ;
		'ALT+D#:	DO tz_del				## ALT+D-Smaz nก TZ'
RETURN

PROCEDURE tz_pri
PARA app
	DO evtz.spr
RETURN

PROCEDURE tz_del
	IF ask(-1,-1,'A',"Opravdu zru็it TZ")
		DELE
		ENDIF
RETURN

*******************
**	Tiskov sestavy
PROCEDURE hlm_tis
	DEFINE POPUP tis FROM 10,35 RELATIVE SHADOW ;
		TITLE " Tiskov sestavy " COLOR SCHEME 4
	DEFINE BAR 10 OF tis PROMPT " \<Odpisov์ pl n " ;
		MESS "Zpracov nก odpisovho pl nu na zadan obdobก."
*	DEFINE BAR 20 OF tis PROMPT " \<V้D k odpisovmu pl nu " ;
*		MESS "Tisk podkladu k zaฃtov nก odpis…."

	ON SELE POPU tis DO tis

	ACTI POPUP tis

	RELE POPU tis
RETURN

PROCEDURE tis
	hl="tis_"+left(onlya_z(prompt()),3)
	DO &hl
RETURN
**	Tisk odpisovho pl nu
PROCEDURE tis_odp
	PRIV a, _l, br, lmarg, tpt
	br=year(date())-1
	tpt="ๆirok "
	str=0

	SELE 0
	CREA CURS cu1 (cuinvc C(10))
	INDEX ON cuinvc TAG cuinvc
	SET ORDER TO cuinvc

	SELE str
	GO TOP

	DO odppl.spr

	IF lastkey()=27
		RETURN
		ENDIF
	tpt= m.tpt="ๆirok "

	SET ORDER TO cis
	IF !seek(m.str) AND m.str!=0
		WAIT WIND NOWA "Stýedisko ."+strn(m.str)+" nenalezeno."
		nstr=""
		ENDIF
	nstr=txt
	lmarg=8
	SET CONS OFF
	=opfile('Odpplan')

	PRIV uo,porc,zvpc,uzc,do,nls,o10,p,ll
	ll=1

	PRIV cnt_skup
	cnt_skup=7
	DIME pp(m.cnt_skup)
	STORE 0 TO uo,porc,zvpc,uzc,do,nls,o10,p,pp, ;
		chm, ohm, nhm, cnm, onm, nnm, cdhm, odhm, ndhm, cdnm, odnm, ndnm

	DO tiskodpp WITH m.tpt, .T., '(evim.druh="Hmotn์" OR evim.druh="Nehmotn์")'
	IF !m.tpt
		?chr(12)
		ll=1
		STORE 0 TO uo,porc,zvpc,uzc,do,nls,o10,p,pp, ;
			chm, ohm, nhm, cnm, onm, nnm, cdhm, odhm, ndhm, cdnm, odnm, ndnm
		DO tiskodpp WITH m.tpt, .F., '(evim.druh="Hmotn์" OR evim.druh="Nehmotn์")'
		?chr(12)
		ll=1
		ENDIF
	?

	STORE 0 TO uo,porc,zvpc,uzc,do,nls,o10

	DO tiskodpp WITH m.tpt, .T., '(evim.druh="Drobn์ hmotn์" OR evim.druh="Drobn์ nehmotn์")'
	IF !m.tpt
		?chr(12)
		ll=1
		STORE 0 TO uo,porc,zvpc,uzc,do,nls,o10,p, ;
			cdhm, odhm, ndhm, cdnm, odnm, ndnm
		DO tiskodpp WITH m.tpt, .F., '(evim.druh="Drobn์ hmotn์" OR evim.druh="Drobn์ nehmotn์")'
		ENDIF
	?


	ll=ll+4
	IF ll+12>=m.len_pg
		?chr(12)
		ll=1
		ENDIF
	?space(m.lmarg*1.6)+" Odpisy hmotnho a nehmotnho majetku podle jednotliv์ch odpisov์ch skupin"
	?space(m.lmarg*1.6)+"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	?space(m.lmarg*1.6)+'ฺออออออออออออออออออออออออออออออออออออฟ'
	FOR i=1 TO m.cnt_skup
		?space(m.lmarg*1.6)+'ณ '+iif(i=7,'1A.',str(i,1)+'. ')+'odpisov  skupina ณ '+str(pp(i),12,2)+' ณ'
		p=p+pp(i)
		ENDFOR
	?space(m.lmarg*1.6)+'ณออออออออออออออออออออออออออออออออออออณ'
	?space(m.lmarg*1.6)+'ณ Celkem:             ณ '+str(p,12,2)+' ณ'
	?space(m.lmarg*1.6)+'ภออออออออออออออออออออออออออออออออออออู'
	?
	ll=ll+12
	IF ll+13>=m.len_pg
		?chr(12)
		ll=1
		ENDIF
	?space(m.lmarg*1.6)+" Odpisy hmotnho a nehmotnho majetku podle druhu"
	?space(m.lmarg*1.6)+"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	?space(m.lmarg*1.6)+'ฺอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออฟ'
	?space(m.lmarg*1.6)+'ณ Druh majetku    ณ Poýiz. cena  ณ Daๅov์ odpis ณ    N L S     ณ ้etnก odpis ณ'
	?space(m.lmarg*1.6)+'ณอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออณ'
	?space(m.lmarg*1.6)+'ณ Hmotn์          ณ '+str(m.chm,12,2)+' ณ '+str(m.ohm-m.nhm,12,2)+' ณ '+str(m.nhm,12,2)+' ณ '+str(m.ohm,12,2)+' ณ '
	?space(m.lmarg*1.6)+'ณ Nehmotn์        ณ '+str(m.cnm,12,2)+' ณ '+str(m.onm-m.nnm,12,2)+' ณ '+str(m.nnm,12,2)+' ณ '+str(m.onm,12,2)+' ณ '
	?space(m.lmarg*1.6)+'ณ Drobn์ hmotn์   ณ '+str(m.cdhm,12,2)+' ณ '+str(m.odhm-m.ndhm,12,2)+' ณ '+str(m.ndhm,12,2)+' ณ '+str(m.odhm,12,2)+' ณ '
	?space(m.lmarg*1.6)+'ณ Drobn์ nehmotn์ ณ '+str(m.cdnm,12,2)+' ณ '+str(m.odnm-m.ndnm,12,2)+' ณ '+str(m.ndnm,12,2)+' ณ '+str(m.odnm,12,2)+' ณ '
	?space(m.lmarg*1.6)+'ณอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออณ'
	?space(m.lmarg*1.6)+'ณ Celkem:         ณ '+str(m.chm+m.cnm+m.cdhm+m.cdnm,12,2)+' ณ '+ ;
		str((m.ohm+m.onm+m.odhm+m.odnm)-(m.nhm+m.nnm+m.ndhm+m.ndnm),12,2)+' ณ '+str(m.nhm+m.nnm+m.ndhm+m.ndnm,12,2)+' ณ '+ ;
		str(m.ohm+m.onm+m.odhm+m.odnm,12,2)+' ณ '
	?space(m.lmarg*1.6)+'ภอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออู'
	?
	ll=ll+13
	ll=ll+12
	IF ll+10>=m.len_pg
		?chr(12)
		ll=1
		ENDIF
	?space(m.lmarg*1.6)+space(14)+" Vysvุtlivky zkratek"
	?space(m.lmarg*1.6)+space(13)+"~~~~~~~~~~~~~~~~~~~~~"
	?space(m.lmarg*1.6)+"TZ  - Technick zhodnocenก"
	?space(m.lmarg*1.6)+"ZPC - Zv์็en  poýizovacก cena"
	?space(m.lmarg*1.6)+"ZC  - Z…statkov  cena"
	?space(m.lmarg*1.6)+"MO  - Minul obdobก"
	?space(m.lmarg*1.6)+"DO  - Doba ฃetnกho odpisov nก v mุsกcกch"
	?space(m.lmarg*1.6)+"MU  - Poet cel์ch kal. mุsกc… uงกv nก IM v danm obdobก"
	?space(m.lmarg*1.6)+"M   - Metoda odpisov nก (Z-Zrychlenุ, R-Rovnomุrnุ, J-Jinak)"
	?space(m.lmarg*1.6)+"S   - Odpisov  skupina"

	x=.T.
	t=.F.
	WAIT WIND NOWA "Kontrola realizace v็ech odpis…"
	SELE evim
	SCAN ALL FOR (m.str=0 OR evim.str=m.str) AND (evim.druh="Hmotn์" OR evim.druh="Nehmotn์")
		IF !seek(evim.invc,"cu1") AND (empty(evim.dtzrus) OR year(evim.dtzrus)>=m.br)
			IF x
				IF ask(-1,-1,"A","Pýidat seznam neodepsanho majetku do sestavy")
					t=.T.
					?
					?
					ENDIF
				x=!x
				ENDIF
			WAIT WIND "Neproveden odpis majetku ."+evim.invc+" - "+left(evim.naz,30)
			IF t
				?space(m.lmarg*1.6)+"!!!  Neodeps n majetek "+evim.invc+" - "+left(evim.naz,40)
				ENDIF
			ENDIF
		ENDSCAN
	WAIT CLEAR
	
	?m.cnoff+chr(12)
	SET ALTE TO
	SET CONS ON
	DO tisk WITH m.lmarg, .T.

	SELE cu1
	USE

RETURN
**	Vlastnก tisk odpisovho pl nu
PROCEDURE tiskodpp
PARA tpt, leva, him

	SELE evim
	SET ORDER TO invc
	SET FILT TO (m.str=0 OR evim.str=m.str) AND &him
	GO TOP
	IF eof()
		RETURN
		ENDIF

	IF m.him!='(evim.druh="Drobn์ hmotn์" OR evim.druh="Drobn์ nehmotn์")' AND m.leva
		a="O D P I S O V ํ   P L ต N"+iif(!empty(m.nstr),"    stýedisko "+m.nstr,"")
		?m.cnoff+m.nlq+space(m.lmarg-2)+space((128/1.66-len(a))/2)+a
		?
		a="investinกho majetku pro ฃetnก a zdaๅovacก obdobก "+str(m.br,4)
		?m.cnon+space(m.lmarg*1.66)+space((128-len(a))/2)+a
		a=rtrim("Firma: "+alltrim(firma.jme))
		?space(m.lmarg*1.66)+space((128-len(a))/2)+a
		?m.draft
		ll=ll+4
		ENDIF
	IF m.him!='(evim.druh="Drobn์ hmotn์" OR evim.druh="Drobn์ nehmotn์")'
		?space(m.lmarg*1.66)+"Hmotn์ a nehmotn์ majetek"
	  ELSE
		?space(m.lmarg*1.66)+"Drobn์ hmotn์ a nehmotn์ majetek"
		ENDIF
	ll=ll+1

	DO odp_hlav WITH m.tpt, m.leva
	ll=ll+4

	SCAN ALL
		SELE evodp
		SET ORDER TO rokodp
		a=cs(evim.invc)+str(m.br-year(evim.dat)+1,2)
		IF seek(a)
			PRIV dtz
			dtz={}
			SELE evtz
			SET ORDER TO dttz
			SCAN ALL FOR evim.invc=invc AND year(dttz)<=m.br
				dtz=dttz
				ENDSCAN
			SELE evodp
			IF m.tpt
				?space(m.lmarg*1.6)+'ณ'+evim.invc+'ณ'+left(evim.naz,20)+'ณ'+dtoc(evim.dat)+'ณ'+ ;
					str(evim.porcena,12,2)+'ณ'+iif(empty(m.dtz),space(10),dtoc(m.dtz))+'ณ'+ ;
					iif(empty(m.dtz),space(12),str(porcena(evim.invc,ctod('31.12.'+str(m.br,4))),12,2))+'ณ'+ ;
					str(evodp.zcmouc,12,2)+'ณ'+str(evodp.zcmodan,12,2)+'ณ'+ ;
					str(evodp.uodoba,3)+'ณ'+str(evodp.uopocmes,2)+'ณ'+ ;
					str(evodp.uoodpis+evodp.uomimor,12,2)+'ณ'+str(evodp.uozc,12,2)+'ณ'+left(evim.domet,1)+'ณ'+str(evodp.doskup,1)+'ณ'+ ;
					str(evodp.doodpis,12,2)+'ณ'+str(evodp.dozc,12,2)+'ณ'+ ;
					str(evodp.nadlsl,12,2)+'ณ'+str(evodp.odppar34,10,2)+'ณ'+ ;
					str(evodp.oprpol,10,2)+'ณ'+ ;
					iif(empty(evim.dtzrus),space(10),dtoc(evim.dtzrus))+'ณ'
			  ELSE
				IF m.leva
					?space(m.lmarg*1.6)+'ณ'+evim.invc+'ณ'+left(evim.naz,20)+'ณ'+dtoc(evim.dat)+'ณ'+ ;
						str(evim.porcena,12,2)+'ณ'+iif(empty(m.dtz),space(10),dtoc(m.dtz))+'ณ'+ ;
						iif(empty(m.dtz),space(12),str(porcena(evim.invc,ctod('31.12.'+str(m.br,4))),12,2))+'ณ'+ ;
						str(evodp.zcmouc,12,2)+'ณ'+str(evodp.zcmodan,12,2)+'ณ'
				  ELSE
					?space(m.lmarg*1.6)+'ณ'+evim.invc+'ณ'+str(evodp.uodoba,3)+'ณ'+str(evodp.uopocmes,2)+'ณ'+ ;
						str(evodp.uoodpis+evodp.uomimor,12,2)+'ณ'+str(evodp.uozc,12,2)+'ณ'+left(evim.domet,1)+'ณ'+str(evodp.doskup,1)+'ณ'+ ;
						str(evodp.doodpis,12,2)+'ณ'+str(evodp.dozc,12,2)+'ณ'+ ;
						str(evodp.nadlsl,12,2)+'ณ'+str(evodp.odppar34,10,2)+'ณ'+ ;
						str(evodp.oprpol,10,2)+'ณ'+ ;
						iif(empty(evim.dtzrus),space(10),dtoc(evim.dtzrus))+'ณ'
					ENDIF
				ENDIF
			porc=m.porc+evim.porcena
			zvpc=m.zvpc+iif(empty(m.dtz),0,porcena(evim.invc,ctod('31.12.'+str(m.br,4))))
			uo=m.uo+evodp.uoodpis+evodp.uomimor
			uzc=m.uzc+evodp.uozc
			do=m.do+evodp.doodpis
			nls=m.nls+evodp.nadlsl
			o10=m.o10+evodp.odppar34
			a=iif(empty(m.dtz),evim.porcena,porcena(evim.invc,ctod('31.12.'+str(m.br,4))))
			b=evodp.uoodpis+evodp.uomimor
			c=evodp.nadlsl
* Soupis invc
			INSERT INTO cu1 (cuinvc) VALUES (evim.invc)
			DO CASE
				CASE evim.druh="Hmotn์"
					chm=m.chm+a
					ohm=m.ohm+b
					nhm=m.nhm+c
				CASE evim.druh="Nehmotn์"
					cnm=m.cnm+a
					onm=m.onm+b
					nnm=m.nnm+c
				CASE evim.druh="Drobn์ hmotn์"
					cdhm=m.cdhm+a
					odhm=m.odhm+b
					ndhm=m.ndhm+c
				CASE evim.druh="Drobn์ nehmotn์"
					cdnm=m.cdnm+a
					odnm=m.odnm+b
					ndnm=m.ndnm+c
				ENDCASE
			IF evodp.doskup>0
				pp(evodp.doskup)=pp(evodp.doskup)+evodp.doodpis
				ENDIF
			ll=ll+1
			IF ll+3 >= m.len_pg
				IF m.tpt
					?space(m.lmarg*1.6)+'ภอออออออออออออออออออออออออออออออออออออออออออ'+ ;
						'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
						'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
						'อออออออออออออออออออออออออออออออออออออออออออออู'+chr(12)
				  ELSE
					IF m.leva
						?space(m.lmarg*1.6)+'ภอออออออออออออออออออออออออออออออออออออออออออ'+ ;
							'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออู'+chr(12)
					  ELSE
						?space(m.lmarg*1.6)+'ภออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
							'อออออออออออออออออออออออออออออออออออออออออออออู'+chr(12)
						ENDIF
					ENDIF
				DO odp_hlav WITH m.tpt, m.leva
				ll=4
				ENDIF
*		  ELSE
*			IF empty(evim.dtzrus) OR year(evim.dtzrus)=>m.br
*				a=-1
*				SCAN REST WHILE invc==evim.invc
*					a=uozc+dozc
*					ENDSCAN
*				IF a!=0
*					WAIT WIND "Majetek กslo: "+evim.invc+" nem  proveden odpis !!!"
*					ENDIF
*				ENDIF
			ENDIF
		SELE evim
		ENDSCAN

	SELE evim
	SET FILT TO

	IF m.tpt
		?space(m.lmarg*1.6)+'ภอออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออู'
		?space(m.lmarg*1.6)+space(43)+'ณ'+str(m.porc,12,2)+'ณ'+space(10)+'ณ'+;
			str(m.zvpc,12,2)+'ณ'+space(32)+'ณ'+str(m.uo,12,2)+'ณ'+ ;
			str(m.uzc,12,2)+'ณ   ณ'+str(m.do,12,2)+'ณ'+space(12)+'ณ'+ ;
			str(m.nls,12,2)+'ณ'+str(m.o10,10,2)+'ณ'
		?space(m.lmarg*1.6)+space(43)+'ภออออออออออออู'+space(10)+ ;
			'ภออออออออออออู'+space(32)+'ภอออออออออออออออออออออออออู   '+ ;
			'ภออออออออออออู'+space(12)+'ภอออออออออออออออออออออออู'
	  ELSE
		IF m.leva
			?space(m.lmarg*1.6)+'ภอออออออออออออออออออออออออออออออออออออออออออ'+ ;
				'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออู'
			?space(m.lmarg*1.6)+space(43)+'ณ'+str(m.porc,12,2)+'ณ'+space(10)+'ณ'+;
				str(m.zvpc,12,2)+'ณ'
			?space(m.lmarg*1.6)+space(43)+'ภออออออออออออู'+space(10)+ ;
				'ภออออออออออออู'
		  ELSE
			?space(m.lmarg*1.6)+'ภออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
				'อออออออออออออออออออออออออออออออออออออออออออออู'
			?space(m.lmarg*1.6)+space(18)+'ณ'+str(m.uo,12,2)+'ณ'+ ;
				str(m.uzc,12,2)+'ณ   ณ'+str(m.do,12,2)+'ณ'+space(12)+'ณ'+ ;
				str(m.nls,12,2)+'ณ'+str(m.o10,10,2)+'ณ'
			?space(m.lmarg*1.6)+space(18)+'ภอออออออออออออออออออออออออู   '+ ;
				'ภออออออออออออู'+space(12)+'ภอออออออออออออออออออออออู'
			ENDIF
		ENDIF
RETURN

*************************
**	Vnitýnก ฃetnก doklad k odpisovmu pl nu
PROCEDURE tis_vdk
	ttyp=99
	tcis=0
	dtuc=date()
	jmuc=space(20)
	ok=1
	m.len_pg=63
	m.ll=63
	DO doklad.spr
	IF m.ok=0
		RETURN
		ENDIF
	SET CONS OFF
	SET CONS OFF
	=opfile('vud')
	a="VNITüNึ ้ฌETNึ DOKLAD . "+str(m.tcis,4)
	?space(m.margin+(66-len(a))/2)+a
	a="Typ: "+str(m.ttyp,3)+"    ze dne "+dtoc(m.dtuc)
	?space(m.margin+(66-len(a))/2)+a
	?m.elit+space(m.margin)+"+----------+----------+------------------------------------------+--------------+"
	?space(m.margin)+"| M  d ti  | Dal      | Text                                     | ฌ stka       |"
	?space(m.margin)+"+----------+----------+------------------------------------------+--------------+"
	ll=ll-5
	DO WHILE !eof()
		IF ll<=1
			?chr(12)+space(m.margin)+space(66)+"Pokraov nก ..."
			?space(m.margin)+"+----------+----------+------------------------------------------+--------------+"
			?space(m.margin)+"| M  d ti  | Dal      | Text                                     | ฌ stka       |"
			?space(m.margin)+"+----------+----------+------------------------------------------+--------------+"
			ll=m.len_pg-4
			ENDIF
		?space(m.margin-1),"|",md,mda,"|",dal,daa,"|",txt,"|",str(cas,12,2),"|"
		ll=m.ll-1
		SKIP 1
		ENDDO
	?space(m.margin)+"+----------+----------+------------------------+-----------------+--------------+"
	?space(m.margin)+"| Vystavil:           | Schv lil:              | Zaฃtoval: "+jmuc+"|"
	?space(m.margin)+"|                     |                        | Dne:  "+dtoc(iif(empty(m.imzau),m.dtuc,m.imzau))+"                 |"
	?space(m.margin)+"+---------------------+------------------------+--------------------------------+"
	?m.pica
	?chr(12)
	SET CONS ON
	SET ALTE TO
	WAIT CLEAR
	DO tisk WITH m.margin
RETURN

PROCEDURE odp_hlav
PARA tpt, leva
	IF m.tpt
		?space(m.lmarg*1.6)+'ฺอออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออฟ'
		?space(m.lmarg*1.6)+'ณInvent rnกณ N zev investinกho ณ  Datum   ณ'+ ;
			' Poýizovacก ณ  Datum   ณ  Hodnota   ณ    ZC MO   ณ    ZC MO   ณ'+ ;
			'    ้ ฌ E T N ึ   O D P I S     ณ   D A ี O V ํ   O D P I S   ณ'+ ;
			' Nadlimitnก ณ Odpoet  ณ Opravn   ณ  Datum   ณ'
		?space(m.lmarg*1.6)+'ณ  กslo   ณ      majetku       ณ poýกzenก ณ'+ ;
			'    cena    ณ    TZ    ณ   Z P C    ณ   ฃetnก   ณ   daๅov    ณMุsณ'+ ;
			'MOณ   Odpis    ณ     ZC     ณMณSณ   Odpis    ณ     ZC     ณ'+ ;
			'   sloงka   ณ   10%    ณ poloงka  ณ vyýazenก ณ'
		?space(m.lmarg*1.6)+'ณอออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
			'อออออออออออออออออออออออออออออออออออออออออออออณ'
	  ELSE
		IF m.leva
			?space(m.lmarg*1.6)+'ฺอออออออออออออออออออออออออออออออออออออออออออ'+ ;
				'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออฟ'
			?space(m.lmarg*1.6)+'ณInvent rnกณ N zev investinกho ณ  Datum   ณ'+ ;
				' Poýizovacก ณ  Datum   ณ  Hodnota   ณ    ZC MO   ณ    ZC MO   ณ'
			?space(m.lmarg*1.6)+'ณ  กslo   ณ      majetku       ณ poýกzenก ณ'+ ;
				'    cena    ณ    TZ    ณ   Z P C    ณ   ฃetnก   ณ   daๅov    ณ'
			?space(m.lmarg*1.6)+'ณอออออออออออออออออออออออออออออออออออออออออออ'+ ;
				'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออณ'
		  ELSE
			?space(m.lmarg*1.6)+'ฺออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
				'อออออออออออออออออออออออออออออออออออออออออออออฟ'
			?space(m.lmarg*1.6)+'ณInvent rnกณ    ้ ฌ E T N ึ   O D P I S     ณ   D A ี O V ํ   O D P I S   ณ'+ ;
				' Nadlimitnก ณ Odpoet  ณ Opravn   ณ  Datum   ณ'
			?space(m.lmarg*1.6)+'ณ  กslo   ณMุsณMOณ   Odpis    ณ     ZC     ณMณSณ   Odpis    ณ     ZC     ณ'+ ;
				'   sloงka   ณ   10%    ณ poloงka  ณ vyýazenก ณ'
			?space(m.lmarg*1.6)+'ณออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ'+ ;
				'อออออออออออออออออออออออออออออออออออออออออออออณ'
			ENDIF
		ENDIF
RETURN
**	V์poet odpis… - vol no z poloงky NADSL
PROCEDURE vypodpis
	PRIV a, b, c, t, zvodp

	DO CASE
	  CASE evim.domet='Rovnomุrnุ'
		IF evim.zvys1r>0
			zvodp=alltrim(str(evim.zvys1r))
		  ELSE
			zvodp=''
		ENDIF
		IF zcpozd(evim.invc)
* R-Zv์็en  cena
			b=tro&zvodp(m.doskup,3)
		  ELSE
* R-Nezv์็en  cena
			b=tro&zvodp(m.doskup,min(m.dorokodp,2))
			ENDIF
		m.doodpis=b/100*m.zvporc

************************
*** Zrychlen odpisy ***

	  CASE evim.domet='Zrychlenุ '

		SELE evtz
		SET ORDER TO dttz
		SET FILT TO evim.invc=evtz.invc AND ;
			(year(evtz.dttz) <= year(evim.dat)+m.dorokodp-1) AND ;
			year(evtz.dttz) > year(evim.dat)
		GO BOTT
		IF zcpozd(evim.invc) && AND eof()
* "Z-Zv์็en  cena v jinm neง prvnกm roce."
			SELE evtz
			SET ORDER TO dttz
			SET FILT TO
			* tech zhodnocenก tohoto roku
			CALC sum(cas) TO t FOR evim.invc=evtz.invc AND ;
				(year(evtz.dttz) = year(evim.dat)+m.dorokodp-1)

			SET FILT TO evim.invc=evtz.invc AND ;
				(year(evtz.dttz) <= year(evim.dat)+m.dorokodp-1)
			GO BOTT
			a=year(evtz.dttz)
			SET FILT TO
			SELE evodp
			* a=poet let odpis… ze ZPC
			a =(year(evim.dat)+m.dorokodp-1) - a
wait wind str(m.t)
			doodpis=2*(m.zcmodan+m.t)/(tzo(m.doskup,3)-a)
		  ELSE
* "Z-Nezv์็en  cena nebo ZVP hned v prvnกm roce odpisov nก."
			SELE evtz
			SET FILT TO
			IF m.dorokodp=1
				doodpis=m.zvporc/tzo(m.doskup,1)
			  ELSE
				* 2*z…st.cena (ale zv์็en  o TZ tohoto roku) !!!!
				doodpis=2*m.zcmodan/(tzo(m.doskup,2)-m.dorokodp+1)
				ENDIF
			ENDIF
		IF m.dorokodp=1 AND evim.zvys1r>0
			doodpis=m.doodpis+m.zvporc*(evim.zvys1r/100)
		ENDIF

	ENDCASE
	IF m.dopolsaz='Ano' AND evim.domet!='Jinak     '
		doodpis=m.doodpis/2
	ENDIF
	doodpis=ceil(m.doodpis)
	SELE evtz
	CALC sum(cas) TO a FOR evim.invc=evtz.invc AND ;
			year(evtz.dttz)=year(evim.dat)+m.dorokodp-1
	SELE evodp
	a=m.zcmodan+a
	IF a-m.doodpis<0
		doodpis=a
	ENDIF
	dozc=a-m.doodpis

	SHOW GETS
RETURN


***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
** P…vodnก chybn zrychlen odpisov nก

do case
************************
*** Zrychlen odpisy ***
	  CASE evim.domet='Zrychlenุ '
		SELE evtz
		SET ORDER TO dttz
		SET FILT TO evim.invc=evtz.invc AND ;
			(year(evtz.dttz) < year(evim.dat)+m.dorokodp-1) AND ;
			year(evtz.dttz) > year(evim.dat)
		GO BOTT
		IF zcpozd(evim.invc) AND eof()
* "Z-Zv์็en  cena v jinm neง prvnกm roce."
			SELE evtz
			SET ORDER TO dttz
			SET FILT TO
			CALC sum(cas) TO t FOR evim.invc=evtz.invc AND ;
				(year(evtz.dttz) = year(evim.dat)+m.dorokodp-1)
			SET FILT TO evim.invc=evtz.invc AND ;
				(year(evtz.dttz) <= year(evim.dat)+m.dorokodp-1)
			GO BOTT
			a=year(evtz.dttz)
			SET FILT TO
			SELE evodp
			* a=poet let odpis… ze ZPC
			a =(year(evim.dat)+m.dorokodp-1) - a
			doodpis=2*(m.zcmodan+t)/(tzo(m.doskup,3)-a)
		  ELSE
* "Z-Nezv์็en  cena nebo ZVP hned v prvnกm roce odpisov nก."
			SELE evtz
			SET FILT TO
			IF m.dorokodp=1
				doodpis=m.zvporc/tzo(m.doskup,1)
			  ELSE
				doodpis=2*m.zcmodan/(tzo(m.doskup,2)-m.dorokodp+1)
				ENDIF
			ENDIF
		IF m.dorokodp=1 AND evim.zvys1r>0
			doodpis=m.doodpis+m.doodpis*(evim.zvys1r/100)
		ENDIF

endcase
*****************************************************	Nataงenก odpisov์ch sazeb dle roku odpisov nก
PROCEDURE Load_odp
**	Nataงenก odpisov์ch sazeb
	a=str(year(evim.dat)+m.dorokodp-1,4)
	IF !file("odps"+a+".FXP")
*		WAIT WIND NOWA "Nenalezeny odpisov sazby - (ODPS"+a+".PRG). Pouงiji rok 1995."
		IF file("odps1995.PRG") OR file("odps1995.FXP")
			DO odps1995
		  ELSE
*			=inkey(0)
			WAIT WIND "Nenalezen soubor se sazbami ani pro rok 1995. Nebudeme odpisovat."
			RETURN .F.
		ENDIF
	  ELSE
		DO odps&a
	ENDIF
RETURN .T.
**	Zji็tุnก zv์็enก PC
**	.T. - Zv์็en  poýizovacก cena pro toto obdobก
PROCEDURE zcpozd
PARA ic
	PRIV _se, _rt, _rc1, _rcs, _ors, _fis
	_se=select()
	_rc1=recno()
	SELE evtz
	_rcs=recno()
	_ors=order()
	_fis=filter()
	SET ORDER TO dttz
	SET FILT TO evtz.invc=m.ic
	GO BOTT
	IF eof()
		_rt=.F.
	  ELSE
		_rt= ( year(evtz.dttz) <= year(evim.dat)+m.dorokodp-1 AND ;
			year(evtz.dttz) > year(evim.dat) )
		ENDIF
	SET FILT TO &_fis
	SET ORDER TO &_ors
	=gorec(m._rcs)
	SELE (m._se)
	=gorec(m._rc1)
RETURN m._rt

**	Spoกt nก ฃetnกch odpis… - vol no z pol. UOPOCMES
PROCEDURE uc_odpis
	IF m.uopocmes=0
		uoodpis=0
		SHOW GETS
		RETURN
		ENDIF
	PRIV a, b, m, u
	* Poýizovacก, nebo zv์็en  cena k 31.12. pýedchozกho roku	
	a=porcena(evim.invc,ctod('31.12.'+str(year(evim.dat)+m.dorokodp-2,4)))
	b=a
	u=0
	SELE evtz
	SET FILT TO evim.invc=evtz.invc ;
		AND year(evtz.dttz)=year(evim.dat)+m.dorokodp-1
	SET ORDER TO dttz
	GO TOP
	m=12-m.uopocmes
	SCAN ALL
		u=u+a*((month(evtz.dttz)-m)/m.uodoba)
		a=a+evtz.cas
		m=month(evtz.dttz)
		ENDSCAN
	uoodpis=min(m.zcmouc+(a-b),ceiling(u+a*(12-m)/m.uodoba))
	SET FILT TO
	SELE evodp
	SHOW GETS

*	ceiling(iif(m.zvporc=0,porcena(invc,m.dorokodp),m.zvporc)* ;
*		(m.uopocmes/m.uodoba))
*	uoodpis=min(m.zcmouc, m.uoodpis)
RETURN
**	Zji็tุnก poýizovacก ceny nebo ceny zv์็en
**	ii - Invent rnก กslo zji็ovanho majetku
**	ll - Datum ke ktermu zjistit ZPC
PROCEDURE porcena
PARA ii, ll
	PRIV a, _se
	_se=select()
	SELE evtz
	IF empty(m.ll)
		CALC sum(cas) TO a FOR evtz.invc=m.ii
	  ELSE
		CALC sum(cas) TO a FOR evtz.invc=m.ii AND dtos(evtz.dttz) <= dtos(m.ll)
		ENDIF
	a=evim.porcena+a
	SELE (m._se)
RETURN a
**	Pýenos do ฃetnictvก
PROCEDURE hlm_pen
	PRIV a, _l, br, lmarg, tpt
	br=year(date())-1
	PRIV _l, _r, f1, f2
	_l=36
	_r=2
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._r-1))/2), INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-(m._r-1))/2)+m._r, INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Rok bุงnho ฃetnกho obdobก: "
	@ 0,30 GET m.br SIZE 1,4 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SET CONS OFF
	DO opfile WITH "prenos"

	SELE evodp
	SET ORDER TO rokodp
	SELE evim
	WAIT WIND NOWA "Probกh  kontrola zad nก v็ech pýedฃtovacกch pýedpis…."
	f1=.F.
	SCAN ALL
		IF empty(odp_md) OR empty(odp_dal) OR empty(nls_md) OR empty(nls_dal)
			IF !f1
				?"Nejsou zad ny pýedฃtovacก pýedpisy "
				?
				f1=.T.
				ENDIF
			?evim.invc+" - "+evim.naz
			ENDIF
		ENDSCAN
	WAIT WIND NOWA "Probกh  kontrola zad nก odpis… pro rok "+strn(m.br)+"."
	f2=.F.
	SCAN ALL
		a=cs(evim.invc)+str(m.br-year(evim.dat)+1,2)
		IF empty(lookup(evodp.invc,m.a,evodp.invc,'ROKODP'))
			IF !f2
				?
				?"Nejsou zad ny odpisy pro rok "+strn(m.br)+"."
				?
				f2=.T.
				ENDIF
			?evim.invc+" - "+evim.naz
			ENDIF
		ENDSCAN
	WAIT CLEAR
	SET ALTE TO
	SET CONS ON
	IF f1 OR f2
		DO tisk
		IF !ask(-1,-1,"A","Provst pýevod bez v์็e uveden์ch poloงek")
			RETURN
			ENDIF
		ENDIF

	PRIV nz
	nz=m.nfirma
	_l=38
	_r=2
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._r-1))/2), INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-(m._r-1))/2)+m._r, INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "N zev cกlov firmy: "
	@ 0,20 GET m.nz SIZE 1,12 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SET COMP ON
	a=file(m.gl_data+m.nz+"\denik.dbf")
	SET COMP OFF
	IF !a
		WAIT WIND NOWA "U zadan firmy '"+m.nz+"' nebyla nalezena datab ze ฃetnกho denกku."
		RETURN
		ENDIF

	SELE 0
	err=0
	ON ERROR err=error()
	USE m.gl_data+m.nz+"\denik.dbf"
	ON ERROR &_onerr
	IF m.err=3 OR m.err=108
		WAIT WIND "Datab ze je pouงกv na je็tุ nุk์m jin์m. Nelze provst aktalizaci."
		RETURN
		ENDIF
	IF err!=0
		WAIT WIND NOWA "Pýi otevกr nก denกku vznikla chyba ."+strn(m.err)
		RETURN
		ENDIF

	PRIV ttyp, tcis, tdat
	ttyp=60
	tcis=0
	tdat=ctod("31.12."+str(m.br,4))
	_l=26
	_r=4
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._r-1))/2), INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-(m._r-1))/2)+m._r, INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Typ dokladu: "
	@ 1,1 SAY "ฌกslo      : "
	@ 2,1 SAY "Datum      : "
	@ 0,14 GET m.ttyp SIZE 1,3 COLOR SCHEME 4
	@ 1,14 GET m.tcis SIZE 1,4 COLOR SCHEME 4
	@ 2,14 GET m.tdat SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SELE 0
	CREA CURS cu (ucet C(14), cas N(13,2), tp C(2))
	INDEX ON ucet TAG ucet
	INDEX ON tp+ucet TAG tp
	SET ORDER TO ucet

	SELE evim
	SET ORDER TO invc
	SELE evodp
	SET RELA TO cs(invc) INTO evim
	SCAN ALL FOR m.br-year(evim.dat)+1 = evodp.dorokodp AND ;
			!(empty(evim.odp_md) OR empty(evim.odp_dal) OR ;
				empty(evim.nls_md) OR empty(evim.nls_dal))
		DO _pen1 WITH str(evim.odp_md,3)+str(evim.odp_mda,4)+str(evim.odp_dal,3)+ ;
			str(evim.odp_daa,4), evodp.doodpis, "DO"
		DO _pen1 WITH str(evim.nls_md,3)+str(evim.nls_mda,4)+str(evim.nls_dal,3)+ ;
			str(evim.nls_daa,4), evodp.nadlsl, "NL"
		DO _pen1 WITH str(evim.opr_md,3)+str(evim.opr_mda,4)+str(evim.opr_dal,3)+ ;
			str(evim.opr_daa,4), evodp.oprpol, "OP"
		ENDSCAN
	SET RELA TO

	SELE cu
	SET ORDER TO tp
	SCAN ALL
		PRIV t
		DO CASE
		  CASE tp="DO"
			t="Odpis roku "+strn(m.br)
		  CASE tp="NL"
			t="Odpis roku "+strn(m.br)+" - NLS"
		  CASE tp="OP"
			t="Tvorba opravn poloงky"
			ENDCASE
		INSERT INTO denik (typ, cis, dat, md, mda, dal, daa, txt, cas, ;
				del, adat, atime, auser) ;
			VALUES (m.ttyp, m.tcis, m.tdat, val(left(cu.ucet,3)), ;
				val(substr(cu.ucet,4,4)), val(substr(cu.ucet,8,3)), ;
				val(substr(cu.ucet,11,4)), ;
				m.t, cu.cas, .F., date(), time(), "EVIM")
		ENDSCAN
	USE
	SELE denik
	USE
	WAIT WIND "Pýevod dat do ฃetnกho denกku proveden."
RETURN

PROCEDURE _pen1
PARA _a, _c, _t
	IF m._c!=0
		SELE cu
		IF seek(_a)
			REPL cas WITH cas+m._c
		  ELSE
			INSERT INTO cu VALUES (m._a, m._c, m._t)
			ENDIF
		SELE evodp
		ENDIF
RETURN
**	R Þ Z N 
PROCEDURE hlm_rzn
	DEFINE POPUP spr FROM 8,40 RELATIVE SHADOW ;
		TITLE " R … z n  " COLOR SCHEME 4
	DEFINE BAR 30 OF spr PROMPT " \<Glob lnก parametry" ;
		MESS "Zad nก ฃdaj… o firmุ."
	DEFINE BAR  5 OF spr PROMPT " Oprava inde\<x…" ;
		MESS "Pýeindexov nก databazก."
	DEFINE BAR  1 OF spr PROMPT " \<Novinky" ;
		MESS "Popis ฃprav v programu."

	ON SELE POPU spr DO spr

	ACTI POPUP spr

	RELE POPU spr
RETURN

PROC spr
	DO CASE
		CASE bar()=1	&&	Informace o novink ch
			DO novinky
		CASE bar()=5	&&	Oprava index…
			DO cldata
			=_open(.T.)
		CASE bar()=30	&&	Glob lnก parametry
			SELE firma
			DO ..\firma.spr
			DO refscr
		CASE bar()=45	&&	Oprava struktu datab zก
			WAIT WIND "? ? ?  Jste opravdu povol ni k provedenก tto operace ? ? ?"
			IF lastkey() = 30
				DO oprstru
				ENDIF
		ENDCASE
RETURN
**	Zad nก firmy k ฃtov nก
PROCEDURE hlm_fir
	PRIV pf,i,tt
	DO wrt_stat WITH "EVIM"
	SELE firma
*	DELE ALL FOR !file((m.gl_data+allt(naz)+"\evim.dbf"))
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
		'CR#:	DO fir_ote			## ENTER-Otevýenก firmy k ฃtov nก',;
		'INS:	DO fir_ins			## INS-Nov  firma',;
		'ALT+A:	DO fir_arc			## ALT+A-Zobrazovat i archivy firem',;
		'ALT+V:	DO fir_vse			## ALT+V-Zobrazovat v็echny firmy'
	IF m.esc
		DO cldata
		m.ggf=' '
		ENDIF
	DO refscr
	DO testflag
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

* Otestuje, jsou-li otevýeny datab ze
	PRIV errordenik
	errordenik=.T.
	ON ERROR errordenik=.F.
	SELE evim
	ON ERROR &_onerr

	IF m.errordenik
		DO cldata
	  ELSE
		DO cldata1
		ENDIF

	ggf=m.gl_data+allt(naz)+"\"
	SELE 0
	ON ERROR erruser=error()
	USE &ggf.evim EXCLUSIVE
	ON ERROR &_onerr
	USE
	IF erruser=108
		WAIT WIND NOWAIT "S touto firmou pracuje jin์ uงivatel. NELZE OTEVüึT."
		RETURN
		ENDIF
	SELE firma
	SET FILT TO
	=_open(.F.)
	SELE evim
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
	IF !_MKDIR(nz)
		WAIT WIND "Chyba pýi otevกr nก nov firmy."
		m.toexit=.T.
		RETURN
		ENDIF
RETURN
**	ฌกselnกky
PROCEDURE hlm_sel
	DEFINE POPUP cis FROM 8,35 RELATIVE SHADOW ;
		TITLE " ฌ ึ S E L N ึ K Y " COLOR SCHEME 4
*	DEFINE BAR 20 OF cis PROMPT " \<Daๅov  tabulka odpis… " ;
*		MESS "ฌกselnกk ronกch odpisov์ch sazeb." SKIP FOR !ladeni
	DEFINE BAR 50 OF cis PROMPT " \<Stýediska " ;
		MESS "ฌกselnกk stýedisek."
	ON SELE POPU cis DO cis

	ACTI POPUP cis

	RELE POPU cis
RETURN

PROC cis
	DO CASE
		CASE bar()=20	&&	Daๅov odpisy
			
		CASE bar()=50	&&	Stýediska
			DO cis_str WITH ""
		ENDCASE
RETURN
**	Pýid nก a oprava stýedisek
PROCEDURE cis_str
PARA p
	PRIV s, o

	s=sele()
	SELE str
	o=order()
	SET ORDER TO cis
	IF !seek(m.p)
		GO TOP
		ENDIF
	SET ORDER TO &o

	DO list_c WITH -1,-1,12,0,;
		[str(str.cis,4)+'  ณ '+str.txt],;
		" Stýediska ",;
		"//ฌกslo ณ N zev",;
		"\ F1-Help    ENTER-Oprava  INS-Pýid nก stýediska   F7-Týกdit podle ...",;
		'{cis,txt}',;
		'F4#:	DO str_pri WITH .F.		## F4-Oprava stýediska',;
		'INS:	DO str_pri WITH .T.		## INS-Pýid nก stýediska',;
		'ALT+D#:DO str_del				## ALT+D-Smaz nก stýediska',;
		'CR#:	EXIT					## Enter-V์bุr stýediska'

	SELE (s)
		
RETURN str.cis

PROCEDURE str_pri
PARA app
PRIV mx
	IF app
 		INSERT INTO str (cis) VALUES (cis+1)
		ENDIF
	DO str.spr
	IF m.app.and.m.esc
		DELE
		PACK
		ENDIF
RETURN

PROCEDURE str_del
PRIV r, f, o
	SELE evim
	r=recno()
	LOCA ALL for str=str.cis
	f=found()
	=gorec(r)
	SELE str
	IF f
		WAIT WIND "Stýedisko nalezeno v evidenci majetku. NELZE VYPUSTIT."
	  ELSE
		IF ask(-1,-1,'N',"Smazat stýedisko . "+str(cis))
			DELE
			WAIT WIND NOWA 'Stýedisko vymaz no'
			refscr=.T.
			ENDIF
		ENDIF
RETURN

*******************
**	LIST_C pro text
PROCEDURE ltext
	PRIV m.sel
	sel=SELE()
	SELE etext
	SET ORDER TO text
	DO list_c WITH -1,-1,15,0,;
		[etext.text],;
		" Popisn texty ",;
		"//  Text",;
		"\ F1-Help   ENTER-V์bุr   F4-Oprava   INS-Pýid nก textu   F7-Týกdit podle ...",;
		'F4#:		DO text_pri WITH .F.		## F4-V์bุr textu',;
		'INS:		DO text_pri WITH .T.		## INS-Pýid nก textu',;
		'ALT+D#:	DO text_del					## ALT+D-Smaz nก textu'
	SELE (m.sel)
RETURN rtrim(etext.text)
**	Pýid nก text…
PROCEDURE text_pri
PARA app
PRIV mx
	IF app
		APPE BLANK
		ENDIF
	DO text.spr
	IF m.app.and.m.esc
		DELE
		ENDIF
RETURN

PROCEDURE text_del
	DELE
RETURN
****	Otevýenก datab zก vybran firmy
PROCEDURE _open
	PARA rei

	IF (m.rei OR file(m.ggf+"evim.rei") AND ;
			ask(-1,-1,'A',"Obnovit indexy po hav rii"))
		rei=.T.
		WAIT WIND NOWAIT "Obnovuji indexov soubory"
	  ELSE
		rei=.F.
		ENDIF

	SET ALTE TO &ggf.evim.rei
	SET ALTE ON
	SET ALTE TO
	SET TALK OFF

**	Aktivace statistiky
	=eff_start()
	=eff_ktim(90,2)
	st_on_dat=date()
	st_on_time=time()

**	Pýejmenov v nก soubor…
	IF file(m.ggf+"text_im.dbf")
		RENA (m.ggf+"text_im.dbf") TO (m.ggf+"etext.dbf")
		ENDIF

* EVIM
	DO open_dbf WITH m.ggf, "EVIM", "op_evim", "INVC", .T., m.rei
* EVODP
	DO open_dbf WITH m.ggf, "EVODP", "op_evodp", "INVC", .T., m.rei
* EVTZ
	DO open_dbf WITH m.ggf, "EVTZ", "op_evtz", "INVC", .T., m.rei
* ETEXT
	DO open_dbf WITH m.ggf, "ETEXT", "op_text", "TEXT", .T., m.rei
* STR
	DO open_dbf WITH m.ggf, "STR", "op_str", "txt", .T., m.rei

	a=m.ggf
	IF file(ggf+"firma.mem")
		REST FROM (ggf+"firma.mem") ADDI
		ENDIF
	ggf=a
	WAIT CLEAR
RETURN
**	Open EVIM
PROCEDURE op_evim
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) ;
			(dat D, invc C(10), str N(3), naz C(60), skp C(8), druh C(15), ;
			porcena N(12,2), domet C(10), doskup N(1), zvys1r N(2), ;
			zpodpisu C(50), dtzrus D, zpzrus C(10), ozn C(1), ;
			odp_md N(3), odp_mda N(4), ;
			odp_dal N(3), odp_daa N(4), ;
			nls_md N(3), nls_mda N(4), ;
			nls_dal N(3), nls_daa N(4), ;
			opr_md N(3), opr_mda N(4), ;
			opr_dal N(3), opr_daa N(4), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16) )
	  CASE m._cfn=2
		SET TALK ON
		INDEX ON cs(invc) TAG invc
		INDEX ON cs(naz) TAG naz
		INDEX ON dtos(dat)+cs(invc) TAG dat_ic
		INDEX ON str(year(dat),4)+cs(invc) TAG rok_ic
		SET TALK OFF
	  CASE m._cfn=3
		RETURN type("EVIM.zvys1r")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_evodp
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) ;
			(invc C(10), popodp C(25), zcmouc N(12,2), zcmodan N(12,2), ;
			uodoba N(3), uopocmes N(2), ;
			uoodpis N(12,2), uomimor N(12,2), uozc N(12,2), ;
			doskup N(1), dopolsaz C(3), dorokodp N(2), ;
			doodpis N(12,2), dozc N(12,2), ;
			nadlsl N(12,2), odppar34 N(12,2), dtoprpol D, oprpol N(12,2), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16) )
	  CASE m._cfn=2
		SET TALK ON
		INDEX ON cs(invc) TAG invc
		INDEX ON cs(invc)+str(dorokodp,2) TAG rokodp
		SET TALK OFF
	  CASE m._cfn=3
		RETURN type("EVODP.popodp")="U"
	  CASE m._cfn=4

		WAIT WIND NOWA "Pýenos odpisov์ch skupin do jednotliv์ch odpis…."
		SELE evim
		PRIV _ord, _rc
		_ord=order()
		_rc=recno()
		SET ORDER TO invc
		SELE evodp
		SET RELA TO cs(invc) INTO evim
		REPL ALL evodp.doskup WITH evim.doskup
		SET RELA TO
		SELE evim
		SET ORDER TO &_ord
		=gorec(m._rc)
		SELE evodp

		ENDCASE
RETURN

PROCEDURE op_evtz
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) ;
			(invc C(10), dttz D, cas N(12,2), txt C(25), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16) )
	  CASE m._cfn=2
		SET TALK ON
		INDEX ON cs(invc) TAG invc
		INDEX ON cs(invc)+dtos(dttz) TAG dttz
		SET TALK OFF
	  CASE m._cfn=3
		RETURN type("EVTZ.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_text
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREA DBF (m._nn) (text C(50), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(text) TAG text
	  CASE m._cfn=3
		RETURN type("etext.m_add")="U"
	  CASE m._cfn=4
		IF recc()=0
			INSERT INTO etext (text) VALUES ("Z hlediska doby pouงกv nก")
			INSERT INTO etext (text) VALUES ("Podle potu ujet์ch kilometr… - limit")
			INSERT INTO etext (text) VALUES ("Podle potu vyroben์ch ")
			ENDIF
		ENDCASE
RETURN
PROCEDURE op_str
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis N(3), txt C(30), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON cs(txt) TAG txt
	  CASE m._cfn=3
		RETURN type("str.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

***************************************
**	Uzavýe aktu lnก zpracov vanou firmu
PROCEDURE cldata
	IF file(m.ggf+"evim.rei")
		ERASE (m.ggf+"evim.rei")
		ENDIF
	DO cldata1
RETURN

****************************************
**************  FUNKCE  ****************
****************************************

***************************
**	Vypก็e do waitu pracuji
PROC wpr
	WAIT WIND NOWA "Pracuji ..."
RETURN

******************************
**	Otevýenก tiskovho souboru
PROCEDURE opfile
PARA _ff
	PUBLIC m.tskf
	IF para()=0
		m.tskf=ggf+"evim.prt"
	  ELSE
		m.tskf=ggf+_ff+".prt"
		ENDIF
	SET ALTE TO (m.tskf)
	SET ALTE ON
	??m.resetprn
RETURN

**	V์pis helpovho oknka		ALT + F1 / Global
PROC winhelp
	DEFINE WIND w_help FROM 8,5 TO 16,74 DOUBLE SHADOW ;
		TITLE " H E L P " COLOR SCHEME 7
	ACTI WIND w_help
	HIDE WIND w_help
	@2,4 SAY "ALT + P  -  Pozn mky k ฃtov nก k jednotliv์m firm m."
	@4,4 SAY "ALT + Z  -  Hlavnก z pisnกk."
	SHOW WIND w_help
	=INKEY(0)
	RELE WIND w_help
RETURN



*******************************
**	Vol nก Nortona		ALT + C
PROCEDURE call_nc
	DO nc
	DO refscr
RETURN

