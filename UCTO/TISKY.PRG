
*                               TISKOVê SESTAVY K
* PODVOJNêMU È¨ETNICTV÷PRIV _se, _rc, _or
	_se=sele()
	_rc=recno()
	_or=order()

	WAIT WIND NOWA 'TiskovÏ vÏstup: '+imp_output+' - '+rtrim(m.&imp_output)

	DEFINE POPUP tis FROM 7,35 RELATIVE SHADOW ;
		TITLE " TISKOVê SESTAVY " COLOR SCHEME 4
	DEFINE BAR 22 OF tis PROMPT " Hlavn° \<kniha " ;
		MESS "Hlavn° kniha."
	DEFINE BAR 30 OF tis PROMPT " \<Souhrnn† hl. kniha " ;
		MESS "Souhrnn† hlavn° kniha."
	DEFINE BAR 23 OF tis PROMPT " \<Den°k " ;
		MESS "Èüetn° den°k."
	DEFINE BAR 25 OF tis PROMPT " \<Rozvaha " ;
		MESS "Èüetn° rozvaha."
	DEFINE BAR 27 OF tis PROMPT " \<VÏsledovka" ;
		MESS "DaÂov† a £üetn° vÏsledovka."
	DEFINE BAR 29 OF tis PROMPT " \<Hosp. vÏsledek " ;
		MESS "Hospod†˝skÏ vÏsledek s rozdÿlen°m do oblast°."
	DEFINE BAR 31 OF tis PROMPT " \<Mÿs°ün° vÏsledovka " ;
		MESS "N†klady, vÏnosy a zisk po mÿs°c°ch."
	DEFINE BAR 45 OF tis PROMPT "\-"
	DEFINE BAR 50 OF tis PROMPT " P˝eh\<ledy " ;
		MESS "P˝ehledy a pomocnÇ sestavy. P˝izn†n° k DPH. Soupis daÂovÏch dokladÖ."
	DEFINE BAR 52 OF tis PROMPT " D\<oklady " ;
		MESS "Tisk vnit˝n°ch, vÏdajovÏch, p˝°jmovÏch dokladÖ. KoÁilky k faktur†m."
	DEFINE BAR 54 OF tis PROMPT " VÏka\<zy " ;
		MESS "VÏkazy a dalÁ° sestavy pot˝ebnÇ k £üetn° z†vÿrce."
	DEFINE BAR 56 OF tis PROMPT " Vnitro\<podnikovÇ £üetnictv° " ;
		MESS "Sestavy £ütovÏch t˝°d 800 a 900."
	DEFINE BAR 59 OF tis PROMPT "\-"
	DEFINE BAR 60 OF tis PROMPT " \<TiskovÇ soubory " ;
		MESS "Prohl°ßen° nÿkterÏch vÏÁe uvedenÏch sestav s moßnost° jejich p˝°mÇ editace."

	ON SELE POPU tis DO tis

	ACTI POPUP tis

	RELE POPU tis

	SELE (_se)
	SET ORDER TO &_or
	=gorec(_rc)
RETURN

PROC tis
	hl="tis_"+left(onlya_z(prompt()),3)
	DO &hl
RETURN
**	Extern° program pro tisk pokladn°ch dokladÖ
PROCEDURE tis_dok
	DO doklad
RETURN
**	Tisk hlavn° knihy
PROCEDURE tis_hla
	PRIV m.fi, tiskcm
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	lmarg=m.margin
	tiskcm="N"
	DO t-kniha.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	tiz=seco()
	fi='tdto<=dat.AND.tdtd>=dat'+;
		iif(m.ttyp<>0,'.and.typ=m.ttyp','')+;
		iif(m.tstr<>0,'.and.str=m.tstr','')
	SET TALK ON
	DO wpr
	DO transden	WITH m.fi		&&	Transformace den°ku do doüasnÇ datab†ze
	INDEX ON ord1+dtos(dat)+str(typ,3)+str(cis,5) TAG uct
	SET ORDER TO uct
	SELE sel1
*	SET FILT TO &fi
	rec=recc()
	GO TOP
	tskf=ggf+"kniha.prt"
	headprog="hk_1"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	ll=0
	fuct=uct
	fuctana=ord1
	SET CONS OFF
	SET TALK OFF
	STORE 0 TO pp,csmd,csdal,ll
	DO head			&&	Nov† str†nka
	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("HLAVN÷ KNIHA"+space(54))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	IF tstr!=0
		?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
		ll=ll+1
		ENDIF
	?m.cnon+m.draft
	ll=ll+5
	DO WHILE !eof()
		STORE 0 TO smd,sdal,cau,scascm		&&	Sumy za syntetiky,poü. AU

		DO WHILE m.fuct=uct .and. !eof()	&&	Tisk syntetik
			=proc(m.rec,pp)
			=tstpgend(5)
			?m.nlq+space(m.lmarg)+"Èüet: "+right("00"+ltrim(str(sel1.uct,3)),3)+;
				iif(ana<>0,str(sel1.ana,5),space(5))+"  "+;
				rtrim(lookup(osn.txt,str(uct,3),osn.uct,'uct'))+;
				iif(ana>0," / "+lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct'),"")
			IF m.tiskCM="N"
				?m.draft+space(m.lmarg)+"+----------+-----------+--------------+--------------+----------+------------------------------------------+-----+"
				?space(m.lmarg)+"|  Datum   |  Doklad   |  ¨†stka  MD  |  ¨†stka DAL  | Souvztah |  T e x t                                 | St˝.|"
				?space(m.lmarg)+"+----------+-----------+--------------+--------------+----------+------------------------------------------+-----+"
			  ELSE
				?m.draft+space(m.lmarg)+"+----------+-----------+--------------+--------------+----------+-----------------------------+-------------+----+"
				?space(m.lmarg)+		"|  Datum   |  Doklad   |  ¨†stka  MD  |  ¨†stka DAL  | Souvztah |  T e x t                    |  Ciz° mÿna  |St˝.|"
				?space(m.lmarg)+		"+----------+-----------+--------------+--------------+----------+-----------------------------+-------------+----+"
			ENDIF
			ll=m.ll+4
			cau=m.cau+1

			&&	Tisk jednotlivÏch analitik sestavy
			STORE 0 TO m.smda,m.sdala		&&	Sumy za analytiky
			DO WHILE m.fuctana=ord1 .and. !eof()
				IF m.tiskCM="N"
					?space(m.lmarg)+"| "+dtoc(dat)+" | "+str(typ,3)+str(cis,6)+" |",;
						casm PICT '@Z 999999999.99',"|",;
						casd PICT '@Z 999999999.99',"|",;
						puct PICT '@L 999',pana PICT '@Z 9999',;
						"| "+txt+" | "+str(str,3)+" |"
				  ELSE
					?space(m.lmarg)+"| "+dtoc(dat)+" | "+str(typ,3)+str(cis,6)+" |",;
						casm PICT '@Z 999999999.99',"|",;
						casd PICT '@Z 999999999.99',"|",;
						puct PICT '@L 999',pana PICT '@Z 9999',;
						"| "+left(txt,27)+" |",cascm PICT '@Z 99999999.99',"|"+str(str,3)+" |"
				ENDIF
				smda=m.smda+casm
				sdala=m.sdala+casd
				ll=m.ll+1
				IF m.ll+1 > m.len_pg
					DO head
					ENDIF
				pp=m.pp+1
				SKIP 1
				ENDDO
			smd=m.smd+m.smda
			sdal=m.sdal+m.sdala
			IF m.fuct=uct .OR. m.cau>1
				STORE space(12) TO mdo,dalo
				IF m.smda-m.sdala>0
					mdo=str(m.smda-m.sdala,12,2)
				  ELSE
					dalo=str(m.sdala-m.smda,12,2)
					ENDIF
				=tstpgend(4)
				?space(m.lmarg)+"+----------+-----------+--------------+--------------+----------+------------------------------------------+-----+"
				?space(m.lmarg)+"| Obdob°:              | "+str(m.smda,12,2)+" | "+str(m.sdala,12,2)+" | Obrat"+space(53)+"|"
				?space(m.lmarg)+"| "+dtoc(m.tdto)+" - "+dtoc(m.tdtd)+"  | "+m.mdo+" | "+m.dalo+" | ZÖstatek"+space(50)+"|"
				?space(m.lmarg)+"+----------------------+--------------+--------------+-----------------------------------------------------------+"
				ll=m.ll+4
				ENDIF
			m.fuctana=ord1
			ENDDO
		m.fuct=uct
		STORE space(12) TO mdo,dalo
		IF m.smd-m.sdal>0
			mdo=str(m.smd-m.sdal,12,2)
		  ELSE
			dalo=str(m.sdal-m.smd,12,2)
			ENDIF
		=tstpgend(5)
		?space(m.lmarg)+"+======================+==============+==============+===========================================================+"
		?space(m.lmarg)+"| Obdob°:              | "+str(m.smd,12,2)+" | "+str(m.sdal,12,2)+" | Obrat"+space(53)+"|"
		?space(m.lmarg)+"| "+dtoc(m.tdto)+" - "+dtoc(m.tdtd)+"  | "+m.mdo+" | "+m.dalo+" | ZÖstatek"+space(50)+"|"
		?space(m.lmarg)+"+======================+==============+==============+===========================================================+"
		?
		ll=m.ll+5
		csmd=csmd+smd
		csdal=csdal+sdal
		ENDDO
	STORE space(12) TO mdo,dalo
	IF m.csmd-m.csdal>0
		mdo=str(m.csmd-m.csdal,12,2)
	  ELSE
		dalo=str(m.csdal-m.csmd,12,2)
		ENDIF
	=tstpgend(4)
	?space(m.lmarg)+repl('*',113)
	?space(m.lmarg)+"* Obdob°:             * "+str(m.csmd,12,2)+" * "+str(m.csdal,12,2)+" * Obrat"+space(53)+"*"
	?space(m.lmarg)+"* "+dtoc(m.tdto)+" - "+dtoc(m.tdtd)+" * "+m.mdo+" * "+m.dalo+" * ZÖstatek"+space(50)+"*"
	?space(m.lmarg)+repl('*',113)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	tik=seco()
	WAIT WIND NOWA "Poüet str†nek:"+str(m.page-1,5)+"   ¨as zpracov†n°: "+str(m.tik-m.tiz,7,3)+" s"
	USE
	SELE denik
	SET CONS ON
	SET FILT TO !del
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
RETURN
**	Tisk zkr†cenÇ hlavn° knihy
PROCEDURE tis_sou
	PRIV m.fi
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	lmarg=m.margin
	DO t-knihaz.spr
	IF lastkey()=27
		RETURN
		ENDIF
	ON ESCA DO stoptisk
	SET ESCA ON
	tiz=seco()
	fi='tdtd>=dat'
*		iif(m.ttyp<>0,'.and.typ=m.ttyp','')+;
*		iif(m.tstr<>0,'.and.str=m.tstr','')
	SET TALK ON
	DO wpr
	masku='*'
	maska='*'
	DO transden	WITH m.fi	&&	Transformace den°ku do doüasnÇ datab†ze

	SELECT sel1.dat, sel1.uct, sel1.ana, sel1.typ, ;
			sum(sel1.casm) AS casm, sum(sel1.casd) AS casd, ;
			min(sel1.cis) AS micis, max(sel1.cis) AS mxcis, ;
			str(uct,3)+str(ana,4)+str(month(sel1.dat),2)+ ;
				iif(!(sel1.puct=701 OR sel1.puct=702),str(sel1.typ,4),;
					iif(sel1.puct=701,'   0','9999')) ;
				AS vv1 ;
		FROM sel1 INTO CURSOR cr1 ;
		ORDER BY vv1 GROUP BY vv1
	
	rec=recc()
	GO TOP
	tskf=ggf+"kniha.prt"
	headprog="hk_1"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	SET CONS OFF
	SET TALK OFF
	STORE 0 TO pp,csmd,csdal,ll
	DO head			&&	Nov† str†nka
	ll=3
	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("HLAVN÷ KNIHA"+space(54))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
*	IF tstr!=0
*		?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
*		ll=ll+1
*		ENDIF
	?m.cnon+m.draft
	ll=ll+5
	PRIV mobd, obd, od, do
	DO WHILE !eof()
		STORE 0 TO m.smd,m.sdal,m.cau		&&	Sumy za syntetiky,poü. AU
		fuct=left(vv1,3)
		DO WHILE m.fuct=left(vv1,3) and !eof()	&&	Tisk syntetik
			=proc(m.rec,pp)
			=tstpgend(5)
			?m.nlq+space(m.lmarg)+"Èüet: "+right("00"+ltrim(str(uct,3)),3)+;
				iif(ana<>0,str(ana,5),space(5))+"  "+;
				rtrim(lookup(osn.txt,str(uct,3),osn.uct,'uct'))+;
				iif(ana>0," / "+lookup(osn.txt,str(uct,3)+str(ana,4),osn.uct,'uct'),"")+m.draft
			?space(m.lmarg)+"+---------------------+--------------------------------------+--------------+--------------+"
			?space(m.lmarg)+"|   Obdob°            |  Doklad                              |  Obrat   MD  |  Obrat  DAL  |"
			?space(m.lmarg)+"+---------------------+--------------------------------------+--------------+--------------+"
			ll=m.ll+4
			cau=m.cau+1

			&&	Tisk jednotlivÏch analitik sestavy
			STORE 0 TO m.smda,m.sdala		&&	Sumy za analytiky
			fuctana=left(vv1,7)
			obd=''
			DO WHILE m.fuctana=left(vv1,7) and !eof()
				DO CASE
					CASE right(vv1,4)='   0'
						txt="      Poü†teün° zÖstatek £ütu       "
					CASE right(vv1,4)='9999'
						txt="      KoneünÏ zÖstatek £ütu         "
					OTHE
						txt=str(typ,3)+' - '+lookup(typ.txt,typ,typ.cis,'cis')
					ENDCASE
				mobd=m.obd
				od='01.'+right(dtoc(dat),5)
				do=dtoc(gomonth(ctod(od),1)-1)
				obd=m.od+" - "+m.do
				?space(m.lmarg)+"| "+iif(m.mobd=m.obd,space(19),m.obd)+" | "+;
					txt,'|',;
					casm PICT '@Z 999999999.99',"|",;
					casd PICT '@Z 999999999.99',"|"
				smda=m.smda+casm
				sdala=m.sdala+casd
				ll=m.ll+1
				IF m.ll+1 > m.len_pg
					DO head
					ENDIF
				pp=m.pp+1
				muct=uct
				mana=ana
				SKIP 1
				ENDDO
			smd=m.smd+m.smda
			sdal=m.sdal+m.sdala
			IF m.fuct=left(vv1,3) OR m.cau>1
				STORE space(12) TO mdo,dalo
				IF m.smda-m.sdala>0
					mdo=str(m.smda-m.sdala,12,2)
				  ELSE
					dalo=str(m.sdala-m.smda,12,2)
					ENDIF
				=tstpgend(4)
				?space(m.lmarg)+"+---------------------+--------------------------------------+--------------+--------------+"
				?space(m.lmarg)+"| Obrat £ütu"+space(49)+"| "+ ;
					str(m.smda,12,2)+" | "+str(m.sdala,12,2)+" |"
				?space(m.lmarg)+"| KoneünÏ zÖstatek"+space(43)+"| "+m.mdo+" | "+m.dalo+" |"
				?space(m.lmarg)+"+------------------------------------------------------------+--------------+--------------+"
				ll=m.ll+4
				ENDIF
			m.fuctana=left(vv1,7)
			ENDDO
		m.fuct=left(vv1,3)
		STORE space(12) TO mdo,dalo
		IF m.smd-m.sdal>0
			mdo=str(m.smd-m.sdal,12,2)
		  ELSE
			dalo=str(m.sdal-m.smd,12,2)
			ENDIF
		=tstpgend(5)
		?space(m.lmarg)+"+============================================================+==============+==============+"
		?space(m.lmarg)+"| Obrat £ütu: "+ ;
			right("00"+ltrim(str(m.muct,3)),3)+ ;
			space(44)+"| "+str(m.smd,12,2)+" | "+str(m.sdal,12,2)+" |"
		?space(m.lmarg)+"| KoneünÏ zÖstatek"+space(43)+"| "+m.mdo+" | "+m.dalo+" |"
		?space(m.lmarg)+"+============================================================+==============+==============+"
		?
		ll=m.ll+5
		csmd=csmd+smd
		csdal=csdal+sdal
		ENDDO
	STORE space(12) TO mdo,dalo
	IF m.csmd-m.csdal>0
		mdo=str(m.csmd-m.csdal,12,2)
	  ELSE
		dalo=str(m.csdal-m.csmd,12,2)
		ENDIF
	=tstpgend(4)
	?space(m.lmarg)+repl('*',113)
	?space(m.lmarg)+"* Obdob°:             * "+str(m.csmd,12,2)+" * "+str(m.csdal,12,2)+" * Obrat"+space(53)+"*"
	?space(m.lmarg)+"* "+dtoc(m.tdto)+" - "+dtoc(m.tdtd)+" * "+m.mdo+" * "+m.dalo+" * ZÖstatek"+space(50)+"*"
	?space(m.lmarg)+repl('*',113)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	tik=seco()
	WAIT WIND NOWA "Poüet str†nek:"+str(m.page-1,5)+"   ¨as zpracov†n°: "+str(m.tik-m.tiz,7,3)+" s"
	USE
	SELE denik
	SET CONS ON
	SET FILT TO !del
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
RETURN
PROCEDURE hk_1
	obd=''
	?m.cnon+space(m.lmarg)+"Hlavn° kniha"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(89), ;
			space(23)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+;
			space(24))+"Strana:",m.page PICT '@L 9999'
	?
	ll=2
RETURN
**	Tisk £üetn°ho den°ku
PROCEDURE tis_den
	PRIV fi
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	lmarg=m.margin
	DO t-denik.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	m.fi=iif(m.ttyp<>0,'.and.typ=m.ttyp','')+;
		iif(m.tstr<>0,'.and.str=m.tstr','')
	SET FILT TO !del.AND.dat>=m.tdto.and.dat<=m.tdtd &fi
	GO TOP
	SET CONS OFF
	DO wpr
	tskf=ggf+"denik.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll,pp
	COUNT ALL TO rc
	headprog="ddat_1"
	DO head
	IF m.ttis="Datumu"
		SET ORDER TO dat
		GO TOP
		mes=month(dat)
	  ELSE
		SET ORDER TO typ
		GO TOP
		tpd=typ
		ENDIF
	DO WHILE !eof()
		IF m.ttis="Datumu"
			IF month(dat)<>m.mes
				=tstpgend(1)
				?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
				ll=ll+1
				mes=month(dat)
				ENDIF
		  ELSE
			IF typ<>m.tpd
				=tstpgend(1)
				?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
				ll=ll+1
				tpd=typ
				ENDIF
			ENDIF
		=tstpgend(2)
		?space(m.lmarg)+"| "+dtoc(dat)+" | "+str(typ,3)+str(cis,6)+" |",;
			md PICT '@L 999', mda PICT '@Z 9999','|',;
			dal PICT '@L 999', daa PICT '@Z 9999','|',;
			str(cas,12,2),"| "+txt+" | "+str(str,3)+" |"+;
			iif(dph=0,'    ',str(dph,4))+" |"
		ll=ll+1
		pp=pp+1
		=proc(rc,pp)
		SKIP 1
		ENDDO
	=tstpgend(1)
	?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
	SET ALTE ON
	??m.cnoff+chr(12)
	ll=m.ll+1
	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	SET ALTE TO
	DO tisk WITH m.margin
	SET CONS ON
	SET FILT TO !del
	SET TALK OFF
RETURN
PROCEDURE ddat_1
	?m.cnon+space(m.lmarg)+"Èüetn° den°k"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(91), ;
			space(24)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(25))+ ;
		"Strana:",m.page PICT '@L 9999'
	?
	ll=2
	IF m.page=1
		?m.nlq+m.cnoff
		?space(m.margin/1.6)+alignc("È¨ETN÷ DEN÷K"+space(54))
		?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon
		ll=ll+6
		ENDIF
	?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
	?m.nlq+space(m.lmarg)+"|          |  Doklad   |   Èüet   |  Èüet    |              |                                          |     | Typ |"
	?space(m.lmarg)+"|  Datum   | typ ü°s.  |  m† d†ti |  d a l   |  ¨†stka Kü   |  Text                                    | Str.| DPH |"+m.draft
	?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
	ll=ll+4
RETURN
**	Tisk rozvahy
PROCEDURE tis_roz
	PRIV zisk
	lmarg=m.margin
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	GO TOP
	tdto=dat
	DO t-rozvah.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	m.fi='dat<=m.tdtd'+iif(m.tstr<>0,' AND str=m.tstr','')+ ;
		iif(m.uzucty="Ano",""," AND md<702 AND dal<702")
	DO wpr
	SELE osn
	SET ORDER TO uct
	SET TALK ON
	REPL ALL cmd WITH 0, cdal WITH 0
	SELE denik
	IF m.yana='Ano'
		SET RELA TO str(md,3)+str(mda,4) INTO osn
	  ELSE
		SET RELA TO str(md,3) INTO osn
		ENDIF
	REPL ALL osn.cmd WITH osn.cmd+denik.cas FOR &fi
	IF m.yana='Ano'
		SET RELA TO str(dal,3)+str(daa,4) INTO osn
	  ELSE
		SET RELA TO str(dal,3) INTO osn
		ENDIF
	REPL ALL osn.cdal WITH osn.cdal+denik.cas FOR &fi
	SELE OSN
	PRIV m5, d5, m6, d6
*	VÏpoüet £üetn°ho zisku
	SET CONS OFF
	SET TALK OFF
	CALC sum(cmd), sum(cdal) TO m5, d5 FOR betw(uct,500,599)
	CALC sum(cmd), sum(cdal) TO m6, d6 FOR betw(uct,600,699)
	zisk=d6-m6-(m5-d5)
	m.tskf=ggf+"rozvaha.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll,pp
	COUNT ALL TO rc
	headprog="h_rozvah"
	DO head
	?m.nlq+m.cnoff

	DO CASE
	  CASE tjazyk="¨eskÏ   "
		?space(m.margin/1.6)+alignc("È ¨ E T N ÷   R O Z V A H A  ke dni "+dtoc(m.tdtd)+space(22))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon+m.draft
		?space(m.margin)+repl('=',55)
		?space(m.margin)+"|       Aktivn° £üty       |       Pasivn° £üty       |"
		?space(m.margin)+"+--------------------------+--------------------------+"
	  CASE tjazyk="AnglickÏ"
		?space(m.margin/1.6)+alignc("B A L A N C E  to date "+dtoc(m.tdtd)+space(22))
		?space(m.margin/1.6)+alignc("Firm: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("Center: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon+m.draft
		?space(m.margin)+repl('=',55)
		?space(m.margin)+"|      Active account      |      Passive account     |"
		?space(m.margin)+"+--------------------------+--------------------------+"
	  CASE tjazyk="NÿmeckÏ "
		?space(m.margin/1.6)+alignc("R E C H N U N G  ke dni "+dtoc(m.tdtd)+space(22))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon+m.draft
		?space(m.margin)+repl('=',55)
		?space(m.margin)+"|       Aktivn° £üty       |       Pasivn° £üty       |"
		?space(m.margin)+"+--------------------------+--------------------------+"
	  ENDCASE

	ll=ll+7
	SET FILT TO roz<>'-'
	SET ORDER TO uct

* Nastaven° jazyku pro tisk
	PRIV ttj
	DO CASE
	  CASE tjazyk="¨eskÏ   "
		ttj=""
	  CASE tjazyk="AnglickÏ"
		ttj="A"
	  CASE tjazyk="NÿmeckÏ "
		ttj="N"
	  ENDCASE

	GO TOP
	STORE 0 TO sakt, spas, _ss, _cs
	DO WHILE !eof()
		=tstpgend(2)
		IF cmd-cdal<>0
			IF roz="A"
				?space(m.margin)+'| '+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cmd-cdal,14,2)+'   | '+space(24)+;
					' |  - '+osn.txt&ttj.
				sakt=m.sakt+cmd-cdal
				_ss=_ss+cmd-cdal
				_cs=_cs+1
			  ELSE
				?space(m.margin)+'| '+space(24)+' | '+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cdal-cmd,14,2)+'   |  - '+osn.txt&ttj.
				spas=m.spas+cdal-cmd
				_ss=_ss+cdal-cmd
				_cs=_cs+1
				ENDIF
			ll=ll+1
			ENDIF
		_roz=roz
		_cu=uct
		IF ana=0
			_nu=rtrim(txt&ttj.)
			_cs=0
			_ss=0
			ENDIF
		SKIP 1
		IF uct<>_cu AND _cs>1 AND m.yana='Ano'
			IF _roz="A"
				?space(m.margin)+'|   '+;
					right('00'+ltrim(str(_cu)),3)+"  *"+;
					str(_ss,14,2)+' * | '+space(24)+' |  - '+_nu
*				?space(m.margin)+'|'+repl('~',24)+'| '+space(22)+' |'
			  ELSE
				?space(m.margin)+'| '+space(24)+' |   '+;
					right('00'+ltrim(str(_cu)),3)+"  *"+;
					str(_ss,14,2)+' * |  - '+_nu
*				?space(m.margin)+'| '+space(22)+' |'+repl('~',24)+'|'
				ENDIF
			ll=ll+2
			ENDIF
		ENDDO
	SET FILT TO

**	Z†pis zisku
	=tstpgend(2)
	?space(m.margin)+'| '+space(24)+' | 710     '+str(m.zisk,14,2)+'   |  - '+;
		lookup(osn.txt&ttj.,'710',osn.uct,'uct')
	spas=m.spas+m.zisk
**	Uzav˝en° rozvahy
	?space(m.margin)+"+==========================+==========================+"
	ll=ll+2
	=tstpgend(4)
	?space(m.margin)+'| '+str(m.sakt,22,2)+'   | '+str(m.spas,22,2)+'   |'
	?space(m.margin)+repl('=',55)
	DO CASE
	  CASE tjazyk="¨eskÏ   "
		?space(m.margin)+'| Aktiva - Pasiva ='+str(m.sakt-m.spas,13,2)+;
			iif(m.sakt-m.spas<>0,'    ** C H Y B A **   ',space(22))+'|'
	  CASE tjazyk="AnglickÏ"
		?space(m.margin)+'| Aktiva - Pasiva ='+str(m.sakt-m.spas,13,2)+;
			iif(m.sakt-m.spas<>0,'    ** C H Y B A **   ',space(22))+'|'
	  CASE tjazyk="NÿmeckÏ "
		?space(m.margin)+'| Aktiva - Pasiva ='+str(m.sakt-m.spas,13,2)+;
			iif(m.sakt-m.spas<>0,'    ** C H Y B A **   ',space(22))+'|'
	  ENDCASE
	?space(m.margin)+repl('=',55)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	SET CONS ON
	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
	SELE denik
	SET RELA TO
RETURN
PROCEDURE h_rozvah
	DO CASE
	  CASE tjazyk="¨eskÏ   "
		?m.cnon+space(m.lmarg)+"Rozvaha"+ ;
			iif(m.page!=1 OR empty(m.copyright),space(96), ;
				space(27)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(27))+ ;
			"Strana:",m.page PICT '@L 9999'
	  CASE tjazyk="AnglickÏ"
		?m.cnon+space(m.lmarg)+"Balance"+ ;
			iif(m.page!=1 OR empty(m.copyright),space(96), ;
				space(27)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(27))+ ;
			"  Page:",m.page PICT '@L 9999'
	  CASE tjazyk="NÿmeckÏ "
		?m.cnon+space(m.lmarg)+"Rozvaha"+ ;
			iif(m.page!=1 OR empty(m.copyright),space(96), ;
				space(27)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(27))+ ;
			"Strana:",m.page PICT '@L 9999'
	  ENDCASE
	?
	ll=2
RETURN
**	Tisk vÏsledovky
PROCEDURE tis_vsl
	PRIV fi, casrozl
	lmarg=m.margin
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	app=.T.
	casrozl="N"
	title=" Tisk vÏsledovky "
	DO t-vysle.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	DO wpr
	IF m.casrozl="N"
		fi='tdto<=dat AND tdtd>=dat'+;
			iif(m.tstr<>0,'.and.str=m.tstr','')+ ;
			iif(m.uzucty="Ano",""," AND md<702 AND dal<702")
	  ELSE
		REPL ALL datnakl WITH dat FOR empty(datnakl)
		fi='tdto<=datnakl AND tdtd>=datnakl'+;
			iif(m.tstr<>0,'.and.str=m.tstr','')+ ;
			iif(m.uzucty="Ano",""," AND md<702 AND dal<702")
	ENDIF

	SELE osn
	SET ORDER TO uct
	SET FILT TO
	SET TALK ON
	REPL ALL cmd WITH 0, cdal WITH 0 FOR betw(uct,500,699)
	SELE denik

*	IF m.yana='Ano'
		SET RELA TO str(md,3)+str(mda,4) INTO osn
*	  ELSE
*		SET RELA TO str(md,3) INTO osn
*		ENDIF
	REPL ALL osn.cmd WITH osn.cmd+denik.cas ;
		FOR &fi AND betw(md,500,699) AND (osn.dan='A' OR m.vall='Ano')
*	IF m.yana='Ano'
		SET RELA TO str(dal,3)+str(daa,4) INTO osn
*	  ELSE
*		SET RELA TO str(dal,3) INTO osn
*		ENDIF
	REPL ALL osn.cdal WITH osn.cdal+denik.cas ;
		FOR &fi AND betw(dal,500,699) AND (osn.dan='A' OR m.vall='Ano')
	SET RELA TO
	DO wpr
	SET CONS OFF
	SET TALK OFF

	SELE osn
	SET ORDER TO uct
	m.tskf=ggf+"vysled_d.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll
	headprog="h_vysle"
	DO head
	?m.nlq+m.cnoff

	DO CASE
	  CASE tjazyk="¨eskÏ   "
		?space(m.margin/1.6)+alignc("VÌSLEDOVKA - "+IIF(m.vall='Ne ',"daÂov†","£üetn°")+space(66-19))
		?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon+m.draft
		?space(m.margin)+repl('=',55)
		?space(m.margin)+'    6xx          ¨†stka        5xx          ¨†stka'
		?space(m.margin)+repl('=',55)
	  CASE tjazyk="AnglickÏ"
		?space(m.margin/1.6)+alignc("RESULT - "+IIF(m.vall='Ne ',"daÂov†","£üetn°")+space(66-19))
		?space(m.margin/1.6)+alignc("From "+dtoc(m.tdto)+" to "+dtoc(m.tdtd)+space(36))
		?space(m.margin/1.6)+alignc("Firm: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("Center: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon+m.draft
		?space(m.margin)+repl('=',55)
		?space(m.margin)+'    6xx          ¨†stka        5xx          ¨†stka'
		?space(m.margin)+repl('=',55)
	  CASE tjazyk="NÿmeckÏ "
		?space(m.margin/1.6)+alignc("VÌSLEDOVKA - "+IIF(m.vall='Ne ',"daÂov†","£üetn°")+space(66-19))
		?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon+m.draft
		?space(m.margin)+repl('=',55)
		?space(m.margin)+'    6xx          ¨†stka        5xx          ¨†stka'
		?space(m.margin)+repl('=',55)
	  ENDCASE

	ll=ll+8
	PRIV s1, s2, _ss, _cs, _nu
	_nu=""
	STORE 0 TO s1, s2, _ss, _cs
	SET FILT TO betw(uct,500,699)

* Nastaven° jazyku pro tisk
	PRIV ttj
	DO CASE
	  CASE tjazyk="¨eskÏ   "
		ttj=""
	  CASE tjazyk="AnglickÏ"
		ttj="A"
	  CASE tjazyk="NÿmeckÏ "
		ttj="N"
	  ENDCASE

	GO TOP
	DO WHILE !eof()
		=tstpgend(2)
		IF (dan="A" OR m.vall='Ano') AND cmd-cdal != 0
			IF uct>=600
				IF m.yana='Ano'
					?space(m.margin)+'| '+;
						right('00'+ltrim(str(uct)),3)+ ;
							iif(m.yana="Ano",str(ana,5),"     ")+;
						str(cdal-cmd,14,2)+'   | '+space(24)+' |  - '+osn.txt&ttj.
					ll=ll+1
					ENDIF
				s1=s1+cdal-cmd
				_ss=_ss+cdal-cmd
				_cs=_cs+1
			  ELSE
				IF m.yana='Ano'
					?space(m.margin)+'| '+space(24)+' | '+;
						right('00'+ltrim(str(uct)),3)+;
							iif(m.yana="Ano",str(ana,5),"     ")+;
						str(cmd-cdal,14,2)+'   |  - '+osn.txt&ttj.
					ll=ll+1
					ENDIF
				s2=s2+cmd-cdal
				_ss=_ss+cmd-cdal
				_cs=_cs+1
				ENDIF
			ENDIF
		_roz=roz
		_cu=uct
		IF empty(m._nu)
			_nu=rtrim(txt&ttj.)
			ENDIF
		SKIP 1
		IF uct<>_cu AND _ss != 0
			IF m.yana='Ano'		&&	Pro analytiky
				IF m._cs>1
					IF m._cu>=600
						?space(m.margin)+'|   '+;
							right('00'+ltrim(str(_cu)),3)+"  *"+;
							str(_ss,14,2)+' * | '+space(24)+' |  - '+m._nu
					  ELSE
						?space(m.margin)+'| '+space(24)+' |   '+;
							right('00'+ltrim(str(_cu)),3)+"  *"+;
							str(_ss,14,2)+' * |  - '+m._nu
						ENDIF
					ll=ll+1
					ENDIF
			  ELSE						&&	¨istÇ £üty
				IF m._cu>=600
					?space(m.margin)+'|   '+;
						right('00'+ltrim(str(_cu)),3)+"   "+;
						str(_ss,14,2)+'   | '+space(24)+' |  - '+m._nu
				  ELSE
					?space(m.margin)+'| '+space(24)+' |   '+;
						right('00'+ltrim(str(_cu)),3)+"   "+;
						str(_ss,14,2)+'   |  - '+m._nu
					ENDIF
				ll=ll+1
				ENDIF
			ENDIF
		IF ana=0
			_nu=rtrim(txt&ttj.)
			_cs=0
			_ss=0
			ENDIF
		ENDDO
	=tstpgend(5)
	?space(m.margin)+repl('=',55)
	?space(m.margin)+'| 6xx     '+str(s1,14,2)+'   | 5xx     '+str(s2,14,2)+'   |'
	?space(m.margin)+repl('-',55)
	?space(m.margin)+'| '+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(6)+;
		'6xx - 5xx ='+str(s1-s2,13,2)+'   |'
	?space(m.margin)+repl('=',55)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	SET CONS ON
	SELE osn
	SET FILT TO
	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
RETURN
PROCEDURE h_vysle
	DO CASE
	  CASE tjazyk="¨eskÏ   "
		?m.cnon+space(m.lmarg)+"VÏsledovka"+ ;
			iif(m.page!=1 OR empty(m.copyright),space(93), ;
				space(25)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(26))+ ;
			"Strana:",m.page PICT '@L 9999'
	  CASE tjazyk="AnglickÏ"
		?m.cnon+space(m.lmarg)+"Result    "+ ;
			iif(m.page!=1 OR empty(m.copyright),space(93), ;
				space(25)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(26))+ ;
			"  Page:",m.page PICT '@L 9999'
	  CASE tjazyk="NÿmeckÏ "
		?m.cnon+space(m.lmarg)+"Result    "+ ;
			iif(m.page!=1 OR empty(m.copyright),space(93), ;
				space(25)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(26))+ ;
			"  Page:",m.page PICT '@L 9999'
	  ENDCASE
	?
	ll=2
RETURN
**	Tisk hospod†˝skÇho vÏsledku
PROCEDURE tis_hos
	lmarg=m.margin
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	app=.F.
	title=" Tisk hospod†˝skÇho vÏsledku "
	DO t-vysle.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	SET TALK ON
	DO wpr
	fi='tdto<=dat.AND.tdtd>=dat'+;
		iif(m.tstr<>0,'.and.str=m.tstr','')+ ;
		iif(m.uzucty="Ano",""," AND md<702 AND dal<702")
	SELE osn
	SET ORDER TO uct
	SET FILT TO
	SET TALK ON
	REPL ALL cmd WITH 0, cdal WITH 0 FOR betw(uct,500,699)
	SELE denik
	IF m.yana='Ano'
		SET RELA TO str(md,3)+str(mda,4) INTO osn
	  ELSE
		SET RELA TO str(md,3) INTO osn
		ENDIF
	REPL ALL osn.cmd WITH osn.cmd+denik.cas FOR &fi AND betw(md,500,699)
	IF m.yana='Ano'
		SET RELA TO str(dal,3)+str(daa,4) INTO osn
	  ELSE
		SET RELA TO str(dal,3) INTO osn
		ENDIF
	REPL ALL osn.cdal WITH osn.cdal+denik.cas FOR &fi AND betw(dal,500,699)
	SET RELA TO
	DO wpr
	SET CONS OFF
	SET TALK OFF
	SELE osn
	SET ORDER TO uct

	m.tskf=ggf+"vysled_u.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll
	headprog="h_hospod"
	DO head
	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("HOSPODµ¸SKÌ VÌSLEDEK"+space(46))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	IF tstr!=0
		?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
		ll=ll+1
		ENDIF
	?m.cnon+m.draft
	STORE 0 TO s1,s2,s605,s505,s667,s567,s68,s58

**	Èüty 600-649 a 500-549 a £üty 697,597
	?space(m.margin)+repl('=',52)
	?space(m.margin)+'| 6xx            ¨†stka   | 5xx            ¨†stka  |'
	?space(m.margin)+repl('=',52)
	?space(m.margin)+'| Hospod†˝skÏ vÏsledek z bÿßnÇ üinnosti            |'
	?space(m.margin)+'| a) PROVOZN÷'+space(38)+'|'
	?space(m.margin)+'|'+repl('˙',50)+'|'
	ll=ll+11
	SET FILT TO ((uct>=600 .AND. uct<660) .OR. (uct>=500 .AND. uct<560);
		.OR. uct=697 .OR. uct=597) AND cmd-cdal != 0
	GO TOP
	ll=ll+6
	DO WHILE !eof()
		=tstpgend(1)
		IF uct>=600
			?space(m.margin)+'|'+iif(m.yana='Ano'.and.dan='N','*',' ')+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cdal-cmd,14,2)+'  | '+space(22)+' |  - '+;
				osn.txt
			s605=m.s605+cdal-cmd
			s1=m.s1+cdal-cmd
		ELSE
			?space(m.margin)+'| '+space(22)+'  |'+;
				iif(m.yana='Ano'.and.dan='N','*',' ')+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cmd-cdal,14,2)+' |  - '+osn.txt
			s505=m.s505+cmd-cdal
			s2=s2+cmd-cdal
			ENDIF
		SKIP 1
		ll=ll+1
		ENDDO
	=tstpgend(4)
	?space(m.margin)+'|'+repl('-',50)+'|'
	?space(m.margin)+'| 6xx     '+str(s605,14,2)+'  | 5xx     '+str(s505,14,2)+' |'
	?space(m.margin)+'| '+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(5)+;
		'6xx - 5xx ='+str(s605-s505,13,2)+' |'
	?space(m.margin)+repl('=',52)
	ll=ll+4
**	Èüty 660-679 a 560-579, 598, 698
	SET FILT TO ((uct>=660 .AND. uct<680) .OR. (uct>=560 .AND. uct<580);
		.OR. uct=698 .OR. uct=598) AND cmd-cdal != 0
	GO TOP
	=tstpgend(3)
	?space(m.margin)+'| b) FINAN¨N÷'+space(38)+'|'
	?space(m.margin)+'|'+repl('˙',50)+'|'
	ll=ll+2
	DO WHILE !eof()
		=tstpgend(1)
		IF uct>=600
			?space(m.margin)+'|'+iif(m.yana='Ano'.and.dan='N','*',' ')+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cdal-cmd,14,2)+'  | '+space(22)+' |  - '+osn.txt
			s667=m.s667+cdal-cmd
			s1=m.s1+cdal-cmd
		ELSE
			?space(m.margin)+'| '+space(22)+'  |'+;
				iif(m.yana='Ano'.and.dan='N','*',' ')+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cmd-cdal,14,2)+' |  - '+osn.txt
			s567=m.s567+cmd-cdal
			s2=s2+cmd-cdal
			ENDIF
		SKIP 1
		ll=ll+1
		ENDDO
	=tstpgend(4)
	?space(m.margin)+'|'+repl('-',50)+'|'
	?space(m.margin)+'| 6xx     '+str(s667,14,2)+'  | 5xx     '+;
		str(s567,14,2)+' |'
	?space(m.margin)+'| '+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(5)+;
		'6xx - 5xx ='+str(s667-s567,13,2)+' |'
	?space(m.margin)+repl('=',52)
	ll=ll+4
**	Celkov† sumarizace
	=tstpgend(5)
	?space(m.margin)+repl('*',52)
	?space(m.margin)+'| 6xx     '+str(s1,14,2)+'  | 5xx     '+;
		str(s2,14,2)+' |'
	?space(m.margin)+repl('-',52)
	?space(m.margin)+'| '+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(5)+;
		'6xx - 5xx ='+str(s1-s2,13,2)+' |'
	?space(m.margin)+repl('*',52)
	ll=ll+5

**	Èüty 680-689 a 580-589
	SET FILT TO ((uct>=680 .AND. uct<690) .OR. (uct>=580 .AND. uct<590));
		AND cmd-cdal != 0
	GO TOP
	=tstpgend(3)
	?space(m.margin)+'| Hospod†˝skÏ vÏsledek z mimo˝†dnÇ üinnosti        |'
	?space(m.margin)+'|'+repl('˙',50)+'|'
	ll=ll+2
	DO WHILE !eof()
		=tstpgend(1)
		IF uct>=600
			?space(m.margin)+'|'+iif(m.yana='Ano'.and.dan='N','*',' ')+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cdal-cmd,14,2)+'  | '+space(22)+' |  - '+osn.txt
			s68=m.s68+cdal-cmd
			s1=m.s1+cdal-cmd
		ELSE
			?space(m.margin)+'| '+space(22)+'  |'+;
				iif(m.yana='Ano'.and.dan='N','*',' ')+;
				right('00'+ltrim(str(uct)),3)+;
				iif(m.yana="Ano",str(ana,5),"     ")+;
				str(cmd-cdal,14,2)+' |  - '+osn.txt
			s58=m.s58+cmd-cdal
			s2=s2+cmd-cdal
			ENDIF
		SKIP 1
		ll=ll+1
		ENDDO
	=tstpgend(4)
	?space(m.margin)+'|'+repl('-',50)+'|'
	?space(m.margin)+'| 6xx     '+str(s68,14,2)+'  | 5xx     '+str(s58,14,2)+' |'
	?space(m.margin)+'| '+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(5)+;
		'6xx - 5xx ='+str(s68-s58,13,2)+' |'
	?space(m.margin)+repl('=',52)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	ll=ll+4
	SET FILT TO
	SET CONS ON
	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
RETURN
PROCEDURE h_hospod
	?m.cnon+space(m.lmarg)+"Hospod†˝skÏ vÏsledek"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(83), ;
			space(20)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(21))+ ;
		"Strana:",m.page PICT '@L 9999'
	?
	ll=2
RETURN
**	Tisk vÏsledovky po mÿs°c°ch
PROCEDURE tis_msn

	PRIV fi
	lmarg=m.margin
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	app=.T.
	title=" VÏsledovka po mÿs°c°ch "
	DO t-mvysl.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF

	SET ESCA ON
	ON ESCA DO stoptisk
	DO wpr
	fi='!del AND tdto<=dat.AND.tdtd>=dat'+;
		iif(m.tstr<>0,'.and.str=m.tstr','')+ ;
		" AND (betw(md,500,701) OR betw(dal,500,701))"

	CREA CURS cu (dat C(4), md N(12,2), dal N(12,2), vysl N(12,2))
	INDEX ON dat TAG dat
	
	DO wpr
	SET CONS OFF
	SET TALK OFF

	m.tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

	PRIV rr
	SELE denik
	SCAN ALL FOR &fi
		SELE cu
		rr=right(strn(year(denik.dat)),2)+right('0'+strn(month(denik.dat)),2)
		IF !seek(m.rr)
			INSERT INTO cu VALUES (m.rr, 0, 0, 0)
			ENDIF
		IF betw(denik.md,500,599)
			REPL cu.md WITH cu.md+denik.cas
			ENDIF
		IF betw(denik.dal,500,599)
			REPL cu.md WITH cu.md+denik.cas*-1
			ENDIF
		IF betw(denik.dal,600,699)
			REPL cu.dal WITH cu.dal+denik.cas
			ENDIF
		IF betw(denik.md,600,699)
			REPL cu.dal WITH cu.dal+denik.cas*-1
			ENDIF
		SELE denik
		ENDSCAN

	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("VÌSLEDOVKA po mÿs°c°ch"+space(44))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	IF tstr!=0
		?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
		ENDIF

	?m.draft
	?space(m.margin)+repl('=',52)
	?space(m.margin)+"|  Mÿs°c |    N†klady  |    VÏnosy   |      Zisk   |"
	?space(m.margin)+repl('=',52)
	PRIV s1, s2
	STORE 0 TO s1, s2

	SELE cu
	SCAN ALL
		?space(m.margin)+"|  "+dat+"  |"+str(md,12,2)+" |"+str(dal,12,2)+" |"+str(dal-md,12,2)+" |"
		s1=m.s1+md
		s2=m.s2+dal
		ENDSCAN

	?space(m.margin)+repl('=',52)
	?space(m.margin)+"| Celkem |"+str(m.s1,12,2)+" |"+str(m.s2,12,2)+" |"+str(s2-s1,12,2)+" |"
	?space(m.margin)+repl('=',52)
	SET ALTE ON
	?chr(12)
	SET ALTE TO
	SET CONS ON

	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
RETURN
**	P˝ehledy
PROCEDURE tis_peh
	DEFINE POPUP pre FROM 8,37 RELATIVE SHADOW ;
		TITLE " P¸EHLEDY " COLOR SCHEME 4
	DEFINE BAR 15 OF pre PROMPT " DaÂovÇ \<p˝izn†n° - podklad od 1.5.2004" SKIP FOR m.ggpldph='Ne ';
		MESS "VÏpoüet daÂovÇho p˝izn†n° za zadanÇ obdob°."
	DEFINE BAR 20 OF pre PROMPT " Soupis \<daÂovÏch dokladÖ od 1.5.2004" SKIP FOR m.ggpldph='Ne ';
		MESS "Soupis daÂovÏch dokladÖ rozdÿlenÏ podle druhÖ DPH."
	DEFINE BAR 35 OF pre PROMPT " \<Souüty po dokladech " ;
		MESS "Souüty po jednotlivÏch ü°slech zadanÇho dokladu"
	DEFINE BAR 38 OF pre PROMPT " Prop\<lacen° dokladÖ" ;
		MESS "ZjiÁtÿn° nedoplatkÖ a p˝eplatkÖ typu dokladÖ."
	DEFINE BAR 58 OF pre PROMPT " Tisk z†znamÖ s \<otazn°kem " ;
		MESS "Tisk seznamu poloßek maj°c°ch v textu otazn°k."
	DEFINE BAR 59 OF pre PROMPT "\-"
	DEFINE BAR 60 OF pre PROMPT " P˝edva\<ha " ;
		MESS "P˝edvaha £ütÖ."
	DEFINE BAR 61 OF pre PROMPT " \<NedaÂovÇ n†klady a vÏnosy " ;
		MESS "VÏpis nedaÂovÏch n†kladovÏch n†kladÖ a vÏnosÖ."
	DEFINE BAR 62 OF pre PROMPT " \<ZÖstatky £ütu po dnech " ;
		MESS "Soupis denn°ch zÖstatkÖ zadanÏch £ütÖ."
	DEFINE BAR 90 OF pre PROMPT " Èüty s \<analytikou " ;
		MESS "Èüty rozdÿlenÇ analyticky."
	DEFINE BAR 91 OF pre PROMPT " ÈütovÏ \<rozvrh" ;
		MESS "ÈütovÏ rozvrh poußitÏch £ütÖ."
	DEFINE BAR 92 OF pre PROMPT " Typy do\<kladÖ" ;
		MESS "Sestava poußitÏch typÖ dokladÖ."
	DEFINE BAR 94 OF pre PROMPT "\-"
	DEFINE BAR 95 OF pre PROMPT " StarÇ sestavy pro DPH " SKIP FOR m.ggpldph='Ne ';
		MESS "StarÇ tiskopisy a sestavy pro DPH platnÇ do 30.4.2004."

	ON SELE POPU pre DO pre

	ACTI POPUP pre

	RELE POPU pre
RETURN
**	Obsluha menu P˝ehledy
PROC pre
	DO CASE
		CASE bar()=15
			DO pre_pri
		CASE bar()=20
			DO pre_sou
		CASE bar()=35
			DO pre_ctyp
		CASE bar()=38
			DO pre_prop
		CASE bar()=58
			DO pre_otaz
		CASE bar()=60
			DO pre_pre
		CASE bar()=61
			DO pre_neda
		CASE bar()=62
			DO pre_dz
		CASE bar()=90
			SELE osn
			SET FILT TO ana>0
			SET CONS OFF
			m.tskf=m.ggf+'ekont.prt'
			REPO FORM tp_ana TO FILE (m.tskf)
			SET CONS ON
			SET FILT TO
			DO tisk
		CASE bar()=91
			DO pre_ucr
		CASE bar()=92
			DO pre_typd
		CASE bar()=95
			DO m_olddph
		ENDCASE
RETURN
**	Sestava daÂovÇho p˝izn†n° - od 1.5.2004
PROCEDURE pre_pri 

********* Nechat zadat v dotazovÇm screenu
	PRIV DodatDPH, K550, K560, R560

	DodatDPH="N"
	K550=1
	K560=1
	R560=0
	
	
	lmarg=m.margin
	DO t-priz.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk

	SELE denik
	DO wpr
	SET TALK ON
	REPL dph WITH iif(md=m.ggdph OR dal=m.ggdph,iif(md=m.ggdph,mda,daa),dph) ;
		 ALL FOR betw(dat,m.tdto,m.tdtd)

	SELE 0
	CREATE CURSOR cr1 (dph N(4), z_dane N(12,2), dan N(12,2))
	INDEX ON dph TAG dph
	SET ORDER TO dph

	SELE osn
	SCAN ALL FOR osn.ana>1000 OR osn.ana<20 OR betw(osn.ana,98,100)
		INSERT INTO cr1 (dph) VALUES (osn.ana)
	ENDSCAN

* Naüten° souütÖ jednotlivÏch typÖ DPH
	SELE denik
	SET RELA TO dph INTO cr1 ADDI

	SCAN ALL FOR denik.dph>0 AND !denik.del AND betw(denik.dat,m.tdto,m.tdtd)
		IF eof("cr1") AND ask(-1,-1,"N","Nenalezen typ DPH ("+strn(denik.dph)+"). Ukonüit")
			SELE cr1
			USE
			SET ESCA OFF
			ON ESCA
			RETURN
		ENDIF
		REPL cr1.dan WITH cr1.dan+denik.cas, cr1.z_dane WITH cr1.z_dane+denik.zad
	ENDSCAN

* Vytvo˝en° pamÿúovÏch promÿnÏch a uloßen° ü†stek z analytik
	SELE cr1
	SCAN ALL
		c="Z"+right("000"+strn(dph),4)
		&c=cr1.z_dane
		c="D"+right("000"+strn(dph),4)
		&c=cr1.dan
	ENDSCAN

* PamÿúovÇ promÿnnÇ pro vÏpoüet formul†˝e
	STORE 0 TO Z210, Z215, Z220, Z225, Z230, Z235, Z240, Z245, Z250, Z255, Z260, Z265, Z270, Z275
	STORE 0 TO R210, R215, R220, R225, R230, R235, R240, R245, R250, R255, R260, R265, R270, R275

	STORE 0 TO Z310, Z315, Z320, Z325, Z330, Z335, Z340, Z345, Z350, Z355, Z360, Z365, Z370
	STORE 0 TO R310, R315, R320, R325, R330, R335, R340, R345, R350, R355, R360, R365, R370, R380, R390
	STORE 0 TO K310, K315, K320, K325, K330, K335, K340, K345, K350, K355, K360, K365, K370

	STORE 0 TO R410, R420, R425, R430, R440, R510, R520, R530, R540, R550, R560, R570, R580, R600
* Nadefinov†no jiß na zaü†tku, kvÖli ruün°mu zad†n°
*	STORE 0 TO K550, K560
	STORE 0 TO R710, R730, R750, R753, R754, R780, R810, R815

* Naplnÿn° a vÏpoüet formul†˝e

*R210-275

	Z210=round(Z2100+Z2101+Z2102,0)
	Z215=round(Z2150+Z2151+Z2152,0)
	Z220=round(Z2200,0)
	Z225=round(Z2250,0)
	Z230=round(Z2300,0)
	Z235=round(Z2350,0)
	Z240=round(Z2400,0)
	Z245=round(Z2450,0)
	Z250=round(Z2500,0)
	Z255=round(Z2550,0)
	Z260=round(Z2600,0)
	Z265=round(Z2650,0)
	Z270=round(Z2700,0)
	Z275=round(Z2750,0)

	R210=round(D2100+D2101+D2102,0)
	R215=round(D2150+D2151+D2152,0)
	R220=round(D2200,0)
	R225=round(D2250,0)
	R230=round(D2300,0)
	R235=round(D2350,0)
	R240=round(D2400,0)
	R245=round(D2450,0)
	R250=round(D2500,0)
	R255=round(D2550,0)
	R260=round(D2600,0)
	R265=round(D2650,0)
	R270=round(D2700,0)
	R275=round(D2750,0)

* R310-370

	Z310=round(Z3100+Z3101,0)
	Z315=round(Z3150+Z3151,0)
	Z320=round(Z3200+Z3201,0)
	Z325=round(Z3250+Z3251,0)
	Z330=round(Z3300+Z3301,0)
	Z335=round(Z3350+Z3351,0)
	Z340=round(Z3400+Z3401,0)
	Z345=round(Z3450+Z3451,0)
	Z350=round(Z3500+Z3501,0)
	Z355=round(Z3550+Z3551,0)
	Z360=round(Z3600+Z3601,0)
	Z365=round(Z3650+Z3651,0)
	Z370=round(Z3700+Z3701,0)

	R310=round(D3100,0)
	R315=round(D3150,0)
	R320=round(D3200,0)
	R325=round(D3250,0)
	R330=round(D3300,0)
	R335=round(D3350,0)
	R340=round(D3400,0)
	R345=round(D3450,0)
	R350=round(D3500,0)
	R355=round(D3550,0)
	R360=round(D3600,0)
	R365=round(D3650,0)
	R370=round(D3700,0)

	K310=round(D3101,0)
	K315=round(D3151,0)
	K320=round(D3201,0)
	K325=round(D3251,0)
	K330=round(D3301,0)
	K335=round(D3351,0)
	K340=round(D3401,0)
	K345=round(D3451,0)
	K350=round(D3501,0)
	K355=round(D3551,0)
	K360=round(D3601,0)
	K365=round(D3651,0)
	K370=round(D3701,0)

* R380-
	R380=round(K310+K315+K320+K325+K330+K335+K340+K345+K350+K355+K360+K365+K370,0)
	R390=round(R310+R315+R320+R325+R330+R335+R340+R345+R350+R355+R360+R365+R370,0)

*???  4101 ??? 
	R410=round(D4100,0)
	R420=round(D4200,0)
	R425=round(D4250,0)
	R430=round(D4300,0)
	R440=round(D4400,0)

	R510=Z210+Z215+R410+R420+R425+R430+R440+ round(D5101+D5102,0)
	R520=round(D2101+D2151,0)

	R530=round(D5300+D5400,0)
	R540=round(D5400,0)

	R550=round(R380*K550,0)
* Zat°m zad†no ruünÿ
*	R560="???"	&& ************

	R570=round(D5700,0)
	R580=round(D5800,0)
	R600=round(D6000,0)
	R710=round(D7100,0)
	R730=R210+R215+R220+R225+R230+R235+R240+R245+R250+R255+R260+R265+R270+R275-R600+R710
	R750=R390+R550+R560+R570+R580

	R753=iif(R730-R750>0,R730-R750,0)
	R754=iif(R750-R730>0,R750-R730,0)
	R780=iif(m.dodatDPH="A",R730-R750,0)

	R810=round(D8100,0)
	R815=round(D8150,0)


* Tisk formul†˝e	
	
	SET TALK OFF
	m.tskf=ggf+"ekont.prt"
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

	??m.nlq+m.cnoff+space(m.margin/1.6)+alignc("VÌPO¨ET DAN∑ Z P¸IDANê HODNOTY"+space(36))
	?space(m.margin/1.6)+;
		alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))+m.draft+m.elit

	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| II. Povinnost p˝iznat daÂ za              | Z†kl.danÿ | DaÂ vÏstup|"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| ˝.≥ 1. UZP s m°stem plnÿn° v tuzemsku"
	?space(m.margin/1.2)+"|210| se z†kladn° sazbou danÿ               |"+str(z210,10)+" |"+str(r210,10)+" |"
	?space(m.margin/1.2)+"|215| se sn°ßenou sazbou danÿ               |"+str(z215,10)+" |"+str(r215,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 2. po˝°zen° zboß° z J¨S"
	?space(m.margin/1.2)+"|220| se z†kladn° sazbou danÿ               |"+str(z220,10)+" |"+str(r220,10)+" |"
	?space(m.margin/1.2)+"|225| se sn°ßenou sazbou danÿ               |"+str(z225,10)+" |"+str(r225,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 3. poskytnut° slußby os.reg.k dani v J¨S"
	?space(m.margin/1.2)+"|230| se z†kladn° sazbou danÿ               |"+str(z230,10)+" |"+str(r230,10)+" |"
	?space(m.margin/1.2)+"|235| se sn°ßenou sazbou danÿ               |"+str(z235,10)+" |"+str(r235,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 4. zas°l†n° zboß° z J¨S s m°stem plnÿn° v tuzemsku"
	?space(m.margin/1.2)+"|240| se z†kladn° sazbou danÿ               |"+str(z240,10)+" |"+str(r240,10)+" |"
	?space(m.margin/1.2)+"|245| se sn°ßenou sazbou danÿ               |"+str(z245,10)+" |"+str(r245,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 5. po˝°zen° novÇho dopravn°ho prost˝edku"
	?space(m.margin/1.2)+"|250| od os.registr.k dani                  |"+str(z250,10)+" |"+str(r250,10)+" |"
	?space(m.margin/1.2)+"|255| od os.neregistr.k dani                |"+str(z255,10)+" |"+str(r255,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 6. dovoz zboß°"
	?space(m.margin/1.2)+"|260| se z†kladn° sazbou danÿ               |"+str(z260,10)+" |"+str(r260,10)+" |"
	?space(m.margin/1.2)+"|265| se sn°ßenou sazbou danÿ               |"+str(z265,10)+" |"+str(r265,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 7. poskytnut° slußby zahraniün° osobou"
	?space(m.margin/1.2)+"|270| se z†kladn° sazbou danÿ               |"+str(z270,10)+" |"+str(r270,10)+" |"
	?space(m.margin/1.2)+"|275| se sn°ßenou sazbou danÿ               |"+str(z275,10)+" |"+str(r275,10)+" |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| III. N†rok na odp.daÂ.z plÂ.za| Z†kl.danÿ | DaÂ (PN)  | DaÂ (KN)  |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| ˝.≥ 1. PZP s m°stem plnÿn° v tuzemsku"
	?space(m.margin/1.2)+"|310| se z†kladn° sazbou danÿ   |"+str(z310,10)+" |"+str(r310,10)+" |"+str(k310,10)+" |"
	?space(m.margin/1.2)+"|315| se sn°ßenou sazbou danÿ   |"+str(z315,10)+" |"+str(r315,10)+" |"+str(k315,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 2. po˝°zen° zboß° z J¨S"
	?space(m.margin/1.2)+"|320| se z†kladn° sazbou danÿ   |"+str(z320,10)+" |"+str(r320,10)+" |"+str(k320,10)+" |"
	?space(m.margin/1.2)+"|325| se sn°ßenou sazbou danÿ   |"+str(z325,10)+" |"+str(r325,10)+" |"+str(k325,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 3. poskytnut° slußby os.reg.k dani v J¨S"
	?space(m.margin/1.2)+"|330| se z†kladn° sazbou danÿ   |"+str(z330,10)+" |"+str(r330,10)+" |"+str(k330,10)+" |"
	?space(m.margin/1.2)+"|335| se sn°ßenou sazbou danÿ   |"+str(z335,10)+" |"+str(r335,10)+" |"+str(k335,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 4. dovoz zboß°"
	?space(m.margin/1.2)+"|340| se z†kladn° sazbou danÿ   |"+str(z340,10)+" |"+str(r340,10)+" |"+str(k340,10)+" |"
	?space(m.margin/1.2)+"|345| se sn°ßenou sazbou danÿ   |"+str(z345,10)+" |"+str(r345,10)+" |"+str(k345,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 5. poskytnut° slußby zahraniün° osobou"
	?space(m.margin/1.2)+"|350| se z†kladn° sazbou danÿ   |"+str(z350,10)+" |"+str(r350,10)+" |"+str(k350,10)+" |"
	?space(m.margin/1.2)+"|355| se sn°ßenou sazbou danÿ   |"+str(z355,10)+" |"+str(r355,10)+" |"+str(k355,10)+" |"
	?space(m.margin/1.2)+"| ˝.≥ 6. po˝°zen° novÇho dopravn°ho prost˝edku"
	?space(m.margin/1.2)+"|360| od os.registr.k dani      |"+str(z360,10)+" |"+str(r360,10)+" |"+str(k360,10)+" |"
	?space(m.margin/1.2)+"|365| od os.neregistr.k dani    |"+str(z365,10)+" |"+str(r365,10)+" |"+str(k365,10)+" |"
	?space(m.margin/1.2)+"|370| p˝i zmÿnÿ reßimu          |"+str(z370,10)+" |"+str(r370,10)+" |"+str(k370,10)+" |"
	?space(m.margin/1.2)+"|380| Celk.suma pro kr†cen° n†roku na odpoüet           |"+str(r380,10)+" |"
	?space(m.margin/1.2)+"|390| Celk.suma plnÇho n†roku na odpoüet    |"+str(r390,10)+" |           |"
	?space(m.margin/1.2)+repl('-',69)
	??chr(12)
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| IV. Uskut.plnÿn° osv.od danÿ s n†rokem na odpoü.danÿ  |¨as.za plÂ.|"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|410| Dod†n° zboß° do J¨S                               |"+str(r410,10)+" |"
	?space(m.margin/1.2)+"|420| Dod†n° novÇho dopr.prost˝. do J¨S os.reg.k dani   |"+str(r420,10)+" |"
	?space(m.margin/1.2)+"|425| Dod†n° novÇho dopr.prost˝. do J¨S os.nereg.k dani |"+str(r425,10)+" |"
	?space(m.margin/1.2)+"|430| VÏvoz zboß°                                       |"+str(r430,10)+" |"
	?space(m.margin/1.2)+"|440| Ostatn° plnÿn° osv.od danÿ s n†rokem na odpoüet   |"+str(r440,10)+" |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| V. Uskuteünÿn† plnÿn° celkem                          |¨as.za plÂ.|"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|510| Celkov† ü†stka uskut.plnÿn° s n†rokem na odpoüet  |"+str(r510,10)+" |"
	?space(m.margin/1.2)+"|520| Uskut.plnÿn°,kter† se nezapoü. do vÏpoütu koef.   |"+str(r520,10)+" |"
	?space(m.margin/1.2)+"|530| Celkem uskut.plÂ.osv.od danÿ bez n†roku na odpoü. |"+str(r530,10)+" |"
	?space(m.margin/1.2)+"|540| Uskut.plnÿn°,kter† se nezap.do vÏp.koef.z ˝.530   |"+str(r540,10)+" |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| Kr†cen°, vypo˝., £prava a vyrovn†n° danÿ  | koeficient| Zm.odpoütu|"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|550| Vypoüten† pomÿrn† ü†st odpoütu        |"+str(k550,10,4)+" |"+str(r550,10)+" |"
	?space(m.margin/1.2)+"|560| Vypo˝†d†n° odpoütu danÿ               |"+str(k560,10,4)+" |"+str(r560,10)+" |"
	?space(m.margin/1.2)+"|570| Èprava odpoütu danÿ                               |"+str(r570,10)+" |"
	?space(m.margin/1.2)+"|580| Vyrovn†n° odpoütu danÿ                            |"+str(r580,10)+" |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| VI. Vr†cen° danÿ                                      | DaÂ vÏstup|"
	?space(m.margin/1.2)+"|600| Vr†cen° danÿ                                      |"+str(r600,10)+" |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| VII. VÏpoüet daÂovÇ povinnosti            | Odpoü.danÿ| DaÂ vÏstup|"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|710| Vypo˝†d†n° danÿ na vÏstupu                        |"+str(r710,10)+" |"
	?space(m.margin/1.2)+"|730| DaÂ na vÏstupu                                    |"+str(r730,10)+" |"
	?space(m.margin/1.2)+"|750| Odpoüet danÿ                          |"+str(r750,10)+" |           |"
	?space(m.margin/1.2)+"|753| Vlastn° daÂov† povinnost (˝.730-750)              |"+str(r753,10)+" |"
	?space(m.margin/1.2)+"|750| NadmÿrnÏ odpoüet (˝.750-730)          |"+str(r754,10)+" |           |"
	?space(m.margin/1.2)+"|780| Zmÿna daÂ.pov.p˝i dodateünÇm daÂ.p˝iz. (730-750)  |"+str(r780,10)+" |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| VIII. DoplÂuj°c° £daje                                |  Hodnota  |"
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|   ≥ Hodnota p˝i zjedn.postupu u t˝°strannÇho obchodu"
	?space(m.margin/1.2)+"|810| Po˝°zen° zboß° prost˝edn° osobou                  |"+str(r810,10)+" |"
	?space(m.margin/1.2)+"|815| Dod†n° zboß° prost˝edn° osobou                    |"+str(r815,10)+" |"
	?space(m.margin/1.2)+repl('-',69)

**	Ukonüen° tisku
	SET ALTE ON
	??m.pica+chr(12)
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.2
	SET CONS ON

	IF !ask(-1,-1,"N","Export sestavy do HTML")
		RETURN
		ENDIF

** Export do HTML
	dp_typ="Fyzick†  "
	dp_fu=space(12)
	dp_dic=m.ggdic
	dp_mes=0
	dp_ctv=0
	dp_rok=m.ggrok
	dp_jme=space(10)
	dp_pri=space(21)
	dp_tit=space(4)
	dp_oj=space(37)
	dp_oj2=space(25)
	dp_doj=space(11)
	dp_adr=left(alltrim(m.gguli)+iif(!empty(m.gguli),", ","")+ ;
		alltrim(m.ggmes)+iif(!empty(m.ggmes),", ","")+ ;
		alltrim(m.ggpsc)+space(35),35)

	dp_stat1=space(37)
	dp_stat2=space(37)
	dp_eu="Ne "
	dp_ppr=space(25)
	dp_pjm=space(11)
	dp_post=space(37)
	dp_dat=date()
	dp_jo="  "

	IF file(m.ggf+"dph.MEM")
		REST FROM (m.ggf+"dph.MEM") ADDI
		ENDIF

	DO t-prizn2.spr

	IF m.esc
		esc=.F.
		RETURN
		ENDIF

	SAVE TO (m.ggf+"dph.MEM") ALL LIKE dp_*

** Vlastn° export

	SET CONS OFF
	SET PRINT TO "..\html\Dph12.js"
	SET PRINT ON
	
	?[function GetData()]
	?[  {document.write]
	?[    ("<xml id='Dph'>"+]
	?[     "<data>"+]
	?[       "<veta "+]
	?[         "finurad=']+chrtran(m.dp_fu,m.kodlat,m.kodwin)+[' "+]
	?[         "dic=']+m.dp_dic+[' "+]
	?[         "radne='X' "+]
	?[         "opravne=' ' "+]
	?[         "dodatecne=' ' "+]
	?[         "datDuv=' ' "+]
	?[         "mesic=']+iif(m.dp_mes>0,str(m.dp_mes,2)," ")+[ ' "+]
	?[         "ctvrt=']+iif(m.dp_ctv>0,str(m.dp_ctv,1)," ")+[' "+]
	?[         "rok=']+strn(dp_rok)+[' "+]
	?[         "r1='X' "+]
	?[         "r2=' ' "+]
	?[         "r3=' ' "+]
	?[         "r4=' ' "+]
	?[         "dicZastupce=' ' "+]
	?[         "pOsoba1=']+chrtran(m.dp_oj,m.kodlat,m.kodwin)+[ ' "+]
	?[         "pOsoba2=']+chrtran(m.dp_oj2,m.kodlat,m.kodwin)+[ ' "+]
	?[         "pOsoba3=']+chrtran(m.dp_doj,m.kodlat,m.kodwin)+[' "+]
	?[         "fPrijm=']+iif(m.dp_typ="Fyzick†  ",chrtran(m.dp_pri,m.kodlat,m.kodwin)," ")+[' "+]
	?[         "fJmeno=']+iif(m.dp_typ="Fyzick†  ",chrtran(m.dp_jme,m.kodlat,m.kodwin)," ")+[' "+]
	?[         "fTitul=']+chrtran(m.dp_tit,m.kodlat,m.kodwin)+[ ' "+]
	?[         "adrObec=']+chrtran(alltrim(m.ggmes),m.kodlat,m.kodwin)+[' "+]
	?[         "adrPsc=']+m.ggpsc+[ ' "+]
	?[         "adrTlf=' ' "+]
	?[         "adrUlice=']+chrtran(m.gguli,m.kodlat,m.kodwin)+[ ' "+]
	?[         "adrFax=' ' "+]
	?[         "adrOkres=' ' "+]
	?[         "adrStat=' ' "+]
	?[         "cinnost1=']+chrtran(m.dp_stat1,m.kodlat,m.kodwin)+[ ' "+]
	?[         "cinnost2=']+chrtran(m.dp_stat2,m.kodlat,m.kodwin)+[ ' "+]
	?[         "neDPH=' ' "+]
	?
	?[         "r210z=']+tisktis(m.z210)+[' "+]
	?[         "r210d=']+tisktis(m.r210)+[' "+]
	?[         "r215z=']+tisktis(m.z215)+[' "+]
	?[         "r215d=']+tisktis(m.r215)+[' "+]
	?[         "r220z=']+tisktis(m.z220)+[' "+]
	?[         "r220d=']+tisktis(m.r220)+[' "+]
	?[         "r225z=']+tisktis(m.z225)+[' "+]
	?[         "r225d=']+tisktis(m.r225)+[' "+]
	?[         "r230z=']+tisktis(m.z230)+[' "+]
	?[         "r230d=']+tisktis(m.r230)+[' "+]
	?[         "r235z=']+tisktis(m.z235)+[' "+]
	?[         "r235d=']+tisktis(m.r235)+[' "+]
	?[         "r240z=']+tisktis(m.z240)+[' "+]
	?[         "r240d=']+tisktis(m.r240)+[' "+]
	?[         "r245z=']+tisktis(m.z245)+[' "+]
	?[         "r245d=']+tisktis(m.r245)+[' "+]
	?[         "r250z=']+tisktis(m.z250)+[' "+]
	?[         "r250d=']+tisktis(m.r250)+[' "+]
	?[         "r255z=']+tisktis(m.z255)+[' "+]
	?[         "r255d=']+tisktis(m.r255)+[' "+]
	?[         "r260z=']+tisktis(m.z260)+[' "+]
	?[         "r260d=']+tisktis(m.r260)+[' "+]
	?[         "r265z=']+tisktis(m.z265)+[' "+]
	?[         "r265d=']+tisktis(m.r265)+[' "+]
	?[         "r270z=']+tisktis(m.z270)+[' "+]
	?[         "r270d=']+tisktis(m.r270)+[' "+]
	?[         "r275z=']+tisktis(m.z275)+[' "+]
	?[         "r275d=']+tisktis(m.r275)+[' "+]
	?[         "r310z=']+tisktis(m.z310)+[' "+]
	?[         "r310p=']+tisktis(m.r310)+[' "+]
	?[         "r310k=']+tisktis(m.k310)+[' "+]
	?[         "r315z=']+tisktis(m.z315)+[' "+]
	?[         "r315p=']+tisktis(m.r315)+[' "+]
	?[         "r315k=']+tisktis(m.k315)+[' "+]
	?[         "r320z=']+tisktis(m.z320)+[' "+]
	?[         "r320p=']+tisktis(m.r320)+[' "+]
	?[         "r320k=']+tisktis(m.k320)+[' "+]
	?[         "r325z=']+tisktis(m.z325)+[' "+]
	?[         "r325p=']+tisktis(m.r325)+[' "+]
	?[         "r325k=']+tisktis(m.k325)+[' "+]
	?[         "r330z=']+tisktis(m.z330)+[' "+]
	?[         "r330p=']+tisktis(m.r330)+[' "+]
	?[         "r330k=']+tisktis(m.k330)+[' "+]
	?[         "r335z=']+tisktis(m.z335)+[' "+]
	?[         "r335p=']+tisktis(m.r335)+[' "+]
	?[         "r335k=']+tisktis(m.k335)+[' "+]
	?[         "r340z=']+tisktis(m.z340)+[' "+]
	?[         "r340p=']+tisktis(m.r340)+[' "+]
	?[         "r340k=']+tisktis(m.k340)+[' "+]
	?[         "r345z=']+tisktis(m.z345)+[' "+]
	?[         "r345p=']+tisktis(m.r345)+[' "+]
	?[         "r345k=']+tisktis(m.k345)+[' "+]
	?[         "r350z=']+tisktis(m.z350)+[' "+]
	?[         "r350p=']+tisktis(m.r350)+[' "+]
	?[         "r350k=']+tisktis(m.k350)+[' "+]
	?[         "r355z=']+tisktis(m.z355)+[' "+]
	?[         "r355p=']+tisktis(m.r355)+[' "+]
	?[         "r355k=']+tisktis(m.k355)+[' "+]
	?[         "r360z=']+tisktis(m.z360)+[' "+]
	?[         "r360p=']+tisktis(m.r360)+[' "+]
	?[         "r360k=']+tisktis(m.k360)+[' "+]
	?[         "r365z=']+tisktis(m.z365)+[' "+]
	?[         "r365p=']+tisktis(m.r365)+[' "+]
	?[         "r365k=']+tisktis(m.k365)+[' "+]
	?[         "r370z=']+tisktis(m.z370)+[' "+]
	?[         "r370p=']+tisktis(m.r370)+[' "+]
	?[         "r370k=']+tisktis(m.k370)+[' "+]
	?[         "r380=']+tisktis(r380)+[' "+]
	?[         "r390=']+tisktis(r390)+[' "+]
	?
	?[         "r410=']+tisktis(r410)+[' "+]
	?[         "r420=']+tisktis(r420)+[' "+]
	?[         "r425=']+tisktis(r425)+[' "+]
	?[         "r430=']+tisktis(r430)+[' "+]
	?[         "r440=']+tisktis(r440)+[' "+]
	?[         "r510=']+tisktis(r510)+[' "+]
	?[         "r520=']+tisktis(r520)+[' "+]
	?[         "r530=']+tisktis(r530)+[' "+]
	?[         "r540=']+tisktis(r540)+[' "+]
	?[         "r550k=']+tisktis(m.k550)+[' "+]
	?[         "r550=']+tisktis(r550)+[' "+]
	?[         "r560k=']+tisktis(m.k560)+[' "+]
	?[         "r560=']+tisktis(r560)+[' "+]
	?[         "r570=']+tisktis(r570)+[' "+]
	?[         "r580=']+tisktis(r580)+[' "+]
	?[         "r600=']+tisktis(r600)+[' "+]
	?[         "r710=']+tisktis(r710)+[' "+]
	?[         "r730=']+tisktis(r730)+[' "+]
	?[         "r750=']+tisktis(r750)+[' "+]
	?[         "r753=']+tisktis(r753)+[' "+]
	?[         "r754=']+tisktis(r754)+[' "+]
	?[         "r780=']+tisktis(r780)+[' "+]
	?[         "r810=']+tisktis(r810)+[' "+]
	?[         "r815=']+tisktis(r815)+[' "+]
	?
	?[         "oPrijm=']+chrtran(m.dp_ppr,m.kodlat,m.kodwin)+[' "+]
	?[         "oJmeno=']+chrtran(m.dp_pjm,m.kodlat,m.kodwin)+[' "+]
	?[         "postav=']+chrtran(m.dp_post,m.kodlat,m.kodwin)+[' "+]
	?[         "datum=']+right(dtos(m.dp_dat),2)+ ;
				subst(dtos(m.dp_dat),5,2)+left(dtos(m.dp_dat),4)+[' "+]
	?[         "sestavil=' ' "+]
	?[         "tlf=' ' "+]
	?[         "pf=']+m.dp_dic+[' />"+]
	?[     "</data>"+]
	?[     "</xml>");}]
	SET PRINT OFF
	SET PRINT TO

	SET PRINT TO "..\html\Dataakt.js"
	SET PRINT ON
	?[var aktDph=        "]+dtoc(date())+"  "+time()+[";]
	SET PRINT OFF
	SET PRINT TO

	SET CONS ON
RETURN

**	Soupis daÂovÏch dokladÖ od 1.5.2004
PROCEDURE PRE_SOU 

	SELE denik
	SET FILT TO
	lmarg=m.margin
	DO t-soupis.spr

	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF

	SET ESCA ON
	ON ESCA DO stoptisk
	m.tskf=ggf+"soupis.prt"
	SET CONS OFF

	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

	DO wpr
	SET ORDER TO dph

	REPL dph WITH iif(md=m.ggdph OR dal=m.ggdph,iif(md=m.ggdph,mda,daa),dph) ;
		 ALL FOR betw(dat,m.tdto,m.tdtd)

	SET TALK ON

	SET FILT TO (!del AND betw(dat,m.tdto,m.tdtd) AND (denik.dph>1000 OR betw(denik.dph,10,20)))

	SELE osn
	SET ORDER TO uct

	SELE denik
	SET RELA TO "343"+str(dph,4) INTO osn
	SET TALK OFF

	ff=.F.
	page=1
	PRIV	   ll, pp, scas, szad, sccas, sczad, ddph, ccis, sdph
	STORE 0 TO ll, pp, scas, szad, sccas, sczad, ddph

	PRIV _cis,_typ,_dph,_dphdph
	fdph=.F.

	COUNT ALL TO rc
	headprog="hsoup"

	DO head
	?m.nlq+m.cnoff+space(m.margin/1.6)+alignc("SOUPIS DA’OVÌCH DOKLADﬁ"+space(43))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	?m.cnoff+m.draft
	ll=m.ll+4
	GO TOP

	ccis=-1
	sdph=osn.SazbaDPH
	_cis=cis
	_typ=typ
	_dph=dph
	_dphdph=osn.SazbaDPH

	SCAN ALL

		IF m.ddph<>dph		&&	VÏpisy jednotlivÏch hlaviüek ülenÿn°

			IF m.ddph<>0
				=tstpgend(4)
				ll=m.ll+4
				IF m.sdph=0
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
					?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'+m.cnoff
				  ELSE
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
					?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'+m.cnoff
				ENDIF
				?
				STORE 0 TO scas,szad,sccas,sczad, ccis
			ENDIF

			sdph=osn.SazbaDPH
			ddph=denik.dph
			
			=tstpgend(4)
			m.ll=m.ll+4
			?space(m.margin/1.6)+"Typ DPH "+strn(m.ddph)+" - "+osn.txt+m.cnon
			IF m.sdph=0
				?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
				?space(m.lmarg)+'|  Datum   |  Doklad   |    ¨†stka    |'+'  T e x t'+space(33)+'|'
				?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
			  ELSE
				?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
				?space(m.lmarg)+'|  Datum   |  Doklad   |  Z†kl. danÿ  | Sazba  |    D a Â     |'+'  T e x t'+space(34)+'|'
				?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
			ENDIF
		ENDIF

		=tstpgend(2)

		ll=m.ll+1
		ccis=m.ccis+1

		IF !(m._typ=typ AND m._cis=cis)
			IF m.ccis>1
				IF m.sdph=0
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+str(sccas,12,2)+' | '+left('S: '+strn(_typ,3)+'/'+strn(_cis,5)+space(40),40)+' | '
				  ELSE
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+;
						str(sczad,12,2)+' | '+str(m._dphdph,5)+'% | '+str(sccas,12,2)+' | '+;
						iif(betw(sccas,sczad*m._dphdph/100-5,sczad*m._dphdph/100+5),' ','*')+left('S: '+strn(_typ,3)+'/'+strn(_cis,5)+space(40),40)+' | '
					ENDIF
				ll=m.ll+1
				ENDIF
			ccis=0
			sccas=0
			sczad=0
		ENDIF

		_cis=cis
		_typ=typ
		_dph=dph
		_dphdph=osn.SazbaDPH

		IF m.sdph=0
			?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+str(cas,12,2)+' | '+txt+' | '
			scas=m.scas+cas
			szad=0
			sccas=m.sccas+cas
		  ELSE
			?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+;
				str(zad,12,2)+' | '+str(osn.SazbaDPH,5)+'% | '+str(cas,12,2)+' | '+;
				iif(betw(cas,zad*osn.SazbaDPH/100-2,zad*osn.SazbaDPH/100+2),' ','*')+txt+' | '
			scas=m.scas+cas
			szad=m.szad+zad
			sccas=m.sccas+cas
			sczad=m.sczad+zad
		ENDIF

		pp=m.pp+1
		=proc(rc,pp)
	ENDSCAN

	=tstpgend(3)
	IF m.sdph=0
		?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
		?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
		?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'+m.cnoff
	  ELSE
		?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
		?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
		?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'+m.cnoff
	ENDIF

	SELE denik
	SET ALTE ON
	??m.cnoff+chr(12)

	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA

	DO tisk WITH m.margin
	SET CONS OFF
	SET FILT TO !del
	SET ORDER TO
	GO BOTT
RETURN



**** Vyhodit
	SCAN ALL

		IF m.ddph<>dph		&&	VÏpisy jednotlivÏch hlaviüek ülenÿn°

			IF m.ddph<>0
				=tstpgend(4)
				ll=m.ll+4
				IF m.sdph=0
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
					?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'+m.cnoff
				  ELSE
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
					?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'+m.cnoff
				ENDIF
				?
				STORE 0 TO scas,szad,sccas,sczad, ccis
			ENDIF

			=seek("343"+str(m.ddph,4),"osn")
			sdph=osn.SazbaDPH
			ddph=denik.dph
			
			=tstpgend(4)
			m.ll=m.ll+4
			?space(m.margin/1.6)+"¸†dek "+strn(m.ddph)+" - "+osn.txt+m.cnon
			IF m.sdph=0
				?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
				?space(m.lmarg)+'|  Datum   |  Doklad   |    ¨†stka    |'+'  T e x t'+space(33)+'|'
				?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
			  ELSE
				?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
				?space(m.lmarg)+'|  Datum   |  Doklad   |  Z†kl. danÿ  | Sazba  |    D a Â     |'+'  T e x t'+space(34)+'|'
				?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
			ENDIF
		ENDIF

		=tstpgend(2)

		ll=m.ll+1
		ccis=m.ccis+1

		IF !(m._typ=typ AND m._cis=cis)
			IF m.ccis>1
				IF m.sdph=0
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+str(sccas,12,2)+' | '+left('S: '+strn(_typ,3)+'/'+strn(_cis,5)+space(40),40)+' | '
				  ELSE
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+;
						str(sczad,12,2)+' | '+str(m._dphdph,5)+'% | '+str(sccas,12,2)+' | '+;
						iif(betw(sccas,sczad*m._dphdph/100-5,sczad*m._dphdph/100+5),' ','*')+left('S: '+strn(_typ,3)+'/'+strn(_cis,5)+space(40),40)+' | '
					ENDIF
				ll=m.ll+1
				ENDIF
			ccis=0
			sccas=0
			sczad=0
		ENDIF

		_cis=cis
		_typ=typ
		_dph=dph
		_dphdph=osn.SazbaDPH

		IF m.sdph=0
			?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+str(cas,12,2)+' | '+txt+' | '
			scas=m.scas+cas
			szad=0
			sccas=m.sccas+cas
		  ELSE
			?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+;
				str(zad,12,2)+' | '+str(osn.SazbaDPH,5)+'% | '+str(cas,12,2)+' | '+;
				iif(betw(cas,zad*osn.SazbaDPH/100-2,zad*osn.SazbaDPH/100+2),' ','*')+txt+' | '
			scas=m.scas+cas
			szad=m.szad+zad
			sccas=m.sccas+cas
			sczad=m.sczad+zad
		ENDIF

		pp=m.pp+1
		=proc(rc,pp)
	ENDSCAN

	=tstpgend(3)
	IF m.sdph=0
		?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
		?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
		?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'+m.cnoff
	  ELSE
		?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
		?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
		?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'+m.cnoff
	ENDIF

**	Sestava nedaÂovÏch vÏnosÖ a n†kladÖ
PROCEDURE pre_neda
	lmarg=m.margin
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	app=.F.
	title=" Sestava nedaÂovÏch n†kladÖ a vÏnosÖ "
	DO t-nedan.spr
	IF m.esc
		esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	SET TALK ON
	DO wpr
	fi='tdto<=dat AND tdtd>=dat'+iif(m.tstr<>0,'.and.str=m.tstr','')
	SELE osn
	SET ORDER TO uct
	SET FILT TO
	SET TALK ON
	REPL ALL cmd WITH 0, cdal WITH 0 FOR betw(uct,500,699)
	SELE denik
	SET RELA TO str(md,3)+str(mda,4) INTO osn
	REPL ALL osn.cmd WITH osn.cmd+denik.cas FOR &fi AND betw(md,500,699)
	SET RELA TO str(dal,3)+str(daa,4) INTO osn
	REPL ALL osn.cdal WITH osn.cdal+denik.cas FOR &fi AND betw(dal,500,699)
	SET RELA TO
	DO wpr
	SET CONS OFF
	SET TALK OFF
	SELE osn
	SET ORDER TO uct
	m.tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll
	headprog="h_nedan"
	DO head
	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("NEDA’OVê È¨TY"+space(52))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	IF tstr!=0
		?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
		ll=ll+1
		ENDIF
	?m.cnon+m.draft
	STORE 0 TO s1,s2,s605,s505,s667,s567,s68,s58
	?space(m.margin)+repl('=',52)
	?space(m.margin)+'| 6xx            ¨†stka   | 5xx            ¨†stka  |'
	?space(m.margin)+repl('=',52)
	ll=ll+8
	SET FILT TO betw(uct,500,699) AND cmd-cdal!=0 AND dan='N'
	GO TOP
	ll=ll+6
	DO WHILE !eof()
		=tstpgend(1)
		IF uct>=600
			?space(m.margin)+'| '+right('00'+ltrim(str(uct)),3)+str(ana,5)+;
				str(cdal-cmd,14,2)+'  | '+space(22)+' |  - '+osn.txt
			s605=m.s605+cdal-cmd
			s1=m.s1+cdal-cmd
		ELSE
			?space(m.margin)+'| '+space(22)+'  | '+;
				right('00'+ltrim(str(uct)),3)+;
				str(ana,5)+str(cmd-cdal,14,2)+' |  - '+osn.txt
			s505=m.s505+cmd-cdal
			s2=s2+cmd-cdal
			ENDIF
		SKIP 1
		ll=ll+1
		ENDDO
	=tstpgend(4)
	?space(m.margin)+'|'+repl('-',50)+'|'
	?space(m.margin)+'| 6xx     '+str(s605,14,2)+'  | 5xx     '+str(s505,14,2)+' |'
	?space(m.margin)+'| '+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(5)+;
		'6xx - 5xx ='+str(s605-s505,13,2)+' |'
	?space(m.margin)+repl('=',52)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	ll=ll+4
	SET FILT TO
	SET CONS ON
	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
RETURN
PROCEDURE h_nedan
	?m.cnon+space(m.lmarg)+"NedaÂovÇ £üty"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(83), ;
			space(25)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(21))+ ;
		"Strana:",m.page PICT '@L 9999'
	?
	ll=2
RETURN
**	P˝edvaha
PROCEDURE pre_pre
	PRIV m.fi
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	lmarg=m.margin
	DO t-predva.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF

	SET ESCA ON
	ON ESCA DO stoptisk

	tiz=seco()
	fi='!del'+iif(m.ttyp<>0,' AND typ=m.ttyp','')+ ;
		iif(m.tstr<>0,' AND str=m.tstr','')+ ;
		' AND dat<=m.tdtd'

	DO wpr
	SELE 0
	CREATE CURS predvah (ucet C(8), cmd1 N(12,2), cdal1 N(12,2), ;
		cmdo N(12,2), cdalo N(12,2), cmdc N(12,2), cdalc N(12,2), fl L)
	INDEX ON ucet TAG ucet
	
	SET ORDER TO ucet
	SELE denik
	IF type('m.masku')='U'
		m.masku='???'
		m.maska='????'
		ENDIF
	SET FILT TO like(masku,str(md,3)) AND like(maska,str(mda,4)) AND &fi

	IF m.yana='Ano'
		SET RELA TO str(md,3)+str(mda,5) INTO predvah
	  ELSE
		SET RELA TO str(md,3) INTO predvah
		ENDIF

	rec=recc()*2
	pp=0

	GO TOP
	DO WHILE !eof()
		pp=pp+1
		IF eof('predvah')
			IF m.yana='Ano'
				INSERT INTO predvah (ucet, fl) ;
					VALUES (str(denik.md,3)+str(denik.mda,5),.F.)
			  ELSE
				INSERT INTO predvah (ucet, fl) VALUES (str(denik.md,3),.F.)
				ENDIF
			=proc(m.rec,pp)
			ENDIF
		IF dal=0 OR dal=701
			REPL predvah.cmd1 WITH predvah.cmd1+denik.cas
		  ELSE
			REPL predvah.cmdc WITH predvah.cmdc+denik.cas
			IF betw(denik.dat,m.tdto,m.tdtd)
				REPL predvah.cmdo WITH predvah.cmdo+denik.cas
				ENDIF
			ENDIF
		IF betw(denik.dat,m.tdto,m.tdtd)
			REPL predvah.fl WITH .T.
			ENDIF
		SKIP 1
		ENDDO

	SET FILT TO like(masku,str(dal,3)) AND like(maska,str(daa,4)) AND &fi

	IF m.yana='Ano'
		SET RELA TO str(dal,3)+str(daa,5) INTO predvah
	  ELSE
		SET RELA TO str(dal,3) INTO predvah
		ENDIF

	GO TOP
	DO WHILE !eof()
		pp=pp+1
		IF eof('predvah')
			IF m.yana='Ano'
				INSERT INTO predvah (ucet,fl) ;
					VALUES (str(denik.dal,3)+str(denik.daa,5),.F.)
			  ELSE
				INSERT INTO predvah (ucet, fl) VALUES (str(denik.dal,3),.F.)
				ENDIF
			=proc(m.rec,pp)
			ENDIF
		IF md=0 OR md=701
			REPL predvah.cdal1 WITH predvah.cdal1+denik.cas
		  ELSE
			REPL predvah.cdalc WITH predvah.cdalc+denik.cas
			IF betw(denik.dat,m.tdto,m.tdtd)
				REPL predvah.cdalo WITH predvah.cdalo+denik.cas
				ENDIF
			ENDIF
		IF betw(denik.dat,m.tdto,m.tdtd)
			REPL predvah.fl WITH .T.
			ENDIF
		SKIP 1
		ENDDO
	=proc(m.rec,m.rec)

	SELE predvah
	rec=recc()
	tskf=ggf+"predvaha.prt"
	headprog="h_prdv"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	ll=0
	SET CONS OFF
	SET TALK OFF
	DO head			&&	Prvn° str†nka

	STORE 0 TO pp,scmdc,scdalc,scmdo,scdalo,scmd1,scdal1,skmd,skdal
	SCAN ALL 				&& FOR fl
		=proc(m.rec,pp)
		pp=m.pp+1
		=tstpgend(2)
		STORE space(12) TO mdo,dalo
		smda=cmdc+cmd1
		sdala=cdalc+cdal1
		IF m.smda-m.sdala>0
			mdo=str(m.smda-m.sdala,12,2)
			skmd=skmd+(m.smda-m.sdala)
		  ELSE
			dalo=str(m.sdala-m.smda,12,2)
			skdal=skdal+(m.sdala-m.smda)
			ENDIF
		?space(m.lmarg)+'| '+ucet+' |'+strbl(cmd1,12,2)+'|'+strbl(cdal1,12,2)+'|'+ ;
			strbl(cmdo,12,2)+'|'+strbl(cdalo,12,2)+'|'+ ;
			mdo+'|'+dalo+'||'+ ;
			strbl(cmdc,12,2)+'|'+strbl(cdalc,12,2)+'|'

		ll=ll+1
		scmdc=scmdc+cmdc
		scmdo=scmdo+cmdo
		scmd1=scmd1+cmd1
		scdalc=scdalc+cdalc
		scdalo=scdalo+cdalo
		scdal1=scdal1+cdal1
		ENDSCAN

	=tstpgend(4)
	?space(m.lmarg)+repl('=',117)
	?space(m.lmarg)+'| Celkem:  |'+str(scmd1,12,2)+'|'+str(scdal1,12,2)+'|'+ ;
		str(scmdo,12,2)+'|'+str(scdalo,12,2)+'|'+ ;
		str(skmd,12,2)+'|'+str(skdal,12,2)+'||'+ ;
		str(scmdc,12,2)+'|'+str(scdalc,12,2)+'|'

	?space(m.lmarg)+'| Celkem:  |'+ ;
		iif(scmd1-scdal1>0,str(scmd1-scdal1,12,2)+'|'+space(12),space(12)+'|'+str(abs(scmd1-scdal1),12,2))+'|'+ ;
		iif(scmdo-scdalo>0,str(scmdo-scdalo,12,2)+'|'+space(12),space(12)+'|'+str(abs(scmdo-scdalo),12,2))+'|'+ ;
		iif(skmd -skdal >0,str(skmd -skdal ,12,2)+'|'+space(12),space(12)+'|'+str(abs(skmd -skdal ),12,2))+'||'+ ;
		iif(scmdc-scdalc>0,str(scmdc-scdalc,12,2)+'|'+space(12),space(12)+'|'+str(abs(scmdc-scdalc),12,2))+'|'

	?space(m.lmarg)+repl('=',117)
	SET ALTE ON
	?m.cnoff+chr(12)
	SET ALTE TO
	USE
	SELE denik
	SET CONS ON
	SET FILT TO !del
	SET ESCA OFF
	ON ESCA
	WAIT CLEAR
	tik=seco()
	WAIT WIND NOWA "Poüet str†nek:"+str(m.page-1,5)+"   ¨as zpracov†n°: "+str(m.tik-m.tiz,7,3)+" s"
	DO tisk WITH m.margin/1.6
RETURN
PROCEDURE h_prdv
	?m.cnon+space(m.lmarg)+"P˝edvaha"+space(96)+"Strana:",m.page PICT '@L 9999'
	?
	ll=2
	IF m.page=1
		?m.nlq+m.cnoff
		?space(m.margin/1.6)+alignc("P¸EDVAHA"+space(58))
		?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
		?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
		IF tstr!=0
			?space(m.margin/1.6)+alignc("St˝edisko: "+lookup(str.txt,m.tstr,str.cis,'cis')+space(25))
			ll=ll+1
			ENDIF
		?m.cnon
		ll=ll+5
		ENDIF
	?space(m.lmarg)+"+----------+-------------------------+-------------------------+-------------------------++-------------------------+"
	?space(m.lmarg)+"|  ¨°slo   |      Poü†teün° stav     |      Obraty za obdob°   |     KoneünÏ stav £ütu   ||      CelkovÇ obraty     |"
	?space(m.lmarg)+"|  £ütu    |      MD           DAL   |      MD           DAL   |      MD           DAL   ||      MD           DAL   |"
	?space(m.lmarg)+"+----------+-------------------------+-------------------------+-------------------------++-------------------------+"
	ll=ll+4
RETURN
**	Tisk denn°ch zÖstatkÖ £ütÖ
PROCEDURE pre_dz
	DO t-dz.spr
	IF m.esc
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	DO wpr
	CREATE CURSOR se1 (dat D, cas N(12,2))
	INDEX ON dtos(dat) TAG dat
	SET ORDER TO dat

	SELE osn
	SET ORDER TO uct

	SELE denik
	SET ORDER TO dat
	PRIV cc
	SCAN ALL FOR !del AND (md=m.tuct AND mda=m.tana) OR ;
				(dal=m.tuct AND daa=m.tana)
		IF md=m.tuct AND mda=m.tana
			cc=iif(lookup(osn.roz,str(denik.md,3)+str(denik.mda,4),osn.uct,'uct')='A' OR ;
				betw(md,500,599), cas,cas*-1)
		  ELSE
			cc=iif(lookup(osn.roz,str(denik.dal,3)+str(denik.daa,4),osn.uct,'uct')='P' OR ;
				betw(dal,600,699), cas,cas*-1)
			ENDIF
		SELE se1
		SEEK dtos(denik.dat)
		IF found()
			REPL cas WITH cas+m.cc
		  ELSE
			INSERT INTO se1 (dat, cas) VALUES ;
				(denik.dat, m.cc)
			ENDIF
		SELE denik
		ENDSCAN

	tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	SET CONS OFF
	SET TALK OFF
	STORE 0 TO pp, ll
	lmarg=10
	?m.cnoff
	?space(m.margin/1.6+10)+"Denn° zÖstatky pro £üet "+str(m.tuct,3)+"."+str(m.tana,4)
	?m.draft
	?space(m.lmarg)+"+----------+----------------+----------------+"
	?space(m.lmarg)+"|  Datum   |     Pohyb      |    ZÖstatek    |"
	?space(m.lmarg)+"+----------+----------------+----------------+"
	SELE se1
	SET ORDER TO dat
	SCAN ALL
		pp=m.pp+cas
		?space(m.lmarg)+"| "+dtoc(dat)+" | "+str(cas,14,2)+" | "+str(m.pp,14,2)+" | "
		ENDSCAN
	?space(m.lmarg)+"+----------+----------------+----------------+"+m.cnoff+chr(12)
	SET ALTE TO
	USE
	SET CONS ON
	SELE denik
	SET FILT TO !del
	SET ESCA OFF
	ON ESCA
	WAIT CLEAR
	DO tisk WITH m.lmarg
RETURN
**	Propl†cen° dokladÖ
PROCEDURE pre_prop
	PRIV ucet, struct, souct, tol
	struct='MD '
	tol=1
	souct='Ne '
	ucet=311
	DO propl.spr
	IF m.esc
		RETURN
		ENDIF
	CREA CURS cu1 (dat D, typ N(3), cis N(5), cas N(12,2))
	INDEX ON str(cu1.typ,3)+str(cu1.cis,5)+dtos(cu1.dat) TAG typ
	SET ORDER TO typ
	SELE denik
	SET FILT TO !del
	SET ORDER TO typ
	DO wpr
	GO TOP
	SCAN ALL FOR typ=m.ttyp AND (md=m.ucet OR dal=m.ucet)
		IF &struct = m.ucet
			INSERT INTO cu1 VALUES (denik.dat, denik.typ, denik.cis, denik.cas)
		  ELSE
			INSERT INTO cu1 VALUES (denik.dat, denik.typ, denik.cis, denik.cas*-1)
			ENDIF
		ENDSCAN
	SET ORDER TO
	GO TOP
	SCAN ALL FOR rtyp=m.ttyp AND (md=m.ucet OR dal=m.ucet)
		IF &struct = m.ucet
			INSERT INTO cu1 VALUES (denik.dat, denik.rtyp, denik.rcis, denik.cas)
		  ELSE
			INSERT INTO cu1 VALUES (denik.dat, denik.rtyp, denik.rcis, denik.cas*-1)
			ENDIF
		ENDSCAN
	SELE cu1
	SET TALK ON
	SELECT cu1.typ, cu1.cis, sum(cu1.cas) AS scas, ;
			str(cu1.typ,3)+str(cu1.cis,5) AS gr ;
		FROM cu1 INTO CURSOR se1 ;
		GROUP BY gr ORDER BY gr
	SET CONS OFF
	tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	SET TALK OFF
	DO wpr
	IF m.souct='Ano'
		SELE se1
		SCAN ALL FOR scas<-1*m.tol OR scas>tol
			WAIT WIND NOWA str(se1.cis)
			?space(m.margin), se1.typ, se1.cis, se1.scas
			ENDSCAN
	  ELSE
		SELE cu1
		PRIV dcis, dsuma
		dcis=-1
		dsuma=0
		SCAN ALL FOR lookup(se1.scas,str(cu1.typ,3)+str(cu1.cis,5),se1.gr,'GR') < -1*m.tol OR ;
				lookup(se1.scas,str(cu1.typ,3)+str(cu1.cis,5),se1.gr,'GR') > m.tol
			WAIT WIND NOWA str(cu1.cis)
			IF dcis!=cu1.cis
				IF dcis!=-1
					?space(m.margin), "Celkem: ", space(8), m.dsuma
					?
					ENDIF
				dcis=cu1.cis
				ENDIF
			?space(m.margin), cu1.dat, cu1.typ, cu1.cis, cu1.cas
			dsuma=lookup(se1.scas,str(cu1.typ,3)+str(cu1.cis,5),se1.gr,'GR')
			ENDSCAN
		IF dcis!=-1
			?space(m.margin), "Celkem: ", space(8), m.dsuma
			?
			ENDIF
		ENDIF
	SET ALTE TO
	SELE cu1
	USE
	SELE se1
	USE
	SELE denik
	DO tisk WITH m.margin
	SET CONS ON
	WAIT CLEAR
RETURN
**	Tisk z†znamÖ s otazn°kem
PROCEDURE pre_otaz
	PRIV fi
	SELE denik
	SET ORDER TO dat
	lmarg=m.margin
	SET FILT TO !del AND at('?',denik.txt)>0
	SET CONS OFF
	tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	DO wpr
	ff=.F.
	page=1
	STORE 0 TO ll,pp
	COUNT ALL TO rc
	headprog="h_otaz"
	DO head
	SET ORDER TO dat
	GO TOP
	mes=month(dat)
	DO WHILE !eof()
		IF month(dat)<>m.mes
			=tstpgend(1)
			?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
			ll=m.ll+1
			mes=month(dat)
			ENDIF
		?space(m.lmarg)+"| "+dtoc(dat)+" | "+str(typ,3)+str(cis,6)+" |",;
			md PICT '@L 999', mda PICT '@Z 9999','|',;
			dal PICT '@L 999', daa PICT '@Z 9999','|',;
			str(cas,12,2),"| "+txt+" | "+str(str,3)+" |"+str(dph,4)+" |"
		ll=ll+1
		=tstpgend(1)
		SKIP 1
		pp=m.pp+1
		=proc(rc,pp)
		ENDDO
	?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
	SET ALTE ON
	??m.cnoff+chr(12)
	ll=m.ll+1
	WAIT CLEAR
	SET ALTE TO
	DO tisk WITH m.margin
	SET CONS ON
	SET FILT TO !del
	SET TALK OFF
	SET ORDER TO
RETURN
PROCEDURE h_otaz
	ll=6
	?m.cnon+space(m.lmarg)+"Soupis daÂovÏch dokladÖ"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(75), ;
			space(16)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(17))+ ;
		"Strana:",m.page PICT '@L 9999'
	?
	?m.cnon+space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
	?space(m.lmarg)+"|          |  Doklad   |   Èüet   |  Èüet    |              |                                          |     |     |"
	?space(m.lmarg)+"|  Datum   | typ ü°s.  |  m† d†ti |  d a l   |  ¨†stka Kü   |  Text                                    | Str.| DPH |"
	?space(m.lmarg)+"+----------+-----------+----------+----------+--------------+------------------------------------------+-----+-----+"
RETURN
**	Tisk souütÖ po dokladech
PROCEDURE pre_ctyp
	PRIV m.fi
	SELE denik
	SET ORDER TO dat
	SET FILT TO !del
	lmarg=m.margin
	DO counttyp.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	m.fi=iif(m.ttyp<>0,'.and.typ=m.ttyp','')+ ;
		iif(m.tstr<>0,'.and.str=m.tstr','')

	SET TALK ON
	DO wpr
	SELECT de.dat, de.typ, de.cis, sum(de.cas) AS cas, count(*) AS cou,;
			str(de.typ,3)+str(de.cis,5) AS vv1 ;
		WHERE dtoc(dat,1) BETW dtoc(m.tdto,1) AND dtoc(m.tdtd,1);
			AND !del;
			&fi;
		FROM denik de INTO CURSOR cr1 ;
		GROUP BY vv1
	INDEX ON vv1 TAG vv1
	SET ORDER TO vv1
	SET TALK OFF
	SET CONS OFF
	tskf=ggf+"ekont.prt"
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	ff=.F.
	page=1
	STORE 0 TO ll,pp
	COUNT ALL TO rc
	headprog="h_coutyp"
	STORE 0 TO ss, pp, ftyp
	GO TOP
	DO WHILE !eof()
		IF m.ftyp<>typ
			DO head
			m.ftyp=typ
			ss=0
			ENDIF
		ss=ss+cas
		=tstpgend(2)
		?space(m.lmarg/1.6)+dtoc(dat)+str(typ,6)+'/'+str(cis,6)+;
			str(cas,15,2)+str(ss,15,2)+str(cou,10)
		ll=ll+1
		SKIP 1
		pp=m.pp+1
		=proc(rc,pp)
		ENDDO
	SET ALTE ON
	??chr(12)
	SET ALTE TO
	WAIT CLEAR
	SET ESCA OFF
	ON ESCA
	SET CONS ON
	SET TALK OFF
	DO tisk WITH m.margin/1.6
RETURN
PROCEDURE h_coutyp
	?m.cnon+space(m.lmarg)+"Souüty jednotlivÏch dokladÖ"+space(76)+"Strana:",m.page PICT '@L 9999'+m.cnoff
	?
	?space(m.lmarg/1.6)+" Datum     Typ/ ¨°slo      ‰ Doklad    ‰ Celkem typ     Poüet"
	?space(m.lmarg/1.6)+"-------------------------------------------------------------"
	ll=4
RETURN
**	ÈütovÏ rozvrh
PROCEDURE pre_ucr
	m.tskf=ggf+"ekont.prt"
	SET TALK OFF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	headprog="hrozvrh"
	DO wpr
	SET ESCA ON
	ON ESCA DO stoptisk
	ff=.F.
	lmarg=m.margin
	SELE osn
	SET ORDER TO uct
	SET FILT TO
	page=1
	pgbeg=0
	pgend=9999
	typtisk="Oboj°"
	ll=0
	DO head
	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("È¨TOVÌ ROZVRH"+space(53))
	?space(m.margin/1.6)+;
		alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	?m.draft
	?
	ll=ll+6
	rc=recc()
	aa=1
	GO TOP
	DO WHILE !eof()
		aa=aa+1
		IF ana=0
			IF jeuct(uct,0,.F.)
				=tstpgend(3)
				?
				?space(m.margin/1.6)+right('000'+ltrim(str(uct)),3)+'     - '+txt
				ll=ll+2
				ENDIF
		  ELSE
			IF jeuct(uct,ana,.T.)
				=proc(rc,aa)
				=tstpgend(2)
				?space(m.margin/1.6)+'   '+right('0000'+ltrim(str(ana)),4)+' - '+txt
				ll=ll+1
				ENDIF
			ENDIF
		SKIP 1
		ENDDO
	SET ALTE ON
	?chr(12)
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.6
	SET CONS ON
RETURN
PROCEDURE hrozvrh
	?m.cnon+space(m.lmarg)+"ÈütovÏ rozvrh"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(75), ;
			space(16)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(17))+ ;
		"Strana:",m.page PICT '@L 9999'+m.cnoff
	?
	ll=2
RETURN
**	Sestava poußitÏch typÖ dokladÖ
PROCEDURE PRE_TYPD
	m.tskf=ggf+"ekont.prt"
	SET TALK OFF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	SET ESCA ON
	ON ESCA DO stoptisk
	ff=.F.
	lmarg=m.margin

	SELE denik
	SET ORDER TO typ
	SET FILT TO
	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("PoußitÇ typy dokladÖ"+space(46))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	?m.draft
	?

	SELE typ
	SET ORDER TO cis
	SCAN ALL FOR seek(str(cis,3),"denik")
		?space(m.margin/1.6)+str(cis)+" - "+txt
		ENDSCAN

	SET ALTE ON
	?chr(12)
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.6
	SET CONS ON
RETURN
**	Rozdÿl° ü°slo mezerami na tis°ce Return=Char
PROCEDURE tisktis 
PARA c
PRIV r
	r=right(space(9)+strn(c),9)
RETURN left(r,3)+" "+substr(r,4,3)+" "+right(r,3)
**	Star† p˝izn†n° k DPH
PROCEDURE M_OLDDPH
	DEFINE POPUP old_dph FROM 9,40 RELATIVE SHADOW ;
		TITLE " Star† p˝izn†n° k DPH " COLOR SCHEME 4
	DEFINE BAR 22 OF old_dph PROMPT " DaÂovÇ \<p˝izn†n° - podklad do 30.4.2004" SKIP FOR m.ggpldph='Ne ';
		MESS "VÏpoüet daÂovÇho p˝izn†n° za zadanÇ obdob°."
	DEFINE BAR 20 OF old_dph PROMPT " Soupis \<daÂovÏch dokladÖ do 30.4.2004 " SKIP FOR m.ggpldph='Ne ';
		MESS "Soupis daÂovÏch dokladÖ rozdÿlenÏ podle druhÖ DPH."
	DEFINE BAR 59 OF old_dph PROMPT "\-"
	DEFINE BAR 32 OF old_dph PROMPT " DaÂovÇ p˝izn†n° - podklad do 2000" SKIP FOR m.ggpldph='Ne ';
		MESS "VÏpoüet daÂovÇho p˝izn†n° za zadanÇ obdob°."
	DEFINE BAR 30 OF old_dph PROMPT " Soupis daÂovÏch dokladÖ do 2000" SKIP FOR m.ggpldph='Ne ';
		MESS "Soupis daÂovÏch dokladÖ rozdÿlenÏ podle druhÖ DPH."
	DEFINE BAR 33 OF old_dph PROMPT " DaÂovÇ p˝izn†n° - tiskopis do 2000" SKIP FOR m.ggpldph='Ne ';
		MESS "VÏpoüet daÂovÇho p˝izn†n° s tiskem odeslatelnÇho tiskopisu."

	ON SELE POPU old_dph DO old_dph

	ACTI POPUP old_dph

	RELE POPU old_dph
RETURN
PROCEDURE old_dph 
	DO CASE
		CASE bar()=20
			DO pre_sou2
		CASE bar()=22
			DO pre_pri2
		CASE bar()=30
			DO pre_sou1
		CASE bar()=32
			DO pre_pri1
		CASE bar()=33
			DO pre_tis1
		ENDCASE
RETURN
**	Soupis daÂovÏch dokladÖ do 2004/04
PROCEDURE pre_sou2

	SELE denik
	SET FILT TO
	lmarg=m.margin
	DO t-soupis.spr

	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF

	SET ESCA ON
	ON ESCA DO stoptisk
	m.tskf=ggf+"soupis.prt"
	SET CONS OFF

	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

	DO wpr
	SET ORDER TO dph

	SET TALK ON

	SET FILT TO (!del AND betw(dat,m.tdto,m.tdtd) AND dph>200)

	SELE dph
	SET ORDER TO cis

	SELE osn
	SET ORDER TO uct

	SELE denik
	SET RELA TO dph INTO dph
	SET TALK OFF

	ff=.F.
	page=1
	PRIV	   ll, pp, scas, szad, sccas, sczad, ddph, ccis, sdph
	STORE 0 TO ll, pp, scas, szad, sccas, sczad, ddph

	PRIV _cis,_typ,_dph,_dphdph
	fdph=.F.

	COUNT ALL TO rc
	headprog="hsoup"

	DO head
	?m.nlq+m.cnoff+space(m.margin/1.6)+alignc("SOUPIS DA’OVÌCH DOKLADﬁ"+space(43))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	?m.cnoff+m.draft
	ll=m.ll+4
	GO TOP

	ccis=-1
	sdph=dph.dph
	_cis=cis
	_typ=typ
	_dph=dph
	_dphdph=dph.dph

	SCAN ALL

		IF m.ddph<>dph		&&	VÏpisy jednotlivÏch hlaviüek ülenÿn°
			IF m.ddph<>0
				=tstpgend(4)
				ll=m.ll+4
				IF m.sdph=0
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
					?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'+m.cnoff
				  ELSE
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
					?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'+m.cnoff
					ENDIF
				?
				STORE 0 TO scas,szad,sccas,sczad, ccis
				ENDIF

			sdph=dph.dph
			ddph=denik.dph
			
			=tstpgend(4)
			m.ll=m.ll+4
			=seek("343"+str(m.ddph,4),"osn")
			?space(m.margin/1.6)+"¸†dek "+strn(m.ddph)+" - "+osn.txt+m.cnon
			IF m.sdph=0
				?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
				?space(m.lmarg)+'|  Datum   |  Doklad   |    ¨†stka    |'+'  T e x t'+space(33)+'|'
				?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
			  ELSE
				?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
				?space(m.lmarg)+'|  Datum   |  Doklad   |  Z†kl. danÿ  | Sazba  |    D a Â     |'+'  T e x t'+space(34)+'|'
				?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
				ENDIF
			ENDIF

		=tstpgend(2)

		ll=m.ll+1
		ccis=m.ccis+1

		IF !(m._typ=typ AND m._cis=cis)
			IF m.ccis>1
				IF m.sdph=0
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+str(sccas,12,2)+' | '+left('S: '+strn(_typ,3)+'/'+strn(_cis,5)+space(40),40)+' | '
				  ELSE
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+;
						str(sczad,12,2)+' | '+str(m._dphdph,5)+'% | '+str(sccas,12,2)+' | '+;
						iif(betw(sccas,sczad*m._dphdph/100-5,sczad*m._dphdph/100+5),' ','*')+left('S: '+strn(_typ,3)+'/'+strn(_cis,5)+space(40),40)+' | '
					ENDIF
				ll=m.ll+1
				ENDIF
			ccis=0
			sccas=0
			sczad=0
			ENDIF

		_cis=cis
		_typ=typ
		_dph=dph
		_dphdph=dph.dph

		IF m.sdph=0
			?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+str(cas,12,2)+' | '+txt+' | '
			scas=m.scas+cas
			szad=0
			sccas=m.sccas+cas
		  ELSE
			?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+;
				str(zad,12,2)+' | '+str(dph.dph,5)+'% | '+str(cas,12,2)+' | '+;
				iif(betw(cas,zad*dph.dph/100-2,zad*dph.dph/100+2),' ','*')+txt+' | '
			scas=m.scas+cas
			szad=m.szad+zad
			sccas=m.sccas+cas
			sczad=m.sczad+zad
			ENDIF

		pp=m.pp+1
		=proc(rc,pp)
		ENDSCAN

	=tstpgend(3)
	IF m.sdph=0
		?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
		?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
		?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'+m.cnoff
	  ELSE
		?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
		?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
		?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'+m.cnoff
		ENDIF

	SELE denik
	SET ALTE ON
	??m.cnoff+chr(12)

	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA

	DO tisk WITH m.margin
	SET CONS OFF
	SET FILT TO !del
	SET ORDER TO
	GO BOTT
RETURN
**	Sestava daÂovÇho p˝izn†n° - P˝ehled do 2004/04
PROCEDURE pre_pri2
	lmarg=m.margin
	DO t-priz.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	SELE denik
	DO wpr
	SET TALK ON
	REPL dph WITH iif(md=m.ggdph OR dal=m.ggdph,iif(md=m.ggdph,mda,daa),dph) ;
		 ALL FOR betw(dat,m.tdto,m.tdtd)
	SELECT round(sum(den.cas),0) AS cas, den.dph, round(sum(den.zad),0) AS zad;
		WHERE den.dph>0 AND !del;
			AND dtoc(dat,1) BETW dtoc(m.tdto,1) AND dtoc(m.tdtd,1);
		FROM denik den INTO CURSOR cr1 ;
		GROUP BY dph

*HAVING (!del AND betw(dat,m.tdto,m.tdtd) AND dph>0) ;
	INDEX ON dph tag dph

	SET TALK OFF
	m.tskf=ggf+"ekont.prt"
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

**	P˝°prava a vÏpoüet promÿnnÏch pro tisk p˝izn†n°
	PRIV koe, koef, r204, r310, r340, r360, r651, r652, a
	a=0
	a=lookup(cr1.cas,410,cr1.dph,'dph')+lookup(cr1.cas,420,cr1.dph,'dph')+ ;
		lookup(cr1.cas,430,cr1.dph,'dph')+ ;
		lookup(cr1.zad,444,cr1.dph,'dph')+lookup(cr1.zad,445,cr1.dph,'dph')

	koe=round(a/(a+lookup(cr1.cas,400,cr1.dph,'dph')),4)

	DO CASE
		CASE m.koe>=0.95 
			koef=1
		CASE m.koe<0.05
			koef=0
		OTHE
			koef=m.koe
		ENDCASE

	r201=lookup(cr1.cas,201,cr1.dph,'dph')
	r203=lookup(cr1.cas,203,cr1.dph,'dph')
	r204=r203*koef
	r210=r201+r204

	r301=lookup(cr1.cas,301,cr1.dph,'dph')
	r302=lookup(cr1.cas,302,cr1.dph,'dph')
	r303=lookup(cr1.cas,303,cr1.dph,'dph')
	r304=lookup(cr1.cas,304,cr1.dph,'dph')
	r305=lookup(cr1.cas,305,cr1.dph,'dph')
	r310=r301+r302+r303+r304+r305

	r331=lookup(cr1.cas,331,cr1.dph,'dph')
	r332=lookup(cr1.cas,332,cr1.dph,'dph')
	r333=lookup(cr1.cas,333,cr1.dph,'dph')
	r334=lookup(cr1.cas,334,cr1.dph,'dph')
	r335=lookup(cr1.cas,335,cr1.dph,'dph')
	r340=r331+r332+r333+r334+r335

	r360=round(koef*r340,0)
	
	r444=lookup(cr1.cas,444,cr1.dph,'dph')
	r445=lookup(cr1.cas,445,cr1.dph,'dph')

	r450=lookup(cr1.cas,450,cr1.dph,'dph')
	r451=lookup(cr1.cas,451,cr1.dph,'dph')

	r534=lookup(cr1.cas,534,cr1.dph,'dph')
	r535=lookup(cr1.cas,535,cr1.dph,'dph')
	
	r651=r210+r310+r360+r450
	r652=r444+r445+r451-r534-r535
	

	IF r652-r651 > 0
		r753=r652-r651
		r754=0
	  ELSE
		r754=r651-r652
		r753=0
		ENDIF



	??m.nlq+m.cnoff+space(m.margin/1.6)+alignc("VÌPO¨ET DAN∑ Z P¸IDANê HODNOTY"+space(36))
	?space(m.margin/1.6)+;
		alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))+m.draft+m.elit

	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| II. Odpoüet danÿ p˝i zmÿnÿ reßimu"
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"|201| n†rok na odpoüet DPH      |"+ ;
		str(lookup(cr1.zad,201,cr1.dph,'dph'),10)+" |"+str(r201,10)+" |"
	?space(m.margin/1.2)+"|202| bez n†roku na odpoüet     |"+ ;
		str(lookup(cr1.zad,202,cr1.dph,'dph'),10)+" |"+ ;
		str(lookup(cr1.cas,202,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|203| povinnost kr†tit n†rok    |"+ ;
		str(lookup(cr1.zad,203,cr1.dph,'dph'),10)+" |"+str(r203,10)+" |"
	?space(m.margin/1.2)+"|204| pomÿrn† ü†st odpoütu      |"+ ;
		space(10)+" |"+str(r204,10)+" |"
	?space(m.margin/1.2)+"|210| Suma ˝.201+204            |"+ ;
		space(10)+" |"+str(r210,10)+" |"

	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| III. PZP s n†rokem na odpoüet    "
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"|301| z dov.- reßim doü.pouß.   |"+ ;
		space(10)+" |"+str(r301,10)+" |"
	?space(m.margin/1.2)+"|302| z dovozu              "+ ;
		str(lookup(dph.dph,302,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,302,cr1.dph,'dph'),10)+" |"+str(r302,10)+" |"
	?space(m.margin/1.2)+"|303| z dovozu              "+ ;
		str(lookup(dph.dph,303,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,303,cr1.dph,'dph'),10)+" |"+str(r303,10)+" |"
	?space(m.margin/1.2)+"|304| z tuzemska            "+ ;
		str(lookup(dph.dph,304,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,304,cr1.dph,'dph'),10)+" |"+str(r304,10)+" |"
	?space(m.margin/1.2)+"|305| z tuzemska            "+ ;
		str(lookup(dph.dph,305,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,305,cr1.dph,'dph'),10)+" |"+str(r305,10)+" |"
	?space(m.margin/1.2)+"|310| Suma n†roku na odpoüet    |"+ ;
		space(10)+" |"+str(r310,10)+" |"

	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| - bez n†roku na odpoüet          "
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"|321| z dov.- reßim doü.pouß.   |"+ ;
		space(10)+" |"+str(lookup(cr1.cas,321,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|322| z dovozu              "+ ;
		str(lookup(dph.dph,322,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,322,cr1.dph,'dph'),10)+" |"+ ;
		str(lookup(cr1.cas,322,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|323| z dovozu              "+ ;
		str(lookup(dph.dph,323,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,323,cr1.dph,'dph'),10)+" |"+ ;
		str(lookup(cr1.cas,323,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|324| z tuzemska            "+ ;
		str(lookup(dph.dph,324,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,324,cr1.dph,'dph'),10)+" |"+ ;
		str(lookup(cr1.cas,324,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|325| z tuzemska            "+ ;
		str(lookup(dph.dph,325,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,325,cr1.dph,'dph'),10)+" |"+ ;
		str(lookup(cr1.cas,325,cr1.dph,'dph'),10)+" |"

	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| - povinnost kr†tit odpoüet       "
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"|331| z dov.- reßim doü.pouß.   |"+ ;
		space(10)+" |"+str(r331,10)+" |"
	?space(m.margin/1.2)+"|332| z dovozu              "+ ;
		str(lookup(dph.dph,332,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,332,cr1.dph,'dph'),10)+" |"+str(r332,10)+" |"
	?space(m.margin/1.2)+"|333| z dovozu              "+ ;
		str(lookup(dph.dph,333,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,333,cr1.dph,'dph'),10)+" |"+str(r333,10)+" |"
	?space(m.margin/1.2)+"|334| z tuzemska            "+ ;
		str(lookup(dph.dph,334,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,334,cr1.dph,'dph'),10)+" |"+str(r334,10)+" |"
	?space(m.margin/1.2)+"|335| z tuzemska            "+ ;
		str(lookup(dph.dph,335,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,335,cr1.dph,'dph'),10)+" |"+str(r335,10)+" |"
	?space(m.margin/1.2)+"|340| Suma ˝.331 aß 335         |"+ ;
		space(10)+" |"+str(r340,10)+" |"
	?space(m.margin/1.2)+"|360| Pomÿrn† ü†st odpoütu      |"+ ;
		"k.| "+str(m.koef,6,4)+" |"+str(r360,10)+" |"

	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| IV. Uskuteünÿn† zdaniteln† plnÿn°"
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"|400| Osvobozen†                |"+ ;
		str(lookup(cr1.cas,400,cr1.dph,'dph'),10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+"|410| VÏvoz se zm. vlast. pr†va |"+ ;
		str(lookup(cr1.cas,410,cr1.dph,'dph'),10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+"|411| VÏvoz bez zm. vlast. pr†va|"+ ;
		str(lookup(cr1.cas,411,cr1.dph,'dph'),10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+"|420| VÏvoz slußeb              |"+ ;
		str(lookup(cr1.cas,420,cr1.dph,'dph'),10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+"|430| Mezin†rodn° p˝eprava      |"+ ;
		str(lookup(cr1.cas,430,cr1.dph,'dph'),10)+" |"+space(10)+" |"+ ;
		repl('-',12)
	?space(m.margin/1.2)+"|444| v sazbÿ               "+ ;
		str(lookup(dph.dph,444,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,444,cr1.dph,'dph'),10)+" |"+space(10)+" |"+ ;
		str(r444,10)+" |"
	?space(m.margin/1.2)+"|445| v sazbÿ               "+ ;
		str(lookup(dph.dph,445,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,445,cr1.dph,'dph'),10)+" |"+space(10)+" |"+ ;
		str(r445,10)+" |"
	?space(m.margin/1.2)+"|450| vypo˝†d†n° danÿ na vstupu |"+space(10)+" |"+ ;
		str(r450,10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+"|451| vypo˝†d†n° danÿ na vÏstupu|"+space(10)+" |"+ ;
		space(10)+" |"+str(r451,10)+" |"
		
	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| V. Vr†cen° danÿ"
*	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|534| v sazbÿ               "+ ;
		str(lookup(dph.dph,534,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,534,cr1.dph,'dph'),10)+" |"+space(10)+" |"+ ;
		str(r534,10)+" |"
	?space(m.margin/1.2)+"|535| v sazbÿ               "+ ;
		str(lookup(dph.dph,535,dph.cis,'cis'),2)+"% |"+ ;
		str(lookup(cr1.zad,535,cr1.dph,'dph'),10)+" |"+space(10)+" |"+ ;
		str(r535,10)+" |"

	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| VI. CelkovÏ souüet"
*	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"|651| danÿ na vstupu            |"+space(10)+" |"+ ;
		str(r651,10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+"|652| danÿ na vÏstupu           |"+space(10)+" |"+ ;
		space(10)+" |"+str(r652,10)+" |"

	?space(m.margin/1.2)+repl('-',69)
	?space(m.margin/1.2)+"| VII. DaÂov† povinnost"
	?space(m.margin/1.2)+repl('=',69)
	?space(m.margin/1.2)+"|753| Vlastn° daÂov† povinnost  |"+space(10)+" |"+ ;
		space(10)+" |"+str(r753,10)+" |"
	?space(m.margin/1.2)+"|754| NadmÿrnÏ odpoüet          |"+space(10)+" |"+ ;
		str(r754,10)+" |"+space(10)+" |"
	?space(m.margin/1.2)+repl('=',69)

	SET ALTE ON
	??m.pica+chr(12)
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.2
	SET CONS ON

	IF !ask(-1,-1,"N","Export sestavy do HTML")
		RETURN
		ENDIF

** Export do HTML
	dp_typ="Fyzick†  "
	dp_fu=space(12)
	dp_dic=m.ggdic
	dp_mes=0
	dp_ctv=0
	dp_rok=m.ggrok
	dp_jme=space(10)
	dp_pri=space(21)
	dp_tit=space(4)
	dp_oj=space(37)
	dp_oj2=space(25)
	dp_doj=space(11)
	dp_adr=left(alltrim(m.gguli)+iif(!empty(m.gguli),", ","")+ ;
		alltrim(m.ggmes)+iif(!empty(m.ggmes),", ","")+ ;
		alltrim(m.ggpsc)+space(35),35)

	dp_stat1=space(37)
	dp_stat2=space(37)
	dp_eu="Ne "
	dp_ppr=space(25)
	dp_pjm=space(11)
	dp_post=space(37)
	dp_dat=date()
	dp_jo="  "

	IF file(m.ggf+"dph.MEM")
		REST FROM (m.ggf+"dph.MEM") ADDI
		ENDIF

	DO t-prizn2.spr

	IF m.esc
		esc=.F.
		RETURN
		ENDIF

	SAVE TO (m.ggf+"dph.MEM") ALL LIKE dp_*

** Vlastn° export

	SET CONS OFF
	SET PRINT TO "..\html\Dph11.js"
	SET PRINT ON
	
	?[function GetData()]
	?[  {document.write]
	?[    ("<xml id='Dph'>"+]
	?[     "<data>"+]
	?[       "<veta "+]
	?[         "finurad=']+chrtran(m.dp_fu,m.kodlat,m.kodwin)+[' "+]
	?[         "dic=']+m.dp_dic+[' "+]
	?[         "radne='X' "+]
	?[         "oprav=' ' "+]
	?[         "dodat=' ' "+]
	?[         "datDuv=' ' "+]
	?
	?[         "mesic=']+iif(m.dp_mes>0,str(m.dp_mes,2)," ")+[ ' "+]
	?[         "ctvrt=']+iif(m.dp_ctv>0,str(m.dp_ctv,1)," ")+[' "+]
	?[         "rok=']+strn(dp_rok)+[' "+]
	?
	?[         "pOsoba1=']+chrtran(m.dp_oj,m.kodlat,m.kodwin)+[ ' "+]
	?[         "pOsoba2=']+chrtran(m.dp_oj2,m.kodlat,m.kodwin)+[ ' "+]
	?[         "pOsoba3=']+chrtran(m.dp_doj,m.kodlat,m.kodwin)+[' "+]
	?[         "fPrijm=']+iif(m.dp_typ="Fyzick†  ",chrtran(m.dp_pri,m.kodlat,m.kodwin)," ")+[' "+]
	?[         "fJmeno=']+iif(m.dp_typ="Fyzick†  ",chrtran(m.dp_jme,m.kodlat,m.kodwin)," ")+[' "+]
	?[         "fTitul=']+chrtran(m.dp_tit,m.kodlat,m.kodwin)+[ ' "+]
	?[         "adrSidlo=']+chrtran(left(alltrim(m.gguli)+iif(!empty(m.gguli),", ","")+ ;
		alltrim(m.ggmes)+iif(!empty(m.ggmes),", ","")+ ;
		alltrim(m.ggpsc)+space(35),35),m.kodlat,m.kodwin)+[' "+]
	?
	?[         "stat1=']+chrtran(m.dp_stat1,m.kodlat,m.kodwin)+[ ' "+]
	?[         "stat2=']+chrtran(m.dp_stat2,m.kodlat,m.kodwin)+[ ' "+]
	?[         "euAno=']+iif(m.dp_eu="Ano","X"," ")+[' "+]
	?[         "euNe=']+iif(m.dp_eu!="Ano","X"," ")+[' "+]
	?
	?[         "r1=' ' "+]
	?[         "r201a=']+tisktis(lookup(cr1.zad,201,cr1.dph,'dph'))+[' "+]
	?[         "r201b=']+tisktis(r201)+[' "+]
	?[         "r202a=']+tisktis(lookup(cr1.zad,202,cr1.dph,'dph'))+[' "+]
	?[         "r202b=']+tisktis(lookup(cr1.cas,202,cr1.dph,'dph'))+[' "+]
	?[         "r203a=']+tisktis(lookup(cr1.zad,203,cr1.dph,'dph'))+[' "+]
	?[         "r203b=']+tisktis(lookup(cr1.cas,203,cr1.dph,'dph'))+[' "+]
	?[         "r204b=']+tisktis(r204)+[' "+]
	?[         "r210b=']+tisktis(r210)+[' "+]
	?

	?[         "r301b=']+tisktis(r301)+[' "+]
	?[         "r302a=']+tisktis(lookup(cr1.zad,302,cr1.dph,'dph'))+[' "+]
	?[         "r302b=']+tisktis(r302)+[' "+]
	?[         "r303a=']+tisktis(lookup(cr1.zad,303,cr1.dph,'dph'))+[' "+]
	?[         "r303b=']+tisktis(r303)+[' "+]
	?[         "r304a=']+tisktis(lookup(cr1.zad,304,cr1.dph,'dph'))+[' "+]
	?[         "r304b=']+tisktis(r304)+[' "+]
	?[         "r305a=']+tisktis(lookup(cr1.zad,305,cr1.dph,'dph'))+[' "+]
	?[         "r305b=']+tisktis(r305)+[' "+]
	?[         "r310b=']+tisktis(r310)+[' "+]
	?
	?[         "r321b=']+tisktis(lookup(cr1.cas,321,cr1.dph,'dph'))+[' "+]
	?[         "r322a=']+tisktis(lookup(cr1.zad,322,cr1.dph,'dph'))+[' "+]
	?[         "r322b=']+tisktis(lookup(cr1.cas,322,cr1.dph,'dph'))+[' "+]
	?[         "r323a=']+tisktis(lookup(cr1.zad,323,cr1.dph,'dph'))+[' "+]
	?[         "r323b=']+tisktis(lookup(cr1.cas,323,cr1.dph,'dph'))+[' "+]
	?[         "r324a=']+tisktis(lookup(cr1.zad,324,cr1.dph,'dph'))+[' "+]
	?[         "r324b=']+tisktis(lookup(cr1.cas,324,cr1.dph,'dph'))+[' "+]
	?[         "r325a=']+tisktis(lookup(cr1.zad,325,cr1.dph,'dph'))+[' "+]
	?[         "r325b=']+tisktis(lookup(cr1.cas,325,cr1.dph,'dph'))+[' "+]
	?
	?[         "r331b=']+tisktis(r331)+[' "+]
	?[         "r332a=']+tisktis(lookup(cr1.zad,332,cr1.dph,'dph'))+[' "+]
	?[         "r332b=']+tisktis(r332)+[' "+]
	?[         "r333a=']+tisktis(lookup(cr1.zad,333,cr1.dph,'dph'))+[' "+]
	?[         "r333b=']+tisktis(r333)+[' "+]
	?[         "r334a=']+tisktis(lookup(cr1.zad,334,cr1.dph,'dph'))+[' "+]
	?[         "r334b=']+tisktis(r334)+[' "+]
	?[         "r335a=']+tisktis(lookup(cr1.zad,335,cr1.dph,'dph'))+[' "+]
	?[         "r335b=']+tisktis(r335)+[' "+]
	?[         "r340b=']+tisktis(r340)+[' "+]
	?[         "koef=']+str(m.koef,6,4)+[' "+]
	?[         "r360b=']+tisktis(r360)+[' "+]
	?
	?[         "r400a=']+tisktis(lookup(cr1.cas,400,cr1.dph,'dph'))+[' "+]
	?[         "r410a=']+tisktis(lookup(cr1.cas,410,cr1.dph,'dph'))+[' "+]
	?[         "r411a=']+tisktis(lookup(cr1.cas,411,cr1.dph,'dph'))+[' "+]
	?[         "r420a=']+tisktis(lookup(cr1.cas,420,cr1.dph,'dph'))+[' "+]
	?[         "r430a=']+tisktis(lookup(cr1.cas,430,cr1.dph,'dph'))+[' "+]
	?
	?[         "r444a=']+tisktis(lookup(cr1.zad,444,cr1.dph,'dph'))+[' "+]
	?[         "r444c=']+tisktis(r444)+[' "+]
	?[         "r445a=']+tisktis(lookup(cr1.zad,445,cr1.dph,'dph'))+[' "+]
	?[         "r445c=']+tisktis(r445)+[' "+]
	?
	?[         "r450b=']+tisktis(r450)+[' "+]
	?[         "r451c=']+tisktis(r451)+[' "+]
	?
	?[         "r534a=']+tisktis(lookup(cr1.zad,534,cr1.dph,'dph'))+[' "+]
	?[         "r534c=']+tisktis(r534)+[' "+]
	?[         "r535a=']+tisktis(lookup(cr1.zad,535,cr1.dph,'dph'))+[' "+]
	?[         "r535c=']+tisktis(r535)+[' "+]
	?
	?[         "r651b=']+tisktis(r651)+[' "+]
	?[         "r652c=']+tisktis(r652)+[' "+]
	?[         "r753c=']+tisktis(r753)+[' "+]
	?[         "r754b=']+tisktis(r754)+[' "+]
	?[         "r780c=' ' "+]
	?
	?[         "oPrijm=']+chrtran(m.dp_ppr,m.kodlat,m.kodwin)+[' "+]
	?[         "oJmeno=']+chrtran(m.dp_pjm,m.kodlat,m.kodwin)+[' "+]
	?[         "postav=']+chrtran(m.dp_post,m.kodlat,m.kodwin)+[' "+]
	?[         "datum=']+right(dtos(m.dp_dat),2)+ ;
				subst(dtos(m.dp_dat),5,2)+left(dtos(m.dp_dat),4)+[' "+]
	?[         "pf=']+m.dp_dic+[' />"+]
	?[     "</data>"+]
	?[     "</xml>");}]
	SET PRINT OFF
	SET PRINT TO

	SET PRINT TO "..\html\Dataakt.js"
	SET PRINT ON
	?[var aktDph=        "]+dtoc(date())+"  "+time()+[";]
	SET PRINT OFF
	SET PRINT TO

	SET CONS ON
RETURN
PROCEDURE hsoup
	?m.cnon+space(m.lmarg)+"Soupis daÂovÏch dokladÖ"+ ;
		iif(m.page!=1 OR empty(m.copyright),space(75), ;
			space(16)+m.copyrtxt+dtoc(m.copyright)+" "+left(time(),5)+space(17))+ ;
		"Strana:",m.page PICT '@L 9999'
	?
	ll=2
RETURN
**	Soupis daÂovÏch dokladÖ - 2000
PROCEDURE pre_sou1
	SELE denik
	SET FILT TO
	lmarg=m.margin
	DO t-soupis.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	m.tskf=ggf+"soupis.prt"
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	DO wpr
	SET ORDER TO dph
	SET TALK ON
	REPL dph WITH iif(md=m.ggdph OR dal=m.ggdph,iif(md=m.ggdph,mda,daa),dph) ;
		 ALL FOR betw(dat,m.tdto,m.tdtd)
	SELECT de.str, de.typ, de.cis, de.dat, de.cas AS cas, ;
			de.dph, de.zad AS zad, de.del, de.txt, de.md, de.mda, ;
			de.dal, de.daa, ;
			str(de.dph,4)+str(de.typ,3)+dtos(de.dat)+str(de.cis,5) AS gr1 ;
		FROM denik de INTO CURS sel1 ;
		HAVING (!del AND betw(dat,m.tdto,m.tdtd) AND dph>0) ;
		ORDER BY gr1
	SELE dph
	SET ORDER TO cis
	SELE sel1
	SET RELA TO dph INTO dph
	SET TALK OFF

	ff=.F.
	page=1
	STORE 0 TO ll, pp, scas, szad, sccas, sczad, ccis
	STORE .T. TO fa0,fa1,fa2,fa3,fb0,fb3,fb4,fb5,fb6,fc0,fc1
	PRIV _cis,_typ
	fdph=.F.
	COUNT ALL TO rc
	headprog="hsoup"
	DO head
	?m.nlq+m.cnoff+space(m.margin/1.6)+alignc("SOUPIS DA’OVÌCH DOKLADﬁ"+space(43))
	?space(m.margin/1.6)+alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	?m.cnoff+m.draft
	ll=m.ll+4
	GO TOP
	ddph=0
	fin=.T.
	DO WHILE !eof()
		IF m.ddph<>dph		&&	VÏpisy jednotlivÏch hlaviüek ülenÿn°
			IF ddph<>0
				=tstpgend(4)
				ll=m.ll+3
				IF m.fin
					IF m.fdph
						?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
						?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
						?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
					  ELSE
						?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
						?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
						?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
						ENDIF
					ccis=0
					ENDIF
				STORE 0 TO scas,szad,sccas,sczad
				ENDIF
			IF betw(dph,10,39) .AND. m.fa0
				=tstpgend(8)
				?m.cnoff+m.nlq
				?space(m.lmarg/1.6)+"A/ P˝ijat† zdaniteln† plnÿn° (n†kupy)"+m.draft
				?space(m.lmarg/1.6)+"-------------------------------------"+m.cnon
				ll=m.ll+3
				fa0=.F.
				ENDIF
			IF betw(dph,40,79) .AND. m.fb0
				=tstpgend(12)
				?
				?m.cnoff+m.nlq
				?space(m.lmarg/1.6)+"B/ Uskuteünÿn† zdaniteln† plnÿn° (prodeje)"+m.draft
				?space(m.lmarg/1.6)+"------------------------------------------"+m.cnon
				ll=m.ll+4
				fb0=.F.
				ENDIF
			IF betw(dph,80,89) .AND. m.fc0
				=tstpgend(12)
				?
				?m.cnoff+m.nlq
				?space(m.lmarg/1.6)+"C/ Vr†cen† daÂ zahraniün°m osob†m"+m.draft
				?space(m.lmarg/1.6)+"---------------------------------"+m.cnon
				ll=m.ll+4
				fc0=.F.
				ENDIF
			fin=.T.
			DO CASE
				CASE betw(dph,10,19)
					=tstpgend(8)
					IF fa1
						?m.nlq
						?space(m.lmarg)+"1. Soupis dokladÖ o n†kupech bez n†roku na odpoüet (P˝ijat† zdaniteln† plnÿn°)"+m.draft
						ll=m.ll+2
						fa1=.F.
						ENDIF
					DO CASE
						CASE dph=11
							?
							?space(m.lmarg)+"- osvobozen† podle par.25 z†kona o dani z p˝idanÇ hodnoty"
						CASE dph=12
							?
							?space(m.lmarg)+"- od fyzickÏch nebo pr†vnickÏch osob, kterÇ nejsou pl†tci DPH"
						CASE dph=13
							?
							?space(m.lmarg)+"- u nichß nen° n†rok na odpoüet podle par.19 odst.4 z†kona"
						CASE dph=14
							?
							?space(m.lmarg)+"- u dovozu osvob. od danÿ podle par.43 odst.4 z†kona o dani z p˝idanÇ hodnoty"
							ENDCASE
					ll=m.ll+2
				CASE betw(dph,20,29)
					=tstpgend(8)
					IF fa2
						?m.nlq
						?space(m.lmarg)+"2. Soupis daÂovÏch dokladÖ o dovozu zboß° vüetnÿ dokladÖ o zaplacen° danÿ"+m.draft
						ll=m.ll+2
						fa2=.F.
						ENDIF
					?
					?space(m.lmarg)+lookup(osn.txt,str(md,3)+str(mda,4),osn.uct,'uct')
					ll=m.ll+2
				CASE betw(dph,30,39)
					=tstpgend(8)
					IF fa3
						?m.nlq
						?space(m.lmarg)+"3. Soupis daÂovÏch dokladÖ za p˝ijat† zdaniteln† plnÿn° v tuzemsku"+m.draft
						ll=m.ll+2
						fa3=.F.
						ENDIF
					?
					?space(m.lmarg)+lookup(osn.txt,str(md,3)+str(mda,4),osn.uct,'uct')
					ll=m.ll+2
				CASE betw(dph,41,42)
					=tstpgend(8)
					?m.nlq
					?space(m.lmarg)+iif(dph=41,;
						"1. Soupis dokladÖ o uskuteünÿnÏch zdanitelnÏch plnÿn°ch osvob. od danÿ podle par.19 odst.6 a par.20 odst.1"+m.draft,;
						"2. Soupis dokladÖ o uskuteünÿnÏch zdanitelnÏch plnÿn°ch osvob. od danÿ podle par.25"+m.draft)
					ll=m.ll+2
				CASE betw(dph,43,49)
					=tstpgend(8)
					IF fb3
						?m.nlq
						?space(m.lmarg)+"3. Soupis daÂovÏch dokladÖ o uskuteünÿnÏch vÏvozech vüetnÿ dokladÖ o uskut. mezin†rodn° p˝epravÿ"+m.draft
						ll=m.ll+2
						fb3=.F.
						ENDIF
					DO CASE
						CASE dph=43
							?
							?space(m.lmarg)+"- vÏvoz zboß°"
						CASE dph=44
							?
							?space(m.lmarg)+"- vÏvoz slußeb"
						CASE dph=45
							?
							?space(m.lmarg)+"- mezin†rodn° doprava"
							ENDCASE
					ll=m.ll+2
				CASE betw(dph,50,59)
					=tstpgend(8)
					IF fb4
						?m.nlq
						?space(m.lmarg)+"4. Soupis daÂovÏch dokladÖ a dokladÖ o poußit° o uskut. zdanitelnÏch plnÿn° v tuzemsku"+m.draft
						ll=m.ll+2
						fb4=.F.
						ENDIF
					?
					?space(m.lmarg)+lookup(osn.txt,str(dal,3)+str(daa,4),osn.uct,'uct')
					ll=m.ll+2
				CASE betw(dph,60,69)
					=tstpgend(8)
					IF fb5
						?m.nlq
						?space(m.lmarg)+"5. Soupis ostatn°ch dokladÖ o prodeji"+m.draft
						ll=m.ll+2
						fb5=.F.
						ENDIF
					?
					?space(m.lmarg)+lookup(osn.txt,str(dal,3)+str(daa,4),osn.uct,'uct')
					ll=m.ll+2
				CASE betw(dph,71,79)
					=tstpgend(8)
					IF fb6
						?m.nlq
						?space(m.lmarg)+"6. Denn° z†znamy o trßb†ch v ülenÿn° podle sazeb"+m.draft
						ll=m.ll+2
						fb6=.F.
						ENDIF
					?
					?space(m.lmarg)+lookup(osn.txt,str(m.ggdph,3)+str(dph,4),osn.uct,'uct')
					ll=m.ll+2
				CASE betw(dph,81,89)
					=tstpgend(8)
					IF fc1
						?m.nlq
						?space(m.lmarg)+"1. Vr†cen† daÂ zahraniün°m osob†m v ülenÿn° po sazb†ch"+m.draft
						ll=m.ll+2
						fc1=.F.
						ENDIF
					?
					?space(m.lmarg)+lookup(osn.txt,str(m.ggdph,3)+str(dph,4),osn.uct,'uct')
					ll=m.ll+2
				OTHE
					m.fin=.F.
				ENDCASE
			ddph=dph
			IF m.fin
				=tstpgend(3)
				IF betw(dph,10,19) OR betw(dph,40,49) OR dph=71
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
					?space(m.lmarg)+'|  Datum   |  Doklad   |    ¨†stka    |'+'  T e x t'+space(33)+'|'
					?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
				  ELSE
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
					?space(m.lmarg)+'|  Datum   |  Doklad   |  Z†kl. danÿ  | Sazba  |    D a Â     |'+'  T e x t'+space(34)+'|'
					?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
					ENDIF
				m.ll=m.ll+3
				ENDIF
			ENDIF
		=tstpgend(2)
		IF m.fin
			IF betw(dph,10,19) OR betw(dph,40,49) OR dph=71
				?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+str(cas,12,2)+' | '+txt+' | '
				scas=m.scas+cas
				szad=0
				sccas=m.sccas+cas
				fdph=.F.
			  ELSE
				?space(m.lmarg)+'| '+dtoc(dat)+' | '+str(typ,3)+'/'+str(cis,5)+' | '+;
					str(zad,12,2)+' | '+str(dph.dph,5)+'% | '+str(cas,12,2)+' | '+;
					iif(betw(cas,zad*dph.dph/100-2,zad*dph.dph/100+2),' ','*')+txt+' | '
				scas=m.scas+cas
				szad=m.szad+zad
				sccas=m.sccas+cas
				sczad=m.sczad+zad
				fdph=.T.
				ENDIF
			ll=m.ll+1
			ccis=m.ccis+1
			ENDIF
		_cis=cis
		_typ=typ
		_dph=dph
		_dphdph=dph.dph
		SKIP 1
		IF !(m._typ=typ AND m._cis=cis)
			IF m.ccis>1
				IF betw(m._dph,10,19) OR betw(m._dph,40,49) OR dph=71
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+str(sccas,12,2)+' | '+space(40)+' | '
				  ELSE
					?space(m.lmarg)+'| Celkem:  | '+str(_typ,3)+'/'+str(_cis,5)+' | '+;
						str(sczad,12,2)+' | '+str(m._dphdph,5)+'% | '+str(sccas,12,2)+' | '+;
						iif(betw(sccas,sczad*m._dphdph/100-5,sczad*m._dphdph/100+5),' ','*')+space(40)+' | '
					ENDIF
				ll=m.ll+1
				ENDIF
			ccis=0
			sccas=0
			sczad=0
			ENDIF
		pp=m.pp+1
		=proc(rc,pp)
		ENDDO
	=tstpgend(3)
	IF m.fin
		IF m.fdph
			?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
			?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.szad,12,2)+' |        | '+str(m.scas,12,2)+' |'+space(43)+'|'+m.draft
			?space(m.lmarg)+'+----------+-----------+--------------+--------+--------------+'+repl('-',43)+'+'
		  ELSE
			?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
			?m.nlq+space(m.lmarg)+'| Celkem:              | '+str(m.scas,12,2)+' |'+space(42)+'|'+m.draft
			?space(m.lmarg)+'+----------+-----------+--------------+'+repl('-',42)+'+'
			ENDIF
		ENDIF
	USE
	SELE denik
	SET ALTE ON
	??m.cnoff+chr(12)
	ll=m.ll+1
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin
	SET CONS OFF
	SET FILT TO !del
	SET ORDER TO
	GO BOTT
RETURN
**	Sestava daÂovÇho p˝izn†n° - P˝ehled 2000
PROCEDURE pre_pri1
	lmarg=m.margin
	DO t-priz.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	SET ESCA ON
	ON ESCA DO stoptisk
	SELE denik
	DO wpr
	SET TALK ON
	REPL dph WITH iif(md=m.ggdph OR dal=m.ggdph,iif(md=m.ggdph,mda,daa),dph) ;
		 ALL FOR betw(dat,m.tdto,m.tdtd)
	SELECT round(sum(den.cas),0) AS cas, den.dph, round(sum(den.zad),0) AS zad;
		WHERE den.dph>0 AND !del;
			AND dtoc(dat,1) BETW dtoc(m.tdto,1) AND dtoc(m.tdtd,1);
		FROM denik den INTO CURSOR cr1 ;
		GROUP BY dph

*HAVING (!del AND betw(dat,m.tdto,m.tdtd) AND dph>0) ;
	INDEX ON dph tag dph

	SET TALK OFF
	m.tskf=ggf+"ekont.prt"
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

**	P˝°prava a vÏpoüet promÿnnÏch pro tisk p˝izn†n°
	PRIV koef, r20, r51, r52
* Bylo *	a=lookup(cr1.zad,51,cr1.dph,'dph')+lookup(cr1.zad,52,cr1.dph,'dph')
	a=lookup(cr1.zad,51,cr1.dph,'dph')+lookup(cr1.zad,61,cr1.dph,'dph')+ ;
		lookup(cr1.zad,72,cr1.dph,'dph')+lookup(cr1.zad,52,cr1.dph,'dph')+ ;
		lookup(cr1.zad,62,cr1.dph,'dph')+lookup(cr1.zad,73,cr1.dph,'dph')+ ;
		lookup(cr1.cas,43,cr1.dph,'dph')+lookup(cr1.cas,44,cr1.dph,'dph')+ ;
		lookup(cr1.cas,45,cr1.dph,'dph')
	koe=round(a/(a+lookup(cr1.cas,42,cr1.dph,'dph')+lookup(cr1.cas,71,cr1.dph,'dph')),4)
	DO CASE
		CASE m.koe>=0.95 OR a=0
			koef=1
		CASE m.koe<0.05
			koef=0
		OTHE
			koef=m.koe
		ENDCASE

	sum cas to r20 for betw (dph, 21, 39)
	r20=round(r20*m.koef,0)

	r35=lookup(cr1.zad,51,cr1.dph,'dph')+lookup(cr1.zad,61,cr1.dph,'dph')+;
		lookup(cr1.zad,72,cr1.dph,'dph')
	r35a=lookup(cr1.zad,81,cr1.dph,'dph')
	r36=lookup(cr1.zad,52,cr1.dph,'dph')+lookup(cr1.zad,62,cr1.dph,'dph')+;
		lookup(cr1.zad,73,cr1.dph,'dph')
	r36a=lookup(cr1.zad,82,cr1.dph,'dph')

	r37=lookup(cr1.cas,51,cr1.dph,'dph')+lookup(cr1.cas,61,cr1.dph,'dph')+;
		lookup(cr1.cas,72,cr1.dph,'dph')
	r37a=lookup(cr1.cas,81,cr1.dph,'dph')
	r38=lookup(cr1.cas,52,cr1.dph,'dph')+lookup(cr1.cas,62,cr1.dph,'dph')+;
		lookup(cr1.cas,73,cr1.dph,'dph')
	r38a=lookup(cr1.cas,82,cr1.dph,'dph')

	r51=r20
	r52=r37-r37a+r38-r38a
	IF r51-r52 < 0
		r53=abs(r51-r52)
		r54=0
	  ELSE
		r53=0
		r54=r51-r52
		ENDIF

	?m.nlq+m.cnoff
	?space(m.margin/1.6)+alignc("VÌPO¨ET DAN∑ Z P¸IDANê HODNOTY"+space(36))
	?space(m.margin/1.6)+;
		alignc("Obdob° od "+dtoc(m.tdto)+" do "+dtoc(m.tdtd)+space(36))
	?space(m.margin/1.6)+alignc("Firma: "+firma.jme+space(34))
	?m.draft+m.elit
	?
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| III. P¸IJATµ ZDANITELNµ PLN∑N÷"
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"|     z dovozu    "+ ;
		str(lookup(dph.dph,21,dph.cis,'cis'),2)+"% | 12 |"+;
		str(lookup(cr1.zad,21,cr1.dph,'dph'),10)+" | 16 |"+;
		str(lookup(cr1.cas,21,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|                 "+ ;
		str(lookup(dph.dph,22,dph.cis,'cis'),2)+"% | 13 |"+;
		str(lookup(cr1.zad,22,cr1.dph,'dph'),10)+" | 17 |"+;
		str(lookup(cr1.cas,22,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|     z tuzemska  "+ ;
		str(lookup(dph.dph,31,dph.cis,'cis'),2)+"% | 14 |"+;
		str(lookup(cr1.zad,31,cr1.dph,'dph'),10)+" | 18 |"+;
		str(lookup(cr1.cas,31,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|                 "+ ;
		str(lookup(dph.dph,32,dph.cis,'cis'),2)+"% | 15 |"+;
		str(lookup(cr1.zad,32,cr1.dph,'dph'),10)+" | 19 |"+;
		str(lookup(cr1.cas,32,cr1.dph,'dph'),10)+" |"+;
		iif(m.koe<0.99,"  koef."+str(m.koe,6,4),'')
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+"| ODPO¨ET"+space(30)+;
		"| 20 |"+str(r20,10)+" |"
	?space(m.margin/1.2)+repl('-',57)
	?space(m.margin/1.2)+'| IV.  USKUTE¨N∑Nµ ZDANITELNµ PLN∑N÷'
	?space(m.margin/1.2)+repl('-',40)
	?space(m.margin/1.2)+"|   Osvobozen†        | 31 |"+;
		str(lookup(cr1.cas,41,cr1.dph,'dph')+;
			lookup(cr1.cas,42,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|   VÏvoz zboß°       | 32 |"+;
		str(lookup(cr1.cas,43,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|   VÏvoz slußeb      | 33 |"+;
		str(lookup(cr1.cas,44,cr1.dph,'dph'),10)+" |"
	?space(m.margin/1.2)+"|   Mezin†r. p˝eprava | 34 |"+;
		str(lookup(cr1.cas,45,cr1.dph,'dph'),10)+" |"+space(16)+repl('-',18)
	?space(m.margin/1.2)+"|  Z†klad danÿ    "+ ;
		str(lookup(dph.dph,51,dph.cis,'cis'),2)+"% | 35 |"+;
		str(r35,10)+" |"+space(16)+"| 37 |"+str(r37,10)+" |"
	?space(m.margin/1.2)+"|  Vr†cen† daÂ    "+ ;
		str(lookup(dph.dph,81,dph.cis,'cis'),2)+"% | 35a|"+;
		str(r35a,10)+" |"+space(16)+"| 37a|"+str(r37a,10)+" |"
	?space(m.margin/1.2)+"|  Z†klad danÿ    "+ ;
		str(lookup(dph.dph,52,dph.cis,'cis'),2)+"% | 36 |"+;
		str(r36,10)+" |"+space(16)+"| 38 |"+str(r38,10)+" |"
	?space(m.margin/1.2)+"|  Vr†cen† daÂ    "+ ;
		str(lookup(dph.dph,82,dph.cis,'cis'),2)+"% | 36a|"+;
		str(r36a,10)+" |"+space(16)+"| 38a|"+str(r38a,10)+" |"
	?space(m.margin/1.2)+repl('-',74)
	?space(m.margin/1.2)+"| VI.  CELKOVÌ SOU¨ET"+space(18)+;
		"| 51 |"+str(r51,10)+" | 52 |"+str(r52,10)+" |"
	?space(m.margin/1.2)+repl('-',74)+m.nlq
	?space(m.margin/1.2)+"| VII. VLASTN÷ DA’OVµ POVINNOST"+space(25)+;
		"| 53 |"+str(r53,10)+" |"
	?space(m.margin/1.2)+"|      NADM∑RNÌ ODPO¨ET"+space(16)+;
		"| 54 |"+str(r54,10)+" |"+repl('-',17)+m.draft
	?space(m.margin/1.2)+repl('-',57)
	SET ALTE ON
	?m.pica+chr(12)
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.2
	SET CONS ON
RETURN
**	Sestava daÂovÇho p˝izn†n° - Tiskopis 2000
PROCEDURE pre_tis1
	lmarg=m.margin

	dp_typ="Fyzick†  "
	dp_fu=space(25)
	dp_dic=m.ggdic
	dp_mes=0
	dp_ctv=0
	dp_rok=m.ggrok
	dp_jme=space(11)
	dp_pri=space(23)
	dp_oj=space(19)
	dp_doj=space(11)
	dp_adr=left(alltrim(m.gguli)+iif(!empty(m.gguli),", ","")+ ;
		alltrim(m.ggmes)+iif(!empty(m.ggmes),", ","")+ ;
		alltrim(m.ggpsc)+space(35),35)
	dp_pjm=space(13)
	dp_ppr=space(21)
	dp_post=space(35)
	dp_dat=date()
	dp_sest=space(36)
	dp_tel=space(20)
	dp_vvst=0
	dp_vvys=0
	dp_jo="  "

	IF file(m.ggf+"dph.MEM")
		REST FROM (m.ggf+"dph.MEM") ADDI
		ENDIF

	DO t-prizn.spr

	IF m.esc
		esc=.F.
		RETURN
		ENDIF

	SAVE TO (m.ggf+"dph.MEM") ALL LIKE dp_*

	SET ESCA ON
	ON ESCA DO stoptisk
	SELE denik
	DO wpr
	SET TALK ON
	REPL dph WITH iif(md=m.ggdph OR dal=m.ggdph,iif(md=m.ggdph,mda,daa),dph) ;
		 ALL FOR betw(dat,m.tdto,m.tdtd)
	SELECT round(sum(den.cas),0) AS cas, den.dph, round(sum(den.zad),0) AS zad;
		WHERE den.dph>0 AND !del;
			AND dtoc(dat,1) BETW dtoc(m.tdto,1) AND dtoc(m.tdtd,1);
		FROM denik den INTO CURSOR cr1 ;
		GROUP BY dph
	INDEX ON dph tag dph
	GO TOP
	dp_jo="  "
	COUNT FOR betw(dph,1,2) OR betw(dph,20,46) OR betw(dph,50,79) TO a
	IF a>0
		dp_jo="  "
	  ELSE
		dp_jo="XX"
		ENDIF
	SET TALK OFF
	m.tskf=ggf+"ekont.prt"
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn

**	P˝°prava a vÏpoüet promÿnnÏch pro tisk p˝izn†n°
	PRIV koef, r20, r51, r52
* Bylo *	a=lookup(cr1.zad,51,cr1.dph,'dph')+lookup(cr1.zad,52,cr1.dph,'dph')
	a=lookup(cr1.zad,51,cr1.dph,'dph')+lookup(cr1.zad,61,cr1.dph,'dph')+ ;
		lookup(cr1.zad,72,cr1.dph,'dph')+lookup(cr1.zad,52,cr1.dph,'dph')+ ;
		lookup(cr1.zad,62,cr1.dph,'dph')+lookup(cr1.zad,73,cr1.dph,'dph')+ ;
		lookup(cr1.cas,43,cr1.dph,'dph')+lookup(cr1.cas,44,cr1.dph,'dph')+ ;
		lookup(cr1.cas,45,cr1.dph,'dph')
	koe=round(a/(a+lookup(cr1.cas,42,cr1.dph,'dph')+lookup(cr1.cas,71,cr1.dph,'dph')),4)
	DO CASE
		CASE m.koe>=0.95 OR a=0
			koef=1
		CASE m.koe<0.05
			koef=0
		OTHE
			koef=m.koe
		ENDCASE
	SUM cas TO r20 FOR betw(dph, 21, 39) OR dph<=2



	r20=round(r20*m.koef,0)

	r35=lookup(cr1.zad,51,cr1.dph,'dph')+lookup(cr1.zad,61,cr1.dph,'dph')+;
		lookup(cr1.zad,72,cr1.dph,'dph')
	r36=lookup(cr1.zad,52,cr1.dph,'dph')+lookup(cr1.zad,62,cr1.dph,'dph')+;
		lookup(cr1.zad,73,cr1.dph,'dph')
	r37=lookup(cr1.cas,51,cr1.dph,'dph')+lookup(cr1.cas,61,cr1.dph,'dph')+;
		lookup(cr1.cas,72,cr1.dph,'dph')
	r38=lookup(cr1.cas,52,cr1.dph,'dph')+lookup(cr1.cas,62,cr1.dph,'dph')+;
		lookup(cr1.cas,73,cr1.dph,'dph')

	r35a=lookup(cr1.zad,81,cr1.dph,'dph')
	r36a=lookup(cr1.zad,82,cr1.dph,'dph')
	r37a=lookup(cr1.cas,81,cr1.dph,'dph')
	r38a=lookup(cr1.cas,82,cr1.dph,'dph')

	IF m.tdto<{01.04.2000}
		STORE 0 TO r35a,r36a,r37a,r38a
		ENDIF

	r51=r20+m.dp_vvst
	r52=r37-r37a+r38-r38a+m.dp_vvys
	IF r51-r52 < 0
		r53=abs(r51-r52)
		r54=0
	  ELSE
		r53=0
		r54=r51-r52
		ENDIF

	SET CENT ON

	?"             Neß zaünete vyplÂovat tiskopis, p˝eütÿte si, pros°m, pouüen°."
	?
	?"       Finanün°mu £˝adu"
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø       ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"     ≥ "+m.dp_fu+              " ≥       ≥                                   ≥"
	?"     ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ       ≥                                   ≥"
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø       ≥                                   ≥"
	?"     ≥"+proloz(m.dp_dic)+       "≥       ≥                                   ≥"
	?"     ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ       ≥                                   ≥"
	?"       daÂovÇ identifikaün° ü°slo        ≥                                   ≥"
	?"                                         ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
	?"                                          "+m.cnon+"otisk prezentaün°ho raz°tka finanün°ho £˝adu"+m.cnoff
	?
	?
	?m.nlq+"                                 P ¸ I Z N µ N ÷"
	?"                            k dani z p˝idanÇ hodnoty"
	?"                                   ⁄ƒƒƒƒƒƒø           ⁄ƒƒƒƒƒƒø     ⁄ƒƒƒƒƒƒø"
	?"        za zdaÂovac° obdob°: mÿs°c ≥  "+iif(m.dp_mes!=0,str(m.dp_mes,2),"  ")+ ;
		"  ≥ ütvrtlet° ≥   "+iif(m.dp_ctv!=0,str(m.dp_ctv,1)," ")+"  ≥ rok ≥ "+str(m.dp_rok,4)+" ≥"
	?"                                   ¿ƒƒƒƒƒƒŸ           ¿ƒƒƒƒƒƒŸ     ¿ƒƒƒƒƒƒŸ"
	?m.draft
	?"       pro fyzickou osobu:"
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"     ≥"+m.nlq+proloz(m.dp_jme)+m.draft+"≥   ≥"+m.nlq+proloz(m.dp_pri)+m.draft+"≥"
	?"     ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ   ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ"
	?"              jmÇno                                p˝°jmen°"
	?
	?"       pro pr†vnickou osobu:"
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø           ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"     ≥"+m.nlq+proloz(m.dp_oj)+m.draft+"≥           ≥"+m.nlq+proloz(m.dp_doj)+m.draft+"≥"
	?"     ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ           ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ"
	?"                obchodn° jmÇno                        dodatek obchodn°ho jmÇna"
	?
	?"       pro fyzickou i pr†vnickou osobu:"
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"     ≥"+proloz(m.dp_adr)+"≥"
	?"     ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ"
	?"                                         adresa"
	?
	?"     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ"
	?m.cnon+"         PROHLAÊUJI, ¶E VÊECHNY MNOU UVEDENê ÈDAJE V TOMTO P¸IZNµN÷ JSOU PRAVDIVê A ÈPLNê"+m.cnoff
	?
	?"       Podnikatel nebo osoba opr†vnÿn† k podpisu za pr†vnickou osobu:"
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø   ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"     ≥"+proloz(m.dp_pjm)+     "≥   ≥"+proloz(m.dp_ppr)+"≥"
	?"     ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ   ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ"
	?"                jmÇno                                p˝°jmen°"
	?
	?"     ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"     ≥"+proloz(m.dp_post)+"≥"
	?"     ¿ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒ¡ƒŸ"
	?"             u pr†vnickÇ osoby: postaven° vzhledem k pr†vnickÇ osobÿ"
	?
	?"                                  ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø        ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"                                  ≥                ≥        ≥                ≥"
	?"                                  ≥                ≥        ≥                ≥"
	?"           ⁄ƒƒƒƒƒƒƒƒƒƒƒƒø         ≥                ≥        ≥                ≥"
	?"     Datum ≥ "+dtoc(m.dp_dat)+" ≥ Raz°tko ≥                ≥ Podpis ≥                ≥"
	?"           ¿ƒƒƒƒƒƒƒƒƒƒƒƒŸ         ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ        ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
	?
	?m.cnon+"         DaÂovÇ p˝izn†n° se p˝edkl†d† nejpozdÿji do 25 dnÖ po skonüen° zdaÂovac°ho obdob°."
	?
	?"         25 5401     MFin 5401     Vzor ü.7"+m.cnoff
	?chr(12)+"                         VÌPO¨ET DAN∑ Z P¸IDANê HODNOTY"+m.cnon
*	m.nlq=""
*	draft=""
	?"            ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"            ≥   I.Jestliße v obdob° nedoÁlo k ß†dnÇmu                           ≥        ÈDAJE PRO VÌPO¨ET DAN∑           ≥"
	?"            ≥     zdanitelnÇmu plnÿn° proÁkrtnÿte (X)                 ⁄ƒƒƒƒ¬ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
	?"            ≥                                                         ≥ 01 ≥ "+m.dp_jo+" ≥    DaÂ na vstupu   ≥   DaÂ na vÏstupu   ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒ¡ƒƒƒƒ≈ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
	?"            ≥  II.ODPO¨ET DAN∑ P¸I ZM∑N∑ RE¶IMU                                 ≥ 02 ≥  "+m.nlq, lookup(cr1.cas,1,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥ III.P¸IJATµ ZDANITELNµ PLN∑N÷                                     ≥                    ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥     z dovozu - reßim doüasnÇho poußit°                            ≥ 11 ≥  "+m.nlq, lookup(cr1.cas,2,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥ z dovozu     ≥ se sn°ßenou sazbou danÿ       ≥ 12 ≥  "+m.nlq, lookup(cr1.zad,21,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥ 16 ≥  "+m.nlq, lookup(cr1.cas,21,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥"
	?"            ≥              √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥              ≥ se z†kladn° sazbou danÿ       ≥ 13 ≥  "+m.nlq, lookup(cr1.zad,22,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥ 17 ≥  "+m.nlq, lookup(cr1.cas,22,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥ z tuzemska   ≥ se sn°ßenou sazbou danÿ       ≥ 14 ≥  "+m.nlq, lookup(cr1.zad,31,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥ 18 ≥  "+m.nlq, lookup(cr1.cas,31,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥"
	?"            ≥              √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥              ≥ se z†kladn° sazbou danÿ       ≥ 15 ≥  "+m.nlq, lookup(cr1.zad,32,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥ 19 ≥  "+m.nlq, lookup(cr1.cas,32,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥     ODPO¨ET vüetnÿ prom°tnut° par.20                              ≥ 20 ≥  "+m.nlq, m.r20 PICT '@Z 9999999999',m.draft+" ≥ "+iif(m.koe<0.99,"  koef."+str(m.koe,6,4),space(13))+"      ≥"			
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥"
	?"            ≥  IV.USKUTE¨N∑Nµ ZDANITELNµ PLN∑N÷                                 ≥                    ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥                    ≥"
	?"            ≥     osvobozen†                               ≥ 31 ≥  "+m.nlq, lookup(cr1.cas,41,cr1.dph,'dph')+lookup(cr1.cas,42,cr1.dph,'dph')+lookup(cr1.cas,71,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥                    ≥"
	?"            ≥     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥                    ≥"
	?"            ≥     VÏvoz zboß°                              ≥ 32 ≥  "+m.nlq, lookup(cr1.cas,43,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥                    ≥"
	?"            ≥     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥                    ≥"
	?"            ≥     VÏvoz slußeb                             ≥ 33 ≥  "+m.nlq, lookup(cr1.cas,44,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥                    ≥"
	?"            ≥     ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    ≥                    ≥"
	?"            ≥     Mezin†rodn° p˝eprava                     ≥ 34 ≥  "+m.nlq, lookup(cr1.cas,45,cr1.dph,'dph') PICT '@Z 9999999999',m.draft+" ≥                    ≥                    ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    √ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
	?"            ≥ Z†klad danÿ se sn°ßenou sazbou danÿ          ≥ 35 ≥  "+m.nlq, m.r35 PICT '@Z 9999999999',m.draft+" ≥                    ≥ 37 ≥  "+m.nlq, m.r37 PICT '@Z 9999999999',m.draft+" ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    √ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
	IF m.tdto>{31.03.2000}
		?"            ≥ Z†klad vr†cenÇ danÿ (sn°ßen† sazba)          ≥ 35a≥  "+m.nlq, m.r35a PICT '@Z 9999999999',m.draft+" ≥                    ≥ 37a≥  "+m.nlq, m.r37a PICT '@Z 9999999999',m.draft+" ≥"
		?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    √ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
		ENDIF
	?"            ≥ Z†klad danÿ se z†kladn° sazbou danÿ          ≥ 36 ≥  "+m.nlq, m.r36 PICT '@Z 9999999999',m.draft+" ≥                    ≥ 38 ≥  "+m.nlq, m.r38 PICT '@Z 9999999999',m.draft+" ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥                    √ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
	IF m.tdto>{31.03.2000}
		?"            ≥ Z†klad vr†cenÇ danÿ (z†kladn° sazba)         ≥ 36a≥  "+m.nlq, m.r36a PICT '@Z 9999999999',m.draft+" ≥                    ≥ 38a≥  "+m.nlq, m.r38a PICT '@Z 9999999999',m.draft+" ≥"
		?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
		ENDIF
	?"            ≥   V.VYPO¸µDµN÷                                                    ≥ 41 ≥  "+m.nlq, m.dp_vvst PICT '@Z 9999999999',m.draft+" ≥ 42 ≥  "+m.nlq, m.dp_vvys PICT '@Z 9999999999',m.draft+" ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
	?"            ≥  VI.CELKOVÌ SOU¨ET                                                ≥ 51 ≥  "+m.nlq, m.r51 PICT '@Z 9999999999',m.draft+" ≥ 52 ≥  "+m.nlq, m.r52 PICT '@Z 9999999999',m.draft+" ≥"
	?"            ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
	?"            ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"            ≥ VII.VLASTN÷ DA’OVµ POVINNOST ˝.52-˝.51                                                 ≥ 53 ≥  "+m.nlq, m.r53 PICT '@Z 9999999999',m.draft+" ≥"
	?"            √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
	?"            ≥     NADM∑RNÌ ODPO¨ET ˝.51-˝.52                                    ≥ 54 ≥  "+m.nlq, m.r54 PICT '@Z 9999999999',m.draft+" ≥"
	?"            ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
	?
	?m.nlq+"                                  ÈDAJE V DA’OVêM P¸IZNµN÷ SE UVEDOU ZAOKROUHLENê NA CELê KORUNY."+m.draft
	?"                              ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø                             ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
	?"            P˝izn†n° sestavil ≥ "+m.dp_sest+                       " ≥                     telefon ≥ "+m.dp_tel+" ≥"
	?"                              ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ                             ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
	IF m.tdto>{31.03.2000}
		?"            "+m.cnon+"Finanün° £˝ad p˝iznanou daÂ vymÿ˝il/dodateünÿ vymÿ˝il podle 46 odst.5 z†kona ¨NR ü.337/1992 Sb., o spr†vÿ dan°"+m.cnoff
		?"            "+m.cnon+"                                             ⁄ƒƒƒƒƒ¬ƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒø        ⁄ƒƒƒƒƒ¬ƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒø"+m.cnoff
		?"            "+m.cnon+"a poplatkÖ ve znÿn° pozdÿjÁ°ch p˝edpisÖ, dne ≥     ≥     ≥        ≥ ke dni ≥     ≥     ≥        ≥"+m.cnoff
		?"            "+m.cnon+"                                             ¿ƒƒƒƒƒ¡ƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒŸ        ¿ƒƒƒƒƒ¡ƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒŸ"+m.cnoff
		?"            "+m.cnon+"                                                                             ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"+m.cnoff
		?"            "+m.cnon+"                                                                             ≥                               ≥"+m.cnoff
		?"            "+m.cnon+"                                                                             ≥                               ≥"+m.cnoff
		?"            "+m.cnon+"                                                                             ≥                               ≥"+m.cnoff
		?"            "+m.cnon+"                                                                             ≥                               ≥"+m.cnoff
		?"            "+m.cnon+"                                               Podpis opr†vnÿnÇho pracovn°ka ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"+m.cnoff
	  ELSE
		?"            ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
		?"            ≥ Vypln° finanün° £˝ad:       ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø ≥"
		?"            ≥                             ≥ vymÿ˝en† vlastn° daÂov† povinnost  Kü                                       ≥ ≥"
		?"            ≥                             √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥ ≥"
		?"            ≥                             ≥ vymÿ˝enÏ nadmÿrnÏ odpoüet          Kü                                       ≥ ≥"
		?"            ≥                             ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ ≥"
		?"            ≥                                                                                                             ≥"
		?"            ≥                                               p˝edeps†no v p˝edpisnÇm poukaze ü. .......................... ≥"
		?"            ≥                                                                                                             ≥"
		?"            ≥ vymÿ˝il ................. dne ..............  pod poloßkou ü. ............................................. ≥"
		?"            ≥                                                                                                             ≥"
		?"            ≥ p˝ezkouÁel .............. dne ..............  p˝edepsal ........................... dne ................... ≥"
		?"            ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
		ENDIF
	?m.cnoff+chr(12)
	SET ALTE ON
	SET CENT OFF
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.2
	SET CONS ON
RETURN
**	Prohl°ßen° tiskovÏch souborÖ
PROCEDURE tis_tis
	DEFINE POPUP sou FROM 10,40 RELATIVE SHADOW ;
		TITLE " TISKOVê SOUBORY " COLOR SCHEME 4
	DEFINE BAR 22 OF sou PROMPT " Hlavn° \<kniha ";
		SKIP FOR !file(ggf+'kniha.prt')
	DEFINE BAR 23 OF sou PROMPT " \<Den°k ";
		SKIP FOR !file(ggf+'denik.prt')
	DEFINE BAR 25 OF sou PROMPT " \<Rozvaha ";
		SKIP FOR !file(ggf+'rozvaha.prt')
	DEFINE BAR 27 OF sou PROMPT " \<VÏsledovka";
		SKIP FOR !file(ggf+'vysled_d.prt')
	DEFINE BAR 29 OF sou PROMPT " \<Hospod†˝skÏ vÏsledek ";
		SKIP FOR !file(ggf+'vysled_u.prt')
*	DEFINE BAR 36 OF sou PROMPT " \<P˝edvaha ";
*		SKIP FOR !file(ggf+'predvaha.prt')
	DEFINE BAR 39 OF sou PROMPT " \<Soupis daÂovÏch dokladÖ ";
		SKIP FOR !file(ggf+'soupis.prt')
	DEFINE BAR 42 OF sou PROMPT " Èüt\<ovÏ rozvrh ";
		SKIP FOR !file(ggf+'rozvrh.prt')
	DEFINE BAR 90 OF sou PROMPT " \<E K O N T ";
		SKIP FOR !file(ggf+'ekont.prt') ;
		MESS "Prohl°ßen° posledn° sestavy vÏÁe neuvedenÇ."
	ON SELE POPU sou DO sou

	ACTI POPUP sou
	RELE POPU sou
RETURN

PROCEDURE sou
	m.vf_lmrg=m.margin
	DO CASE
		CASE bar()=22
			tskf=m.ggf+'kniha.prt'
		CASE bar()=23
			tskf=m.ggf+'denik.prt'
		CASE bar()=25
			tskf=m.ggf+'rozvaha.prt'
		CASE bar()=27
			tskf=m.ggf+'vysled_d.prt'
		CASE bar()=29
			tskf=m.ggf+'vysled_u.prt'
		CASE bar()=36
			tskf=m.ggf+'predvaha.prt'
		CASE bar()=39
			tskf=m.ggf+'soupis.prt'
		CASE bar()=42
			vf_lmrg=m.margin/1.6
			tskf=m.ggf+"rozvrh.prt"
		CASE bar()=90
			vf_lmrg=0
			tskf=m.ggf+"ekont.prt"
		ENDCASE
	IF file(m.tskf)
		DO tisk WITH m.vf_lmrg,.T.		&&	Povolen° editace
		ENDIF
RETURN
**	VnitropodnikovÇ £üetnictv°
PROCEDURE tis_vni
	DEFINE POPUP vni FROM 10,40 RELATIVE SHADOW ;
		TITLE " VnitropodnikovÇ £üetnictv° " COLOR SCHEME 4
	DEFINE BAR 10 OF vni PROMPT " VÏsledovka p†ru £ütÖ " ;
		MESS "Provede vÏpis a saldo dvou k sobÿ p˝°sluÁej°c°ch £ütÖ."
*	DEFINE BAR 59 OF vni PROMPT "\-"

	ON SELE POPU vni DO vni

	ACTI POPUP vni

	RELE POPU vni
RETURN

PROC vni
	PRIV skod, skdo, _rc, _fi, ll
	STORE 0 TO skod, skdo
	DO CASE
		CASE bar()=10
			DO vni_vysl
		ENDCASE
RETURN
**	Tisk vnitropodnikovÇ vÏsledovky za dva proti£üty
PROCEDURE vni_vysl
	PRIV _rc, _fi, _or, ll
	lmarg=m.margin
* zmenen screen vni1ucet
	lltdto={  .  .  }
	lltdtd={  .  .  }
	llnopa=.F.
	ll_p=0
	ll_l=0
	DO lli1ucet.spr
	IF m.esc
		m.esc=.F.
		RETURN
		ENDIF
	m.tskf=ggf+"ekont.prt"
	SET TALK OFF
	SET CONS OFF
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	headprog="vni_vysh"
	DO wpr
	SET ESCA ON
	ON ESCA DO stoptisk
	ff=.F.
	page=1
	pgbeg=1
	pgend=9999
	typtisk="Oboj°"
	ll=0

	SELE osn
	SET ORDER TO uct
	SELE denik
	_fi=filter()
	SET FILT TO
	_or=order()
	SET ORDER TO dat
	_rc=recno()
	fi=iif(!empty(m.ttyp),"AND typ=m.ttyp ","")

**********************
* Pokud nejsou prazdne tak zpracovat
	llzust=.F.
*	IF !empty(m.lltdto) OR !empty(m.lltdtd)
*		DO wpr
*		DO llvni_vl with m.lltdto, m.lltdtd
*		llzust=.T.
*		llnopa=.T.
*		ENDIF

	SET TALK ON
	SELECT de.typ, de.cis, de.dat, de.cas, ;
			de.del, de.txt, de.rtyp, de.rcis, de.md, de.mda, ;
			de.dal, de.daa, dtos(dat)+str(typ,3)+str(cis,5) AS gr1;
		FROM denik de INTO CURS sel1 ;
		HAVING (betw(de.dat,m.tdto,m.tdtd) &fi. ;
				AND (betw(de.md,800+m.skod,800+m.skdo) OR ;
					betw(de.md,900+m.skod,900+m.skdo) OR ;
					betw(de.dal,800+m.skod,800+m.skdo) OR ;
					betw(de.dal,900+m.skod,900+m.skdo)) ;
				AND !de.del) ;
		ORDER BY gr1

	IF recc()=0
		WAIT WIND NOWA "Nenalezeny ß†dn† data pro tiskovÏ vÏstup."
		ENDIF

* m.len_pg=30

	SELE denik
	SET FILT TO !del
	SET ORDER TO typ
	SELE sel1

	FOR i=m.skod TO m.skdo
		m=i+800
		d=i+900
		WAIT WIND NOWA "Zpracov†n° £ütÖ: "+str(m,3)+'/'+str(d,3)
		STORE 0 TO sl,sp
		page=1
		f=.T.
		SCAN ALL FOR (md=m OR md=d OR dal=m OR dal=d) 
			IF ll=0
				IF !empty(m.lltdto) OR !empty(m.lltdtd)
					DO wpr
					DO llvni_vl with m.lltdto, m.lltdtd
					llzust=.T.
					ENDIF
				llnopa =!llzust
				DO vni_vysh
				ENDIF
			IF md=m OR dal=m
				l=iif(md=m,cas,cas*-1)
				p=0
			  ELSE
				l=0
				p=iif(dal=d,cas,cas*-1)
				ENDIF
			IF tstpgend(6)
				t=txt
				IF !empty(rtyp) AND !empty(rcis)
					t=vniaptxt(t)
					ENDIF
				?space(m.margin)+'| '+dtoc(dat)+' |'+str(typ,4)+' |'+ ;
					str(cis,5)+' | '+t+' |', ;
					m.l PICT '@Z 9999999999.99',"|", ;
					m.p PICT '@Z 9999999999.99',"|"
				ll=m.ll+1
				ENDIF
			sl=m.sl+l
			sp=m.sp+p
			ENDSCAN
		IF m.ll=0
			LOOP
			ENDIF
		?space(m.margin)+"+==========+=====+=====+==========================================+===============+===============+"
		?space(m.margin)+"| Celkem obrat za "+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(29)+'|',;
			m.sl PICT '@Z 9999999999.99',"|", ;
			m.sp PICT '@Z 9999999999.99',"|"
		IF m.sl>m.sp
			l=m.sl-m.sp
			p=0
		  ELSE
			l=0
			p=m.sp-m.sl
			ENDIF
		?space(m.margin)+"| Celkem saldo"+space(52)+'|', ;
			m.l PICT '@Z 9999999999.99',"|", ;
			m.p PICT '@Z 9999999999.99',"|"
	
IF m.llzust
	IF m.ll_l<>0
		IF m.l<>0
			l=-1*(m.ll_l+m.l)
		  ELSE(p<>0)
			l=-1*(m.ll_l-m.p)	
			IF m.l>0
				p=m.l
				l=0
			  ELSE
			  	p=0
				ENDIF	
		  	ENDIF		
	  ELSE(ll_p<>0)
		IF m.l<>0
			m.p=m.ll_p-m.l
			IF m.p<0
				l=m.p
				p=0
			  ELSE
			    l=0
				ENDIF
		  ELSE(p<>0)
			m.p=m.ll_p+m.p
		  	ENDIF		
	  	ENDIF	
IF m.l<0
	ll_l=m.l*(-1)
  ELSE
    ll_l=m.l
	ENDIF
ll_p=m.p







*		IF m.llzust
*			IF m.p=0
*				ll_l=((-1)*ll_l)-m.l	
*			  	IF ll_l<0
*			  		ll_l=ll_l*(-1)
*			  	  	ll_p=0
*			  	  ELSE
*			  	  	ll_p=ll_l	
*			  		ll_l=0
*			  		ENDIF
*			  ELSE
*				ll_p=m.ll_p+m.p
*				IF ll_l!=0
*					ll_p=m.ll_p-m.ll_l				
*					IF ll_p<0
*						ll_l=-1*(ll_p)
*						ll_p=0
*					  ELSE
*						ll_l=0
*						ENDIF 
*					ENDIF
*				ENDIF
			?space(m.margin)+"+=================================================================+===============+===============+"			
			?space(m.margin)+"| Celkem saldo vüetnÿ zÖstatku "+space(35)+'|', ;
			m.ll_l PICT '@Z 9999999999.99',"|",;
			m.ll_p PICT '@Z 9999999999.99',"|"
			ENDIF
		?space(m.margin)+"+=================================================================+===============+===============+"+ ;
			m.cnoff+chr(12)
		ll=0
		ENDFOR
	SELE sel1
	USE
	SET TALK OFF
	WAIT CLEAR
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
	DO tisk WITH m.margin/1.6
	SET CONS OFF
	SELE denik
	SET FILT TO &_fi
	SET ORDER TO &_or
	=gorec(m._rc)
RETURN
**	Hlaviüka str†nky vnitropodnikovÇho £üetnictv°
PROCEDURE vni_vysh
	IF m.page>1
		??chr(12)
		ENDIF
	NaslePodm=.F.
	IF !m.llnopa and m.page>1
		a=lookup(osn.txt,str(m,3),osn.uct,'uct')
		a=left(alltrim(substr(a,at('-',a)+2))+space(80),80-(30+int(m.margin/1.6)))
		?m.cnoff+space(m.margin/1.6)+a+"   Strana:",m.page PICT '@L 999'+m.cnon
		?
		llnopa=.F.
		NaslePodm=.T.
		ENDIF
	IF !NaslePodm AND empty(m.lltdto) OR empty(m.lltdtd)
		a=lookup(osn.txt,str(m,3),osn.uct,'uct')
		a=left(alltrim(substr(a,at('-',a)+2))+space(80),80-(30+int(m.margin/1.6)))
		?m.cnoff+space(m.margin/1.6)+a+"   Strana:",m.page PICT '@L 999'+m.cnon
		?
		ENDIF
	
	?space(m.margin)+"+==========+=====+=====+==========================================+===============+===============+"
	?space(m.margin)+"| Datum    | Typ | ¨°s.| Text                                     |    N†klady    |    VÏnosy     |"
	?space(m.margin)+"+==========+=====+=====+==========================================+===============+===============+"
	ll=5
	page=m.page+1
RETURN
PROCEDURE vniaptxt
PARA t
PRIV tt, a,_sel
_sel=sele()
	SELE denik
	IF !seek(str(sel1.rtyp,3)+str(sel1.rcis,5))
		SELE (_sel)
		RETURN t
		ENDIF
	DO CASE
	  CASE md=311 OR md=321
		tt=iif(mda>0,lookup(osn.txt,str(md,3)+str(mda,4),osn.uct,'uct'),"")
	  CASE dal=311 OR dal=321
		tt=iif(daa>0,lookup(osn.txt,str(dal,3)+str(daa,4),osn.uct,'uct'),"")
	  OTHER
		tt=""
	  ENDCASE
	a=at(',',tt)
	tt=iif(a=0,tt,left(tt,a-1))
	a=len(t)
	SELE (_sel)
RETURN left(rtrim(t)+"  "+tt+space(a),a)

* Kopie vypoctu pro potreby zjisteni salda pro zadane obdobi

PROCEDURE llvni_vl
	PARAMETERS tdto,tdtd
	PRIVAT tdto,tdtd
*	IF m.esc
*		m.esc=.F.
*		RETURN
*		ENDIF
*	m.tskf=ggf+"ekont.prt"
*	SET TALK OFF
*	SET CONS OFF
*	SET ALTE TO &tskf
*	SET ALTE ON
*	headprog="vni_vysh"
*	DO wpr
*	SET ESCA ON
*	ON ESCA DO stoptisk
*	ff=.F.
*	page=1
*	pgbeg=1
*	pgend=9999
*	typtisk="Oboj°"
*	ll=0
*
*	SELE osn
*	SET ORDER TO uct
*	SELE denik
*	_fi=filter()
*	SET FILT TO
*	_or=order()
*	SET ORDER TO dat
*	_rc=recno()
*	fi=iif(!empty(m.ttyp),"AND typ=m.ttyp ","")
*
	SET TALK ON
	SELECT de.typ, de.cis, de.dat, de.cas, ;
			de.del, de.txt, de.rtyp, de.rcis, de.md, de.mda, ;
			de.dal, de.daa, dtos(dat)+str(typ,3)+str(cis,5) AS gr1;
		FROM denik de INTO CURS sel2 ;
		HAVING (betw(de.dat,m.tdto,m.tdtd) &fi. ;
				AND (betw(de.md,800+m.skod,800+m.skdo) OR ;
					betw(de.md,900+m.skod,900+m.skdo) OR ;
					betw(de.dal,800+m.skod,800+m.skdo) OR ;
					betw(de.dal,900+m.skod,900+m.skdo)) ;
				AND !de.del) ;
		ORDER BY gr1

	IF recc()=0
		WAIT WIND NOWA "Nenalezeny ß†dn† data pro tiskovÏ vÏstup."
		ENDIF

* m.len_pg=30

	SELE denik
	SET FILT TO !del
	SET ORDER TO typ
	SELE sel2

*	FOR i=m.skod TO m.skdo
		m=i+800
		d=i+900
		WAIT WIND NOWA "Zpracov†n° £ütÖ: "+str(m,3)+'/'+str(d,3)
		STORE 0 TO sl,sp
		f=.T.
		SCAN ALL FOR (md=m OR md=d OR dal=m OR dal=d)
*			IF ll=0
*				DO vni_vysh
*				ENDIF
			IF md=m OR dal=m
				l=iif(md=m,cas,cas*-1)
				p=0
			  ELSE
				l=0
				p=iif(dal=d,cas,cas*-1)
				ENDIF
			*IF tstpgend(6)
*				t=txt
*				IF !empty(rtyp) AND !empty(rcis)
*					t=vniaptxt(t)
*					ENDIF
**				?space(m.margin)+'≥ '+dtoc(dat)+' ≥'+str(typ,4)+' ≥'+ ;
**					str(cis,5)+' ≥ '+t+' ≥', ;
**					m.l PICT '@Z 9999999999.99',"≥", ;
**					m.p PICT '@Z 9999999999.99',"≥"
*				ll=m.ll+1
*				ENDIF
			sl=m.sl+l
			sp=m.sp+p
			ENDSCAN
		IF m.ll=0
*			LOOP
			ENDIF
			a=lookup(osn.txt,str(m,3),osn.uct,'uct')
			a=left(alltrim(substr(a,at('-',a)+2))+space(80),80-(30+int(m.margin/1.6)))
			?m.cnoff+space(m.margin/1.6)+a+"   Strana:",m.page PICT '@L 999'+m.cnon
			?
		llhere=.T.
		?space(m.margin)+"+=================================================================+===============+===============+"
		?space(m.margin)+"| ZÖstatek                                                        |    N†klady    |    VÏnosy     |"
		?space(m.margin)+"+=================================================================+===============+===============+"
		?space(m.margin)+"| Celkem obrat za "+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(29)+'|',;
			m.sl PICT '@Z 9999999999.99',"|", ;
			m.sp PICT '@Z 9999999999.99',"|"
		IF m.sl>m.sp
			l=m.sl-m.sp
			p=0
		  ELSE
			l=0
			p=m.sp-m.sl
			ENDIF
		?space(m.margin)+"| Celkem saldo za "+dtoc(m.tdto)+' - '+dtoc(m.tdtd)+space(29)+'|',;
			m.l PICT '@Z 9999999999.99',"|", ;
			m.p PICT '@Z 9999999999.99',"|"
		?space(m.margin)+"+=================================================================+===============+===============+"
		ll=0
*		ENDFOR
*	ll_s=m.s
*	ll_l=m.l
*	ll_sl=m.sl
*	ll_sp=m.sp
	ll_p=m.p
	STORE 0 TO s,l,sl,sp
	SELE sel1
RETURN
**	P˝eruÁen° tisku
PROCEDURE stoptisk
	IF !ask(-1,-1,'A',"P˝eruÁit tvorbu sestavy")
		RETRY
		ENDIF
	WAIT CLEAR
	SET TALK OFF
	SET CONS ON
	IF used()
		SET FILT TO
		ENDIF
	SELE denik
	SET FILT TO !del
	SET ALTE TO
	SET ESCA OFF
	ON ESCA
RETURN TO tisky
**	Transformace den°ku do doüasnÇ datab†ze MD & DAL
PROCEDURE transden
PARA _fi

**	Jak nastavit tiskovÏ vÏstup
*PROCEDURE tis_tis
*	wait wind "Nastaven° tiskovÇho souboru se provede kdekoliv pomoc° ALT+F9"
*RETURN

	PRIV _like
	IF type('m.masku')='U'
		m.masku='*'
		m.maska='*'
		ENDIF
	_like="AND like(masku,str(md,3)) AND like(maska,str(mda,4))"
	IF (m.masku='*' AND m.maska='*') OR (m.masku='???' AND m.maska='????')
		_like=''
		ENDIF
	
	CREATE CURSOR sel1 (dat D,str N(3),typ N(3),cis N(5), md N(3), ;
		mda N(4), dal N(3), daa N(4), txt C(40), cas N(12,2), ;
		uct N(3), ana N(4), puct N(3), pana N(4), ;
		casm N(12,2), casd N(12,2), cascm N(12,2), ord1 C(7), del L)
	IF !empty(m._fi)
		__fi='.AND.'+_fi
	  ELSE
		__fi=''
		ENDIF
	SET TALK ON
	APPE FROM &ggf.denik FOR !del &__fi &_like
	REPL ALL uct WITH md, ana WITH mda, ;
		puct WITH dal, pana WITH daa, ;
		ord1 WITH str(uct,3)+str(ana,4), ;
		casm WITH cas				&&	FOR uct=0
	rc=recno()
*	wait wind str(rc)+str(recc())
	IF !empty(m._like)
		_like="AND like(masku,str(dal,3)) AND like(maska,str(daa,4))"
	ENDIF
	APPE FROM &ggf.denik FOR !del &__fi &_like
	REPL ALL uct WITH dal, ana WITH daa, ;
		puct WITH md, pana WITH mda, ;
		ord1 WITH str(uct,3)+str(ana,4), ;
		casd WITH cas FOR rc<=recno()
RETURN

****************************
**	VÏpoüet zisku - £üetn°ho
PROCEDURE zisk
PARA _fi
	SELE denik
	SET FILT TO !del
	SET TALK ON
	DO transden WITH _fi
	REPL cas WITH cas * -1 FOR betw(md,600,699).OR.betw(dal,500,599)
	SELECT sel1.dat AS dat, sel1.uct AS uct, sel1.ana, ;
			sum(sel1.cas) AS cas, ;
			str(uct,3)+iif(m.yana="Ano",str(ana,5),"     ") AS vv1;
		WHERE sel1.uct=os.uct AND sel1.ana=os.ana;
			AND uct BETW 500 AND 699;
		FROM sel1, osn os INTO CURSOR cr1 ;
		GROUP BY vv1
	SET CONS OFF
	SET TALK OFF
	STORE 0 TO s1,s2,c
	GO TOP
	DO WHILE !eof()
		IF uct>=600
			s1=s1+cas
		ELSE
			s2=s2+cas
			ENDIF
		c=m.c+1
		SKIP 1
		ENDDO
RETURN m.s1-m.s2

**	Proloß° zaslanÏ text mezerama
PROCEDURE proloz
PARA _tx
	PRIV _tx1, i
	_tx1=""
	FOR i=1 TO len(m._tx)
		_tx1=m._tx1+substr(m._tx,i,1)+' '
		ENDFOR
RETURN left(m._tx1, len(m._tx1)-1)

******************************
**	Zjist° je-li £üet v den°ku
PROCEDURE jeuct
PARA uc,an,fan		&&	FAN=.T. - Prohled†vat i analytiky
PRIV sel, ord
	sel=sele()
	SELE denik
	ord=order()
	IF !m.fan
*		LOCA ALL FOR denik.md=m.uc
		SET ORDER TO md
		SEEK str(m.uc,3)
		IF found('denik')
			SET ORDER TO &ord
			SELE (m.sel)
			RETURN .T.
			ENDIF
*		LOCA ALL FOR denik.dal=m.uc
		SET ORDER TO dal
		SEEK str(m.uc,3)
		SET ORDER TO &ord
		SELE (m.sel)
		RETURN found('denik')
	  ELSE
*		LOCA ALL FOR denik.md=m.uc AND denik.mda=m.an
		SET ORDER TO md
		SEEK str(m.uc,3)+str(m.an,4)
		IF found('denik')
			SET ORDER TO &ord
			SELE (m.sel)
			RETURN .T.
			ENDIF
*		LOCA ALL FOR denik.dal=m.uc AND denik.daa=m.an
		SET ORDER TO dal
		SEEK str(m.uc,3)+str(m.an,4)
		SET ORDER TO &ord
		SELE (m.sel)
		RETURN found('denik')
		ENDIF
RETURN


******************************************************************
**					Konec programu pro tisk						**
******************************************************************



**				V Y M A Z A T


**	Vr†t° pr†zdnÏ ˝etÿzec, je-li parametr ss=0
PROCEDURE strbl   
PARA ss,ll,dd

RETURN iif(m.ss=0,space(m.ll),str(m.ss,m.ll,m.dd))
