SET CLOCK TO 0,0

SET PROC TO faktury
SET CENT ON

**********************************
****	D…le§it‚ glob ln¡ promØnn‚ - budou ukl d ny na disk

**	Dalç¡ d…le§it‚ glob ln¡ promØnn‚
PUBLIC typtisk, pgbeg, pgend, page, nfirma
typtisk="Oboj¡"		&&	Typ tisku str nek	Oboj¡,Lich‚,Sud‚
pgbeg=1
pgend=9999
page=1
kalk_vysl='0'
nfirma=''
app=.F.

******************
**	Dalç¡ promØnn‚
PUBLIC m.esc, fl_open
end=.F.
fl_open=.F.

* PUBLIC z20, Z10, D20, D10 <== je to souŸ st datab ze 

PUBLIC penale, min_zpozd, min_penale, xclen, tskl1, tskl2, splatnost, ;
	druhydistr

tskl1=" Sklad - Vinea "
tskl2=" Sklad - Pneu "
splatnost=14

=ini("FAKTURY.ENI")

penale=m.penale*.01

druhydistr="Pý¡mì, Z voz, Smluv, Prajz, Kamio"

PRIV dtod, dtdo
dtod={  .  .  }
dtdo=date()

*******************
****	Hlavn¡ MENU
DEFINE POPUP hlm_fak FROM 7,30 RELATIVE SHADOW TITLE " Fakturace/Sklad " ;
	COLOR SCHEME 4
DEFINE BAR  5 OF hlm_fak PROMPT " \<Fakturace " ;
	MESS "Tvorba faktur a jejich tisk." ;
	SKIP FOR !m.fl_open
DEFINE BAR 6 OF hlm_fak PROMPT " Sklad \<1 " ;
	MESS m.tskl1 ;
	SKIP FOR !m.fl_open
*DEFINE BAR 7 OF hlm_fak PROMPT " Sklad \<2 " ;
	MESS m.tskl2 ;
	SKIP FOR !m.fl_open
DEFINE BAR  9 OF hlm_fak PROMPT "\-"
DEFINE BAR 17 OF hlm_fak PROMPT " ¬¡seln¡\<ky " ;
	MESS "éprava Ÿ¡seln¡k… (zbo§¡, dealer…, ...)." ;
	SKIP FOR !m.fl_open
DEFINE BAR 50 OF hlm_fak PROMPT " \<Seznam klient… " ;
	MESS "Z kladn¡ £daje o klientech a mØs¡Ÿn¡ n klady." ;
	SKIP FOR !m.fl_open
DEFINE BAR 89 OF hlm_fak PROMPT "\-"
DEFINE BAR 90 OF hlm_fak PROMPT " \<R…zn‚" ;
	MESS "Indexov n¡ dat, aktualizace a import z jinìch poboŸek" ;
	SKIP FOR !m.fl_open
ON SELE POPU hlm_fak DO hlm_fak

*********************************
**	ZA¬µTEK HLAVNÖ ¬µSTI PROGRAMU

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
nfirma=firma.naz

*********************************
****	PromØnn‚ ukl dan‚ na disk
PUBLIC gl_rad_fa, gl_rskl, gl_rskl2
PUBLIC gl_naz, gl_nazd, gl_uli, gl_mes, gl_psc, gl_ico, gl_dic, gl_ban, ;
		gl_uct, gl_ks, gl_or

gl_or=space(20)

IF file(m.ggf+"gl_udaje.mem")
	PRIV a, b, a1, a2, a3, a4, a5, a6 
	a=m.gl_data
	b=m.gl_usernam
	a1=m.gl_prior
	a2=m.gl_pri_PU
	a3=m.gl_pri_IM
	a4=m.gl_pri_SC
	a5=m.gl_pri_FS
	a6=m.gl_verze

	RESTORE FROM &ggf.gl_udaje.mem ADDI

	m.gl_prior=a1
	m.gl_pri_PU=a2
	m.gl_pri_IM=a3
	m.gl_pri_SC=a4
	m.gl_pri_FS=a5
	m.gl_verze=a6

	gl_usernam=b
	gl_data=a
	IF type('m.gl_nazd')!='C'
		gl_nazd=space(25)
		ENDIF
	IF type('m.gl_rskl')!='C'
		gl_rskl='01'
		ENDIF
	IF type('m.gl_rfs2')!='C'
		gl_rfs2=' '
		ENDIF
  ELSE
	gl_naz=space(25)
	gl_nazd=space(25)
	gl_uli=space(25)
	gl_mes=space(15)
	gl_psc=space(6)
	gl_ico=space(8)
	gl_dic=space(14)
	gl_ban=space(20)
	gl_uct=space(20)
	gl_rad_fa=' '
	gl_rskl='01'
	gl_rfs2=' '
	gl_ks=space(4)
	ENDIF

**********************************
****	D…le§it‚ glob ln¡ promØnn‚
PUBLIC radafakt, radaskl
PUBLIC frada
gglm=10

DEFINE WIND w_memo FROM 5,2 TO 20,75 DOUBLE SHADOW COLOR SCHEME 13

*	Vytvoýen¡ n hodn‚ho jm‚na tiskov‚ho souboru
tskf=sys(3)
tskf=chr(asc(m.tskf)+17)+substr(m.tskf,2)+".prt"

* DoŸasnì CDX soubor
PRIV fcdxplatby
fcdxplatby=m.ggf+"F"+right(sys(3),7)

=_open(.F., .F.)

SELE faktury
radaskl=m.gl_rskl
radafakt=m.gl_rad_fa
IF m.fl_open
	DO fak_rada WITH .F.
	_bar=1
  ELSE
	_bar=99
	ENDIF

DO refscr

fp=fopen('..\Novinky.Tex')
IF fp!=-1
	WAIT WIND NOWA "Posledn¡ novinka v programu je z "+left(fgets(fp),6)
	=fclose(fp)
	ENDIF

_bar=1
DO WHILE .T.

	ACTIVATE POPUP hlm_fak BAR _bar

	IF bar()=0
		DO hlm_kon
		EXIT
		ENDIF
	ENDDO
RETURN

**	KONEC HLAVNÖ ¬µSTI PROGRAMU
*******************************

**	Obsluha HLAVNÖHO MENU
PROCEDURE hlm_fak
	DO CASE
		CASE bar()=5
			DO m_fakt
		CASE bar()=6
			DO m_skl1
*		CASE bar()=7
*			DO m_skl2
		CASE bar()=17
			DO m_cisel
		CASE bar()=50
			DO klienti
		CASE bar()=90
			DO m_ruzne
		ENDCASE
	DO refscr
RETURN
**  UkonŸen¡ pr ce programu a n vrat do HLAVNÖHO SYSTMU            
PROCEDURE hlm_kon

	DO wrt_stat WITH "SKLAD"
	IF !empty(m.ggf)
		SAVE TO (m.ggf+"firma.mem") ALL LIKE gg*
		ENDIF
	DO cldata
*	CLOSE ALL

	IF file(m.fcdxplatby+".CDX")
		DELE FILE &fcdxplatby..cdx
		ENDIF

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
**	MENU FAKTURY
PROCEDURE m_fakt  

	DEFINE POPUP faktury FROM 10,40 RELATIVE SHADOW ;
		TITLE " Fakturace " COLOR SCHEME 4

	DEFINE BAR  5 OF faktury PROMPT " \<Faktury " ;
		MESS "Tvorba faktur a jejich tisk." ;
		SKIP FOR !m.fl_open
	DEFINE BAR 80 OF faktury PROMPT " \<Sestavy " ;
		MESS "Pýehledy a seznamy pro fakturaci." ;
		SKIP FOR !m.fl_open

	ON SELE POPU faktury DO fakt
	ACTIVATE POPUP faktury
	RELE POPU faktury
RETURN
**  Obsluha menu Faktury
PROCEDURE fakt
	DO CASE
		CASE bar()=5
			DO faktur
		CASE bar()=80
			DO m_sest_f
		ENDCASE
	DO refscr
RETURN
**	MENU SKLAD 1
PROCEDURE m_skl1  

*	=_opskl1(.F., .F.)

	DEFINE POPUP sklad1 FROM 10,40 RELATIVE SHADOW ;
		TITLE m.tskl1 COLOR SCHEME 4
	DEFINE BAR 10 OF sklad1 PROMPT " \<Vìdejky " ;
		MESS "VyskladnØn¡ zbo§¡ ze skladu a tisk dokladu." ;
		SKIP FOR !m.fl_open
	DEFINE BAR 15 OF sklad1 PROMPT " \<Pý¡jemky " ;
		MESS "NaskladnØn¡ zbo§¡ do skladu." ;
		SKIP FOR !m.fl_open
	DEFINE BAR 25 OF sklad1 PROMPT " \<Maloprodej " ;
		MESS "Maloobchodn¡ prodej." ;
		SKIP FOR !m.fl_open
	DEFINE BAR 85 OF sklad1 PROMPT " \<Sestavy " ;
		MESS "Pýehledy a seznamy pro "+m.tskl1+"." ;
		SKIP FOR !m.fl_open

	ON SELE POPU sklad1 DO sklad1
	ACTIVATE POPUP sklad1
	RELE POPU sklad1
RETURN
**  Obsluha menu Sklad1
PROCEDURE sklad1  
	DO CASE
		CASE bar()=10
			DO sklad WITH .T.
		CASE bar()=15
			DO sklad WITH .F.
		CASE bar()=25
			DO malop
		CASE bar()=85
			DO m_sest_s
		ENDCASE
	DO refscr
RETURN
**	Fakturace
PROCEDURE faktur
	SELE fseznam
	SET ORDER TO ico
	SELE fakturyu
	SET ORDER TO cis
	SELE fakturyp
	SET ORDER TO cis
	SELE faktury

	DO fak_rada WITH .F.

* Nezaplacen‚ faktury udØlat pýez funkci SUM LISTu-C
	CALC sum(cas-caszap) TO s ALL FOR !sto
	WAIT "Celkem nezaplaceno: "+str(s,10,2) WINDOW NOWAIT

	SELE faktury
	SET RELA TO cis INTO fakturyu, cis INTO fakturyp
	SET ORDER TO cis
	GO BOTT

	PRIV l11, l11a, l12, l21, l22, b, a
	a=""

	l11=[:cis+' ³ '+left(o_naz,25)+' ³'+str(cas,10,2)+' ³ '+ ;
		iif(sto,"* STORNO *",iif(!empty(zap),dtoc(zap),str(caszap,10,2)))+ ;
		' ³'+str(cas-caszap,10,2)]
	l12="// ¬.fa.  ³  N zev firmy              ³   ¬ stka  ³  Uhrazeno  ³  Zbìv "

	l21=[:cis+' ³ '+dtoc(dtv)+' ³ '+left(faktury.o_naz,18)+' ³ '+str(faktury.cas,9,2)+;
		' ³ '+iif(sto,"* STORNO *",iif(!empty(zap),dtoc(zap),str(caszap,10,2)))+' ³'+ ;
		iif(pena,'*',' ')+str(iif(empty(zap),date(),zap)-dts,4)]
	l22="// ¬.fa.  ³  Vystaveno ³  N zev firmy       ³   ¬ stka  ³  Uhrazeno  ³  D/P"

	b=[||1PF=cases(sto, "bg/w", !empty(zap), "0", ;
		empty(zap) AND date()>dts, "r/w", "b/w")]


*		[||1PF=iif(dts< iif(empty(zap),date(),zap),"b/w","0")]
	l1='ALT+F6#:	DO fak_chdo			## ALT+F6-Nastaven¡ aktu ln¡ho dodavatele '


	DO list_c WITH 6,1,10,0,;
		" Seznam vystavenìch faktur ",;
		m.l11, [|1], m.l12, ;
		m.l21, [|1], m.l22, ;
		"\ F1-Help  ENTER-Oprava  F4-Proplacen¡  F5-Tisk faktury  F7-Tý¡dit podle ...",;
		'{CIS2000 ¬¡slo \<faktury, CAS ¬ \<stka, ODB \<OdbØratel, ;
			DAV \<Vystaveno, ZAP \<Zaplaceno}', ;
		'F8#: #FILTER', ;
		'#FLT= Nezaplacen‚ faktury: empty(zap), '+ ;
			'Nevyfakturovan‚ pen le: faktury.pena', ;
		b, ;
		'ALT+R:	DO fak_rada	WITH .T.	## ALT+R-Nastaven¡ ýady faktury',;
		'CR#:	DO fak_edit				## ENTER-Prohl¡§en¡/oprava faktury',;
		'INS:	DO fak_ins				## INS-Nov  faktura',;
	  iif(m.gl_pri_fs>=60, ;
		'F6#:	DO fak_chod				## F6-ZmØna odbØratele',''), ;
		m.l1, ;
		'F5#:	DO fak_tisk				## F5-Tisk faktury',;
		'F4#:	DO fak_zapl				## F4-Proplacen¡ faktury',;
		'ALT+D#:DO fak_dele				## ALT+D-Vìmaz faktury', ;
		'ALT+P#:DO fak_pena				## ALT+P-Zruçen¡/Obnoven¡ pen le'



	SET FILT TO
RETURN

**	Oprava £daj… faktury
PROCEDURE fak_edit

	DO faktury.spr

	refscr=.T.
RETURN
**	ZmØna odbØratele na ji§ vystaven‚ faktuýe
PROCEDURE fak_chod

	SELE fseznam
	SET FILT TO del
	SET ORDER TO m_add
	IF !seek(faktury.idkdo)
		GO TOP
		ENDIF
	SET ORDER TO naz
	DO list_c WITH -1,-1,10,0,;
        " VìbØr klienta pro zmØnu faktury ",;
        [:iif(del,' ','>')+left(fseznam.naz,25)+' ³ '+left(fseznam.jme,25)+;
        	' ³ '+fseznam.ico],;
	        "// N zev firmy               ³  Jm‚no osoby              ³    I¬",;
        "\ F1-Help  ENTER-ZmØna klient  F7-Tý¡dit podle ...",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
        'CR#:	EXIT					## ENTER-ZmØna klienta',;
		'INS:	DO sez_edit WITH .T.	## INS-Pýid n¡ klienta',;
		'F4#:	DO sez_edit WITH .F.	## F4-Prohl¡§en¡/oprava klienta'
	SET FILT TO
	SET ORDER TO m_add
	SELE FAKTURY
	IF m.esc
		RETURN
		ENDIF

	REPL ico WITH fseznam.ico, o_naz WITH fseznam.naz, ;
		o_naz1 WITH fseznam.jme, idkdo WITH fseznam.m_add, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	SELE fakturyu
	REPL o_uli WITH fseznam.uli, o_mes WITH fseznam.mes, ;
		o_psc WITH fseznam.psc, o_dic WITH fseznam.dic, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()

	refscr=.T.
RETURN
**	ZmØna dodavatele na ji§ vystaven‚ faktuýe
PROCEDURE fak_chdo

	SELE fakturyu

	REPL d_naz WITH m.gl_naz, d_nazd WITH m.gl_nazd, d_uli WITH m.gl_uli, ;
		d_mes WITH m.gl_mes, d_psc WITH m.gl_psc, d_dic WITH m.gl_dic, ;
		d_ico WITH m.gl_ico, d_or WITH m.gl_or, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()

	SELE faktury
	REPL d_ban WITH m.gl_ban, d_uct WITH m.gl_uct, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()

	refscr=.T.
RETURN
**	VìpoŸet penalizace
PROCEDURE fak_pena
	IF pena
		REPL pena WITH !pena, m_user WITH m.gl_usernam, ;
				m_date WITH date(), m_time WITH time()
	  ELSE
		IF faktury.zap-faktury.dts >= m.min_zpozd AND ;
				faktury.cas*m.penale*(faktury.zap-faktury.dts) >= m.min_penale
			REPL pena WITH !pena, m_user WITH m.gl_usernam, ;
				m_date WITH date(), m_time WITH time()
		  ELSE
			WAIT WIND NOWA "U t‚to faktury nen¡ d…vod £Ÿtovat pen le"
			ENDIF
		ENDIF
RETURN

**	Tisk faktury
PROCEDURE fak_tisk
	PRIV _l, kopie, pohl
	kopie=2
	_l=17
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "PoŸet kopi¡: "
	@ 0,14 GET m.kopie SIZE 1,1 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	DO tisk_poh
	pohl=recc('cu1')!=0 AND ask(-1,-1,"A","Pýitisknout do faktury neuhrazen‚ pohled vky")

	DO tiskon
	FOR k=1 TO m.kopie
		DO tisk_1fa
		ENDFOR	
	DO tiskoff

	SELE cu1
	USE

	SELE faktury
*	SET RELA TO
	DO tisk WITH 5
RETURN
**	Pomocn  procedura tisku faktury
PROCEDURE tisk_1fa
*	SELE fakturyu
*	SET ORDER TO cis
	SELE faktury
*	SET RELA TO cis INTO fakturyu
	PRIV fldph
	fldph= (z20!=0 OR z10!=0 OR z22!=0 OR z19!=0 OR z_5!=0 OR z_0!=0)
	??m.nlq+space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(5)+"³ Dodavatel:                 ³  "+iif(m.fldph,"BØ§nì daåovì doklad   ",space(22))+iif(faktury.sto,"*S*T*O*R*N*O*",space(13))+"   ³"
	?space(5)+"³                            ³                     ÉÍÍÍÍÍÍÍÍÍ»        ³"
	?space(5)+"³ "+fakturyu.d_naz+ "  ³  F A K T U R A  Ÿ.: º "+faktury.cis+" º var.s. ³"
	?space(5)+"³ "+fakturyu.d_nazd+"  ³                     ÈÍÍÍÍÍÍÍÍÍ¼        ³"
	?space(5)+"³ "+fakturyu.d_uli+ "  ³  Konstantn¡ symbol:  "+faktury.k_s+"              ³"
	?space(5)+"³ "+fakturyu.d_psc+ " "+left(fakturyu.d_mes,19)+" ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(5)+"³ DI¬: "+fakturyu.d_dic+"        ³  OdbØratel:    "+iif(!empty(fakturyu.o_dic),"DI¬: "+fakturyu.o_dic,space(19))+"     ³"
	?space(5)+"³ I¬O: "+fakturyu.d_ico+"            ³                I¬O: "+faktury.ico+space(11)+"³"
	?space(5)+"³                            ³          "+faktury.o_naz+" ³"
	?space(5)+"³ Bankovn¡ spojen¡:          ³          "+faktury.o_naz1+" ³"
	?space(5)+"³ "+faktury.d_ban+"       ³          "+fakturyu.o_uli+"     ³"
	?space(5)+"³ "+faktury.d_uct+"       ³  "+fakturyu.o_psc+"  "+fakturyu.o_mes+"     ³"
	?space(5)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(5)+"³ PýedmØt fakturace: "+faktury.pred+" ³"
	?space(5)+"Ã"+repl('Ä',69)+"´"
	?space(5)+"³ Dodac¡ a platebn¡ podm¡nky:   Datum splatnosti faktury : "+dtoc(faktury.dts)+" ³"
	?space(5)+"³ Forma £hrady: "+faktury.uhr+"        Datum vystaven¡ dokladu  : "+dtoc(faktury.dtv)+" ³"
	IF m.fldph
		?space(5)+"³ OR: "+fakturyu.d_or+"      Datum uskut.zdanit.plnØn¡: "+dtoc(faktury.dtdph)+" ³"
	  ELSE
		?space(5)+"³ OR: "+fakturyu.d_or+""+space(44)+"³"
		ENDIF
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.cnon
	?space(5)+"  éŸtujeme V m:                                                                                         Sazba    ¬ stka    "
	?space(5)+"  ~~~~~~~~~~~~~                                                                                        DPH v %             "
	i=1
	SELE fakturyp
	SCAN ALL FOR faktury.cis=fakturyp.cis
		?space(5)+"  "+fakturyp.txt
		IF fakturyp.cas!=0
			??"   "+ ;
				iif(left(fakturyp.dph,1)!='_',fakturyp.dph,' '+ ;
				right(fakturyp.dph,1))+"    "+str(fakturyp.cas,10,2)+"  "
*		  ELSE
*			??space(19)+"  "
			ENDIF
		i=m.i+1
		ENDSCAN
	PRIV a
	a=iif(faktury.z22 != 0,1,0)+iif(faktury.z19 != 0,1,0)+iif(faktury.z_5 != 0,1,0)+;
		iif(faktury.z_0 != 0,1,0)+iif(faktury.ost != 0,1,0)

	x=iif(m.pohl,recc('cu1')+6,0)

	FOR j=i TO 27-x-a
		?
		ENDFOR

	IF m.pohl
		SELE cu1
		GO TOP
		?m.cnoff+space(5)+" POHLEDµVKY PO SPLATNOSTI KE DNI "+dtoc(date())
		?space(5)+repl('~',44)
		?space(5)+" ¬.fa.    Vystavena    Splatn    Dn¡ PS   Celkem    Zaplaceno    Zbìv "
		?space(5)+repl('-',72)
		SCAN ALL
			?space(5)+cu1.cis+"  "+dtoc(cu1.dtv)+"  "+dtoc(cu1.dts)+"   "+ ;
				str(date()-cu1.dts,4)+str(cu1.cas,11,2)+ ;
				str(cu1.caszap,11,2)+str(cu1.cas-cu1.caszap,11,2)
			ENDSCAN
		ENDIF

	SELE faktury
	?
	?m.cnoff+space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	IF faktury.z20 != 0
		?space(5)+"³ Z klad danØ 20%    "+str(faktury.z20,10,2)+"   DPH 20%  "+str(faktury.d20,10,2)+"                 ³"
		ENDIF
	IF faktury.z10 != 0
		?space(5)+"³ Z klad danØ 10%    "+str(faktury.z10,10,2)+"   DPH 10%  "+str(faktury.d10,10,2)+"                 ³"
		ENDIF
	IF faktury.z22 != 0
		?space(5)+"³ Z klad danØ 22%    "+str(faktury.z22,10,2)+"   DPH 22%  "+str(faktury.d22,10,2)+"                 ³"
		ENDIF
	IF faktury.z19 != 0
		?space(5)+"³ Z klad danØ 19%    "+str(faktury.z19,10,2)+"   DPH 19%  "+str(faktury.d19,10,2)+"                 ³"
		ENDIF
	IF faktury.z_5 != 0
		?space(5)+"³ Z klad danØ  5%    "+str(faktury.z_5,10,2)+"   DPH  5%  "+str(faktury.d_5,10,2)+"                 ³"
		ENDIF
	IF faktury.z_0 != 0
		?space(5)+"³ Osvobozeno od DPH  "+str(faktury.z_0,10,2)+"                                       ³"
		ENDIF
	IF faktury.ost != 0
		?space(5)+"³ Nepodl‚h  DPH      "+str(faktury.ost,10,2)+"                                       ³"
		ENDIF
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³                                                   ÉÍÍÍÍÍÍÍÍÍÍÍÍ»    ³"
	?space(5)+"³                                 Celkem k £hradØ:  º "+str(faktury.cas,10,2)+" º KŸ ³"
	?space(5)+"³                                                   ÈÍÍÍÍÍÍÍÍÍÍÍÍ¼    ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³"+space(69)+"³"
	IF faktury.uhr="Pý¡kazem"
		?space(5)+"³ Fakturu pýevzal:                                                    ³"
		?space(5)+"³ dne:                                                                ³"
		?space(5)+"³"+space(69)+"³"
		?space(5)+"³"+space(69)+"³"
		?space(5)+"³                                               Fakturu vystavil      ³"
	  ELSE
		?space(5)+"³ Zaplaceno hotovØ                                                    ³"
		?space(5)+"³ Ÿ stku "+str(faktury.cas,10,2)+" KŸ                                                ³"
		?space(5)+"³ pýevzal:                                                            ³"
		?space(5)+"³ dne:                                                                ³"
		?space(5)+"³                                               Fakturu vystavil      ³"
		ENDIF
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.draft+"    "
	?chr(12)
RETURN

**	Vymaz n¡/storno faktury
PROCEDURE fak_dele
	SELE faktury
	PRIV rc,mx,_cis,o
	rc=recno()


******************************************************
****	üeçen¡ roku 2000 a 00 na zaŸ tku Ÿ¡sla faktury

	o=order()	
	SET ORDER TO cis2000
	SET FILT TO .T. &frada
	GO BOTT
	mx=cis
	SET FILT TO
	SET ORDER TO &o
	
*	CALCULATE max(cis) TO mx FOR .T. &frada

	=gorec(m.rc)
	IF cis!=m.mx OR sto
		IF sto
			WAIT WIND NOWA "Stornovan  faktura je obnovena."
			REPL sto WITH .F., m_user WITH m.gl_usernam, ;
				m_date WITH date(), m_time WITH time()
		  ELSE
			WAIT WIND NOWA "Faktura je pouze STORNOVµNA. Byly po n¡ vyd ny dalç¡ faktury v ýadØ."
			REPL sto WITH .T., m_user WITH m.gl_usernam, ;
				m_date WITH date(), m_time WITH time()
	        ENDIF
	  ELSE
	  	IF ask(-1,-1,"N","Opravdu smazat fakturu Ÿ. "+cis)
	  		m._cis=cis
	  		SCAN ALL FOR cis=m._cis
	  			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	  			DELE
	  			ENDSCAN
	  		SELE fakturyp
	  		SCAN ALL FOR cis=m._cis
	  			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	  			DELE
	  			ENDSCAN
	  		SELE fakturyu
	  		SCAN ALL FOR cis=m._cis
	  			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	  			DELE
	  			ENDSCAN
	  		SELE faktury
	  		GO BOTT
	  		refscr=.T.
	  		ENDIF
		ENDIF
RETURN
**	Pý¡tisk pohled vek v…Ÿi dodavateli
PROCEDURE Tisk_poh

	SELE 0
	CREATE CURSOR cu1 (cis C(7), dtv D, dts D, cas N(10,2), caszap N(10,2))

	PRIV ff, r, o, pe
	SELE faktury
	r=recno()
	o=faktury.idkdo

	SET ORDER TO cis

	PRIV ff
	ff=filter()

	SET FILTER TO empty(zap) AND right(cis,4)!="0000" AND !sto AND dts<date()
	GO TOP

	SCAN ALL FOR idkdo=o
		INSERT INTO cu1 VALUES (faktury.cis, faktury.dtv, faktury.dts, faktury.cas, faktury.caszap)
		ENDSCAN

	SET FILTER TO &ff
	=gorec(r)

RETURN


**	Nov  faktura
PROCEDURE fak_ins
	SELE faktury

* Pýekladov‚ omezen¡ na 10 faktur pro demo verzi
	IF !m.FULLVER
		IF recc()>=10
			WAIT WIND "V DEMO verzi lze zadat pouze 10 faktur."
			RETURN
			ENDIF
		ENDIF

	IF empty(m.radafakt)
		WAIT WIND NOWA "Nen¡ specifikov na ýada faktur v kter‚ se m  nov  faktura vydat."
		RETURN
		ENDIF
	PRIV mxc, c, r, o
	c=cis
	r=recno()

******************************************************
****	üeçen¡ roku 2000 a 00 na zaŸ tku Ÿ¡sla faktury
	
	SET ORDER TO cis2000
	SET FILT TO substr(faktury.cis,3,1)=m.radafakt
	GO BOTT
	mxc=right("000000"+alltrim(str(val(cis)+1,7)),7)
	SET FILT TO
	SET ORDER TO cis
	
*	CALC max(val(cis)) ALL TO mxc FOR substr(faktury.cis,3,1)=m.radafakt
*	mxc=right("000000"+alltrim(str(m.mxc+1,7)),7)

	IF ask(-1,-1,"N","Pýevz¡t £daje z aktu ln¡ faktury")
* Rozkop¡rov n¡ £daj… z minul‚ faktury
		SELE faktury
		=gorec(r)
		SCAT MEMVAR
		cis=m.mxc
		m_add=dtos(date())+sys(3)
		m_user=m.gl_usernam
		m_date=date()
		m_time=time()
		dts=date()+(m.dts-m.dtv)
		dtv=date()
		dtdph=date()
		zap={}
		caszap=0
		INSERT INTO faktury FROM MEMVAR
		SET ORDER TO cis
		=seek(m.mxc)
		SELE fakturyu
		=seek(c)
		SCAT MEMVAR
		cis=m.mxc
		m_add=dtos(date())+sys(3)
		m_user=m.gl_usernam
		m_date=date()
		m_time=time()
		INSERT INTO fakturyu FROM MEMVAR
		SELE fakturyp
		=seek(c)
		SCAN REST WHILE c=cis
			r=recno()
			SCAT MEMVAR
			cis=m.mxc
			m_add=dtos(date())+sys(3)
			m_user=m.gl_usernam
			m_date=date()
			m_time=time()
			INSERT INTO fakturyp FROM MEMVAR
			=gorec(r)
			ENDSCAN
		SELE faktury
	  ELSE
		SELE fseznam
		SET FILT TO del
		SET ORDER TO naz
		GO TOP
		DO list_c WITH -1,-1,10,0,;
	        " VìbØr klienta pro novou fakturu Ÿ. "+m.mxc+" ",;
	        [:iif(del,' ','>')+fseznam.naz+' ³ '+fseznam.jme+' ³ '+fseznam.ico],;
		        "// N zev firmy                   ³  Jm‚no osoby                  ³    I¬",;
	        "\ F1-Help  ENTER-Nov  faktura  F7-Tý¡dit podle ...",;
			'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
	        'CR#:	EXIT					## ENTER-Nov  faktura',;
			'INS:	DO sez_edit WITH .T.	## INS-Pýid n¡ klienta',;
			'F4#:	DO sez_edit WITH .F.	## F4-Prohl¡§en¡/oprava klienta'
		SET FILT TO
		SET ORDER TO ico
		SELE FAKTURY
		IF m.esc
			RETURN
			ENDIF
		INSERT INTO faktury (ico, idkdo, o_naz, cis, dtv, dtdph, dts, ;
				d_ban, d_uct, k_s, uhr, sto, pred, idkdo, ;
				m_user, m_date, m_time, m_add) ;
			VALUES (fseznam.ico, fseznam.m_add, fseznam.naz, m.mxc, date(), date(), date()+iif(fseznam.splat=-1,m.splatnost,fseznam.splat), ;
				m.gl_ban, m.gl_uct, m.gl_ks, fseznam.uhr, .F., fseznam.sml, fseznam.m_add, ;
				m.gl_usernam, date(), time(), dtos(date())+sys(3))
		INSERT INTO fakturyu (cis,d_naz,d_nazd,d_uli,d_mes,d_psc, ;
				d_ico, d_dic, d_or, o_uli, o_mes, o_psc, o_dic, ;
				m_user, m_date, m_time, m_add) ;
			VALUES (m.mxc, m.gl_naz, m.gl_nazd, ;
				m.gl_uli,m.gl_mes,m.gl_psc, m.gl_ico, m.gl_dic,m.gl_or, ;
				fseznam.uli, fseznam.mes, fseznam.psc, fseznam.dic, ;
				m.gl_usernam, date(), time(), dtos(date())+sys(3))				
		ENDIF
	DO fak_edit
RETURN
**	Propl cen¡ faktur
PROCEDURE fak_zapl
	SELE fplatby

	INDEX ON dtos(datzap) TAG datx OF (m.fcdxplatby) FOR faktury.cis==cis
	SET ORDER TO datx
	GO TOP

	DO list_c WITH -1,-1,10,0,;
		" Platby k faktuýe Ÿ. "+faktury.cis+" na Ÿ stku "+strn(faktury.cas)+" KŸ",;
		[:cis+' ³ '+dtoc(datzap)+' ³ '+str(cas,10,2)+' ³ '+m_user+" "+dtoc(m_date)+' ³ '+str(zbytek,10,2)],;
		"//  ¬.fa. ³ Datum £hr. ³    ¬ stka  ³     Zadal       dne   ³    Zbìv ",;
		"\ F1-Help  Enter-Oprava   Ins-Nov  £hrada   F7-Tý¡dit podle ...",;
		'{DATX \<Datum zaplacen¡}', ;
		'CR#:	DO zap_edit WITH .F.	## ENTER-Prohl¡§en¡/oprava £hrady ',;
		'INS:	DO zap_edit WITH .T.	## INS-Nov  £hrada ',;
		'ALT+D#:DO zap_dele				## ALT+D-Vìmaz £hrady ',;
		'F4:	DO zap_dopl				## F4-Doplatit zbytek faktury ',;
		'CTRL+ENTER:EXIT '

	PRIV x, d
	x=zap_fakt(cis)
	GO BOTT

*wait wind strn(x)+"  "+dtoc(fplatby.datzap)
	SELE faktury
	IF x=cas
		IF zap!=fplatby.datzap
			REPL zap WITH fplatby.datzap, ;
				m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			ENDIF
	  ELSE
		IF !empty(zap)
			REPL zap WITH ctod("  .  .  "), ;
				m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			ENDIF
		ENDIF

	SET ORDER TO cis
RETURN
PROCEDURE zap_edit
PARA app
	DO propl.spr

	DO zap_zbyt
RETURN
PROCEDURE zap_dele
	REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DELE

	DO zap_zbyt
	DO zap_fakt WITH faktury.cis
RETURN
**	Doplat¡ zbytek faktury ke vŸerejçku
PROCEDURE zap_dopl
	PRIV z
	z=zap_zbyt()
	IF z!=0
		INSERT INTO fplatby ;
			(cis, datzap, cas, zbytek, m_user, m_date, m_time, m_add) ;
		  VALUES ;
			(faktury.cis, date()-1, z, 0, m.gl_usernam, date(), time(), dtos(date())+sys(3))
		ENDIF
RETURN
**	PýepoŸet zaplacenìch Ÿ stek faktury a jejich z pis k faktuýe
**	c = Ÿ¡slo faktury
PROCEDURE zap_fakt
PARA c
	PRIV s,o,r,x
	s=sele()
	SELE fplatby
	o=order()
	r=recno()
	SET ORDER TO cis
	=seek(c)
	x=0
	SUM cas REST WHILE cis=c TO x
	SET ORDER TO &o
	=gorec(r)

* Z pis nov‚ hodnoty k faktuýe
	SELE faktury
	o=order()
	r=recno()
	SET ORDER TO cis
	IF seek(c) AND x!=caszap
		REPL caszap WITH x, m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		ENDIF
	SET ORDER TO &o
	=gorec(r)
	SELE (s)
RETURN x
**	PýepoŸte zbytky u £hrad faktury
PROCEDURE zap_zbyt
	PRIV a
	a=faktury.cas
	SCAN ALL
		a=a-cas
		REPL zbytek WITH a
		ENDSCAN
	refscr=.T.
RETURN a

	
PROCEDURE ucty_pol
	SELE fucty
	SET FILT TO
	SET ORDER TO banka
	SEEK cs(m.d_ban)
	DO list_c WITH -1,-1,10,0,;
		[banka+' ³ '+ucet+' ³ '+pozn],;
		" éŸty dodavatele ",;
		"// Banka               ³ ¬¡slo £Ÿtu           ³ Pozn mka",;
		"\ ENTER-VìbØr £Ÿtu dodavatele"
	IF !m.esc
		d_ban=fucty.banka
		d_uct=fucty.ucet
		SHOW GETS
		ENDIF
	SELE faktury
RETURN

PROCEDURE fak_pol
	PRIV _fi,_ord
	SELE fakturyp
	_fi=filter()
	_ord=order()
	SET FILT TO faktury.cis==fakturyp.cis
	SET ORDER TO
	GO TOP
	DO list_c WITH 11,1,8,-10,;
		[left(txt,48)+'³ '+cle+' ³ '+dph+'%'+' ³ '+str(cas,10,2)],;
		" Text                                            ³ ¬len ³ DPH ³ ¬ stka    ",;
		"\ F1-Help  ENTER-Oprava £daje  INS-Pýid n¡",;
		'INS:	DO pol_edit WITH .T.	## INS-Pýid n¡ polo§ky',;
		'CR#:	DO pol_edit WITH .F.	## ENTER-Oprava polo§ky ',;
		'ALT+D#:DO pol_dele 			## ALT+D-Smaz n¡ polo§ky',;
		'Ctrl+CR:EXIT'
	SET FILT TO &_fi
	SET ORDER TO &_ord
	SELE faktury
RETURN

PROCEDURE pol_edit
PARA app
	DO fakturyp.spr
RETURN

PROCEDURE pol_dele
	IF ask(-1,-1,'A',"Opravdu smazat vybranou polo§ku")
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
	DO ref_fakt
	fl_modip=.T.
	refscr=.T.
RETURN
**	Obnoven¡ obrazovky a dat faktury
PROCEDURE ref_fakt
	PRIV _se, _rc, i
	_se=sele()
	SELE fakturyp
	_rc=recno()
	SUM cas TO z22 FOR dph="22" AND faktury.cis=fakturyp.cis
	SUM cas TO z21 FOR dph="21" AND faktury.cis=fakturyp.cis
	SUM cas TO z20 FOR dph="20" AND faktury.cis=fakturyp.cis
	SUM cas TO z19 FOR dph="19" AND faktury.cis=fakturyp.cis
	SUM cas TO z15 FOR dph="15" AND faktury.cis=fakturyp.cis
	SUM cas TO z14 FOR dph="14" AND faktury.cis=fakturyp.cis
	SUM cas TO z10 FOR dph="10" AND faktury.cis=fakturyp.cis
	SUM cas TO z_5 FOR dph="_5" AND faktury.cis=fakturyp.cis
	SUM cas TO z_0 FOR dph="_0" AND faktury.cis=fakturyp.cis
	SUM cas TO ost FOR dph="--" AND faktury.cis=fakturyp.cis
	d22=ceil(m.z22*2.2)/10
	d21=ceil(m.z21*2.1)/10
	d20=ceil(m.z20*2.0)/10
	d19=ceil(m.z19*1.9)/10
	d15=ceil(m.z15*1.5)/10
	d14=ceil(m.z14*1.4)/10
	d10=ceil(m.z10*1.0)/10
	d_5=ceil(m.z_5*.5)/10
	i=1
	SCAN ALL FOR faktury.cis=fakturyp.cis AND m.i<10
		ppp(m.i)=' '+left(txt,48)+' ³ '+cle+' ³ '+dph+'%'+' ³ '+str(cas,10,2)
		i=m.i+1
		ENDSCAN
	cas=m.z20+m.z21+m.z15+m.z14+m.z10+m.z22+m.z19+m.z_5+m.z_0+m.d21+m.d14+m.d15+m.d20+m.d22+m.d19+m.d_5+m.ost
	FOR j=i TO 14
		ppp(m.j)=space(75)
		NEXT j

	SHOW GETS

	=gorec(m._rc)
	SELE (m._se)
RETURN

**	Nastaven¡ ýady faktur
PROCEDURE fak_rada
PARA edt
	IF m.edt
		PRIV _l
		_l=17
		DEFINE WINDOW w_ask ;
			FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
			TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
			NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
		ACTIVATE WINDOW w_ask SAME
		@ 0,1 SAY "üada faktur: "
		@ 0,14 GET m.radafakt SIZE 1,1 COLOR SCHEME 4
		READ MODAL
		RELEASE WINDOW w_ask
		DO wpr
		ENDIF
	PRIV _se, _rc
	_se=sele()
	SELE faktury
	_rc=recno()
	IF !empty(m.radafakt)
		SET FILT TO
		LOCA ALL FOR substr(faktury.cis,3,1)=m.radafakt
		IF eof()
			frada=""
			m.radafakt=" "
			WAIT WIND "Zadan  ýada faktur nen¡ v…bec pou§¡v na."
		  ELSE
			frada="AND substr(faktury.cis,3,1)=m.radafakt"
			ENDIF
	  ELSE
		frada=""
		ENDIF
	=gorec(m._rc)
	__fltr='right(faktury.cis,4)<>"0000"'+m.frada
	SET FILT TO &__fltr
	SELE (m._se)
	refscr=.T.
	WAIT CLEAR
RETURN
**	Obsluha menu sestav  - FAKTURY
PROCEDURE m_sest_f
	DEFINE POPUP sestavy FROM 10,40 RELATIVE SHADOW ;
		TITLE " Tiskov‚ sestavy " COLOR SCHEME 4
	DEFINE BAR  5 OF sestavy PROMPT " Neproplacen‚ faktury - \<Souhrn" ;
		MESS "Souhrnnì seznam neproplacenìch faktur."
	DEFINE BAR  7 OF sestavy PROMPT " Neproplacen‚ faktury - \<Klienti" ;
		MESS "Seznam neproplacenìch faktur po klientech."
	DEFINE BAR  8 OF sestavy PROMPT " \<Nevyfakturovan‚ pen le" ;
		MESS "Nevyfakturovan‚ pen le u ji§ proplacenìch faktur."
	DEFINE BAR 10 OF sestavy PROMPT " \<Vystaveno/inkasov no " ;
		MESS "Zobrazen¡ vystavenìch Ÿ stek k inkasovanìm."
	DEFINE BAR 15 OF sestavy PROMPT " \<Fakturace dle Ÿlen… " ;
		MESS "Pýehled fakturace dle jednotlivìch Ÿlen…."
	DEFINE BAR 20 OF sestavy PROMPT " Fakturace \<1 Ÿlena " ;
		MESS "Pýehled polo§ek fakturovanìch na 1 Ÿlena."
	DEFINE BAR 49 OF sestavy PROMPT "\-"
	DEFINE BAR 50 OF sestavy PROMPT " Podklad k \<za£Ÿtov n¡ " ;
		MESS "Pýehled obsahu faktur pro za£Ÿtov n¡."
	DEFINE BAR 59 OF sestavy PROMPT "\-"
	DEFINE BAR 99 OF sestavy PROMPT " \<Posledn¡ sestava " ;
		MESS "Zobrazen¡ posledn¡ vytvoýen‚ sestavy s mo§nost¡ jej¡ opravy."
	ON SELE POPU sestavy DO sestav_f
	ACTIVATE POPUP sestavy
	RELE POPU sestavy
RETURN

PROCEDURE sestav_F
	DO CASE
		CASE bar()=5
			DO sef_nepl
		CASE bar()=7
			DO sef_nkli
		CASE bar()=8
			DO sef_pena
		CASE bar()=10
			DO sef_vyst
		CASE bar()=15
			DO sef_clen
		CASE bar()=20
			DO sef_cle1
		CASE bar()=50
			DO sef_zau
		CASE bar()=99
			DO tisk WITH 0,.T.
		ENDCASE
	DO refscr
RETURN

**	Neproplacen‚ faktury
PROCEDURE sef_nepl
	SELE fakturyu
	SET ORDER TO cis
	SELE faktury
	SET RELA TO cis INTO fakturyu
	DO fak_rada WITH .T.
	SET ORDER TO dav
	DO tiskon
	PRIV ff, prs, pos, pen, poc, dni
	ff=filter()
	SET FILTER TO empty(zap) AND &ff AND !sto
	GO TOP
	STORE 0 TO prs, pos, pen, poc, dni
	IF !empty(m.radafakt)
		?"üada faktur Ÿ. "+m.radafakt
		ENDIF
	?m.cnon+m.draft
	?" ¬.fa.   N zev firmy               Dat. splat.     Ve lh…tØ      Po lh…tØ        Pen le    Dn¡ PS"
	?repl("~",98)
	DO WHILE !eof()
		?faktury.cis+"  "+left(faktury.o_naz,23)+"   "+dtoc(dts)
		IF dts>=date()
			??str(cas-caszap,14,2)+space(14)
			prs=m.prs+cas-caszap
		  ELSE
			??space(14)+str(cas-caszap,14,2)
			pos=m.pos+cas-caszap
			ENDIF
		pe=-1*iif(dts<date() AND dtoc(zap)="  .  .  ", ;
			(cas-caszap)*m.penale*(faktury.dts-date()),0)
		pen=m.pen+pe
		IF m.pe>0
			??str(m.pe,14,2)+str(-1*(faktury.dts-date()),10)
			dni=m.dni-(faktury.dts-date())
			ENDIF
		poc=m.poc+1
		SKIP 1
		ENDDO
	?repl("~",98)
	?"PoŸet:"+str(m.poc,3)+"       Celkem: "+str(m.prs+m.pos,9,2)+ ;
		space(12)+str(m.prs,14,2)+str(m.pos,14,2)+ ;
		str(m.pen,14,2)+str(m.dni,10)
	?m.cnoff+chr(12)
	DO tiskoff
	SELE faktury
	SET FILTER TO
	SET RELA TO
	DO tisk WITH 0
RETURN

**	Neproplacen‚ faktury klienti
PROCEDURE sef_nkli

* Dotaz na obdob¡
	SELE faktury
	SET ORDER TO dav
	GO TOP
	dtod=dtv
	GO BOTT
	dtdo=dtv

	PRIV _sl, _ra
	_sl=37
	_ra=1
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._ra+2))/2),INT((SCOL()-(m._sl-1))/2) ;
		TO INT((SROW()-(m._ra+2))/2)+m._ra+1,INT((SCOL()-(m._sl-1))/2)+m._sl ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Obdob¡ od          do "
	@ 0,11 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,25 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SET ORDER TO odb
	DO tiskon
	PRIV ff, prs, pos, pen, poc, dni, odb, sprs, spos, spen, sdni
	ff=filter()
	SET FILTER TO empty(zap) AND right(cis,4)!="0000" AND !sto
	GO TOP
	STORE 0 TO prs, pos, pen, poc, dni, sprs, spos, spen, sdni
	odb='_'
	?space(5)+" NEUHRAZEN POHLEDµVKY vystaveny od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?space(5)+"*************************************************************"
	?m.cnon+space(5)+repl("=",102)+m.nlq
	?space(5)+"   ¬.fa.   N zev firmy               Dat. splat.     Ve lh…tØ      Po lh…tØ        Pen le    Dn¡ PS"+m.draft
*m.nlq=""
*m.draft=""
	SCAN ALL FOR betw(dtv,m.dtod,m.dtdo)
		IF m.odb!=faktury.o_naz
			IF m.poc>1
				?space(5)+m.nlq+"  Celkem:  "+str(m.prs+m.pos,9,2)+ ;
					space(27), m.prs PICT '@Z 9999999999.99', ;
					m.pos PICT '@Z 9999999999.99', ;
					m.pen PICT '@Z 9999999999.99', str(m.dni,9)+m.draft
				ENDIF
			?space(5)+repl("-",102)
			sprs=m.sprs+m.prs
			spos=m.spos+m.pos
			spen=m.spen+m.pen
			sdni=m.sdni+m.dni
			STORE 0 TO prs, pos, pen, poc, dni
			odb=faktury.o_naz
			ENDIF
		IF m.poc<1
			?space(5)+"  "+faktury.cis+"  "+left(faktury.o_naz,23)+"   "
		  ELSE
			?space(5)+"  "+faktury.cis+space(28)
			ENDIF
		??dtoc(dts)
		IF dts>=date()
			??str(cas-caszap,14,2)+space(14)
			prs=m.prs+cas-caszap
		  ELSE
			??space(14)+m.nlq+str(cas-caszap,14,2)+m.draft
			pos=m.pos+cas-caszap
			ENDIF
		pe=-1*iif(faktury.dts<date() AND dtoc(faktury.zap)="  .  .  ", ;
			(cas-caszap)*m.penale*(faktury.dts-date()),0)
		pen=m.pen+pe
		IF m.pe>0
			??str(m.pe,14,2)+str(-1*(faktury.dts-date()),10)
			dni=m.dni-(faktury.dts-date())
			ENDIF
		poc=m.poc+1
		ENDSCAN
	IF m.poc>1
		?space(5)+m.nlq+"  Celkem:  "+str(m.prs+m.pos,9,2)+ ;
			space(27)+str(m.prs,14,2)+str(m.pos,14,2)+ ;
			str(m.pen,14,2)+str(m.dni,10)+m.draft
		ENDIF
	sprs=m.sprs+m.prs
	spos=m.spos+m.pos
	spen=m.spen+m.pen
	sdni=m.sdni+m.dni
	?space(5)+m.nlq+repl("=",102)
	?space(5)+"  Celkem:  "+str(m.sprs+m.spos,9,2)+ ;
		space(27)+str(m.sprs,14,2)+str(m.spos,14,2)+ ;
		str(m.spen,14,2)+str(m.sdni,10)
	?space(5)+repl("=",102)+m.draft+m.cnoff+chr(12)
	DO tiskoff
	SET FILTER TO &ff
	SET RELA TO
	DO tisk WITH 7
RETURN
**	Seznam nevyfakturovanìch pen le
PROCEDURE sef_pena
	SELE faktury
	SET ORDER TO odb
	DO tiskon
	PRIV ff, pos, pen, poc, dni, odb, spos, spen, sdni
	ff=filter()
	SET FILTER TO !empty(zap) AND right(cis,4)!="0000" AND pena AND !sto
	GO TOP
	STORE 0 TO pos, pen, poc, dni, spos, spen, sdni
	odb='_'
	?space(15)+" Nevyfakturovan‚ pen le"
	?space(15)+"************************"
	?m.cnon+space(5)+repl("=",94)+m.nlq
	?space(5)+"   ¬.fa.   N zev firmy               Dat.splat.   Zaplaceno     Dn¡ PS     ¬ stka       Pen le"+m.draft
*m.nlq=""
*m.draft=""
	SCAN ALL FOR faktury.zap-faktury.dts >= m.min_zpozd AND ;
			faktury.cas*m.penale*(faktury.zap-faktury.dts) >= m.min_penale
			
		IF m.odb!=faktury.o_naz
			IF m.poc>1
				?space(5)+m.nlq+"  Celkem:  "+space(49)+ ;
					str(m.dni,8)+str(m.pos,14,2)+str(m.pen,13,2)+m.draft
				ENDIF
			?space(5)+repl("-",94)
			spos=m.spos+m.pos
			spen=m.spen+m.pen
			sdni=m.sdni+m.dni
			STORE 0 TO pos, pen, poc, dni
			odb=faktury.o_naz
			ENDIF
		IF m.poc<1
			?space(5)+"  "+faktury.cis+"  "+left(faktury.o_naz,23)+"   "
		  ELSE
			?space(5)+"  "+faktury.cis+space(28)
			ENDIF
		dni=m.dni+(faktury.zap-faktury.dts)
		pos=m.pos+cas
		pe=faktury.cas*m.penale*(faktury.zap-faktury.dts)
		pen=m.pen+pe
		??dtoc(dts)+"   "+dtoc(zap)+str(faktury.zap-faktury.dts,8)+ ;
			str(cas,14,2)+str(m.pe,13,2)
		poc=m.poc+1
		ENDSCAN
	IF m.poc>1
		?space(5)+m.nlq+"  Celkem:  "+space(49)+ ;
			str(m.dni,8)+str(m.pos,14,2)+str(m.pen,13,2)+m.draft
		ENDIF
	spos=m.spos+m.pos
	spen=m.spen+m.pen
	sdni=m.sdni+m.dni
	?space(5)+m.nlq+repl("=",94)
	?space(5)+"  Celkem:  "+space(49)+ ;
		str(m.sdni,8)+str(m.spos,14,2)+str(m.spen,13,2)
	?space(5)+repl("=",94)+m.draft+m.cnoff+chr(12)
	DO tiskoff
	SET FILTER TO &ff
	SET RELA TO
	DO tisk WITH 7
RETURN

**	Podklad pro za£Ÿtov n¡
PROCEDURE sef_zau
	PRIV ffod
	ffod=right(str(year(date()),4),2)+m.radafakt+"    "
	PRIV _l
	_l=25
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Od faktury"
	@ 0,12 GET m.ffod SIZE 1,7 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF
	SELE fakturyp
	SET ORDER TO cis
	SELE fakturyu
	SET ORDER TO cis
	SELE faktury
	SET ORDER TO cis
	SET RELA TO cis INTO fakturyu
	DO tiskon
	??m.cnon+m.draft+space(16)+repl('=',80)
	SEEK m.ffod
	SCAN REST WHILE left(faktury.cis,3)=left(m.ffod,3)
		?space(16)+"¬F: "+faktury.cis+"  DTV: "+dtoc(faktury.dtv)+ ;
			"   Odb.: "+faktury.o_naz+"  KŸ: "+str(faktury.cas,10,2)
		?space(16)+repl('-',80)
		SELE fakturyp
		SEEK faktury.cis
		SCAN REST WHILE faktury.cis=fakturyp.cis
			?space(16)+left(fakturyp.txt,65)+" | "+str(fakturyp.cas,10,2)+" | "+ ;
				CASES (dph="_5"," 5%",dph="22","22%",dph="_0"," 0%",dph="--"," - ")
			ENDSCAN
		SELE faktury
		?space(16)+repl('=',80)
		ENDSCAN
	?m.cnoff+chr(12)
	DO tiskoff
	SELE faktury
	SET FILTER TO
	SET RELA TO
	DO tisk WITH 16
RETURN
**	Sestava vystaveno, utr§eno a rozd¡l
PROCEDURE sef_vyst
PRIV _ff
	SELE 0
	CREA CURS cu1 (mr C(7), zap N(13,2), vys N(13,2), prum N(5))
	INDEX ON mr TAG mr
	SET ORDER TO mr
	SELE faktury
	SET ORDER TO dav
	do tiskon
	DO fak_rada WITH .T.
	_ff=filter()
	SET FILTER TO &_ff AND !sto
	WAIT WIND NOWA "Pracuji ..."
	PRIV rt, rv
	SCAN ALL
		rt=str(year(zap),4)+"/"+str(month(zap),2)
		rv=str(year(dtv),4)+"/"+str(month(dtv),2)
		WAIT WIND NOWA "Zpracov n¡: "+m.rv
		IF !empty(zap)
			IF seek(m.rt,"cu1")
				REPL cu1.zap WITH cu1.zap+faktury.cas
			  ELSE
				INSERT INTO cu1 VALUES (m.rt, faktury.cas, 0, 0)
				ENDIF
			ENDIF
		IF seek(m.rv,"cu1")
			REPL cu1.vys WITH cu1.vys+faktury.cas
		  ELSE
			INSERT INTO cu1 VALUES (m.rv, 0, faktury.cas, 0)
			ENDIF
		ENDSCAN

	IF !empty(m.radafakt)
		?"üada faktur Ÿ. "+m.radafakt
		?
		ENDIF
	?"Rok/MØs         Tr§by     Vystaveno         Rozd¡l      Pr…mØr"
	?repl('=',62)
	PRIV r, czp, cfa, poc, sczp, scfa, spoc, mi, mx
	STORE 0 TO czp, cfa, poc, sczp, scfa, spoc
	SELE cu1
	GO TOP
	r=left(mr,4)
	STORE round(vys/1000,0) TO mi, mx
	SCAN ALL
		IF r!=left(mr,4)
			?repl('-',62)
			?left(r,4)+"   ",m.czp,m.cfa,m.czp-m.cfa, ;
				str(round(m.cfa/m.poc/1000,0),10)
			?repl('-',62)
			sczp=m.sczp+m.czp
			scfa=m.scfa+m.cfa
			spoc=m.spoc+m.poc
			STORE 0 TO czp, cfa, poc
			r=left(mr,4)
			ENDIF
		poc=m.poc+1
		czp=czp+zap
		cfa=cfa+vys
		?mr,zap,vys,zap-vys,str(round(m.cfa/m.poc/1000,0),10)
		mi=min(m.mi,round((m.cfa)/(m.poc)/1000,0))
		mx=max(m.mx,round((m.cfa)/(m.poc)/1000,0))
		ENDSCAN
	?repl('-',62)
	?left(r,4)+"   ",m.czp,m.cfa,m.czp-m.cfa, ;
		str(round(m.cfa/m.poc/1000,0),10)
	?repl('=',62)
	sczp=m.sczp+m.czp
	scfa=m.scfa+m.cfa
	spoc=m.spoc+m.poc
	m.poc=0
	?"Celkem:",m.sczp,m.scfa,m.sczp-m.scfa, ;
		str(round(m.scfa/m.spoc/1000,0),10)
	?repl('=',62)
	?chr(12)
	STORE 0 TO poc, cfa
*	wait wind str(m.mi)+str(m.mx)
	GO TOP
	r=left(mr,4)
	SCAN ALL
		IF r!=left(mr,4)
			?repl('-',62)
			STORE 0 TO cfa, poc
			r=left(mr,4)
			ENDIF
		poc=m.poc+1
		cfa=m.cfa+vys
		?mr, str(round(m.cfa/m.poc/1000,0),6), ;
			procent(45,m.mx-m.mi,round(m.cfa/m.poc/1000,0)-m.mi)
*		wait wind str(m.mx-m.mi)+str(round(m.cfa/m.poc/1000,0)-m.mi)
		ENDSCAN
	?chr(12)
	DO tiskoff
	USE
	DO tisk WITH 0
	SELE faktury
*	SET FILTER &_ff
RETURN
**	Fakturace dle Ÿinnost¡ nebo Ÿlen…
PROCEDURE sef_clen
	WAIT WIND NOWA "Pracuji ..."
	DO tiskon
	SELE faktury
	SET RELA TO
	SET ORDER TO cis
	DO fak_rada

	PRIV ff, mm, _rc
	ff=filter()
	SELE fakturyp
	SET RELA TO cis INTO faktury
	SET DELE ON
	SET TALK ON
	SELE 0
	SELECT str(year(fa.dtv),4)+'/'+str(month(fa.dtv),2)+fp.cle AS ord, ;
			month(fa.dtv) AS mon, fp.cle AS cle, sum(fp.cas) AS cas ;
		FROM faktury fa, fakturyp fp ;
		INTO CURSOR cur ;
		WHERE fa.cis = fp.cis AND !fa.sto;
		GROUP BY ord
	INDEX ON ord TAG ord
	SET ORDER TO ord
	SET TALK OFF
	SELE fakturyp
	SET RELA TO
	SELE cur
	
	PRIV mr, xr, mm, xm, cc
	GO TOP
	mr=val(left(ord,4))
	mm=val(substr(ord,6,2))
	GO BOTT
	xr=val(left(ord,4))
	xm=val(substr(ord,6,2))
	SELE fclen
	SET ORDER TO clen
	?m.cnon+m.draft
	FOR i=m.mr TO m.xr
		?"Rok/MØs"
		q=0
		SCAN ALL
			??space(5)+fclen.clen
			q=q+1
			ENDSCAN
		??"   Celkem"
		?repl("~",17+q*9)
		FOR j=1 TO 12
			IF m.xr=i AND m.xm<j
				EXIT
				ENDIF
			WAIT WIND NOWA "Zpracov n¡: "+str(i,4)+'/'+str(j,2)
			cc=0
			?str(i,4)+'/'+str(j,2)
			SCAN ALL
				c=lookup(cur.cas,str(i,4)+'/'+str(j,2)+fclen.clen,cur.ord,'ORD')
				??str(c,9)
				cc=m.cc+c
				ENDSCAN
			??str(m.cc,9)
			NEXT j
		cc=0
		?repl("~",17+q*9)
		?"Celkem:"
		SCAN ALL
			SELE cur
			SUM cas FOR left(ord,4)=str(i,4) AND fclen.clen=substr(ord,8) TO c
			??str(c,9)
			cc=m.cc+c
			SELE fclen
			ENDSCAN
		??str(m.cc,9)
		?repl("~",17+q*9)
		?
		?
		NEXT i
	cc=0
	??repl("=",17+q*9)
	?"Celkem:"
	SCAN ALL
		SELE cur
		SUM cas FOR fclen.clen=substr(ord,8) TO c
		??str(c,9)
		cc=m.cc+c
		ENDSCAN
	??str(m.cc,9)
	?repl("=",17+q*9)
	?m.cnoff+chr(12)
	SELE cur
	USE
	DO tiskoff
	WAIT CLEAR
	DO tisk WITH 0
RETURN
**	Sestava jednoho Ÿlena po polo§k ch
PROCEDURE sef_cle1
	PRIV dtod, dtdo
	dtod=ctod("01"+substr(dtoc(gomont(date(),-1)),3))
	dtdo=gomont(dtod,1)-1
	DO jedencle.spr
	IF lastkey()=27
		RETURN
		ENDIF

	DO wpr

	SELE faktury
	SET RELA TO
	SET ORDER TO cis
	SELE fakturyp
	SET ORDER TO cis
	SET RELA TO cis INTO faktury
	SET DELE ON
	SET TALK OFF
	
	PRIV pp, cc, scc, _fc
	STORE 0 TO cc, scc, pp
	_fc="     0"
	DO tiskon
	?space(8)+"Obdob¡: "+dtoc(m.dtod)+' - '+dtoc(m.dtdo)+" pro Ÿlena: "+m.xclen
	?space(8)+repl('=',47)
	?
	?
	SCAN ALL FOR cle==xclen AND betw(faktury.dtv,m.dtod,m.dtdo) AND !faktury.sto
		IF _fc!=faktury.cis
			IF _fc!="     0"
				IF pp>1
*					?space(8)+space(47)+repl('~',12)
					?space(8)+space(42)+"****>"+str(m.cc,12,2)+"   <****"
					ENDIF
				?
				ENDIF
			a=alltrim("¬¡slo: "+faktury.cis+"  Datum: "+ ;
				dtoc(faktury.dtv)+"   "+faktury.o_naz)
			?space(8)+a
			?space(8)+repl('~',len(a))
			_fc=faktury.cis
			cc=0
			pp=0
			ENDIF
		?space(8)+txt,cas
		cc=cc+cas
		pp=pp+1
		scc=m.scc+cas
		ENDSCAN
*	?space(8)+space(47)+repl('~',12)
	?space(8)+space(42)+"****>"+str(m.cc,12,2)+"   <****"
	?
	?
	?space(8)+space(47)+repl('=',12)
	?space(8)+"SouŸet za vçechny faktury: "+space(20)+str(m.scc,12,2)
	?space(8)+space(47)+repl('=',12)
	?m.cnoff+chr(12)

	DO tiskoff
	WAIT CLEAR
	DO tisk WITH 8
	SELE fakturyp
	SET RELA TO
RETURN
**	Skladovky
**	.T. = 1-Vìdejka
**	.F. = 2-Pý¡jemka

PROCEDURE sklad 
PARA vydejka
	PRIV filtrv
	SELE fsklcis
	SET ORDER TO czb  
	SELE fseznam
	SET ORDER TO m_add
	SELE fsklpolp
	SET ORDER TO cis  
	SET RELA TO czb INTO fsklcis
	SELE fsklpol
	SET ORDER TO dtv
	SET RELA TO idkdo INTO fseznam

	IF m.vydejka
		filtrv="left(cis,1)='1'"
	  ELSE
		filtrv="left(cis,1)='2'"
		ENDIF
	__fltr=m.filtrv+' AND right(cis,4)!="0000" AND substr(cis,4,2)=m.radaskl'
	SET FILT TO &__fltr

*	DO skl_rad WITH .F.

	GO BOTT

	PRIV l11, l12, l21, l22
*	l11=[:cis+'³'+dtoc(dtv)+'³ '+fseznam.naz+'     ³ '+cfa+' ³ '+str(cas,10,2)]
*	l21=[:cis+'³'+str(z22,10,2)+'³'+str(d22,10,2)+'³'+str(z_5,10,2)+'³'+str(d_5,10,2)+'³'+str(z_0,10,2)+'³'+str(ost,10,2)]

	l11=[:cis+'³'+dtoc(dtv)+'³ '+fseznam.naz+'     ³ '+cfa+' ³ '+str(cas,10,2)]
	l12="// ¬¡slo   ³   Datum  ³ Klient                        ³ Faktura ³  ¬ stka"
	l21=[:cis+'³'+str(z22,10,2)+'³'+str(d22,10,2)+'³'+str(z_5,10,2)+'³'+str(d_5,10,2)+'³'+str(z_0,10,2)+'³'+str(ost,10,2)]
	l22="//  ¬¡slo  ³  ZD 22%  ³  DPH 22% ³  ZD  5%  ³  DPH  5% ³  ZD  0%  ³  Ostatni"
	l31=[:cis+'³'+str(cas,10,2)+'³³'+m_user+'³'+dtoc(m_date)+'³'+m_time+'³'+m_add]
	l32="//  ¬¡slo  ³  ¬ stka  ³³  ZmØnil  ³  Datum   ³  ¬as   ³  m_add           "

	DO list_c WITH 6,1,10,0, ;
		" Pohyby skladu - "+iif(m.vydejka,"Vìdejky","Pý¡jemky"), ;
		m.l11, m.l12, ;
		m.l21, m.l22, ;
		m.l31, m.l32, ;
		"\ F1-Help  ENTER-Oprava  INS-Nov  "+iif(m.vydejka,"vìdejka","pý¡jemka"),;
		'{Cis ¬¡\<slo, cas ¬ s\<tka, dtv \<Datum vystaven¡}', ;
		'CR#:	DO skl_edi			## ENTER-Prohl¡§en¡/oprava skladovky ',;
		'INS:	DO skl_ins			## INS-Nov  skladovka',;
		'ALT+D#:DO skl_del			## ALT+D-Vìmaz skladovky', ;
		'ALT+F6:DO skl_chod			## ALT+F6-ZmØna klienta skladovky', ;
		iif(m.vydejka, ;
			'ALT+V#:DO stisvyd WITH "V"	## ALT+V-Tisk vìdejky', ;
			'ALT+P#:DO stisvyd WITH "P"	## ALT+P-Tisk pý¡jemky'), ;
		iif(m.vydejka, ;
			'F5#:	DO stisvyd WITH "Z"	## F5-Tisk ZDD',''), ;
		iif(m.vydejka, ;
			'F6#:	DO stisvyd WITH "O"	## F6-Tisk dokladu o pou§it¡',''), ;
		iif(m.vydejka, ;
			'ALT+F#:DO skl_fak			## ALT+F-Vygenerov n¡ faktury','')
		
	SET FILT TO
	SET RELA TO
	SELE fsklpolp
	SET RELA TO
RETURN
PROCEDURE skl_ins
	PRIV _rc

	SELE fsklpol
	_rc=recno()
	SET ORDER TO cis
	SET FILTER TO &filtrv AND substr(cis,4,2)=m.radaskl'
	GO BOTT
	mxc=str(val(cis)+1,9)
	SET FILTER TO &filtrv AND right(cis,4)!="0000" AND substr(cis,4,2)=m.radaskl'
	=gorec(m._rc)

	SELE fseznam
	SET FILT TO del
	SET ORDER TO naz
	GO TOP
	DO list_c WITH -1,-1,10,0,;
        " VìbØr klienta pro novou skladovku Ÿ. "+m.mxc+" ",;
        [:iif(del,' ','>')+fseznam.naz+' ³ '+fseznam.jme+' ³ '+fseznam.ico],;
	        "// N zev firmy               ³  Jm‚no osoby              ³    I¬",;
        "\ F1-Help  ENTER-Nov  skladovka pro klienta  INS-Pýid n¡ klienta",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
        'CR#:	EXIT					## ENTER-Nov  skladovka',;
		'INS:	DO sez_edit WITH .T.	## INS-Pýid n¡ klienta',;
		'F4#:	DO sez_edit WITH .F.	## F4-Prohl¡§en¡/oprava £daj… klienta'
	SET FILT TO
	SET ORDER TO m_add

	SELE fsklpol
	IF m.esc
		RETURN
		ENDIF

	SCATTER MEMVAR BLANK
	cis=m.mxc
	dtv=date()
	IF m.vydejka
		doprava="Pý¡mì"
		sleva=fseznam.sleva
		ENDIF
	idkdo=fseznam.m_add
	m_add=dtos(date())+sys(3)
	INSERT INTO (alias()) FROM MEMVAR
	DO skl_edi
RETURN
PROCEDURE skl_edi
	DO skldovka.spr
*	DO skl_ref WITH cis
	refscr=.T.
RETURN
**	ZmØna odbØratele na ji§ vystaven‚ vìdejce
PROCEDURE skl_chod

	SELE fseznam
	SET FILT TO del
	SET ORDER TO m_add
	IF !seek(fsklpol.idkdo)
		GO TOP
		ENDIF
	SET ORDER TO naz

	DO list_c WITH -1,-1,10,0,;
        " VìbØr klienta pro skladovku ",;
        [:iif(del,' ','>')+fseznam.naz+' ³ '+fseznam.jme+' ³ '+fseznam.ico],;
	        "// N zev firmy               ³  Jm‚no osoby              ³    I¬",;
        "\ F1-Help  ENTER-VìbØr klienta  F7-Tý¡dit podle ...",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
        'CR#:	EXIT					## ENTER-VìbØr klienta',;
		'INS:	DO sez_edit WITH .T.	## INS-Pýid n¡ klienta',;
		'F4#:	DO sez_edit WITH .F.	## F4-Prohl¡§en¡/oprava klienta'
	SET FILT TO
	SET ORDER TO m_add

	SELE fsklpol
	IF m.esc
		RETURN
		ENDIF

	REPL idkdo WITH fseznam.m_add, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()

	refscr=.T.
RETURN
**	Vymaz n¡ vìdejky nebo pý¡jemky

PROCEDURE skl_del 
	SELE fsklpol
	PRIV _rc,_mx,_cis
	rc=recno()
	CALCULATE max(cis) TO _mx		&& FOR &filtrv
	=gorec(m.rc)

	IF cis=m._mx
	  	IF ask(-1,-1,"N","Opravdu smazat skladovku Ÿ. "+cis)
	  		m._cis=cis
			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	  		DELE
	  		SELE fsklpolp
	  		SCAN ALL FOR cis=m._cis
		  		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		  		DELE
		  		ENDSCAN
	  		SELE fsklpol
	  		GO BOTT
*	  		DO skl_ref WITH ""
	  		refscr=.T.
	  		ENDIF
	  ELSE
		WAIT WIND "Skladovku nelze smazat, proto§e nen¡ posledn¡ v ýadØ."
		ENDIF
RETURN
**	Vygenerov n¡ faktury
PROCEDURE skl_fak
*	IF ico="99999999" OR ico="88888888"
*		WAIT WIND "Nelze generovat fakturu pro nesledovan‚ odbØratele."
*		RETURN
*		ENDIF

	SELE faktury
*	IF empty(m.radafakt)
*		WAIT WIND NOWA "Nen¡ specifikov na ýada faktur v kter‚ se m  nov  faktura vydat."
*		RETURN
*		ENDIF


*****	DoŸasnØ je napevno urŸeno sklad1=fakt2  sklad3=fakt3
	DO CASE
	  CASE radaskl='01'
		radafakt='2'
	  CASE radaskl='03'
		radafakt='3'
		ENDCASE

	PRIV mxc
	SET FILT TO
	CALC max(val(cis)) ALL TO mxc FOR substr(faktury.cis,3,1)=m.radafakt

	IF !ask(-1,-1,"N","Pýidat polo§ku k posledn¡ faktuýe Ÿ."+strn(m.mxc))
		mxc=str(m.mxc+1,7)
	
		SELE FAKTURY
		INSERT INTO faktury (ico, idkdo, o_naz, cis, dtv, dtdph, dts, ;
			d_ban, d_uct, k_s, uhr, sto, pred, ;
			m_user, m_date, m_time, m_add) ;
		  VALUES (fseznam.ico, fseznam.m_add, fseznam.naz, m.mxc, fsklpol.dtv, fsklpol.dtv, ;
			fsklpol.dtv+iif(fseznam.splat=-1,m.splatnost,fseznam.splat), ;
			m.gl_ban, m.gl_uct, m.gl_ks, fseznam.uhr, .F., fseznam.sml, ;
			m.gl_usernam, date(), time(), dtos(date())+sys(3))
		INSERT INTO fakturyu (cis, d_naz, d_nazd, d_uli, d_mes, d_psc, d_dic, ;
			o_uli, o_mes, o_psc, o_dic, ;
			m_user, m_date, m_time, m_add) ;
		  VALUES (m.mxc, m.gl_naz, m.gl_nazd, ;
			m.gl_uli, m.gl_mes, m.gl_psc, m.gl_dic, ;
			fseznam.uli, fseznam.mes, fseznam.psc, fseznam.dic, ;
			m.gl_usernam, date(), time(), dtos(date())+sys(3))
	  ELSE
		mxc=str(m.mxc,7)
		ENDIF

	SELE fsklpol
	REPL fsklpol.cfa WITH m.mxc, m_user WITH m.gl_username, ;
		m_date WITH date(), m_time WITH time()
	IF fsklpol.z22!=0
		INSERT INTO fakturyp (cis, txt, cas, cle, ;
			dph, noe, m_user, m_date, m_time, m_add) ;
		  VALUES (m.mxc, "Zbo§¡ dle dodac¡ho listu Ÿ. "+fsklpol.cis, ;
			fsklpol.z22, "----", "22", .T., ;
			m.gl_usernam, date(), time(), dtos(date())+sys(3))
		ENDIF
	IF fsklpol.z_5!=0
		INSERT INTO fakturyp (cis, txt, cas, cle, ;
			dph, noe, m_user, m_date, m_time, m_add) ;
		  VALUES (m.mxc, "Zbo§¡ dle dodac¡ho listu Ÿ. "+fsklpol.cis, ;
			fsklpol.z_5, "----", "_5", .T., ;
			m.gl_usernam, date(), time(), dtos(date())+sys(3))
		ENDIF
	IF fsklpol.z_0!=0
		INSERT INTO fakturyp (cis, txt, cas, cle, ;
			dph, noe, m_user, m_date, m_time, m_add) ;
		  VALUES (m.mxc, "Zbo§¡ dle dodac¡ho listu Ÿ. "+fsklpol.cis, ;
			fsklpol.z_0, "----", "_0", .T., ;
			m.gl_usernam, date(), time(), dtos(date())+sys(3))
		ENDIF
	IF fsklpol.ost!=0
		INSERT INTO fakturyp (cis, txt, cas, cle, ;
			dph, noe, m_user, m_date, m_time, m_add) ;
		  VALUES (m.mxc, "Z lohovan‚ obaly dle dodac¡ho listu Ÿ. "+fsklpol.cis, ;
			fsklpol.ost, "----", "--", .T., ;
			m.gl_usernam, date(), time(), dtos(date())+sys(3))
		ENDIF

	PRIV _wrt
	_wrt=.T.

	SELE faktury
	SET ORDER TO cis
	=seek(m.mxc)

	DO fak_edit

	IF ask(-1,-1,"A","Pýej¡t do fakturace")
		KEYB "{ESC}{LEFTARROW}FF"
		ENDIF

RETURN
**	Tisk vìdejky
**	_co = V-Vìdejka   Z-Zjednoduçenì daåovì doklad   O-Doklad o pou§it¡
PROCEDURE stisvyd 
PARA _co
	PRIV _l, kopie
	kopie=1
	_l=17
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "PoŸet kopi¡: "
	@ 0,14 GET m.kopie SIZE 1,1 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	DO tiskon
	FOR k=1 TO m.kopie
		DO CASE
		  CASE m._co='V'
			DO tis_1vyd
		  CASE m._co='Z'
			DO tis_1zdd
		  CASE m._co='O'
			DO tis_1dop
		  CASE m._co='P'
			DO tis_1pri
			ENDCASE
		ENDFOR	
	DO tiskoff
	SELE fsklpol
	DO tisk WITH 5
RETURN
**	Pomocn  procedura tisku vìdejky
PROCEDURE tis_1vyd

	SELE fsklpol

	PRIV fldph, qqq
	fldph= (z22!=0 OR z_5!=0 OR z_0!=0)

	??space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(5)+"³ Dodavatel:                 ³  V í D E J K A      ÉÍÍÍÍÍÍÍÍÍÍÍ»      ³"
	?space(5)+"³                            ³  DODACÖ LIST Ÿ.:    º "+fsklpol.cis+" º      ³"
	?space(5)+"³ "+m.gl_naz+ 			 "  ³                     ÈÍÍÍÍÍÍÍÍÍÍÍ¼      ³"
	?space(5)+"³ "+m.gl_nazd+			 "  ³  "+fseznam.naz+"             ³"
	?space(5)+"³ "+m.gl_uli+			 "  ³  "+fseznam.uli+"             ³"
	?space(5)+"³ "+m.gl_psc+"  "+m.gl_mes+"    ³  "+fseznam.psc+"  "+fseznam.mes+"     ³"
	?space(5)+"³ DI¬: "+m.gl_dic+  "        ³                                        ³"
	?space(5)+"³                            ³  Datum vystaven¡: "+dtoc(fsklpol.dtv)+"           ³"
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"

	?+m.cnon
	?space(8)+"  éŸtujeme V m:                                                               Cena       Cena    Kus…   Sazba      Cena"
	?space(8)+" ~~~~~~~~~~~~~~~                                                             za kus     balen¡  balen¡   DPH      celkem"
	?
	PRIV i, a, sa5, sa22
	STORE 0 TO sa22, sa5
	i=1
	SELE fsklpolp

	SCAN ALL FOR fsklpol.cis=fsklpolp.cis
		?space(8)+"  "+fsklcis.katc+' '+ ;
			iif(akcsleva=0,left(fsklcis.txt,65), ;
				left(fsklcis.txt,len(fsklcis.txt)-17)+"  Akce: -"+str(akcsleva,2)+"%")+ ;
			" "+str(fsklpolp.jcena/fsklpolp.kus/fsklcis.jkus,10,2)+" "+ ;
			str(fsklpolp.jcena/fsklpolp.kus,10,2)+" "+ ;
			str(fsklpolp.kus,6)+"    "+ ;
			iif(left(fsklpolp.dph,1)!='_',fsklpolp.dph,' '+right(fsklpolp.dph,1))+"%  "+ ;
			str(fsklpolp.jcena,10,2)

		DO CASE
		  CASE fsklcis.dph='_5' AND fsklpolp.akcsleva=0
			sa5=m.sa5+fsklpolp.jcena
		  CASE fsklcis.dph='22' AND fsklpolp.akcsleva=0
			sa22=m.sa22+fsklpolp.jcena
			ENDCASE

		i=m.i+1
		IF i>m.len_pg-16
			?chr(12)+space(112)+"PokraŸov n¡ ..."
			i=1
			ENDIF
		ENDSCAN

	IF fsklpol.sleva!=0
		?
		i=i+1
		IF m.sa5!=0
			a=m.sa5*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+" 5%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		IF m.sa22!=0
			a=m.sa22*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+"22%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		ENDIF

	PRIV a
	a=iif(fsklpol.z22 != 0,1,0)+iif(fsklpol.z_5 != 0,1,0)+ ;
			iif(fsklpol.z_0 != 0,1,0)+iif(fsklpol.ost != 0,1,0)

* wait wind "I:"+strn(i)+"  a:"+strn(a)+"  36-a:"+strn(36-a)

	IF i>36-a
		?chr(12)+space(112)+"PokraŸov n¡ ..."
		?
		ENDIF

	FOR j=i TO 36-a
		?
		ENDFOR

	SELE fsklpol
	?m.cnoff+space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	IF fsklpol.z22 != 0
		?space(5)+"³ Z klad danØ 22%    "+str(fsklpol.z22,10,2)+"   DPH 22%  "+str(fsklpol.d22,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_5 != 0
		?space(5)+"³ Z klad danØ  5%    "+str(fsklpol.z_5,10,2)+"   DPH  5%  "+str(fsklpol.d_5,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_0 != 0
		?space(5)+"³ Osvobozeno od DPH  "+str(fsklpol.z_0,10,2)+"                                       ³"
		ENDIF
	IF fsklpol.ost != 0
		?space(5)+"³ Nepodl‚h  DPH      "+str(fsklpol.ost,10,2)+"                                       ³"
		ENDIF
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³                                                   ÉÍÍÍÍÍÍÍÍÍÍÍÍ»    ³"
	?space(5)+"³                                           Celkem: º "+str(fsklpol.cas,10,2)+" º KŸ ³"
	?space(5)+"³                                                   ÈÍÍÍÍÍÍÍÍÍÍÍÍ¼    ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³ Zbo§¡ pýevzal:                                                      ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³ dne:                                                                ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³                                                       Vystavil      ³"
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.draft+"    "
	?chr(12)
RETURN
**	Pomocn  procedura tisku pý¡jemky
PROCEDURE tis_1pri

	SELE fsklpol

	PRIV fldph, qqq
	fldph= (z22!=0 OR z_5!=0 OR z_0!=0)

*	IF ico="99999999" OR ico="88888888"
*		??space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
*		?space(5)+"³ Dodavatel:                 ³                                        ³"
*		?space(5)+"³                            ³                     ÉÍÍÍÍÍÍÍÍÍÍÍ»      ³"
*		?space(5)+"³ Vratn‚ obaly               ³  P ü Ö J E M K A    º "+fsklpol.cis+" º      ³"
*		?space(5)+"³                            ³                     ÈÍÍÍÍÍÍÍÍÍÍÍ¼      ³"
*		?space(5)+"³                            ³                                        ³"
*		?space(5)+"³                            ³                                        ³"
*		?space(5)+"³                            ³  Datum vystaven¡: "+dtoc(fsklpol.dtv)+"           ³"
*		?space(5)+"³                            ³                                        ³"
*		?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
*	  ELSE
		??space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		?space(5)+"³ Dodavatel:                 ³                     ÉÍÍÍÍÍÍÍÍÍÍÍ»      ³"
		?space(5)+"³                            ³  P ü Ö J E M K A    º "+fsklpol.cis+" º      ³"
		?space(5)+"³ "+fseznam.naz+ 			 "  ³                     ÈÍÍÍÍÍÍÍÍÍÍÍ¼      ³"
		?space(5)+"³                            ³                                        ³"
		?space(5)+"³ "+fseznam.uli+			 "  ³                                        ³"
		?space(5)+"³ "+fseznam.mes+"  ³                                        ³"
		?space(5)+"³                            ³                                        ³"
		?space(5)+"³                            ³  Datum vystaven¡: "+dtoc(fsklpol.dtv)+"           ³"
		?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
*		ENDIF
	?+m.cnon
	?space(8)+"  Pýijato na sklad:                                                           Cena       Cena    Kus…   Sazba      Cena"
	?space(8)+" ~~~~~~~~~~~~~~~~~~~                                                         za kus     balen¡  balen¡   DPH      celkem"
	?
	PRIV i, a, sa5, sa22
	STORE 0 TO sa22, sa5
	i=1
	SELE fsklpolp
	SCAN ALL FOR fsklpol.cis=fsklpolp.cis
		?space(8)+"  "+fsklcis.katc+' '+left(fsklcis.txt,65)+" "+ ;
			str(fsklpolp.jcena/fsklpolp.kus/fsklcis.jkus,10,2)+" "+ ;
			str(fsklpolp.jcena/fsklpolp.kus,10,2)+" "+ ;
			str(fsklpolp.kus,6)+"    "+ ;
			iif(left(fsklpolp.dph,1)!='_',fsklpolp.dph,' '+right(fsklpolp.dph,1))+"%  "+ ;
			str(fsklpolp.jcena,10,2)

		DO CASE
		  CASE fsklcis.dph='_5' AND fsklpolp.akcsleva=0
			sa5=m.sa5+fsklpolp.jcena
		  CASE fsklcis.dph='22' AND fsklpolp.akcsleva=0
			sa22=m.sa22+fsklpolp.jcena
			ENDCASE

		i=m.i+1
		IF i>m.len_pg-16
			?chr(12)+space(112)+"PokraŸov n¡ ..."
			i=1
			ENDIF
		ENDSCAN

	IF fsklpol.sleva!=0
		?
		i=i+1
		IF m.sa5!=0
			a=m.sa5*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+" 5%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		IF m.sa22!=0
			a=m.sa22*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+"22%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		ENDIF

	PRIV a
	a=iif(fsklpol.z22 != 0,1,0)+iif(fsklpol.z_5 != 0,1,0)+ ;
			iif(fsklpol.z_0 != 0,1,0)+iif(fsklpol.ost != 0,1,0)

	IF i>36-a
		?chr(12)+space(112)+"PokraŸov n¡ ..."
		?
		ENDIF

	FOR j=i TO 36-a
		?
		ENDFOR

	SELE fsklpol
	?m.cnoff+space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	IF fsklpol.z22 != 0
		?space(5)+"³ Z klad danØ 22%    "+str(fsklpol.z22,10,2)+"   DPH 22%  "+str(fsklpol.d22,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_5 != 0
		?space(5)+"³ Z klad danØ  5%    "+str(fsklpol.z_5,10,2)+"   DPH  5%  "+str(fsklpol.d_5,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_0 != 0
		?space(5)+"³ Osvobozeno od DPH  "+str(fsklpol.z_0,10,2)+"                                       ³"
		ENDIF
	IF fsklpol.ost != 0
		?space(5)+"³ Nepodl‚h  DPH      "+str(fsklpol.ost,10,2)+"                                       ³"
		ENDIF
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³                                                   ÉÍÍÍÍÍÍÍÍÍÍÍÍ»    ³"
	?space(5)+"³                                           Celkem: º "+str(fsklpol.cas,10,2)+" º KŸ ³"
	?space(5)+"³                                                   ÈÍÍÍÍÍÍÍÍÍÍÍÍ¼    ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³ Zbo§¡ pýevzal:                                                      ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³ dne:                                                                ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³                                                       Vystavil      ³"
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.draft+"    "
	?chr(12)
RETURN
**	Tisk jednoho zjednoduçen‚ho daåov‚ho dokladu
PROCEDURE tis_1zdd
	SELE fsklpol

	PRIV fldph
	fldph= (z22!=0 OR z_5!=0 OR z_0!=0)

*	IF ico="99999999" OR ico="88888888"
*		??space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
*		?space(5)+"³ Dodavatel:                 ³                                        ³"
*		?space(5)+"³                            ³  Zjednoduçenì       ÉÍÍÍÍÍÍÍÍÍÍÍ»      ³"
*		?space(5)+"³ "+m.gl_naz+ 			 "  ³  daåovì doklad Ÿ.   º "+fsklpol.cis+" º      ³"
*		?space(5)+"³ "+m.gl_nazd+			 "  ³                     ÈÍÍÍÍÍÍÍÍÍÍÍ¼      ³"
*		?space(5)+"³ "+m.gl_uli+			 "  ³                                        ³"
*		?space(5)+"³ "+m.gl_psc+"  "+m.gl_mes+"    ³                                        ³"
*		?space(5)+"³ DI¬: "+m.gl_dic+  "        ³  Datum vystaven¡: "+dtoc(fsklpol.dtv)+"           ³"
*		?space(5)+"³                            ³                                        ³"
*		?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
*	  ELSE
		??space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		?space(5)+"³ Dodavatel:                 ³  Zjednoduçenì       ÉÍÍÍÍÍÍÍÍÍÍÍ»      ³"
		?space(5)+"³                            ³  daåovì doklad Ÿ.   º "+fsklpol.cis+" º      ³"
		?space(5)+"³ "+m.gl_naz+ 			 "  ³                     ÈÍÍÍÍÍÍÍÍÍÍÍ¼      ³"
		?space(5)+"³ "+m.gl_nazd+			 "  ³  "+fseznam.naz+"             ³"
		?space(5)+"³ "+m.gl_uli+			 "  ³  "+fseznam.uli+"             ³"
		?space(5)+"³ "+m.gl_psc+"  "+m.gl_mes+"    ³  "+fseznam.psc+"  "+fseznam.mes+"     ³"
		?space(5)+"³ DI¬: "+m.gl_dic+  "        ³                                        ³"
		?space(5)+"³                            ³  Datum vystaven¡: "+dtoc(fsklpol.dtv)+"           ³"
		?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
*		ENDIF
	?+m.cnon
	?space(8)+"  éŸtujeme V m:                                                               Cena       Cena    Kus…   Sazba      Cena"
	?space(8)+" ~~~~~~~~~~~~~~~                                                             za kus     balen¡  balen¡   DPH      celkem"
	?
	PRIV i, a, sa5, sa22
	STORE 0 TO sa22, sa5
	i=1
	SELE fsklpolp
	SCAN ALL FOR fsklpol.cis=fsklpolp.cis
		?space(8)+"  "+fsklcis.katc+' '+ ;
			iif(akcsleva=0,left(fsklcis.txt,65), ;
				left(fsklcis.txt,len(fsklcis.txt)-17)+"  Akce: -"+str(akcsleva,2)+"%")+ ;
			" "+ ;
			str(fsklpolp.jcena/fsklpolp.kus/fsklcis.jkus,10,2)+" "+ ;
			str(fsklpolp.jcena/fsklpolp.kus,10,2)+" "+ ;
			str(fsklpolp.kus,6)+"    "+ ;
			iif(left(fsklpolp.dph,1)!='_',fsklpolp.dph,' '+right(fsklpolp.dph,1))+"%  "+ ;
			str(fsklpolp.jcena,10,2)
		DO CASE
		  CASE fsklcis.dph='_5' AND fsklpolp.akcsleva=0
			sa5=m.sa5+fsklpolp.jcena
		  CASE fsklcis.dph='22' AND fsklpolp.akcsleva=0
			sa22=m.sa22+fsklpolp.jcena
			ENDCASE
		i=m.i+1
		IF i>m.len_pg-16
			?chr(12)+space(112)+"PokraŸov n¡ ..."
			i=1
			ENDIF
		ENDSCAN

	IF fsklpol.sleva!=0
		?
		i=i+1
		IF m.sa5!=0
			a=m.sa5*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+" 5%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		IF m.sa22!=0
			a=m.sa22*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+"22%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		ENDIF

	a=iif(fsklpol.z22 != 0,1,0)+iif(fsklpol.z_5 != 0,1,0)+ ;
			iif(fsklpol.z_0 != 0,1,0)+iif(fsklpol.ost != 0,1,0)+ ;
			iif(int(fsklpol.cas*10)!=fsklpol.cas*10,2,0)

	IF i>36-a
		?chr(12)+space(112)+"PokraŸov n¡ ..."
		?
		ENDIF

	FOR j=i TO 36-a
		?
		ENDFOR

	SELE fsklpol
	?m.cnoff+space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	IF fsklpol.z22 != 0
		?space(5)+"³ Z klad danØ 22%    "+str(fsklpol.z22,10,2)+"   DPH 22%  "+str(fsklpol.d22,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_5 != 0
		?space(5)+"³ Z klad danØ  5%    "+str(fsklpol.z_5,10,2)+"   DPH  5%  "+str(fsklpol.d_5,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_0 != 0
		?space(5)+"³ Osvobozeno od DPH  "+str(fsklpol.z_0,10,2)+"                                       ³"
		ENDIF
	IF fsklpol.ost != 0
		?space(5)+"³ Nepodl‚h  DPH      "+str(fsklpol.ost,10,2)+"                                       ³"
		ENDIF
	?space(5)+	  "³"+space(69)+"³"
	IF int(fsklpol.cas*10)!=fsklpol.cas*10
		?space(5)+"³                                 Celkem:             "+str(fsklpol.cas,10,2)+"   KŸ ³"
		a=int(fsklpol.cas*10+1)/10
		?space(5)+"³                                 Hal‚ýov‚ vyrovn n¡: "+str(m.a-fsklpol.cas,10,2)+"   KŸ ³"
		?space(5)+"³                                                   ÉÍÍÍÍÍÍÍÍÍÍÍÍ»    ³"
		?space(5)+"³                                           Celkem: º "+str(m.a,10,2)+" º KŸ ³"
		?space(5)+"³                                                   ÈÍÍÍÍÍÍÍÍÍÍÍÍ¼    ³"
	  ELSE
		?space(5)+"³                                                   ÉÍÍÍÍÍÍÍÍÍÍÍÍ»    ³"
		?space(5)+"³                                           Celkem: º "+str(fsklpol.cas,10,2)+" º KŸ ³"
		?space(5)+"³                                                   ÈÍÍÍÍÍÍÍÍÍÍÍÍ¼    ³"
		ENDIF
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³ Zaplaceno hotovØ                                                    ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³ Hotovost pýevzal dne:                                               ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³                                                       Vystavil      ³"
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.draft+"    "
	?chr(12)
RETURN
**	Tisk jednoho dokladu o pou§it¡
PROCEDURE tis_1dop
	SELE fsklpol

	PRIV fldph
	fldph= (z22!=0 OR z_5!=0 OR z_0!=0)

		??space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		?space(5)+"³ Dodavatel:                 ³                     ÉÍÍÍÍÍÍÍÍÍÍÍ»      ³"
		?space(5)+"³                            ³  Doklad o pou§it¡   º "+fsklpol.cis+" º      ³"
		?space(5)+"³ "+m.gl_naz+ 			 "  ³                     ÈÍÍÍÍÍÍÍÍÍÍÍ¼      ³"
		?space(5)+"³ "+m.gl_nazd+			 "  ³  "+fseznam.naz+"             ³"
		?space(5)+"³ "+m.gl_uli+			 "  ³  "+fseznam.uli+"             ³"
		?space(5)+"³ "+m.gl_psc+"  "+m.gl_mes+"    ³  "+fseznam.psc+"  "+fseznam.mes+"     ³"
		?space(5)+"³ DI¬: "+m.gl_dic+  "        ³                                        ³"
		?space(5)+"³                            ³  Datum vystaven¡: "+dtoc(fsklpol.dtv)+"           ³"
		?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"

	?+m.cnon
	?space(8)+"  Seznam polo§ek:                                                             Cena       Cena    Kus…   Sazba      Cena"
	?space(8)+" ~~~~~~~~~~~~~~~~~                                                           za kus     balen¡  balen¡   DPH      celkem"
	?

	PRIV i, a, sa5, sa22
	STORE 0 TO sa22, sa5
	i=1
	SELE fsklpolp

	SCAN ALL FOR fsklpol.cis=fsklpolp.cis
		?space(8)+"  "+fsklcis.katc+' '+ ;
			iif(akcsleva=0,left(fsklcis.txt,65), ;
				left(fsklcis.txt,len(fsklcis.txt)-17)+"  Akce: -"+str(akcsleva,2)+"%")+ ;
			" "+str(fsklpolp.jcena/fsklpolp.kus/fsklcis.jkus,10,2)+" "+ ;
			str(fsklpolp.jcena/fsklpolp.kus,10,2)+" "+ ;
			str(fsklpolp.kus,6)+"    "+ ;
			iif(left(fsklpolp.dph,1)!='_',fsklpolp.dph,' '+right(fsklpolp.dph,1))+"%  "+ ;
			str(fsklpolp.jcena,10,2)

		DO CASE
		  CASE fsklcis.dph='_5' AND fsklpolp.akcsleva=0
			sa5=m.sa5+fsklpolp.jcena
		  CASE fsklcis.dph='22' AND fsklpolp.akcsleva=0
			sa22=m.sa22+fsklpolp.jcena
			ENDCASE
		i=m.i+1
		IF i>m.len_pg-16
			?chr(12)+space(112)+"PokraŸov n¡ ..."
			i=1
			ENDIF
		ENDSCAN

	IF fsklpol.sleva!=0
		?
		i=i+1
		IF m.sa5!=0
			a=m.sa5*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+" 5%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		IF m.sa22!=0
			a=m.sa22*(fsklpol.sleva/-100)
			?space(8)+"  Sleva "+str(fsklpol.sleva,2)+ ;
				"% z polo§ek bez akŸn¡ch slev"+space(67)+"22%  "+str(m.a,10,2)+"  "
			i=i+1
			ENDIF
		ENDIF

	a=iif(fsklpol.z22 != 0,1,0)+iif(fsklpol.z_5 != 0,1,0)+ ;
			iif(fsklpol.z_0 != 0,1,0)+iif(fsklpol.ost != 0,1,0)+ ;
			iif(int(fsklpol.cas*10)!=fsklpol.cas*10,2,0)

	IF i>36-a
		?chr(12)+space(112)+"PokraŸov n¡ ..."
		?
		ENDIF

	FOR j=i TO 36-a
		?
		ENDFOR

	SELE fsklpol
	?m.cnoff+space(5)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	IF fsklpol.z22 != 0
		?space(5)+"³ Z klad danØ 22%    "+str(fsklpol.z22,10,2)+"   DPH 22%  "+str(fsklpol.d22,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_5 != 0
		?space(5)+"³ Z klad danØ  5%    "+str(fsklpol.z_5,10,2)+"   DPH  5%  "+str(fsklpol.d_5,10,2)+"                 ³"
		ENDIF
	IF fsklpol.z_0 != 0
		?space(5)+"³ Osvobozeno od DPH  "+str(fsklpol.z_0,10,2)+"                                       ³"
		ENDIF
	IF fsklpol.ost != 0
		?space(5)+"³ Nepodl‚h  DPH      "+str(fsklpol.ost,10,2)+"                                       ³"
		ENDIF
	?space(5)+	  "³"+space(69)+"³"
	?space(5)+"³                                 Celkem:             "+str(fsklpol.cas,10,2)+"   KŸ ³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"³"+space(69)+"³"
	?space(5)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"+m.draft+"    "
	?chr(12)
RETURN
PROCEDURE skl_pol
	PRIV _fi,_ord
	SELE fsklpolp
	_fi=filter()
	_ord=order()
	SET FILT TO fsklpol.cis==fsklpolp.cis
	SET ORDER TO
	GO TOP
	DO list_c WITH 4,0,13,13,;
		iif(m.vydejka," Vìdejka "," Pý¡jemka "), ;
		[:fsklcis.katc+' '+left(fsklcis.txt,37)+'³'+str(kus,6)+'³'+str(jcena/kus,10,2)+'³'+fsklcis.dph+'%'+'³'+str(jcena,10,2)],;
		"//Kat¬  N zev polo§ky                       ³ Kusy ³  KŸ/kus  ³DPH³ Celkem   ",;
		[:cis+'³'+str(jcena,10,2)+'³³'+m_user+'³'+dtoc(m_date)+'³'+m_time+'³'+m_add],;
		"//  ¬¡slo  ³  ¬ stka  ³³  ZmØnil  ³  Datum   ³  ¬as   ³  m_add           ",;
		"\ F1-Help  ENTER-Oprava  INS-Pýid n¡ polo§ky",;
		'ALT+J:	DO spol_jc				## ALT+J-Oprava jednotkov‚ ceny ',;
		'INS:	DO spol_edi WITH .T.	## INS-Pýid n¡ polo§ky',;
		'CR#:	DO spol_edi WITH .F.	## ENTER-Oprava polo§ky ',;
		'ALT+V#:DO spol_vrt				## ALT+V-PýepoŸ¡t n¡ vratnìch obal… ',;
		'ALT+D#:DO spol_del 			## ALT+D-Smaz n¡ polo§ky',;
		'Ctrl+CR:EXIT'

	IF fl_modip AND ask(-1,-1,"N","PýepoŸ¡tat vratn‚ obaly")
		DO spol_vrt
		ENDIF

	SET FILT TO &_fi
	SET ORDER TO &_ord
	SELE fsklpol
RETURN
**	Oprava jednotkov‚ ceny
PROCEDURE spol_jc 

	PRIV _l, _r, jc
	jc=jcena/kus/fsklcis.jkus
	_l=70
	_r=4
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._r-1))/2), INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-(m._r-1))/2)+m._r, INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY left(fsklcis.txt,68)
	@ 2,1 SAY "Jedn. cena : "
	@ 2,14 GET m.jc SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	REPL jcena WITH m.jc*kus*fsklcis.jkus, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	orivar=""
	fl_modip=.T.
	DO ref_skl
	refscr=.T.
RETURN
**  PýepoŸ¡t n¡ polo§ek vìdejky
PROCEDURE spol_vrt
	PRIV _cis, _rc, _kus, _rcc
	_cis=cis

	_rcc=recno()

	SET ORDER TO cis
	=seek(m._cis)
	SCAN REST FOR _cis=cis
		IF fsklcis.vratobal="Ano" 		&&	AND kus>=0
			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			DELE
			ENDIF
		ENDSCAN

	PRIV m_user, m_date, m_time, m_add
	=seek(m._cis)
	SCAN REST FOR _cis=cis
		_rc=recno()
		_kus=kus
		SELE fsklcis
		SCATTER MEMVAR
		SELE fsklpolp
		FOR i=1 TO 4
			x=str(i,1)
			IF !empty(m.czb&x)
				SCAN REST FOR m._cis=cis
					IF m.czb&x=czb AND kus>=0
						REPL kus WITH kus+m.kus&x*m._kus, ;
							m_user WITH m.gl_usernam, m_date WITH date(), ;
							m_time WITH time()
						EXIT
						ENDIF
					ENDSCAN
				IF eof() OR m._cis!=cis
					INSERT INTO fsklpolp (cis, czb, kus, dph, m_user, m_date, m_time, m_add) ;
						VALUES (m._cis, m.czb&x., m.kus&x.*m._kus, '--', ;
							m.gl_usernam, date(), time(), dtos(date())+sys(3))
					ENDIF
				ENDIF
			ENDFOR
		=gorec(m._rc)
		ENDSCAN

	=seek(m._cis)
	SCAN REST FOR m._cis=cis
		IF kus=0
			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			DELE
		  ELSE
			IF fsklcis.vratobal="Ano"
				REPL jcena WITH iif(left(cis,1)="1",fsklcis.cas,fsklcis.nakup)*kus
				ENDIF
			ENDIF
		ENDSCAN

	=gorec(m._rcc)

	DO ref_skl

	refscr=.T.
	orivar=""
	fl_modip=.T.
RETURN
**	Editace 1 polo§ky skladovky
PROCEDURE spol_edi
PARA app

	SELE fsklpolp
	SET ORDER TO dph

	DO l_sklcis WITH czb

	IF m.esc
		RETURN
		ENDIF

	PRIV m_user, m_date, m_time, m_add, rc
	orivar=""
	fl_modip=.T.
	IF m.app
		SCATTER MEMVAR BLANK
		cis=fsklpol.cis
		akcsleva=fsklcis.akcsleva
	  ELSE
		SCATTER MEMVAR
		ENDIF

	czb=fsklcis.czb
	dph=fsklcis.dph
	jczb=fsklcis.jczb

	SELE fsklcis
	rc=recno()
	SET ORDER TO czb
	=seek(m.jczb)
	IF eof()
		WAIT WIND "Nenalezeno jednotkov‚ mno§stv¡ zadan‚ho zbo§¡."
		RETURN
		ENDIF
	
	SCATTER FIELDS ncena, nsleva, nspdan, nclo, njine, nakup, stav MEMVAR	
	=gorec(m.rc)

	SELE fsklpolp

	IF m.vydejka
		PRIV _l
		_l=72
		DEFINE WINDOW w_ask ;
			FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
			TO INT((SROW()-3)/2)+4,INT((SCOL()-(m._l-1))/2)+m._l ;
			NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
		ACTIVATE WINDOW w_ask SAME
		@ 0,1 SAY left(fsklcis.txt,70)
		@ 2,1 SAY "PoŸet kus…: "
		@ 2,13 GET m.kus SIZE 1,6 COLOR SCHEME 4
		@ 2,20 SAY "AkŸn¡ sleva:   %"
		@ 2,34 GET m.akcsleva SIZE 1,2 COLOR SCHEME 4
		READ MODAL
		RELEASE WINDOW w_ask

		jcena=(fsklcis.cas*m.kus)*(1-m.akcsleva/100)

		esc= lastkey()==27
	  ELSE
		akcsleva=0
		DO prijemka.spr
	
		jcena=m.nakup*fsklcis.jkus*m.kus
		ENDIF

	IF m.esc OR m.kus=0
		RETURN
		ENDIF

*	IF m.kus<0 AND fsklcis.vratobal="Ne "
*		WAIT WIND NOWA "Vr cen‚ zbo§¡ se m…§e naskladåovat pouze pý¡jemkou."
*		RETURN
*		ENDIF

	m_user=m.gl_usernam
	m_date=date()
	m_time=time()
	IF m.app
		m_add=dtos(date())+sys(3)
		INSERT INTO (alias()) FROM MEMVAR
	  ELSE
		GATHER MEMVAR
		ENDIF

	rc=recno()

	DO ref_skl

	refscr=.T.
	=gorec(m.rc)
RETURN

PROCEDURE spol_del
	REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DELE
	orivar=""
	fl_modip=.T.
	DO ref_skl
	refscr=.T.
RETURN

**	Obnoven¡ obrazovky a dat skladovky
PROCEDURE ref_skl 
	PRIV _se, _rc, i, ord, f
	_se=sele()
	SELE fsklpolp
	_rc=recno()
	ord=order()
	SET ORDER TO cis
	STORE 0 TO z22,z_5,z_0,ost,z22s,z_5s,z_0s,osts
	=seek(fsklpol.cis)
	SCAN REST FOR fsklpol.cis=fsklpolp.cis
		f=iif(akcsleva=0,"","s")
		DO CASE
		  CASE fsklpolp.dph="22"
			z22&f.=z22&f.+jcena
		  CASE fsklpolp.dph="_5"
			z_5&f.=z_5&f.+jcena
		  CASE fsklpolp.dph="_0"
			z_0=z_0+jcena
		  CASE fsklpolp.dph="--"
			ost=ost+jcena
		  ENDCASE
		ENDSCAN

	**	VìpoŸet slevy a pýipoŸten¡ akŸn¡ch polo§ek
	z22=m.z22*(1-m.sleva/100)+m.z22s
	z_5=m.z_5*(1-m.sleva/100)+m.z_5s

	d22=ceil(m.z22*2.2)/10
	d_5=ceil(m.z_5*.5)/10

	i=1
	SCAN ALL FOR fsklpol.cis=fsklpolp.cis AND m.i<15
		ppp(m.i)=' '+fsklcis.katc+' '+left(fsklcis.txt,37)+'³'+str(kus,6)+'³'+str(jcena/kus,10,2)+'³'+fsklcis.dph+'%'+'³'+str(jcena,10,2)
		i=m.i+1
		ENDSCAN

	FOR j=i TO 14
		ppp(m.j)=space(75)
		NEXT j

	SHOW GETS
	=gorec(m._rc)
	SELE (m._se)
RETURN
PROCEDURE malop

	PRIV z22, z_5, z_0, ost, d22, d_5
	STORE 0 TO z22,z_5,z_0,ost, d22, d_5

	SELE fsklcis
	SET ORDER TO czb

	SELE fmalop
	SET ORDER TO cis
	SET RELA TO czb INTO fsklcis
	GO BOTT

	l11=[:right(cis,4)+'³'+fsklcis.katc+' '+left(fsklcis.txt,32)+'³'+str(kus,6)+'³'+str(jcena/kus,10,2)+'³'+fsklcis.dph+'%'+'³'+str(jcena,10,2)]
	l12="//Ÿ.P.³Kat¬  N zev polo§ky                  ³ Kusy ³  KŸ/kus  ³DPH³ Celkem   "

	b=[||1PF=cases(!empty(cisvyd), "gr/bg", flg AND mod(val(cis),2)=0, "n/bg", flg AND mod(val(cis),2)=1, "b/bg", "0")]

	DO list_c WITH 4,0,13,13,;
		" M A L O P R O D E J ", ;
		m.l11,[|1],m.l12,;
		[:cis+'³'+fsklcis.katc+' '+left(fsklcis.txt,32)+'³'+str(jcena,14,2)+'³'+cisvyd],[|1], ;
		"//    ¬¡slo   ³Kat¬  N zev polo§ky                  ³      ¬ stka  ³¬.vìdejky",;
		[:cis+'³'+str(jcena,13,2)+'³³'+m_user+'³'+dtoc(m_date)+'³'+m_time+'³'+m_add],[|1], ;
		"//    ¬¡slo   ³     ¬ stka  ³³  ZmØnil  ³  Datum   ³  ¬as   ³  m_add           ",;
		b, ;
		"\ F1-Help  ENTER-Oprava  INS-Pýid n¡ polo§ky",;
		'ALT+J:		DO ma_jc			## ALT+J-Oprava jednotkov‚ ceny ',;
		'INS:		DO ma_edi WITH .T.	## INS-Pýid n¡ polo§ky',;
		'CR#:		DO ma_edi WITH .F.	## ENTER-Oprava polo§ky ',;
		'ALT+V#:	DO ma_vrt			## ALT+V-PýepoŸ¡t n¡ vratnìch obal… ',;
		'ALT+D#:	DO ma_del	 		## ALT+D-Smaz n¡ polo§ky',;
		'Ctrl+CR:	DO ma_tisk			## Ctrl+Enter-Tisk a uzavýen¡ paragonu ',;
		'ALT+U:		DO ma_tovyd			## ALT+U-Uzavýen¡ maloprodeje na vìdejku '
RETURN
**	Editace 1 polo§ky paragonu
PROCEDURE ma_edi  
PARA app

	IF !app AND flg AND !ask(-1,-1,"N","Paragon je ji§ uzavýen. Chcete jej pýesto editovat ??!!!??")
		RETURN
		ENDIF

	PRIV m_user, m_date, m_time, m_add, rc, rcx

	orivar=""
	fl_modip=.T.
	rc=recno()
	IF m.app
		GO BOTT
		IF flg OR empty(cis)
			IF right(dtos(date()),6)=left(cis,6) AND !empty(cis)
				_cis=left(cis,8)+right("000"+strn(val(right(cis,4))+1),4)
			  ELSE
				_cis=right(dtos(date()),6)+m.radaskl+"0001"
				ENDIF
		  ELSE
			_cis=cis
			ENDIF
		=gorec(m.rc)

		SCATTER MEMVAR BLANK
		cis=m._cis
		akcsleva=fsklcis.akcsleva
	  ELSE
		SCATTER MEMVAR
		ENDIF

	DO l_sklcis WITH czb

	IF m.esc
		RETURN
		ENDIF

	czb=fsklcis.czb
	dph=fsklcis.dph
	jczb=fsklcis.jczb

	SELE fsklcis
	rcx=recno()
	SET ORDER TO czb
	=seek(m.jczb)
	IF eof()
		WAIT WIND "Nenalezeno jednotkov‚ mno§stv¡ zadan‚ho zbo§¡."
		RETURN
		ENDIF
	
	SCATTER FIELDS ncena, nsleva, nspdan, nclo, njine, nakup, stav MEMVAR	
	=gorec(m.rcx)

	SELE fmalop

	PRIV _l
	_l=72
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+4,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY left(fsklcis.txt,70)
	@ 2,1 SAY "PoŸet kus…: "
	@ 2,13 GET m.kus SIZE 1,6 COLOR SCHEME 4
	@ 2,20 SAY "AkŸn¡ sleva:   %"
	@ 2,34 GET m.akcsleva SIZE 1,2 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	jcena=(fsklcis.cas*m.kus)*(1-m.akcsleva/100)

	esc= lastkey()==27

	IF m.esc OR m.kus=0
		RETURN
		ENDIF

	m_user=m.gl_usernam
	m_date=date()
	m_time=time()

	IF m.app
		m_add=dtos(date())+sys(3)
		INSERT INTO (alias()) FROM MEMVAR
	  ELSE
		GATHER MEMVAR
		ENDIF

	rc=recno()

	DO ma_ref

	refscr=.T.
RETURN
**	Oprava jednotkov‚ ceny
PROCEDURE ma_jc 

	IF flg AND !ask(-1,-1,"N","Paragon je ji§ uzavýen. Chcete jej pýesto editovat ??!!!??")
		RETURN
		ENDIF

	PRIV _l, _r, jc
	jc=jcena/kus/fsklcis.jkus
	_l=70
	_r=4
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-(m._r-1))/2), INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-(m._r-1))/2)+m._r, INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY left(fsklcis.txt,68)
	@ 2,1 SAY "Jedn. cena : "
	@ 2,14 GET m.jc SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	REPL jcena WITH m.jc*kus*fsklcis.jkus, ;
		m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DO ma_ref
	refscr=.T.
RETURN
**  PýepoŸ¡t n¡ polo§ek paragonu
PROCEDURE ma_vrt

	IF flg AND !ask(-1,-1,"N","Paragon je ji§ uzavýen. Chcete jej pýesto editovat ??!!!??")
		RETURN
		ENDIF

	PRIV _cis, _rc, _kus, _rcc
	_cis=cis

	_rcc=recno()

	SET ORDER TO cis
	=seek(m._cis)
	SCAN REST FOR _cis=cis
		IF fsklcis.vratobal="Ano" 		&&	AND kus>=0
			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			DELE
			ENDIF
		ENDSCAN

	PRIV m_user, m_date, m_time, m_add
	=seek(m._cis)
	SCAN REST FOR _cis=cis
		_rc=recno()
		_kus=kus
		SELE fsklcis
		SCATTER MEMVAR
		SELE fmalop
		FOR i=1 TO 4
			x=str(i,1)
			IF !empty(m.czb&x)
				SCAN REST FOR m._cis=cis
					IF m.czb&x=czb AND kus>=0
						REPL kus WITH kus+m.kus&x*m._kus, ;
							m_user WITH m.gl_usernam, m_date WITH date(), ;
							m_time WITH time()
						EXIT
						ENDIF
					ENDSCAN
				IF eof() OR m._cis!=cis
					INSERT INTO fmalop (cis, czb, kus, dph, m_user, m_date, m_time, m_add) ;
						VALUES (m._cis, m.czb&x., m.kus&x.*m._kus, '--', ;
							m.gl_usernam, date(), time(), dtos(date())+sys(3))
					ENDIF
				ENDIF
			ENDFOR
		=gorec(m._rc)
		ENDSCAN

	=seek(m._cis)
	SCAN REST FOR m._cis=cis
		IF kus=0
			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			DELE
		  ELSE
			IF fsklcis.vratobal="Ano"
				REPL jcena WITH iif(left(cis,1)="1",fsklcis.cas,fsklcis.nakup)*kus
				ENDIF
			ENDIF
		ENDSCAN

	=gorec(m._rcc)

	DO ma_ref

	refscr=.T.
RETURN
**	Vìmaz polo§ky paragonu
PROCEDURE ma_del
	IF flg AND !ask(-1,-1,"N","Paragon je ji§ uzavýen. Chcete polo§ku pýesto smazat ??!!!??")
		RETURN
		ENDIF

	REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
	DELE
	orivar=""
	DO ma_ref
	refscr=.T.
RETURN

**	Tisk jednoho paragonu

PROCEDURE ma_tisk

	IF flg AND !ask(-1,-1,"N","Paragon je ji§ uzavýen. Chcete jej uzav¡rat znovu ??!!!??")
		RETURN
		ENDIF

	IF ask(-1,-1,"A","PýepoŸ¡tat vratn‚ obaly")
		DO ma_vrt
		ENDIF

	SELE fmalop
	GO BOTT
	_cis=cis

	DO tiskon
   ??space(5)+"Dodavatel:                | Zjednoduçenì daåovì doklad Ÿ: "+cis
	?space(5)+"                          |                Datum vystaven¡: "+ ;
		substr(cis,7,2)+"."+substr(cis,5,2)+"."+left(cis,4)
	?space(5)+""+m.gl_naz+ 			 " | ODB·RATEL:"
	?space(5)+""+m.gl_nazd+			 " |"
	?space(5)+""+m.gl_uli+			 " |"
	?space(5)+""+m.gl_psc+"  "+m.gl_mes+"   |"
	?space(5)+"DI¬: "+m.gl_dic+  "       |"
	?space(5)+"                          |"
	?space(5)+repl('-',74)

	?+m.cnon+space(8)+"Kat.  N zev                                                                       Cena       Cena    Kus…   Sazba      Cena"
	?space(8)+"Ÿ¡slo                                                                            za kus     balen¡  balen¡   DPH      celkem"

	i=1
	=seek(m._cis)

	SCAN REST WHILE m._cis=fmalop.cis
		?space(8)+fsklcis.katc+"  "+ ;
			iif(akcsleva=0,fsklcis.txt, ;
				left(fsklcis.txt,len(fsklcis.txt)-12)+"  Akce: -"+str(akcsleva,2)+"%")+ ;
			" "+ ;
			str(fmalop.jcena/fmalop.kus/fsklcis.jkus,10,2)+" "+ ;
			str(fmalop.jcena/fmalop.kus,10,2)+" "+ ;
			str(fmalop.kus,6)+"    "+ ;
			iif(left(fmalop.dph,1)!='_',fmalop.dph,' '+right(fmalop.dph,1))+"%  "+ ;
			str(fmalop.jcena,10,2)
		i=m.i+1
		ENDSCAN

	PRIV f
	f=ma_ref()

	?space(5)+repl('-',74)
	?m.cnoff+space(5)+"ZD 22%:"+str(m.z22,10,2)+"  DPH 22%:"+str(m.d22,10,2)
	?space(5)+"ZD  5%:"+str(m.z_5,10,2)+"  DPH  5%:"+str(m.d_5,10,2)
	?space(5)+"Osvob.:"+str(m.z_0,10,2)+"  Bez DPH:"+str(m.ost,10,2)
	?space(5)+"        ÉÍÍÍÍÍÍÍÍÍÍÍÍ»"
	?space(5)+"Celkem: º "+str(round(m.z22+m.z_5+m.z_0+m.ost+m.d22+m.d_5,1),10,2)+" º KŸ"
	?space(5)+"        ÈÍÍÍÍÍÍÍÍÍÍÍÍ¼                                    Vystavil"
	?chr(12)

	DO tiskoff
	DO tisk WITH 5

	WAIT WIND NOWA "C E N A: "+strn(m.f)

	IF ask(-1,-1,"A","VytiçtØno spr vnØ? Uzavý¡t paragon")
		=seek(m._cis)
		REPL REST flg WITH .T., m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		ENDIF

	WAIT WIND NOWA "C E N A: "+strn(m.f)

RETURN
**	Uzavýen¡ maloprodeje a vygenerov n¡ vìdejky
PROCEDURE ma_tovyd

	PRIV vydejka
	vydejka=.T.

	IF !ask(-1,-1,"N","Opravdu prov‚st uz vØrku maloprodeje") OR ;
			!ask(-1,-1,"N","Tato operace je nevratn , pokraŸovat")
		RETURN
		ENDIF

	PRIV _rc, filtrv

	filtrv="left(cis,1)='1'"
	SELE fsklpol
	_rc=recno()
	SET ORDER TO cis
	SET FILTER TO &filtrv AND substr(cis,4,2)=m.radaskl
	GO BOTT
	mxc=str(val(cis)+1,9)
	SET FILTER TO &filtrv AND right(cis,4)!="0000" AND substr(cis,4,2)=m.radaskl'
	=gorec(m._rc)

	SELE fseznam
	SET FILT TO del
	SET ORDER TO naz
	GO TOP
	DO list_c WITH -1,-1,10,0,;
        " Vyber polo§ku pro denn¡ tr§bu ",;
        [:iif(del,' ','>')+fseznam.naz+' ³ '+fseznam.jme+' ³ '+fseznam.ico],;
	        "// N zev firmy               ³  Jm‚no osoby              ³    I¬",;
        "\ F1-Help  ENTER-Nov  skladovka pro klienta  INS-Pýid n¡ klienta",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
        'CR#:	EXIT					## ENTER-VìbØr '
	SET FILT TO
	SET ORDER TO m_add

	SELE fsklpol
	IF m.esc
		SELE fmalop
		RETURN
		ENDIF

	SCATTER MEMVAR BLANK
	cis=m.mxc
	dtv=date()
	IF m.vydejka
		doprava="Pý¡mì"
		sleva=0
		ENDIF
	idkdo=fseznam.m_add
	m_add=dtos(date())+sys(3)
	INSERT INTO (alias()) FROM MEMVAR

	SELE 0
	CREAT CURS se (czb C(8), kus N(6), jcena N(10,2), dph C(2))
	INDEX ON czb TAG czb

	PRIV x, s22, s_5, s_0, s__
	STORE 0 TO s22, s_5, s_0, s__

	SELE fmalop
	SCAN ALL FOR empty(cisvyd) AND flg
		IF !seek(fsklcis.jczb,"se")
			INSERT INTO se VALUES (fsklcis.jczb, fmalop.kus*fsklcis.jkus, ;
				fmalop.jcena, fsklcis.dph)
		  ELSE
			REPL se.kus WITH se.kus+fmalop.kus*fsklcis.jkus, ;
				se.jcena WITH se.jcena+fmalop.jcena, se.dph WITH fsklcis.dph
			ENDIF
		REPL cisvyd WITH m.mxc, ;
			m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		x=fsklcis.dph
		IF x="--"
			x="__"
			ENDIF
		s&x=s&x+jcena
		ENDSCAN

	SELE se
	SCAN ALL FOR se.kus!=0
		INSERT INTO fsklpolp (cis, czb, kus, dph, jcena, akcsleva, ;
			m_user, m_date, m_time, m_add) ;
		  VALUES ;
			(m.mxc, se.czb, se.kus, se.dph, se.jcena, 0, ;
				m.gl_usernam, date(), time(), dtos(date())+sys(3))
		ENDSCAN

	WAIT WIND NOWA "ZD 22%: "+strn(m.s22)+" ZD 5%: "+strn(m.s_5)+ ;
		" ZD 0%: "+strn(m.s_0)+" bez DPH: "+strn(m.s__)

	SELE se
	USE

	KEYB "{ESC}V{ENTER}{ENTER}{ENTER}"

RETURN
**	Obnoven¡ obrazovky a dat skladovky
PROCEDURE ma_ref  

	PRIV f, _rc, _cis
	_rc=recno()

	SET ORDER TO cis
	GO BOTT
	_cis=cis

	STORE 0 TO z22,z_5,z_0,ost

	=seek(m._cis)

	SCAN REST FOR m._cis=fmalop.cis
		DO CASE
		  CASE fmalop.dph="22"
			z22=z22+jcena
		  CASE fmalop.dph="_5"
			z_5=z_5+jcena
		  CASE fmalop.dph="_0"
			z_0=z_0+jcena
		  CASE fmalop.dph="--"
			ost=ost+jcena
		  ENDCASE
		ENDSCAN

	d22=ceil(m.z22*2.2)/10
	d_5=ceil(m.z_5*.5)/10

	=gorec(m._rc)

	f=round(m.z22+m.z_5+m.z_0+m.ost+m.d22+m.d_5,1)

	WAIT WIND NOWA "C E N A: "+strn(m.f)

RETURN m.f
**	Obsluha menu sestav  - SKLAD 1
PROCEDURE m_sest_s

	DEFINE POPUP s_sklad1 FROM 8,35 RELATIVE SHADOW ;
		TITLE " Tiskov‚ sestavy " COLOR SCHEME 4

	DEFINE BAR 10 OF s_sklad1 PROMPT " \<S K L A D" ;
		MESS "Stav skladu, skladov‚ karty, obalov‚ konto, obraty zbo§¡ ..."
	DEFINE BAR 20 OF s_sklad1 PROMPT " \<P ü E H L E D Y" ;
		MESS "Pýehledy odbØr… dodavatel…, obrat… zbo§¡, ..."
	DEFINE BAR 30 OF s_sklad1 PROMPT " \<V í K A Z Y " ;
		MESS "Denn¡ tr§by, sestavy dealer…, ..."
	DEFINE BAR 59 OF s_sklad1 PROMPT "\-"
	DEFINE BAR 99 OF s_sklad1 PROMPT " Posledn¡ sestava " ;
		MESS "Zobrazen¡ posledn¡ vytvoýen‚ sestavy s mo§nost¡ jej¡ opravy."

	ON SELE POPU s_sklad1 DO s_sklad1
	ACTIVATE POPUP s_sklad1

	RELE POPU s_sklad1

RETURN
PROCEDURE s_sklad1
	DO CASE
		CASE bar()=10
			DO ms_sklad
		CASE bar()=20
			DO ms_prehl
		CASE bar()=30
			DO ms_vykaz
		CASE bar()=99
			DO tisk WITH 0,.T.
		ENDCASE
	DO refscr
RETURN
**	Menu sestav  - SKLAD
PROCEDURE ms_sklad

	DEFINE POPUP sestavy FROM 9,40 RELATIVE SHADOW ;
		TITLE " S K L A D " COLOR SCHEME 4

	DEFINE BAR 10 OF sestavy PROMPT " S\<tav skladu - £Ÿetn¡" ;
		MESS "Rekonstrukce skladu k zadan‚mu datu."
	DEFINE BAR 11 OF sestavy PROMPT " \<Inventura skladu" ;
		MESS "Rekonstrukce skladu k zadan‚mu datu."
	DEFINE BAR 12 OF sestavy PROMPT " Skladov  \<karta" ;
		MESS "Skladov  karta jedn‚ polo§ky s pr…bØ§nìm stavem na skladØ."
	DEFINE BAR 13 OF sestavy PROMPT " \<Obalov‚ konto" ;
		MESS "Stav a pohyby vratnìch obal… u jedn‚ firmy."
	DEFINE BAR 14 OF sestavy PROMPT " \<Mno§stevn¡ obraty" ;
		MESS "Mno§stevn¡ obraty u jednotlivìch polo§ek zbo§¡."
	DEFINE BAR 34 OF sestavy PROMPT " Mi\<nim ln¡ mno§stv¡" ;
		MESS "Zbo§¡ pod minim ln¡m mno§stv¡m na skladu."
	DEFINE BAR 59 OF sestavy PROMPT "\-"
	DEFINE BAR 99 OF sestavy PROMPT " Posledn¡ \<sestava " ;
		MESS "Zobrazen¡ posledn¡ vytvoýen‚ sestavy s mo§nost¡ jej¡ opravy."

	ON SELE POPU sestavy DO sestav_s
	ACTIVATE POPUP sestavy

	RELE POPU sestavy
RETURN
**	Menu sestav  - PüEHLEDY
PROCEDURE ms_prehl

	DEFINE POPUP sestavy FROM 9,40 RELATIVE SHADOW ;
		TITLE " P ü E H L E D Y " COLOR SCHEME 4

	DEFINE BAR 17 OF sestavy PROMPT " \<Platebn¡ pýehled odbØratel… (NC)" ;
		MESS "Platebn¡ pýehled odbØratel… (v n kupn¡ch cen ch) vybran‚ho dodavatele."
	DEFINE BAR 19 OF sestavy PROMPT " Obraty vybranìc\<h odbØratel… (PC)" ;
		MESS "Platebn¡ pýehled vybranìch odbØratel… (v prodejn¡ch cen ch)."
	DEFINE BAR 18 OF sestavy PROMPT " Ob\<raty vçech odbØratel…   (PC+M)" ;
		MESS "Obraty odbØratel… (v prodejn¡ch cen ch) + MAR¦E. Bez vratnìch obal…."
	DEFINE BAR 22 OF sestavy PROMPT " N kupy od dodavate\<le     (ks+NC)" ;
		MESS "N kup zbo§¡ od dodavatele v kusech a n kupn¡ch cen ch."
	DEFINE BAR 25 OF sestavy PROMPT " \<Druhy distribuce          (PC+M)" ;
		MESS "N kup zbo§¡ od dodavatele v kusech a n kupn¡ch cen ch."
	DEFINE BAR 59 OF sestavy PROMPT "\-"
	DEFINE BAR 99 OF sestavy PROMPT " Posledn¡ \<sestava " ;
		MESS "Zobrazen¡ posledn¡ vytvoýen‚ sestavy s mo§nost¡ jej¡ opravy."

	ON SELE POPU sestavy DO sestav_s
	ACTIVATE POPUP sestavy

	RELE POPU sestavy

RETURN
**	Menu sestav  - VíKAZY
PROCEDURE ms_vykaz

	DEFINE POPUP sestavy FROM 9,40 RELATIVE SHADOW ;
		TITLE " V í K A Z Y " COLOR SCHEME 4

	DEFINE BAR 15 OF sestavy PROMPT " De\<nn¡ tr§by" ;
		MESS "Denn¡ tr§by dle zadan‚ho obdob¡."
	DEFINE BAR 60 OF sestavy PROMPT " \<Dealeýi - souhrn" ;
		MESS "Zaplacen‚ Ÿ stky pro dealery v souhrnnìch sum ch."
	DEFINE BAR 62 OF sestavy PROMPT " Dealeýi - podro\<bnØ" ;
		MESS "Zaplacen‚ Ÿ stky pro dealery po polo§k ch."
	DEFINE BAR 59 OF sestavy PROMPT "\-"
	DEFINE BAR 99 OF sestavy PROMPT " Posledn¡ \<sestava " ;
		MESS "Zobrazen¡ posledn¡ vytvoýen‚ sestavy s mo§nost¡ jej¡ opravy."

	ON SELE POPU sestavy DO sestav_s
	ACTIVATE POPUP sestavy

	RELE POPU sestavy

RETURN
PROCEDURE sestav_s
	DO CASE
		CASE bar()=10
			DO ses_skl
		CASE bar()=11
			DO ses_iskl
		CASE bar()=12
			DO ses_kart
		CASE bar()=13
			DO ses_obko
		CASE bar()=14
			DO ses_obr
		CASE bar()=15
			DO ses_dtr
		CASE bar()=17
			DO ses_pltp
		CASE bar()=19
			DO ses_sodb
		CASE bar()=18
			DO ses_obod
		CASE bar()=22
			DO ses_odod
		CASE bar()=25
			DO ses_dist
		CASE bar()=34
			DO ses_mins
		CASE bar()=60
			DO ses_deal
		CASE bar()=62
			DO ses_dall
		CASE bar()=99
			DO tisk WITH 0,.T.
		ENDCASE
	DO refscr
RETURN
**	N kupy zbo§¡ od vybran‚ho dodavatele
PROCEDURE ses_odod

* VìbØr dodavatele
	PRIV idkdo, kdonaz

	DO l_klient WITH " "
	IF lastkey()=27
		RETURN
		ENDIF

	idkdo=fseznam.m_add
	kdonaz=fseznam.naz

* Stanoven¡ obdob¡
	SELE fsklpol
*	GO TOP
*	dtod=dtv
*	dtdo=date()
	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

* ¬¡seln¡k polo§ek zbo§¡
	CREA CURS szbozi (jczb C(8), naz C(40), nakup N(10,2), prodej N(10,2), ;
			kusy N(6))
	INDEX ON jczb TAG jczb
	INDEX ON cs(naz) TAG naz
	SET ORDER TO jczb

	SELE fsklcis
	SCAN ALL FOR fsklcis.vratobal="Ne " AND fsklcis.czb=fsklcis.jczb
		INSERT INTO szbozi ;
				VALUES (fsklcis.czb, fsklcis.txt, 0, fsklcis.cas, 0)
		ENDSCAN

	SELE fseznam
	SET ORDER TO m_add

	SELE fsklpol
	SET ORDER TO cis
	SET RELA TO idkdo INTO fseznam

	SELE fsklcis
	SET ORDER TO czb
	SET RELA TO jczb INTO szbozi

	SELE fsklpolp
	SET ORDER TO cis
	SET RELA TO cis INTO fsklpol, czb INTO fsklcis


* Proch zen¡ pý¡jemek

	SCAN ALL FOR left(fsklpolp.cis,1)="2" AND m.idkdo=fseznam.m_add ;
			AND betw(fsklpol.dtv, m.dtod, m.dtdo)
		WAIT WIND NOWA "Pý¡jemka: "+cis

		REPL szbozi.kusy WITH szbozi.kusy+fsklcis.jkus*fsklpolp.kus, ;
			szbozi.nakup WITH szbozi.nakup+fsklpolp.jcena
		ENDSCAN

* Vytvoýen¡ sestavy
	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SELE szbozi
	SET ORDER TO naz

	DO tiskon

	a=" Obraty zbo§¡ od vybran‚ho dodavatele od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?m.cnon+space(10)+a
	?space(10)+repl("~",len(a)+1)

	PRIV skusu, snakup
	STORE 0 TO skusu, snakup
	l=-1
	SCAN ALL
		IF l=-1 OR l=60
			IF l!=-1
				?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
				?chr(12)
				ENDIF
			?space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(10)+"³ N zev zbo§¡                               ³ PoŸet kus… ³ N kup v KŸ ³"
			?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
			l=0
			ENDIF
		skusu=skusu+kusy
		snakup=snakup+nakup
		?space(10)+"³ "+left(naz,40)+"  ³ "+str(kusy,10)+" ³ "+str(nakup,10)+" ³"
		l=l+1
		ENDSCAN
	?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(10)+"³ C E L K E M :                             ³ "+str(skusu,10)+" ³ "+str(snakup,10)+" ³"
	?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	??chr(12)
	DO tiskoff

	USE
	SELE fsklcis
	SET RELA TO
	SELE fsklpol
	SET RELA TO
	SELE fsklpolp
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Obraty vybranìch odbØratel… v prodejn¡ch cen ch
PROCEDURE ses_sodb

* VìbØr dodavatele
	PRIV idkdo, kdonaz
	WAIT WIND NOWA "Vyberte soupis odbØratel…."

	SELE fseznam
	SET FILT TO del
	SET ORDER TO naz
	GO TOP
	DO list_c WITH -1,-1,10,0,;
        " VìbØr klienta ",;
        [:iif(del,' ','>')+iif(flg,'*',' ')+fseznam.naz+' ³ '+fseznam.jme+' ³ '+fseznam.skup],;
	        "//  N zev firmy               ³  Jm‚no osoby              ³ Skupina",;
        "\ F1-Help  ENTER-VìbØr  Ctrl+A-Zruçit vìbØr  F7-Tý¡dit podle ...",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}, skup \<Skupina', ;
        'CR#:		DO _selkl				## ENTER-VìbØr klienta', ;
        'CTRL+A#:	REPL ALL flg WITH .F. 	## CRTL+A-Zruçen¡ cel‚ho vìbØru', ;
        'CTRL+CR#:	EXIT					## CTRL+ENTER-UkonŸen¡ vìbØru'
	SET FILT TO

* Stanoven¡ obdob¡
	SELE fsklpol
*	GO TOP
*	dtod=dtv
*	dtdo=date()

	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

* ¬¡seln¡k polo§ek zbo§¡
	CREA CURS szbozi (jczb C(8), naz C(30), nakup N(10,2), prodej N(10,2))
	INDEX ON jczb TAG jczb

	SELE fsklcis
	SCAN ALL FOR fsklcis.vratobal="Ne " AND fsklcis.jkus=1
		INSERT INTO szbozi ;
				VALUES (fsklcis.czb, fsklcis.txt, fsklcis.nakup, fsklcis.cas)
		ENDSCAN

* Proch zen¡ vìdejek
	CREA CURS sklientu (idkdo C(16), naz c(25), hot N(10,2), do14 N(10,2), ;
		do30 N(10,2), do45 N(10,2), nad45 N(10,2), nakup N(10,2))
	INDEX ON idkdo TAG idkdo
	INDEX ON cs(naz) TAG naz
	SET ORDER TO idkdo

	SELE fseznam
	SET ORDER TO m_add
	SELE fsklcis
	SET ORDER TO czb
	SELE faktury
	SET ORDER TO cis
	SELE fsklpol
	SET RELA TO cfa INTO faktury, idkdo INTO fseznam
	SELE fsklpolp
	SET RELA TO cis INTO fsklpol, czb INTO fsklcis

	SCAN ALL FOR left(fsklpolp.cis,1)="1" AND fseznam.flg ;
			AND betw(fsklpol.dtv, m.dtod, m.dtdo) AND fsklcis.vratobal="Ne "

		WAIT WIND NOWA "Vìdejka: "+cis
		IF !seek(fsklpol.idkdo,"sklientu")
			INSERT INTO sklientu VALUES (fsklpol.idkdo, fseznam.naz, 0, 0, 0, 0, 0, 0)
			ENDIF

		=seek(fsklcis.jczb,"szbozi")
		SELE sklientu
		=seek(fsklpol.idkdo)
		SELE fsklpolp

		nc=fsklcis.jkus*fsklpolp.kus*szbozi.nakup
		pc=iif(fsklpolp.akcsleva=0, ;
			fsklpolp.jcena*((100-fsklpol.sleva)/100), fsklpolp.jcena)


		SELE sklientu
		IF empty(fsklpol.cfa)
			REPL sklientu.hot WITH sklientu.hot+m.pc
		  ELSE
			DO CASE
			  CASE faktury.dts-faktury.dtv<=14
				REPL sklientu.do14 WITH sklientu.do14+m.pc
			  CASE faktury.dts-faktury.dtv<=30
				REPL sklientu.do30 WITH sklientu.do30+m.pc
			  CASE faktury.dts-faktury.dtv<=45
				REPL sklientu.do45 WITH sklientu.do45+m.pc
			  OTHER
				REPL sklientu.nad45 WITH sklientu.nad45+m.pc
				ENDCASE
			ENDIF
		REPL sklientu.nakup WITH sklientu.nakup+m.nc
		SELE fsklpolp
		ENDSCAN

* Vytvoýen¡ sestavy
	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SELE sklientu
	SET ORDER TO naz
	DO tiskon

	a=" Obraty vybranìch odbØratel… od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)+" (v prodejn¡ch cen ch)"
	?m.cnon+space(10)+a
	?space(10)+repl("~",len(a)+1)

	PRIV shot, sdo14, sdo30, sdo45, snad45, smarze
	STORE 0 TO shot, sdo14, sdo30, sdo45, snad45, smarze
	l=-1
	SCAN ALL
		IF l=-1 OR l=60
			IF l!=-1
				?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
				?chr(12)
				ENDIF
			?space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(10)+"³ N zev odbØratele/splatnost ³   HotovØ  ³ do 14 dn¡ ³ do 30 dn¡ ³ do 45 dn¡ ³ nad 45 dn¡³   CELKEM  ³"
			?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			l=0
			ENDIF
		s=hot+do14+do30+do45+nad45
		sm=s-nakup
		shot=shot+hot
		sdo14=sdo14+do14
		sdo30=sdo30+do30
		sdo45=sdo45+do45
		snad45=snad45+nad45
		smarze=smarze+sm
		?space(10)+"³ "+naz+"  ³"+str(hot,10)+" ³"+str(do14,10)+" ³"+str(do30,10)+" ³"+str(do45,10)+" ³"+str(nad45,10)+" ³"+str(s,10)+" ³"
		l=l+1
		ENDSCAN
	s=shot+sdo14+sdo30+sdo45+snad45
	?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(10)+"³ C E L K E M :              ³"+str(shot,10)+" ³"+str(sdo14,10)+" ³"+str(sdo30,10)+" ³"+str(sdo45,10)+" ³"+str(snad45,10)+" ³"+str(s,10)+" ³"
	?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	??chr(12)
	DO tiskoff

	USE
	SELE szbozi
	USE
	SELE fsklpol
	SET RELA TO
	SELE fsklpolp
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Obraty vçech odbØratel… v prodejn¡ch cen ch + mar§e
PROCEDURE ses_obod

* Stanoven¡ obdob¡
	SELE fsklpol
*	GO TOP
*	dtod=dtv
*	dtdo=date()
	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

	DO jsklad

* Proch zen¡ vìdejek
	CREA CURS sklientu (idkdo C(16), naz c(25), hot N(10,2), do14 N(10,2), ;
		do30 N(10,2), do45 N(10,2), nad45 N(10,2), nakup N(10,2))
	INDEX ON idkdo TAG idkdo
	INDEX ON cs(naz) TAG naz
	SET ORDER TO idkdo

	SELE faktury
	SET ORDER TO cis

	SELE fseznam
	SET ORDER TO m_add

	SELE fsklpol
	SET ORDER TO cis
	SET RELA TO cfa INTO faktury, idkdo INTO fseznam

	SELE fsklcis
	SET RELA TO jczb INTO jsklad
	SET ORDER TO czb

	SELE fsklpolp
	SET RELA TO cis INTO fsklpol, czb INTO fsklcis

	SCAN ALL FOR left(fsklpolp.cis,1)="1" AND fsklcis.vratobal="Ne ";
			AND betw(fsklpol.dtv, m.dtod, m.dtdo)
		WAIT WIND NOWA "Vìdejka: "+cis
		IF !seek(fsklpol.idkdo,"sklientu")
			IF fseznam.typ="Klient"
				INSERT INTO sklientu VALUES (fsklpol.idkdo, fseznam.naz, 0, 0, 0, 0, 0, 0)
			  ELSE
				LOOP
				ENDIF
			ENDIF

		SELE sklientu
		=seek(fsklpol.idkdo)

		SELE fsklpolp
		nc=fsklcis.jkus*fsklpolp.kus*jsklad.ncena
		pc=iif(fsklpolp.akcsleva=0, ;
			fsklpolp.jcena*((100-fsklpol.sleva)/100), fsklpolp.jcena)

		IF m.pc=0
			LOOP
			ENDIF

		IF empty(fsklpol.cfa)
			REPL sklientu.hot WITH sklientu.hot+m.pc
		  ELSE
			DO CASE
			  CASE faktury.dts-faktury.dtv<=14
				REPL sklientu.do14 WITH sklientu.do14+m.pc
			  CASE faktury.dts-faktury.dtv<=30
				REPL sklientu.do30 WITH sklientu.do30+m.pc
			  CASE faktury.dts-faktury.dtv<=45
				REPL sklientu.do45 WITH sklientu.do45+m.pc
			  OTHER
				REPL sklientu.nad45 WITH sklientu.nad45+m.pc
				ENDCASE
			ENDIF
		REPL sklientu.nakup WITH sklientu.nakup+m.nc
		ENDSCAN

* Vytvoýen¡ sestavy
	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SELE sklientu
	SET ORDER TO naz

	DO tiskon

	a=" Obraty odbØratel… od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)+" (v prodejn¡ch cen ch)"
	?m.cnon+space(10)+a
	?space(10)+repl("~",len(a)+1)

	PRIV shot, sdo14, sdo30, sdo45, snad45, smarze
	STORE 0 TO shot, sdo14, sdo30, sdo45, snad45, smarze
	l=-1
	SCAN ALL
		IF l=-1 OR l=60
			IF l!=-1
				?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
				?chr(12)
				ENDIF
			?space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(10)+"³ N zev odbØratele/splatnost ³   HotovØ  ³ do 14 dn¡ ³ do 30 dn¡ ³ do 45 dn¡ ³ nad 45 dn¡³   CELKEM  ³ M A R ¦ E ³"
			?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			l=0
			ENDIF
		s=hot+do14+do30+do45+nad45
		sm=s-nakup
		shot=shot+hot
		sdo14=sdo14+do14
		sdo30=sdo30+do30
		sdo45=sdo45+do45
		snad45=snad45+nad45
		smarze=smarze+sm
		?space(10)+"³ "+naz+"  ³"+str(hot,10)+" ³"+str(do14,10)+" ³"+str(do30,10)+" ³"+str(do45,10)+" ³"+str(nad45,10)+" ³"+str(s,10)+" ³"+str(sm,10)+" ³"
		l=l+1
		ENDSCAN
	s=shot+sdo14+sdo30+sdo45+snad45
	?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(10)+"³ C E L K E M :              ³"+str(shot,10)+" ³"+str(sdo14,10)+" ³"+str(sdo30,10)+" ³"+str(sdo45,10)+" ³"+str(snad45,10)+" ³"+str(s,10)+" ³"+str(smarze,10)+" ³"
	?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	??chr(12)
	DO tiskoff

	USE
	SELE jsklad
	USE
	SELE fsklpol
	SET RELA TO
	SELE fsklpolp
	SET RELA TO
	SELE fsklcis
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Platebn¡ pýehled odbØratel… od 1 dodavatele
PROCEDURE ses_pltp

* VìbØr dodavatele
	PRIV idkdo, kdonaz

	DO l_klient WITH " "
	IF lastkey()=27
		RETURN
		ENDIF

	idkdo=fseznam.m_add
	kdonaz=fseznam.naz

* Stanoven¡ obdob¡
	SELE fsklpol
*	GO TOP
*	dtod=dtv
*	dtdo=date()
	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

* ¬¡seln¡k polo§ek od dodavatele a vytvoýen¡ n kupn¡ch cen
	CREA CURS szbozi (jczb C(8), naz C(30), nakup N(10,2), skus N(10), scena N(12,2))
	INDEX ON jczb TAG jczb

	SELE fseznam
	SET ORDER TO m_add

	SELE fsklpol
	SET ORDER TO cis
	SET RELA TO idkdo INTO fseznam

	SELE fsklcis
	SET ORDER TO czb

	SELE fsklpolp
	SET ORDER TO cis
	SET RELA TO cis INTO fsklpol, czb INTO fsklcis

	SCAN ALL FOR left(fsklpolp.cis,1)="2" AND fsklpol.dtv<=m.dtdo AND ;
			fsklpol.idkdo=m.idkdo AND fsklcis.vratobal="Ne "
		IF !seek(fsklcis.jczb,"szbozi")
			INSERT INTO szbozi ;
				VALUES (fsklcis.jczb, fsklcis.jtxt, 0, fsklpolp.kus*fsklcis.jkus, fsklpolp.jcena)
			ENDIF
		ENDSCAN

	SET RELA TO
	SELE szbozi

	REPL ALL nakup WITH scena/skus

*	SET RELA TO jczb INTO fsklcis
*	SCAN ALL
*		REPL nakup WITH fsklcis.nakup
*		IF nakup=0
*			WAIT WIND "Zbo§¡: "+naz+" nem  zad nu n kupn¡ cenu!!!"
*			ENDIF
*		ENDSCAN
*	SET RELA TO

* Proch zen¡ vìdejek
	CREA CURS sklientu (idkdo C(16), naz c(25), hot N(10,2), do14 N(10,2), ;
		do30 N(10,2), do45 N(10,2), nad45 N(10,2))
	INDEX ON idkdo TAG idkdo
	INDEX ON cs(naz) TAG naz
	SET ORDER TO idkdo

	SELE faktury
	SET ORDER TO cis
	SELE fsklpol
	SET RELA TO cfa INTO faktury, idkdo INTO fseznam
	SELE fsklpolp
	SET RELA TO cis INTO fsklpol, czb INTO fsklcis

	SCAN ALL FOR left(fsklpolp.cis,1)="1" AND seek(fsklcis.jczb,"szbozi") ;
			AND betw(fsklpol.dtv, m.dtod, m.dtdo) AND fsklcis.vratobal="Ne "
		WAIT WIND NOWA "Vìdejka: "+cis
		IF !seek(fsklpol.idkdo,"sklientu")
			IF fseznam.typ="Klient"
				INSERT INTO sklientu VALUES (fsklpol.idkdo, fseznam.naz, 0, 0, 0, 0, 0)
			  ELSE
				LOOP
				ENDIF
			ENDIF
		SELE sklientu
		=seek(fsklpol.idkdo)
		SELE fsklpolp

		nc=fsklpolp.kus*szbozi.nakup*fsklcis.jkus

		IF empty(fsklpol.cfa)
			REPL sklientu.hot WITH sklientu.hot+m.nc
		  ELSE
			DO CASE
			  CASE faktury.dts-faktury.dtv<=14
				REPL sklientu.do14 WITH sklientu.do14+m.nc
			  CASE faktury.dts-faktury.dtv<=30
				REPL sklientu.do30 WITH sklientu.do30+m.nc
			  CASE faktury.dts-faktury.dtv<=45
				REPL sklientu.do45 WITH sklientu.do45+m.nc
			  OTHER
				REPL sklientu.nad45 WITH sklientu.nad45+m.nc
				ENDCASE
			ENDIF
		ENDSCAN

* Vytvoýen¡ sestavy
	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SELE sklientu
	SET ORDER TO naz

	DO tiskon

	a=" Platebn¡ pýehled odbØratel… firmy: "+ ;
		alltrim(m.kdonaz)+" od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?m.cnon+space(10)+a
	?space(10)+repl("~",len(a)+1)

	PRIV shot, sdo14, sdo30, sdo45, snad45
	STORE 0 TO shot, sdo14, sdo30, sdo45, snad45
	l=-1
	SCAN ALL
		IF l=-1 OR l=60
			IF l!=-1
				?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
				?chr(12)
				ENDIF
			?space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(10)+"³ N zev odbØratele/splatnost ³   HotovØ  ³ do 14 dn¡ ³ do 30 dn¡ ³ do 45 dn¡ ³ nad 45 dn¡³   CELKEM  ³"
			?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			l=0
			ENDIF
		s=hot+do14+do30+do45+nad45
		shot=shot+hot
		sdo14=sdo14+do14
		sdo30=sdo30+do30
		sdo45=sdo45+do45
		snad45=snad45+nad45
		?space(10)+"³ "+naz+"  ³"+str(hot,10)+" ³"+str(do14,10)+" ³"+str(do30,10)+" ³"+str(do45,10)+" ³"+str(nad45,10)+" ³"+str(s,10)+" ³"
		l=l+1
		ENDSCAN
	s=shot+sdo14+sdo30+sdo45+snad45
	?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(10)+"³ C E L K E M :              ³"+str(shot,10)+" ³"+str(sdo14,10)+" ³"+str(sdo30,10)+" ³"+str(sdo45,10)+" ³"+str(snad45,10)+" ³"+str(s,10)+" ³"
	?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	??chr(12)
	DO tiskoff

	USE
	SELE szbozi
	USE
	SELE fsklpol
	SET RELA TO
	SELE fsklpolp
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Sestava obrat… a mar§e pro jednotliv‚ druhy distribuce
PROCEDURE ses_dist

* Stanoven¡ obdob¡
	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

	DO jsklad

* Proch zen¡ vìdejek
	SELE 0
	CREA CURS sklientu (doprava C(5), hot N(10,2), do14 N(10,2), ;
		do30 N(10,2), do45 N(10,2), nad45 N(10,2), nakup N(10,2))
	INDEX ON cs(doprava) TAG doprava
	SET ORDER TO doprava

	SELE faktury
	SET ORDER TO cis

	SELE fseznam
	SET ORDER TO m_add

	SELE fsklpol
	SET ORDER TO cis
	SET RELA TO cfa INTO faktury, idkdo INTO fseznam

	SELE fsklcis
	SET RELA TO jczb INTO jsklad
	SET ORDER TO czb

	SELE fsklpolp
	SET RELA TO cis INTO fsklpol, czb INTO fsklcis

	SCAN ALL FOR left(fsklpolp.cis,1)="1" AND fsklcis.vratobal="Ne ";
			AND betw(fsklpol.dtv, m.dtod, m.dtdo)
		WAIT WIND NOWA "Vìdejka: "+cis
		IF fseznam.typ!="Klient"
			LOOP
			ENDIF
		IF !seek(cs(fsklpol.doprava),"sklientu")
			INSERT INTO sklientu VALUES (fsklpol.doprava, 0, 0, 0, 0, 0, 0)
			ENDIF

		SELE sklientu
		=seek(cs(fsklpol.doprava))

		SELE fsklpolp
		nc=fsklcis.jkus*fsklpolp.kus*jsklad.ncena
		pc=iif(fsklpolp.akcsleva=0, ;
			fsklpolp.jcena*((100-fsklpol.sleva)/100), fsklpolp.jcena)

		IF empty(fsklpol.cfa)
			REPL sklientu.hot WITH sklientu.hot+m.pc
		  ELSE
			DO CASE
			  CASE faktury.dts-faktury.dtv<=14
				REPL sklientu.do14 WITH sklientu.do14+m.pc
			  CASE faktury.dts-faktury.dtv<=30
				REPL sklientu.do30 WITH sklientu.do30+m.pc
			  CASE faktury.dts-faktury.dtv<=45
				REPL sklientu.do45 WITH sklientu.do45+m.pc
			  OTHER
				REPL sklientu.nad45 WITH sklientu.nad45+m.pc
				ENDCASE
			ENDIF
		REPL sklientu.nakup WITH sklientu.nakup+m.nc
		ENDSCAN

* Vytvoýen¡ sestavy
	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SELE sklientu
	SET ORDER TO doprava

	DO tiskon

	a=" Distribuce zbo§¡ od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?m.cnon+space(10)+a
	?space(10)+repl("~",len(a)+1)

	PRIV shot, sdo14, sdo30, sdo45, snad45, smarze
	STORE 0 TO shot, sdo14, sdo30, sdo45, snad45, smarze
	l=-1
	SCAN ALL
		IF l=-1 OR l=60
			IF l!=-1
				?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
				?chr(12)
				ENDIF
			?space(10)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄ¿"
			?space(10)+"³ Zp…sob distribuce ³   HotovØ  ³ do 14 dn¡ ³ do 30 dn¡ ³ do 45 dn¡ ³ nad 45 dn¡³   CELKEM  ³ M A R ¦ E ³"
			?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
			l=0
			ENDIF
		s=hot+do14+do30+do45+nad45
		sm=s-nakup
		shot=shot+hot
		sdo14=sdo14+do14
		sdo30=sdo30+do30
		sdo45=sdo45+do45
		snad45=snad45+nad45
		smarze=smarze+sm
		?space(10)+"³ "+ ;
			CASES (	doprava="Pý¡mì","Prodej ve skladu", ;
					doprava="Z voz","Vlastn¡ z voz   ", ;
					doprava="Smluv","Smluvn¡ z voz   ", ;
					doprava="Prajz","Prajzler        ", ;
					doprava="Kamio","Rozvoz z kami¢nu", ;
					                "Ostatn¡ zp…sob  ")+ ;
			"  ³"+str(hot,10)+" ³"+str(do14,10)+" ³"+str(do30,10)+ ;
			" ³"+str(do45,10)+" ³"+str(nad45,10)+" ³"+str(s,10)+ ;
			" ³"+str(sm,10)+" ³"
		l=l+1
		ENDSCAN
	s=shot+sdo14+sdo30+sdo45+snad45
	?space(10)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(10)+"³ C E L K E M :     ³"+str(shot,10)+" ³"+str(sdo14,10)+" ³"+str(sdo30,10)+" ³"+str(sdo45,10)+" ³"+str(snad45,10)+" ³"+str(s,10)+" ³"+str(smarze,10)+" ³"
	?space(10)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙ"
	??chr(12)
	DO tiskoff

	USE
	SELE jsklad
	USE
	SELE fsklpol
	SET RELA TO
	SELE fsklpolp
	SET RELA TO
	SELE fsklcis
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	éŸetn¡ hodnota skladu
PROCEDURE ses_skl

	PRIV _l
	dtdo=date()
	_l=24
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Do datumu:"
	@ 0,12 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

WAIT WIND NOWA "Prob¡h  rekonstrukce skladu."


	CREAT CURS cu (txt C(20), czb C(8), stav N(8), ;
		skus N(10), scena N(12,2))
	INDEX ON txt TAG jtxt
	INDEX ON czb TAG czb

	SET ORDER TO czb

	PRIV a, _rc, _cz

	SELE fsklcis
	_rc=recno()
	SET ORDER TO czb
	SET RELA TO jczb INTO cu

	SELE fsklpol
	SET RELA TO
	SET ORDER TO cis

	SELE fsklpolp
	SET FILT TO substr(cis,4,2)=m.radaskl
	SET RELA TO czb INTO fsklcis
	SET RELA TO cis INTO fsklpol ADDI

	SCAN ALL FOR fsklpol.dtv<=m.dtdo
		a=fsklcis.jkus*fsklpolp.kus*iif(asc(cis)=49,-1,1)
		IF eof('cu')
			INSERT INTO cu VALUES (fsklcis.jtxt, fsklcis.jczb, m.a, 0, 0)
		  ELSE
			REPL cu.stav WITH cu.stav+m.a
			ENDIF
		ENDSCAN
	SET RELA TO
	SET FILT TO


**	OdeŸten¡ naplnØnìch lahv¡ od vçech lahv¡ ve skladØ
**		po t‚to proceduýe z…stanou ve vratnìch obalech jenom pr zdn‚ lahve
*
*	WAIT WIND NOWA "PropoŸ¡tavaj¡ se vratn‚ obaly."
*
*	SELE fsklpolp
*	SET RELA TO
*	SELE fsklcis
*	SET RELA TO
*
*	SELE cu
*	SET RELA TO czb INTO fsklcis
*	SCAN ALL FOR fsklcis.vratobal="Ano"
*		_rc=recno()
*		a=stav
*		_cz=czb
*		SCAN ALL FOR fsklcis.jkus=1 AND fsklcis.vratobal="Ne "
*			IF fsklcis.czb1=m._cz OR fsklcis.czb2=m._cz ;
*					OR fsklcis.czb3=m._cz OR fsklcis.czb4=m._cz
*				a=a-stav
*				ENDIF
*			ENDSCAN
*		=gorec(m._rc)
*		REPL stav WITH a
*		ENDSCAN

	SELE cu
	SET ORDER TO czb
	SET RELA TO
	SELE fsklcis
	SET ORDER TO czb
	SET RELA TO jczb INTO cu
	SELE fsklpol
	SET ORDER TO cis
	SELE fsklpolp
	SET RELA TO czb INTO fsklcis, cis INTO fsklpol

* VìpoŸet pr…mØrn‚ n kupn¡ ceny
	REPL cu.scena WITH cu.scena+fsklpolp.jcena, ;
			cu.skus WITH cu.skus+fsklpolp.kus*fsklcis.jkus ;
		FOR left(cis,1)="2" AND fsklpol.dtv<=m.dtdo ;
			AND (fsklpolp.jcena>0 OR (fsklpolp.jcena=0 AND fsklcis.vratobal="Ne "))

	SET RELA TO

	SELE fsklcis
	SET RELA TO
	SELE cu
	SET RELA TO czb INTO fsklcis

	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SET ORDER TO jtxt
	DO tiskon
	a="éŸetn¡ inventura skladu Ÿ."+m.radaskl+" k datu: "+dtoc(m.dtdo)
	?space(6)+m.a
	?space(6)+repl('~',len(a)+2)
	?space(6)+"Kat¬  N zev zbo§¡"+space(24)+"Stav v ks   Stav v KŸ  N kup/ks"
	?
	s=0
	SCAN ALL
	a=0
		IF skus*stav!=0
			a=scena/skus*stav
		  ELSE
			a=0
			IF stav!=0
				WAIT WIND "Zbo§¡: "+left(fsklcis.txt,25)+" nem  naskladåovac¡ ceny."
				ENDIF
			ENDIF
		IF stav!=0
			?space(6)+fsklcis.katc+' '+left(fsklcis.txt,39)+str(stav,6)+str(a,12,2)+str(scena/skus,8,2)
			s=s+a
			ENDIF
		ENDSCAN

	USE
	?space(6)+repl("-",73)
	?space(6)+"C E L K E M :"+space(36)+str(s,13,2)
	?chr(12)
	DO tiskoff

	SELE fsklcis
	=gorec(m._rc)

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Inventura fyzick‚ho stavu skladu
PROCEDURE ses_iskl
	PRIV _l
*	dtdo=date()
	_l=24
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Do datumu:"
	@ 0,12 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

WAIT WIND NOWA "Prob¡h  rekonstrukce skladu."


	CREAT CURS cu (txt C(20), czb C(8), stav N(8), ;
		skus N(10), scena N(12,2))
	INDEX ON txt TAG jtxt
	INDEX ON czb TAG czb

	SET ORDER TO czb

	PRIV a, _rc, _cz

	SELE fsklcis
	_rc=recno()
	SET ORDER TO czb
	SET RELA TO jczb INTO cu

	SELE fsklpol
	SET RELA TO
	SET ORDER TO cis

	SELE fsklpolp
	SET FILT TO substr(cis,4,2)=m.radaskl
	SET RELA TO czb INTO fsklcis
	SET RELA TO cis INTO fsklpol ADDI

	SCAN ALL FOR fsklpol.dtv<=m.dtdo
		a=fsklcis.jkus*fsklpolp.kus*iif(asc(cis)=49,-1,1)
		IF eof('cu')
			INSERT INTO cu VALUES (fsklcis.jtxt, fsklcis.jczb, m.a, 0, 0)
		  ELSE
			REPL cu.stav WITH cu.stav+m.a
			ENDIF
		ENDSCAN
	SET RELA TO
	SET FILT TO


**	OdeŸten¡ naplnØnìch lahv¡ od vçech lahv¡ ve skladØ
**		po t‚to proceduýe z…stanou ve vratnìch obalech jenom pr zdn‚ lahve

	WAIT WIND NOWA "PropoŸ¡tavaj¡ se vratn‚ obaly."

	SELE fsklpolp
	SET RELA TO
	SELE fsklcis
	SET RELA TO

	SELE cu
	SET RELA TO czb INTO fsklcis
	SCAN ALL FOR fsklcis.vratobal="Ano"
		_rc=recno()
		a=stav
		_cz=czb
		SCAN ALL FOR fsklcis.jkus=1 AND fsklcis.vratobal="Ne "
			IF fsklcis.czb1=m._cz OR fsklcis.czb2=m._cz ;
					OR fsklcis.czb3=m._cz OR fsklcis.czb4=m._cz
				a=a-stav
				ENDIF
			ENDSCAN
		=gorec(m._rc)
		REPL stav WITH a
		ENDSCAN

	SELE cu
	SET ORDER TO czb
	SET RELA TO
	SELE fsklcis
	SET ORDER TO czb
	SET RELA TO jczb INTO cu
	SELE fsklpol
	SET ORDER TO cis
	SELE fsklpolp
	SET RELA TO czb INTO fsklcis, cis INTO fsklpol

	REPL cu.scena WITH cu.scena+fsklpolp.jcena, ;
			cu.skus WITH cu.skus+fsklpolp.kus*fsklcis.jkus ;
		FOR left(cis,1)="2" AND fsklpol.dtv<=m.dtdo ;
			AND (fsklpolp.jcena>0 OR (fsklpolp.jcena=0 AND fsklcis.vratobal="Ne "))

	SET RELA TO

	SELE fsklcis
	SET RELA TO
	SELE cu
	SET RELA TO czb INTO fsklcis

	WAIT WIND NOWA "Vytv ý¡ se sestava."

	SET ORDER TO jtxt
	DO tiskon
	a="Inventura fyzick‚ho stavu skladu Ÿ."+m.radaskl+" k datu: "+dtoc(m.dtdo)
	?space(6)+m.a
	?space(6)+repl('~',len(a)+2)
	?space(6)+"Kat¬  N zev zbo§¡"+space(33)+"Stav v ks     Stav v KŸ"
	?

	s=0
	SCAN ALL
		IF skus*stav!=0
			a=scena/skus*stav
		  ELSE
			a=0
			IF stav!=0
				WAIT WIND "Zbo§¡: "+left(fsklcis.txt,25)+" nem  naskladåovac¡ ceny."
				ENDIF
			ENDIF
		IF fsklcis.vratobal="Ano"
			a=0
			ENDIF
		?space(6)+fsklcis.katc+' '+left(fsklcis.txt,44)+str(stav,9)+str(a,13,2)
		s=s+a
		ENDSCAN

	USE
	?space(6)+repl("-",73)
	?space(6)+"C E L K E M :"+space(45)+str(s,13,2)
	?chr(12)
	DO tiskoff

	SELE fsklcis
	=gorec(m._rc)

	WAIT CLEAR

	DO tisk WITH 6
RETURN
PROCEDURE ses_kart

	DO l_sklcis WITH " "

	IF lastkey()=27
		RETURN
		ENDIF

	WAIT WIND NOWA "Prob¡h  rekonstrukce skladov‚ karty ..."

	PRIV cotxt, cocis

	SELE fsklcis
	SET ORDER TO czb
	cotxt=fsklcis.txt
	coczb=fsklcis.czb

	DO tiskon
	a=rtrim(" Skladov  karta: "+m.cotxt)
	?m.cnon+space(6)+m.a
	?space(6)+repl('~',len(a)+2)

	SELE fsklpolp
	SET ORDER TO cis
	SET RELA TO czb INTO fsklcis
	SELE fsklpol
	SET ORDER TO dtv
	SET RELA TO cis INTO fsklpolp

	PRIV stav
	stav=0
	SCAN ALL
		SELE fsklpolp
		SCAN REST FOR fsklcis.jczb=m.coczb WHILE fsklpol.cis=fsklpolp.cis
			stav=stav+kus*fsklcis.jkus*iif(left(fsklpolp.cis,1)='2',1,-1)
			?space(6)+dtoc(fsklpol.dtv)+'  '+fsklpol.cis+'   '+ ;
				iif(left(fsklpolp.cis,1)='2',' ','-')+str(kus,6)+'x  '+ ;
				fsklcis.txt+'  =  '+str(m.stav,6)
			ENDSCAN
		SELE fsklpol
		ENDSCAN

	??chr(12)
	DO tiskoff

	SELE fsklpolp
	SET RELA TO
	SELE fsklpol
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**  Mno§stevn¡ obraty u zbo§¡
PROCEDURE ses_obr

	SELE fsklpol
*	GO TOP
*	dtod=dtv
*	dtdo=date()
	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF


	WAIT WIND NOWA "Prob¡h  rekonstrukce skladu."

	CREAT CURS cu (txt C(45), czb C(8), pri N(8), vyd N(8), stav N(8))
	INDEX ON txt TAG jtxt
	INDEX ON pri TAG pri DESC
	INDEX ON vyd TAG vyd DESC
	INDEX ON stav TAG stav DESC
	INDEX ON czb TAG czb

	SET ORDER TO czb

	PRIV a, _rc, _cz

	SELE fsklcis
	_rc=recno()
	SET ORDER TO czb
	SET RELA TO jczb INTO cu

	SELE fsklpol
	SET RELA TO
	SET ORDER TO cis

	SELE fsklpolp
	SET FILT TO substr(cis,4,2)=m.radaskl
	SET RELA TO czb INTO fsklcis
	SET RELA TO cis INTO fsklpol ADDI

	SCAN ALL FOR betw(fsklpol.dtv,m.dtod,m.dtdo)
		a=fsklcis.jkus*fsklpolp.kus*iif(asc(cis)=49,-1,1)
		p=iif(sign(a)>0,a,0)
		v=iif(sign(a)<0,a*-1,0)
		IF eof('cu')
			INSERT INTO cu VALUES (fsklcis.jtxt, fsklcis.jczb, p, v, a)
		  ELSE
			REPL cu.pri WITH cu.pri+p, cu.vyd WITH cu.vyd+v, cu.stav WITH cu.stav+m.a
			ENDIF
		ENDSCAN
	SET RELA TO

	SELE fsklpolp
	SET RELA TO
	SELE fsklcis
	SET RELA TO


*	OdeŸten¡ naplnØnìch lahv¡ od vçech lahv¡ ve skladØ
*		po t‚to proceduýe z…stanou ve vratnìch obalech jenom pr zdn‚ lahve

	WAIT WIND NOWA "Vyýazuj¡ se vratn‚ obaly."

	SELE cu
	SET RELA TO czb INTO fsklcis

	DELE ALL FOR fsklcis.vratobal="Ano"

*	SCAN ALL FOR fsklcis.vratobal="Ano"
*		_rc=recno()
*		a=stav
*		_cz=czb
*		SCAN ALL FOR fsklcis.jkus=1 AND fsklcis.vratobal="Ne "
*			IF fsklcis.czb1=m._cz OR fsklcis.czb2=m._cz ;
*					OR fsklcis.czb3=m._cz OR fsklcis.czb4=m._cz
*				a=a-stav
*				ENDIF
*			ENDSCAN
*		=gorec(m._rc)
*		REPL stav WITH a
*		ENDSCAN

	WAIT CLEAR

	SET ORDER TO jtxt
	DO list_c WITH 6,1,21,0,;
		" Mno§stevn¡ obraty zbo§¡ od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)+" ",;
		[:txt+' ³'+str(pri,8)+' ³'+str(vyd,8)+' ³'+str(stav,8)], ;
		"// N zev zbo§¡"+space(34)+"³ Pýijato ³  Vyd no ³  Stav",;
		"\ F1-Help  F7-Tý¡dØn¡ sestavy  F9-Tisk",;
		'{JTXT \<N zev zbo§¡, PRI \<Pýijato, VYD \<Vyd no, STAV \<Stav}'

	USE
RETURN
**	Obalov‚ konto
PROCEDURE ses_obko
	PRIV cotxt, cocis, idkdo

	DO l_sklcis WITH " "
	IF lastkey()=27
		RETURN
		ENDIF
	cotxt=fsklcis.txt
	coczb=fsklcis.czb

	DO l_klient WITH " "
	IF lastkey()=27
		RETURN
		ENDIF
	idkdo=fseznam.m_add

	WAIT WIND NOWA "Prob¡h  rekonstrukce skladov‚ karty ..."

	DO tiskon

	a=" Pohyb zbo§¡: "+alltrim(m.cotxt)+" pro "+alltrim(fseznam.naz)
	?m.cnon+space(6)+m.a
	?space(6)+repl('~',len(a)+2)

	SELE fseznam
	SET ORDER TO m_add
	SELE fsklcis
	SET ORDER TO czb
	SELE fsklpolp
	SET ORDER TO cis
	SET RELA TO czb INTO fsklcis
	SELE fsklpol
	SET ORDER TO dtv
	SET RELA TO cis INTO fsklpolp, idkdo INTO fseznam

	PRIV stav, pp, vv
	STORE 0 TO stav, pp, vv
	SCAN ALL FOR fseznam.m_add=m.idkdo
		SELE fsklpolp
		SCAN REST FOR fsklcis.jczb=m.coczb WHILE fsklpol.cis=fsklpolp.cis
			kk=kus*iif(left(fsklpolp.cis,1)='2',1,-1)
			stav=stav+m.kk*fsklcis.jkus
			?space(6)+dtoc(fsklpol.dtv)+'  '+fsklpol.cis+'   '+ ;
				str(m.kk,7)+'x  '+fsklcis.txt+'  =  '+str(m.stav,6)
			IF m.kk>0
				pp=m.pp+m.kk
			  ELSE
				vv=m.vv+m.kk
				ENDIF
			ENDSCAN
		SELE fsklpol
		ENDSCAN
	?
	?
	?m.cnoff+space(4)+"Pýijato: "+strn(m.pp)+"    Vyd no: "+strn(m.vv*-1)+ ;
		"    Stav: "+strn(m.stav)+"  "+chr(12)
	DO tiskoff

	SELE fsklpolp
	SET RELA TO
	SELE fsklpol
	SET RELA TO

	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Vìpis zbo§¡ pod minimi ln¡m stavem
PROCEDURE ses_mins

	dtdo=date()

	DO jsklad

	DO tiskon
	a=rtrim("Zbo§¡ pod minim ln¡m stavem k "+dtoc(m.dtdo))
	?space(7)+m.a
	?space(6)+repl('~',len(a)+2)
	?space(6)+" Kat¬ N zev zbo§¡                             Stav   Minimum     Chyb¡"
	?space(6)+repl('-',72)

	SELE fsklcis
	SET ORDER TO czb
	SELE jsklad
	SET ORDER TO jtxt
	SET RELA TO czb INTO fsklcis

	SCAN ALL FOR jsklad.stav<fsklcis.minstav AND fsklcis.vratobal="Ne "
		?space(6)+fsklcis.katc+' '+left(txt,35)+str(stav,10)+' -'+ ;
			str(fsklcis.minstav,8)+' ='+str(fsklcis.minstav-stav,8)
		ENDSCAN

	SET RELA TO
	??chr(12)
	DO tiskoff

	USE
	WAIT CLEAR

	DO tisk WITH 6
RETURN
**	Denn¡ tr§by
PROCEDURE ses_dtr 

	dtod=gomonth(ctod("01"+substr(dtoc(date()),3)),-1)
	dtdo=ctod("01"+substr(dtoc(date()),3))-1
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SELE fseznam
	SET ORDER TO m_add

	SELE fsklpol
	SET RELA TO idkdo INTO fseznam
	SET ORDER TO dtv

	DO tiskon

	l=5
	SELE fsklpol
	?space(4)+" Sestava denn¡ch tr§eb v odbob¡ od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?space(4)+"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"+m.cnon
	?space(6)+"+---------+----------+----------+----------+----------+----------+----------+----------+"
	?space(6)+"³  ¬¡slo  ³  ZD 22%  ³  DPH 22% ³  ZD  5%  ³  DPH  5% ³  ZD  0%  ³  Ostatn¡ ³  Celkem  ³"
	?space(6)+"+---------+----------+----------+----------+----------+----------+----------+----------+"

	PRIV sz22,sd22,sz_5,sd_5,sz_0,sost,scas
	STORE 0 TO sz22,sd22,sz_5,sd_5,sz_0,sost,scas

	SCAN ALL FOR betw(dtv,m.dtod,m.dtdo) AND empty(cfa) ;
			AND left(cis,1)='1' AND fseznam.typ="Klient" ;
			AND substr(cis,4,2)=m.radaskl
		IF l>=m.len_pg
			??chr(12)
			?space(6)+"+---------+----------+----------+----------+----------+----------+----------+----------+"
			?space(6)+"³  ¬¡slo  ³  ZD 22%  ³  DPH 22% ³  ZD  5%  ³  DPH  5% ³  ZD  0%  ³  Ostatn¡ ³  Celkem  ³"
			?space(6)+"+---------+----------+----------+----------+----------+----------+----------+----------+"
			l=3
			ENDIF

		?space(6)+"³"+cis+'³'+str(z22,10,2)+'³'+str(d22,10,2)+'³'+str(z_5,10,2)+'³'+str(d_5,10,2)+'³'+str(z_0,10,2)+'³'+str(ost,10,2)+'³'+str(cas,10,2)+'³'
		sz22=sz22+z22
		sd22=sd22+d22
		sz_5=sz_5+z_5
		sd_5=sd_5+d_5
		sz_0=sz_0+z_0
		sost=sost+ost
		scas=scas+cas
		l=l+1
		ENDSCAN
	?space(6)+"+---------+----------+----------+----------+----------+----------+----------+----------+"
	?space(6)+"³ Celkem: ³"+str(sz22,10,2)+'³'+str(sd22,10,2)+'³'+str(sz_5,10,2)+'³'+str(sd_5,10,2)+'³'+str(sz_0,10,2)+'³'+str(sost,10,2)+'³'+str(scas,10,2)+'³'
	?space(6)+"+---------+----------+----------+----------+----------+----------+----------+----------+"+m.cnoff+chr(12)

	SET RELA TO

	DO tiskoff
	DO tisk WITH 6
RETURN
**	Provize dealer…
PROCEDURE ses_deal
	PRIV dtod, dtdo
	dtod=gomonth(ctod("01"+substr(dtoc(date()),3)),-1)
	dtdo=ctod("01"+substr(dtoc(date()),3))-1
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SELE fdealer
	REPL ALL cas WITH 0
	SET ORDER TO cis

	SELE fseznam
	SET ORDER TO m_add
	SET RELA TO deal INTO fdealer

	SELE fsklpol
	SET RELA TO idkdo INTO fseznam
	SET ORDER TO dtv
	SCAN FOR betw(dtv,m.dtod,m.dtdo) AND empty(cfa)
		REPL fdealer.cas WITH fdealer.cas+z22+z_5
		ENDSCAN

	SET ORDER TO cfa
	SELE faktury
	SET FILT TO !sto
	SET RELA TO cis INTO fsklpol
	SET ORDER TO zap
	SCAN FOR betw(zap,m.dtod,m.dtdo)
		REPL fdealer.cas WITH fdealer.cas+z22+z_5
		ENDSCAN
	SET RELA TO

	DO tiskon
	SELE fsklpol
	SET RELA TO
	SELE fseznam
	SET RELA TO

	PRIV ss
	ss=0
	SELE fdealer
	?space(4)+" Sestava pod¡l… fakturace na dealery od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)
	?space(4)+"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
	?space(4)+"+-----+--------------------------+-------------+-------------+-------+"
	?space(4)+"³¬¡slo³ Jm‚no                    ³  R¬ / I¬    ³   Celkem KŸ ³ Pod¡l ³"
	?space(4)+"+-----+--------------------------+-------------+-------------+-------+"
	SCAN ALL FOR cas!=0 AND cis!="000"
		?space(4)+"³ "+cis+" ³ "+jdeal+"³ "+rcic+" ³"+ ;
			str(cas,12,2)+" ³"+str(cas*.03,6)+" ³"
		ss=m.ss+round(cas*.03,0)
		?space(4)+"+-----+--------------------------+-------------+-------------+-------+"
		ENDSCAN
	?space(4)+"³                                              ³ Celkem pod¡l:"+str(m.ss,6)+" ³"
	?space(4)+"+----------------------------------------------+---------------------+"
	??chr(12)
	DO tiskoff
	DO tisk WITH 6

	SELE faktury
	DO fak_rada WITH .F.
RETURN
**	Provize dealer… po polo§k ch
PROCEDURE SES_DALL
	PRIV dtod, dtdo
	dtod=gomonth(ctod("01"+substr(dtoc(date()),3)),-1)
	dtdo=ctod("01"+substr(dtoc(date()),3))-1
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum od            do "
	@ 0,10 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,24 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	SELE 0
	CREA CURS cu1 (deal C(3), dat D, jme C(25), cas N(10,2))
	INDEX ON deal+dtos(dat) TAG ind

	SELE fdealer
	SET ORDER TO cis

	SELE fseznam
	SET ORDER TO m_add
	SET RELA TO deal INTO fdealer

	SELE fsklpol
	SET RELA TO idkdo INTO fseznam
	SET ORDER TO dtv
	SCAN FOR betw(dtv,m.dtod,m.dtdo) AND empty(cfa)
		INSERT INTO cu1 VALUES (fdealer.cis, fsklpol.dtv, fseznam.naz, ;
			fsklpol.z22+fsklpol.z_5)
		ENDSCAN

	SET ORDER TO cfa
	SELE faktury
	SET FILT TO !sto
	SET RELA TO cis INTO fsklpol
	SET ORDER TO zap
	SCAN FOR betw(zap,m.dtod,m.dtdo)
		INSERT INTO cu1 VALUES (fdealer.cis, faktury.zap, fseznam.naz, ;
			faktury.z22+faktury.z_5)
		ENDSCAN
	SET RELA TO

	DO tiskon
	SELE fsklpol
	SET RELA TO
	SELE fseznam
	SET RELA TO

	SELE cu1
	SET RELA TO deal INTO fdealer
	GO TOP
	PRIV dd, a, scas, sprov
	STORE 0 TO scas, sprov
	dd="xyz"
	SCAN ALL FOR deal!="   " AND deal!="000"
		IF m.dd!=deal
			IF dd!="xyz"
				?space(4)+"+------------+--------------------------+-------------+-------+"
				?space(4)+"³            ³ C E L K E M :            ³"+str(m.scas,12,2)+" ³"+str(m.sprov,6)+" ³"
				?space(4)+"+------------+--------------------------+-------------+-------+"+chr(12)
				ENDIF
			STORE 0 TO scas, sprov
			a=" Sestava polo§ek dealera: "+alltrim(fdealer.jdeal)+".  Od "+dtoc(m.dtod)+" do "+dtoc(m.dtdo)+"."
			?space(4)+a
			?space(4)+repl("~",len(a)+2)
			?space(4)+"+------------+--------------------------+-------------+-------+"
			?space(4)+"³   Datum    ³ Z kazn¡k                 ³   Celkem KŸ ³ Pod¡l ³"
			?space(4)+"+------------+--------------------------+-------------+-------+"
			dd=deal
			ENDIF
		?space(4)+"³ "+dtoc(dat)+" ³ "+jme+"³"+str(cas,12,2)+" ³"+str(cas*.03,6)+" ³"
		scas=m.scas+cas
		sprov=m.sprov+round(cas*.03,0)
		ENDSCAN
	?space(4)+"+------------+--------------------------+-------------+-------+"
	?space(4)+"³            ³ C E L K E M :            ³"+str(m.scas,12,2)+" ³"+str(m.sprov,6)+" ³"
	?space(4)+"+------------+--------------------------+-------------+-------+"+chr(12)
	DO tiskoff
	SELE cu1
	USE
	SELE faktury
	DO fak_rada WITH .F.
	DO tisk WITH 6
RETURN
**	Jednotkovì sklad s pr…mØrnìmi n kupn¡mi cenami
PROCEDURE jsklad  
	SELE 0
	CREAT CURS jsklad (txt C(70), czb C(8), stav N(8), ;
		skus N(10), scena N(12,2), ncena N(12,2))
	INDEX ON left(txt,30) TAG jtxt
	INDEX ON czb TAG czb

	SET ORDER TO czb

	PRIV a, _rc, _cz

	SELE fsklcis

	SCAN ALL FOR jczb=czb
		INSERT INTO jsklad VALUES (fsklcis.jtxt, fsklcis.czb, 0, 0, 0, 0)
		ENDSCAN

	SET ORDER TO czb
	SET RELA TO jczb INTO jsklad
	SELE fsklpol
	SET ORDER TO cis
	SELE fsklpolp
	SET ORDER TO

	PRIV p
	p=recc()
	
	SET RELA TO czb INTO fsklcis, cis INTO fsklpol

	SCAN ALL FOR fsklpol.dtv<=m.dtdo
*		=proc(m.p,recno())
		IF left(cis,1)="2"
			REPL jsklad.stav WITH jsklad.stav+fsklpolp.kus*fsklcis.jkus
*			IF jcena!=0
			IF jcena>0
				REPL jsklad.scena WITH jsklad.scena+fsklpolp.jcena, ;
					jsklad.skus WITH jsklad.skus+fsklpolp.kus*fsklcis.jkus
				ENDIF
			ENDIF
		IF left(cis,1)="1"
			REPL jsklad.stav WITH jsklad.stav-fsklpolp.kus*fsklcis.jkus
			ENDIF
		ENDSCAN

	SET RELA TO
	SELE fsklcis
	SET RELA TO

	SELE jsklad
* VìpoŸet n kupn¡ ceny
	REPL ALL ncena WITH scena/skus FOR skus!=0 OR scena!=0

	SET ORDER TO czb
RETURN
**	Pomocn  procedura pro vìbØr klienta
PROCEDURE _selkl  
	REPL flg WITH !flg
	IF !eof()
		SKIP 1
		ENDIF
RETURN
**	Obsluha dat klient…
PROCEDURE klienti
	SELE faktury
	SET ORDER TO idkdo
	SET FILT TO

*	SELE fsklpol
*	SET ORDER TO idkdo
*	SET FILT TO
*
	SELE fnaklady
	SET ORDER TO idkdo
	SET FILT TO

	SELE fseznam
	SET RELA TO m_add INTO faktury, m_add INTO fnaklady		&&	m_add INTO fsklpol, 
	SET FILT TO del
	SET ORDER TO naz
	GO TOP

	_ln10=[:iif(del,' ','>')+left(fseznam.naz,25)+'³ '+left(fseznam.jme,25)+'³'+fseznam.dic+'³'+ ;
		iif(deal!="000",deal,'   ')]
	_ln11="// Obchodn¡ jm‚no firmy     ³  Pý¡jmen¡, jm‚no, titul  ³       DI¬    ³Dea"

	_ln20=[:iif(del,' ','>')+left(fseznam.naz,25)+'³ '+left(fseznam.jme,20)+'³'+ ;
		left(fseznam.tel,15)+"³"+dtoc(lastkont)]
	_ln21="// Obchodn¡ jm‚no firmy     ³  Jm‚no              ³ Telefon       ³Kontakt"

*	_ln30=[:iif(del,' ','>')+left(fseznam.naz,25)+'³ '+left(fseznam.jme,25)+'³'+left(fseznam.tel,15)+ ;
*		iif(!eof('faktury') OR !eof('fsklpol'),'###','   ')]
*	_ln31="// Obchodn¡ jm‚no firmy     ³  Pý¡jmen¡, jm‚no, titul  ³ Telefon      ³Fak"

*	_ln20=[:iif(del,' ','>')+left(fseznam.naz,15)+' ³ '+fseznam.typ_uc+' ³ '+;
*		fseznam.dph+' ³ '+fseznam.uhr+' ³ '+dtoc(fseznam.dat_sml)+' ³ '+ ;
*		iif(!empty(ico) AND !eof('fnaklady'),'###','   ')]
*	_ln21="// Obchodn¡ jm‚no  ³ Typ £Ÿet.  ³ Typ DPH    ³ Typ £hr. ³ Datum sml. ³ VN"

	DO list_c WITH -1,-1,10,0,;
		" Seznam klient… ",;
		m._ln10, m._ln11, m._ln20, m._ln21, ;
		"\ F1-Help  ENTER-Oprava £daje  INS-Pýid n¡  F5-Tisk ob lky  F7-Tý¡dit podle ...",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
		'INS:	DO sez_edit WITH .T.	## INS-Pýid n¡ £daje',;
		'CR#:	DO sez_edit WITH .F.	## ENTER-Prohl¡§en¡/oprava £daje',;
		'ALT+N#:DO sez_nakl				## ALT+N-Zad v n¡ vedlejç¡ch n klad…',;
		'F5#:	DO sez_obla				## F5-Tisk ob lky - adres t',;
		'F6#:	DO sez_oblo				## F6-Tisk ob lky - odes¡latel',;
		'ALT+D#:DO sez_dele 			## ALT+D-Vyýazen¡/Smaz n¡ klienta',;
		'F2#:	DO sez_zobr 			## F2-Zobrazen¡ vyýazenìch klient…',;
		'ALT+A#:DO sez_ref 				## ALT+A-Aktualizace £daj… seznamu ',;
		'ALT+T:'
	SELE fseznam
	SET FILT TO
	SET RELA TO
RETURN
**	List pro vìbØr klienta
PROCEDURE l_klient
PARA _kli
PRIV _ord, _sel
	_sel=sele()

	SELE fseznam
	_ord=order()
	SET FILT TO del

	SET ORDER TO naz
	IF !seek(cs(m._kli))
		GO TOP
		ENDIF

	DO list_c WITH -1,-1,10,0,;
		" VìbØr klinta ",;
		[:iif(del,' ','>')+left(fseznam.naz,25)+'³ '+left(fseznam.jme,25)+'³'+fseznam.ico+'³'+ ;
			iif(deal!="000",deal,'   ')],;
		"// Obchodn¡ jm‚no firmy     ³  Pý¡jmen¡, jm‚no, titul  ³   I¬   ³Dea",;
		"\ F1-Help  ENTER-VìbØr klienta  F7-Tý¡dit podle ...",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}'

	SET ORDER TO &_ord
	SELE (m._sel)
RETURN

**	List pro vìbØr klienta pýez m_add
PROCEDURE l_seznam
PARA id
PRIV _ord, _sel

	_sel=sele()
	SELE fseznam
	_ord=order()

	SET FILT TO del
	SET ORDER TO m_add
	IF !seek(m.id)
		GO TOP
		ENDIF

	SET ORDER TO naz
	DO list_c WITH -1,-1,10,0,;
        " VìbØr klienta ",;
        [:iif(del,' ','>')+left(fseznam.naz,25)+' ³ '+left(fseznam.jme,25)+' ³ '+fseznam.ico],;
	        "// N zev firmy               ³  Jm‚no osoby              ³    I¬",;
        "\ F1-Help  ENTER-VìbØr  F4-Oprava £daj…  F7-Tý¡dit podle ...",;
		'{NAZ \<N zev firmy, JME \<Jm‚no, ICO \<I¬O}', ;
        'CR#:	EXIT					## ENTER-VìbØr klienta',;
		'INS:	DO sez_edit WITH .T.	## INS-Pýid n¡ klienta',;
		'F4#:	DO sez_edit WITH .F.	## F4-Prohl¡§en¡/oprava klienta'
	SET FILT TO
	SET ORDER TO &_ord
	SELE(m._sel)
RETURN

**	Aktualizace nØkterìch £daj… v datab zi seznam
PROCEDURE sez_ref

	PRIV ord, rc

	ord=order()
	rc=recno()
	SET ORDER TO m_add

	SELE fsklpol
	SET ORDER TO dtv
	SET RELA TO idkdo INTO fseznam

	SCAN ALL
		REPL fseznam.lastkont WITH dtv
		ENDSCAN

	SET RELA TO
	SELE fseznam
	SET ORDER TO &ord
	=gorec(m.rc)

	refscr=.T.

RETURN

**	Zobrazen¡ i vyýazenìch klient…
PROCEDURE sez_zobr
	IF empty(set('filter'))
		__fltr="del"
	  ELSE
		__fltr=""
		ENDIF
	SET FILT TO &__fltr
	refscr=.T.
RETURN

**	Oprava £daj… klienta
PROCEDURE sez_edit
	PARA app
*	PRIV _ico, s_ico, _rc
*	_ico=ico
	DO seznam.spr
*	IF m._ico!=ico AND !app AND !empty(m._ico)
*		s_ico=ico
*		SELE faktury
*		_rc=recno()
*		REPL ALL ico WITH m.s_ico, m_user WITH m.gl_usernam, ;
*			m_date WITH date(), m_time WITH time() FOR ico=m._ico
*		=gorec(m._rc)
*		SELE fnaklady
*		REPL ALL ico WITH m.s_ico, m_user WITH m.gl_usernam, ;
*			m_date WITH date(), m_time WITH time() FOR ico=m._ico
*		SELE fsklpol
*		REPL ALL ico WITH m.s_ico, m_user WITH m.gl_usernam, ;
*			m_date WITH date(), m_time WITH time() FOR ico=m._ico
*		ENDIF
	refscr=.T.
RETURN

**	Vyýazen¡/Smaz n¡ klienta
PROCEDURE sez_dele
	PRIV f
	f=.F.

	SELE fnaklady
	SET ORDER TO idkdo
	f= f OR seek(fseznam.m_add)

	SELE faktury
	SET ORDER TO idkdo
	f= f OR seek(fseznam.m_add)

	SELE fsklpol
	SET ORDER TO idkdo
	f= f OR seek(fseznam.m_add)

	SELE fseznam
	IF !m.f
		IF ask(-1,-1,'A',"Nen vratnØ smazat vybran‚ho klienta ze seznamu.")
			REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
			DELE
			ENDIF
	  ELSE
		REPL fseznam.del WITH !fseznam.del, m_user WITH m.gl_usernam, ;
			m_date WITH date(), m_time WITH time()
		ENDIF
	refscr=.t.
RETURN

**	Tisk ob lek
PROCEDURE sez_obla
	IF ask(-1,-1,"N","Tisk na malou ob lku?")
		sp=space(24)
	  ELSE
		IF m.esc
			RETURN
			ENDIF
		sp=space(43)
		ENDIF
	SET CONS OFF
	tskf=SYS(3)+'.prt'
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	??m.nlq+repl(chr(10),9)
	?sp+'        '+naz
	?
	?sp+'        '+iif(empty(p_jme),jme,p_jme)
	?
	?sp+'        '+iif(empty(p_uli),uli,p_uli)
	?
	?sp+left(iif(empty(p_psc),psc,p_psc),3)+' '+ ;
		right(iif(empty(p_psc),psc,p_psc),2)+'  '+ ;
		iif(empty(p_mes),mes,p_mes)+m.draft+chr(12)
	SET ALTE TO
	SET HEAD OFF
	DO tisk WITH 0
	SET CONS ON
RETURN

PROCEDURE sez_oblo
	sp=space(4)
	SET CONS OFF
	tskf=SYS(3)+'.prt'
	SET ALTE TO &tskf
	SET ALTE ON
	??m.resetprn
	??m.nlq+sp+naz
	?sp+iif(empty(p_jme),jme,p_jme)
	?sp+iif(empty(p_uli),uli,p_uli)
	?sp+iif(empty(p_mes),mes,p_mes)
	?sp+left(iif(empty(p_psc),psc,p_psc),3)+' '+ ;
		right(iif(empty(p_psc),psc,p_psc),2)+m.draft+chr(12)
	SET ALTE TO
	SET HEAD OFF
	DO tisk WITH 0
	SET CONS ON
RETURN

*********************************
**	Zad v n¡ n klad… ke klientovi
PROCEDURE sez_nakl
	SELE fnaklady
	SET FILT TO fnaklady.idkdo==fseznam.m_add
	SET ORDER TO dat
	GO TOP
	DO list_c WITH -1,-1,10,0,;
        [dtoc(fnaklady.dat)+'³ '+fnaklady.txt+'³ '+fnaklady.mno+'³ '+str(fnaklady.cas,10)],;
        " Vedlejç¡ n klady pro "+alltrim(fseznam.naz)+" ",;
        "// Datum    ³ Popis n kladu                      ³ Mno§stv¡       ³ ¬ stka",;
        "\ F1-Help  ENTER-Oprava £daje  INS-Pýid n¡  F7-Tý¡dit podle ...",;
        'INS:	DO nak_edit WITH .T.	## INS-Pýid n¡ n klad…',;
        'CR#:	DO nak_edit WITH .F.	## ENTER-Oprava ',;
        'ALT+D#:DO nak_dele 			## ALT+D-Smaz n¡ n klad…',;
        'F9#:	DO nak_tisk 			## F9-Tisk n klad… k faktuýe'
	SET FILT TO
	SET ORDER TO dat
	SELE fseznam
RETURN

PROCEDURE nak_tisk
	PRIV dtod, dtdo, castka
	dtdo=ctod("01"+substr(dtoc(date()),3))
	dtod=gomonth(m.dtdo,-1)
	dtdo=m.dtdo-1
	castka=0

	PRIV _l
	_l=38
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Obdob¡ tisku: "
	@ 0,25 SAY "-"
	@ 0,15 GET m.dtod SIZE 1,10 COLOR SCHEME 4
	@ 0,26 GET m.dtdo SIZE 1,10 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	SELE fnaklady
	SET ORDER TO dat
	SET FILTER TO
	DO tiskon
	?m.nlq+space(5)+" Vy£Ÿtov n¡ prac¡ a vedlejç¡ch n klad… k faktuýe Ÿ.:"
	?space(5)+		"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"+m.draft+m.cnon
	?
	?space(8)+"ÚÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	?space(8)+"³    Datum   ³  Popis n kladu                      ³  Mno§stv¡       ³ ¬ stka v KŸ ³"
	?space(8)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
	SCAN ALL FOR fseznam.m_add==fnaklady.idkdo AND ;
			betw(fnaklady.dat, m.dtod, m.dtdo)
		?space(8)+'³ '+dtoc(fnaklady.dat)+' ³ '+fnaklady.txt+' ³ '+alignr(fnaklady.mno)+' ³ '+str(fnaklady.cas,10)+'  ³'
		castka=m.castka+cas
		ENDSCAN
	?space(8)+"ÃÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
	?space(8)+"³ CELKEM:"+space(60)+"³ "+str(m.castka,10)+"  ³"
	?space(8)+"ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	?m.cnoff+"  "+chr(12)
	DO tiskoff
	DO tisk WITH 5
RETURN

**	Oprava n klad…
PROCEDURE nak_edit   &&  EDITACE
	PARA app
	DO naklady.spr
	refscr=.T.
RETURN

**	Maz n¡ n klad…
PROCEDURE nak_dele
	IF ask(-1,-1,'A',"Opravdu smazat vybranì n klad")
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
	refscr=.T.
RETURN
**	MENU ¬ÖSELNÖKÞ
PROCEDURE m_cisel
	DEFINE POPUP cisel FROM 10,40 RELATIVE SHADOW ;
		TITLE " ¬¡seln¡ky a seznamy " COLOR SCHEME 4
	DEFINE BAR  5 OF cisel PROMPT " \<Zbo§¡ " ;
		MESS "Oprava a pýid n¡ polo§ek do Ÿ¡seln¡ku zbo§¡."
	DEFINE BAR 15 OF cisel PROMPT " \<Dealeýi " ;
		MESS "Oprava a pýid n¡ do Ÿ¡seln¡ku dealer…."
**		SKIP FOR used('FDEALER')
*	DEFINE BAR 30 OF cisel PROMPT " \<Vzory " ;
		MESS "¬¡seln¡k vzor… pneumatik."
*	DEFINE BAR 40 OF cisel PROMPT " \<RormØry " ;
		MESS "¬¡seln¡k rozmØr… pneumatik."
*	DEFINE BAR 50 OF cisel PROMPT " Dru\<hy " ;
		MESS "¬¡seln¡k druh… (vìrobc…) pneumatik."

	ON SELE POPU cisel DO cisel
	ACTIVATE POPUP cisel
	RELE POPU cisel
RETURN
PROCEDURE cisel
	DO CASE
		CASE bar()=5
			DO ciszbozi			
		CASE bar()=15
			DO cisdeal WITH '   '
*		CASE bar()=30
			DO cisvzor WITH '   '
*		CASE bar()=40
			DO cisrozm WITH '   '
*		CASE bar()=50
			DO cisdruh WITH '   '
		ENDCASE
	DO refscr
RETURN
**	List pro vìbØr zbo§¡
PROCEDURE cisdeal
PARA _cis
PRIV _ord, _sel
	_sel=sele()

	SELE fdealer
	_ord=order()

	SET ORDER TO cis
	IF !seek(m._cis)
		GO TOP
		ENDIF

	SET ORDER TO jdeal

	DO list_c WITH 6,-1,10,0,;
		" Dealeýi ",;
		[:cis+' ³ '+jdeal+' ³ '+rcic], ;
		"//¬¡s.³  Jm‚no                    ³ Rod.Ÿ./I¬",;
		"\ENTER-Vybr n¡   INS-Pýid n¡    F4-Oprava dealera",;
		'{Cis ¬¡\<slo, Jdeal \<Jm‚no}', ;
		'F4#:	DO dea_edit WITH .F.	## F4-Prohl¡§en¡/oprava ',;
		'INS:	DO dea_edit WITH .T.	## INS-Novì dealer '
		
	SET ORDER TO &_ord
	SELE (m._sel)
RETURN

PROCEDURE dea_edit
PARA app
	DO dealer.spr
	refscr=.T.
RETURN

PROCEDURE ciszbozi
	SELE fsklcis
	SET ORDER TO katc

	GO TOP

*		[:left(txt,62)+' ³ '+str(stav,8)], ;
*		"// N zev zbo§¡                                                   ³  Stav",;

	DO list_c WITH 6,1,10,0,;
		" ¬¡seln¡ zbo§¡ ",;
		[:katc+' '+left(txt,50)+' ³ '+str(cas,10,2)+' ³ '+dph], ;
		"//Kat¬  N zev zbo§¡                                       ³   ¬ stka   ³ DPH",;
		[:katc+' '+left(txt,30)+' ³ '+left(jtxt,35)], ;
		"//Kat¬  N zev zbo§¡                   ³ Jednotkov‚ zbo§¡",;
		[:katc+' '+left(txt,20)+'³³'+m_user+'³'+dtoc(m_date)+'³'+m_time+'³'+m_add],;
		"//Kat¬  N zev zbo§¡        ³³  ZmØnil  ³  Datum   ³  ¬as   ³  m_add           ",;
		"\ F1-Help  ENTER-Oprava zbo§¡  INS-Pýid n¡ zbo§¡",;
		'{KATC \<Kat. Ÿ¡slo, TXT \<N zev}', ;
		'CR#:	DO skc_edit WITH .F.	## ENTER-Prohl¡§en¡/oprava polo§ky ',;
		'INS:	DO skc_edit WITH .T.	## INS-Nov  polo§ka',;
		'ALT+D#:DO skc_dele				## ALT+D-Vìmaz polo§ky'
	SET FILT TO

RETURN
PROCEDURE skc_edit
PARA app
	DO sklcis.spr
	IF jkus=1 AND vratobal="Ne "
		PRIV rc, _czb, _cas
		rc=recno()
		_czb=czb
		_cas=cas
		REPL ALL cas WITH m._cas*jkus, m_user WITH m.gl_usernam, ;
			m_date WITH date(), m_time WITH time() FOR jczb=m._czb
		=gorec(m.rc)
		ENDIF
	refscr=.T.
RETURN
PROCEDURE skc_dele
	PRIV a
	SELE fsklpolp
	SET ORDER TO czb
	a=seek(fsklcis.czb)
	SELE fsklcis
	IF a
		WAIT WIND NOWA "Polo§ka je obsa§ena ve vìdejk ch. Nelze ji proto smazat."
	  ELSE
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		refscr=.T.
		ENDIF
RETURN

**	List pro vìbØr zbo§¡
PROCEDURE l_sklcis
PARA _czb
PRIV _ord, _sel, f
	_sel=sele()

	SELE fsklcis
	_ord=order()

	SET ORDER TO czb
	f=seek(m._czb)

	SET ORDER TO m.ggordciszb
	IF !f
		GO TOP
		ENDIF

	DO list_c WITH 6,1,10,0,;
		" VìbØr z Ÿ¡seln¡ku zbo§¡ ",;
		[:katc+' '+left(txt,50)+' ³ '+str(cas,10,2)+' ³ '+dph], ;
		"//Kat¬  N zev zbo§¡                                       ³   ¬ stka   ³ DPH",;
		"\ENTER-Vybr n¡ zbo§¡",;
		'{KATC \<Kat. Ÿ¡slo, TXT \<N zev}'

*		'F8#: #FILTER',	'#FLT=\<Jednotkov‚ zbo§¡: czb=jczb', ;
*

	ggordciszb=order()

	SET ORDER TO &_ord
	SELE (m._sel)

RETURN


*******************
**	LIST_C pro text
PROCEDURE ltext
	PRIV m.sel
	sel=SELE()
	SELE ftext
	SET ORDER TO text
	DO list_c WITH -1,-1,15,0,;
		[ftext.text],;
		" Popisn‚ texty ",;
		"//  Text",;
		"\ F1-Help   ENTER-VìbØr   F4-Oprava   INS-Pýid n¡ textu   F7-Tý¡dit podle ...",;
		'F4#:		DO text_edi WITH .F.		## F4-VìbØr textu',;
		'INS:		DO text_edi WITH .T.		## INS-Pýid n¡ textu',;
		'ALT+D#:	DO text_del					## ALT+D-Smaz n¡ textu'
	SELE (m.sel)
RETURN ftext.text

PROCEDURE text_edi
PARA app
	DO text.spr
RETURN

PROCEDURE text_del
	IF ask(-1,-1,'A',"Smazat text")
		REPL m_user WITH m.gl_usernam, m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
RETURN
**	Obsluha menu RUZNE	**
PROCEDURE m_ruzne
	DEFINE POPUP ruzne FROM 10,40 RELATIVE SHADOW ;
		TITLE " R…zn‚ " COLOR SCHEME 4
	DEFINE BAR 10 OF ruzne PROMPT " \<VìbØr skladu 1 " ;
		MESS "VìbØr skladov‚ho m¡sta pro SKLAD 1."
	DEFINE BAR  5 OF ruzne PROMPT " \<Aktualizace datab z¡ " ;
		MESS "Aktualizace dat s datab zemi na jin‚m m‚diu."
	DEFINE BAR 15 OF ruzne PROMPT " édaje pro \<fakturaci" ;
		MESS "Nastaven¡ £daj… pro fakturaci."
	DEFINE BAR 20 OF ruzne PROMPT " éŸty \<dodavatele " ;
		MESS "Seznam £Ÿt… dodavatele."
	DEFINE BAR 25 OF ruzne PROMPT " \<Seznam Ÿlen… " ;
		MESS "Seznam Ÿlen… pro sledov n¡ fakturace dle pracovn¡k…."
	DEFINE BAR 30 OF ruzne PROMPT " Nov  ýada faktu\<r    (90)" ;
		MESS "Zad n¡ nov‚ ýady vystavovanìch faktur." ;
		SKIP FOR m.gl_pri_fs<90
	DEFINE BAR 35 OF ruzne PROMPT " Nov  ýada s\<kladovek (90)" ;
		MESS "Zad n¡ nov‚ ýady vystavovanìch faktur." ;
		SKIP FOR m.gl_pri_fs<90
*	DEFINE BAR 59 OF ruzne PROMPT "\-"
*	DEFINE BAR 61 OF ruzne PROMPT " Nastaven¡ stanic " ;
*		MESS "Nastaven¡ syst‚movìch parametr… programu pro jednotliv‚ stanice."
*	DEFINE BAR 63 OF ruzne PROMPT " Nastaven¡ tisk ren " ;
*		MESS "Nastaven¡ ý¡d¡c¡ch seqvenc¡ tisk ren."
	DEFINE BAR 79 OF ruzne PROMPT "\-"
	DEFINE BAR 86 OF ruzne PROMPT " \<Novinky" ;
		MESS "Popis £prav v hlavn¡m menu syst‚mu."
	DEFINE BAR 90 OF ruzne PROMPT " \<Oprava index… " ;
		MESS "Oprava index… po hav rii."
	DEFINE BAR 95 OF ruzne PROMPT " \<Glob ln¡ £daje" ;
		MESS "Glob ln¡ £daje o cel‚m syst‚mu."
	ON SELE POPU ruzne DO ruzne
	ACTIVATE POPUP ruzne
	RELE POPU ruzne
RETURN

PROCEDURE ruzne
	DO CASE
		CASE bar()=5
			DO aktual
		CASE bar()=10
			PRIV _l
			_l=19
			DEFINE WINDOW w_ask ;
				FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
				TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
				NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
			ACTIVATE WINDOW w_ask SAME
			@ 0,1 SAY "¬¡slo skladu: "
			@ 0,15 GET m.radaskl SIZE 1,2 COLOR SCHEME 4 FUNC "@M 01,03"
			READ MODAL
			RELEASE WINDOW w_ask
		CASE bar()=15
			DO gl_udaje.spr
			IF !m.esc
				SAVE TO &ggf.gl_udaje ALL LIKE gl_* ALL EXCEPT gl_data, gl_usernam, gl_pri*, gl_verze
				ENDIF
		CASE bar()=20
			DO ruz_ucty
		CASE bar()=25
			DO cleni
		CASE bar()=30
			DO newrada
		CASE bar()=35
			DO nskldov
*		CASE bar()=61
*			DO parstan IN setparam
*		CASE bar()=63
*			DO parprint IN setparam
		CASE bar()=86
			DO novinky
		CASE bar()=90
			DO _open WITH .T., .T.
			IF !_open(.F., .F.)
				_bar=99
				RETURN TO MASTER
				ENDIF
		CASE bar()=95	&&	Glob ln¡ parametry
			SELE firma
			DO ..\firma.spr
		ENDCASE
	DO refscr
RETURN
**	Zad n¡ nov‚ ýady faktur
PROCEDURE newrada
	PRIV newrada
	newrada=right(str(year(date()),4),2)+" "
	PRIV _l
	_l=25
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Nov  ýada faktur: "
	@ 0,19 GET m.newrada SIZE 1,3 COLOR SCHEME 4 ;
		MESS "Zad n¡ nov‚ ýady faktur ve tvaru rrx. Kde rr=rok a x=ýada."
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	newrada=m.newrada+"0000"
	SELE faktury
	SET ORDER TO cis
	SEEK (m.newrada)
	IF found()
		WAIT WIND "Zadan  ýada faktur se ji§ pou§¡v "
		RETURN
		ENDIF
	INSERT INTO faktury (cis, m_user, m_date, m_time, m_add) VALUES ;
		(m.newrada, m.gl_usernam, date(), time(), dtos(date())+sys(3))
RETURN
**	Zad n¡ nov‚ ýady skladovky
PROCEDURE nskldov 
	PRIV newrada
	newrada=right(str(year(date()),4),2)+"01"
	PRIV _l
	_l=34
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Nov  ýada skladovky: t    0000"
	@ 0,23 GET m.newrada SIZE 1,4 COLOR SCHEME 4 ;
		MESS "Zad n¡ nov‚ ýady skladovky: trrss0000. Kde rr=rok, ss=sklad, t-typ V/P"
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF

	newrada=m.newrada+"0000"
	SELE fsklpol
	SET ORDER TO cis
	IF seek("1"+m.newrada)
		WAIT WIND "Zadan  ýada skladovek se ji§ pou§¡v "
		RETURN
		ENDIF

	INSERT INTO fsklpol (cis, m_user, m_date, m_time, m_add) VALUES ;
		("1"+m.newrada, m.gl_usernam, date(), time(), dtos(date())+sys(3))
	INSERT INTO fsklpol (cis, m_user, m_date, m_time, m_add) VALUES ;
		("2"+m.newrada, m.gl_usernam, date(), time(), dtos(date())+sys(3))
RETURN

**	Datab ze £Ÿt… dodavatele
PROCEDURE ruz_ucty
	SELE fucty
	SET FILT TO
	DO list_c WITH -1,-1,10,0,;
		[banka+' ³ '+ucet+' ³ '+pozn],;
		" éŸty dodavatele ",;
		"// Banka               ³ ¬¡slo £Ÿtu           ³ Pozn mka",;
        'INS:	DO uct_edit WITH .T.	## INS-Pýid n¡ £Ÿtu',;
        'CR#:	DO uct_edit WITH .F.	## ENTER-Oprava £Ÿtu',;
        'ALT+D#:DO uct_dele 			## ALT+D-Smaz n¡ £Ÿtu'
RETURN

PROCEDURE uct_edit
PARA app
	DO ucty.spr
	refscr=.T.
RETURN

PROCEDURE uct_dele
	IF ask(-1,-1,'A',"Opravdu smazat vybranou polo§ku")
		REPL m_user WITH "JAKU", m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
	refscr=.T.
RETURN

**	Datab ze Ÿlen…
PROCEDURE cleni
	SELE fclen
	SET FILT TO
	DO list_c WITH -1,-1,10,0,;
		[clen+' ³ '+jmeno],;
		" Seznam Ÿlen… ",;
		"//¬len ³ Pln‚ jm‚no",;
        'INS:		DO clen_edt WITH .T.	## INS-Pýid n¡ Ÿlena',;
        'CR#:		DO clen_edt WITH .F.	## ENTER-Oprava Ÿlena',;
        'ALT+D#:	DO clen_del 			## ALT+D-Smaz n¡ Ÿlena'
RETURN

PROCEDURE clen_edt
PARA app
	DO cleni.spr
	refscr=.T.
RETURN

PROCEDURE clen_del
	IF ask(-1,-1,'A',"Opravdu smazat vybran‚ho Ÿlena")
		REPL m_user WITH "JAKU", m_date WITH date(), m_time WITH time()
		DELE
		ENDIF
	refscr=.T.
RETURN
**	Aktualizace dat s jinou datab z¡ faktur a klient…
PROCEDURE aktual

	PRIV cesta, i, j, x, y, err, arjfile
	arjfile=alltrim(m.nfirma)
	DIME datab(255)
	err=0
	cesta="A:\"
	PRIV _l
	_l=50
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Cesta k aktual. soubor…m: "
	@ 0,27 GET m.cesta SIZE 1,20 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask

	IF lastkey()=27
		RETURN
		ENDIF

	SET COMP ON
	cesta=alltrim(m.cesta)
	IF rat('\',m.cesta)!=len(m.cesta)
		cesta=m.cesta+'\'
		ENDIF

	IF !_mkdir("aktual")
		WAIT WIND NOWA "Nelze vytvoýit pracovn¡ adres ý pro aktualizaci dat."
		RETURN
		ENDIF


	IF !file(m.cesta+m.arjfile+".arj")
		WAIT WIND NOWA "Nelze naj¡t aktualizaŸn¡ soubor. Soubor bude novØ vytvoýen."
	  ELSE
		!arj x &cesta.&arjfile
		CLEAR
		ENDIF

	i=0
	x=0
	e=0

** NaŸten¡ seznamu datab z¡
	ON ERROR e=error()
	FOR i=1 TO 255
		j=alias(i)
		IF e!=0 OR empty(j) OR left(j,1)!='F' OR j="FIRMA"
			e=0
			LOOP
			ENDIF
		x=x+1
		datab(x)=j
		ENDFOR

	ON ERROR &_onerr

	y=0
	FOR j=1 TO x
		y=y+1
		WAIT WIND NOWA "Prov d¡m aktualizace datab z¡: "+repl('þ',y)+repl('',x-y)
		CLOSE DATA


		SELE 2
		IF !file("aktual\"+datab(j)+".DBF")
			a=m.ggf+datab(j)
			b="aktual\"+datab(j)
			COPY FILE &a..DBF TO &b..DBF
			IF file(a+".FPT")
				COPY FILE &a..FPT TO &b..FPT
				ENDIF
			ENDIF

		IF file("aktual\"+datab(j)+".CDX")
			DELE FILE ("aktual\"+datab(j)+".CDX")
			ENDIF
		ON ERROR USE ("aktual\"+datab(j)) EXCL ALIAS akt
		USE ("aktual\"+datab(j)) EXCL ALIAS akt
		ON ERROR &_onerr
		INDEX ON m_add TAG m_add
		SET ORDER TO m_add

		SELE 1
		ON ERROR err=error()
		USE (m.ggf+datab(j)) EXCL ALIAS zdr
		ON ERROR &_onerr
		IF m.err!=0
			IF m.err=3 OR m.err=108
				WAIT WIND "Datab ze jsou pou§¡v ny jeçtØ nØkìm jinìm. Nelze prov‚st aktalizaci."
			  ELSE
				WAIT WIND "Vznikla chyba Ÿ."+strn(m.err)+" pýi kop¡rov n¡ datab z¡. Aktual. neprobØhla."
				ENDIF
			DO _open WITH .F., .F.
			_bar=99
			RETURN TO MASTER
			ENDIF
		INDEX ON m_add TAG m_add
		SET ORDER TO m_add

*	Otevýena a zindexov na datab ze.	1-Data-ZDR, 2-Aktual-AKT
		SET DELE OFF
		SELE 1
		SET RELA TO m_add INTO akt
		SCAN ALL
			IF eof("akt")
				SCAT MEMVAR MEMO
				INSERT INTO akt FROM MEMVAR
			  ELSE
				IF dtos(zdr.m_date)+zdr.m_time > dtos(akt.m_date)+akt.m_time
					SCAT MEMVAR MEMO
					SELE akt
					GATH MEMVAR MEMO
					SELE zdr
					ENDIF
				ENDIF
			IF dele()
				SELE akt
				DELE
				SELE zdr
				ENDIF
			ENDSCAN
		SELE 1
		SET RELA TO
		SELE 2
		SET RELA TO m_add INTO zdr
		SCAN ALL
			IF eof("zdr")
				SCAT MEMVAR MEMO
				INSERT INTO zdr FROM MEMVAR
			  ELSE
				IF dtos(akt.m_date)+akt.m_time > dtos(zdr.m_date)+zdr.m_time
					SCAT MEMVAR MEMO
					SELE zdr
					GATH MEMVAR MEMO
					SELE akt
					ENDIF
				ENDIF
			IF dele()
				SELE zdr
				DELE
				SELE akt
				ENDIF
			ENDSCAN
		SELE 2
		SET RELA TO
		SELE 1
		USE
		IF file(m.ggf+datab(j)+".CDX")
			DELE FILE (m.ggf+datab(j)+".CDX")
			ENDIF
		SELE 2
		USE
		IF file("aktual\"+datab(j)+".CDX")
			DELE FILE ("aktual\"+datab(j)+".CDX")
			ENDIF
		ENDFOR
	CLOSE DATA


	WAIT WIND NOWA "Kop¡ruji datab ze zpØt na pýenosov‚ m‚dium"
**	Kop¡rov n¡ na pýenosov‚ m‚dium
	!arj m &cesta.&arjfile .\aktual\*.* /i1 /y
	!pause

	SET COMP OFF
	SET DELE ON
	=_open(.T., .T.)
	IF !_open(.F., .F.)
		WAIT WIND "Return to master."
		_bar=99
		RETURN TO MASTER
		ENDIF

	RELE datab
RETURN
**	Otevýen¡ databaz¡
PROCEDURE _open
PARA rei, exc
	IF (m.rei OR file(m.ggf+"faktury.rei") AND ;
			ask(-1,-1,'A',"Obnovit indexy po hav rii"))
		rei=.T.
		WAIT WIND NOWAIT "Obnovuji indexov‚ soubory"
	  ELSE
		rei=.F.
		ENDIF

**	Aktivace statistiky
	=eff_start()
	=eff_ktim(90,2)
	st_on_dat=date()
	st_on_time=time()

	IF m.rei
		DO _open1 WITH .T.,.T.
		DO cldata
		ENDIF

	DO _open1 WITH .F., m.exc

RETURN
PROCEDURE _open1  
PARA rei, exc
	DO cldata1
	PRIV _exc
	_exc=set("EXCL")
	IF m.exc
		SET EXCL ON
	  ELSE
		SET EXCL OFF
		ENDIF

**	Pýejmenov v n¡ soubor…
	IF file(m.ggf+"clen.dbf")
		RENA (m.ggf+"clen.dbf") TO (m.ggf+"fclen.dbf")
		ENDIF
	IF file(m.ggf+"seznam.dbf")
		RENA (m.ggf+"seznam.dbf") TO (m.ggf+"fseznam.dbf")
		RENA (m.ggf+"seznam.fpt") TO (m.ggf+"fseznam.fpt")
		ENDIF

**	Otevýen¡ datab z¡
* Nepýehazovat poýad¡ kv…li funkci 4 v OPEN_DBF
	fl_open=.T.
	fl_open=m.fl_open AND open_dbf(m.ggf,"FSEZNAM","op_sezn","naz",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FAKTURYU","op_faku","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FAKTURY","op_fak","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FAKTURYP","op_fakp","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FPLATBY","op_plat","cis",m.exc,m.rei)

* NaplnØn¡ datab ze
	IF recc()=0
		PRIV _se, _ord
		_se=sele()
		SELE faktury
		SCAN ALL FOR !empty(zap)
			INSERT INTO ((alias(m._se))) ;
				(cis, datzap, cas, m_user, m_date, m_time, m_add ) ;
			  VALUES ;
				(faktury.cis, faktury.zap, faktury.cas, ;
				m.gl_usernam, date(), time(), dtos(date())+sys(3))
			ENDSCAN
		SELE (m._se)
		ENDIF

	fl_open=m.fl_open AND open_dbf(m.ggf,"FNAKLADY","op_nakl","dat",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FTEXT","op_text","text",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FSKLCIS","op_skcis","txt",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FSKLPOL","op_skpol","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FSKLPOLP","op_skpop","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FMALOP","op_malop","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FDEALER","op_deal","cis",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FUCTY","op_ucty","banka",m.exc,m.rei)
	fl_open=m.fl_open AND open_dbf(m.ggf,"FCLEN","op_fclen","clen",m.exc,m.rei)
	xclen=fclen.clen

*	fl_open=m.fl_open AND open_dbf(m.ggf,"FPNEU","op_pneu","vzor",m.exc,m.rei)
*	fl_open=m.fl_open AND open_dbf(m.ggf,"FCVZOR","op_cvzor","vzor",m.exc,m.rei)
*	fl_open=m.fl_open AND open_dbf(m.ggf,"FCROZMER","op_rozme","rozmer",m.exc,m.rei)
*	fl_open=m.fl_open AND open_dbf(m.ggf,"FCDRUH","op_druhy","druh",m.exc,m.rei)


* Vytvoýen¡ Ÿ¡seln¡ku dr…h… z datab ze Pneu
*	SELE fcdruh
*	IF recc()=0
*		SET ORDER TO druh
*		SELE fpneu
*		SET RELA TO druh INTO fcdruh
*		SCAN ALL FOR eof('fcdruh')
*			INSERT INTO fcdruh VALUES (fpneu.druh, m.gl_usernam, date(), time(), dtos(date())+sys(3))
*			ENDSCAN
*		ENDIF

* Ozn men¡ probl‚m… s otevýen¡m datab ze
	IF !m.fl_open
		IF m.exc
			WAIT WIND "Nelze vìhradnØ otevý¡t datab ze. Vyu§¡v  je jeçtØ jinì u§ivatel."
		  ELSE
			WAIT WIND "Nelze otevý¡t datab ze. Vyu§¡v  je vìhradnØ jinì u§ivatel."
			ENDIF
		RETURN .F.
		ENDIF
	SET EXCL &_exc
	WAIT CLEAR

**	Aktivace statistiky
*	=eff_start()
*	=eff_ktim(90,2)
*	st_on_dat=date()
*	st_on_time=time()

RETURN .T.

PROCEDURE op_sezn
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (jme C(29), naz C(29), tel C(30), fax C(15), ;
			uli C(25), mes C(25), psc C(6), e_mail C(32), ico C(8), dic C(14), ;
			ban C(20), deal C(3), uhr C(8), splat N(3), dat_sml D, ;
			sml C(48), pozn M, sleva N(5,2), skup C(8), flg L, ;
			p_jme C(25), p_uli C(25), p_mes C(15), p_psc C(6), ;
			typ C(6), del L, lastkont D, ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(naz) TAG naz
		INDEX ON cs(jme) TAG jme
		INDEX ON ico TAG ico
		INDEX ON m_add TAG m_add
		INDEX ON cs(skup)+left(naz,10) TAG skup
	  CASE m._cfn=3
		RETURN type("fseznam.lastkont")="U" OR len(naz)=25
	  CASE m._cfn=4
** Nastaven¡ splatnosti na Default hodnotu
*		LOCATE ALL FOR !empty(splat)
*		IF eof()
*			REPL ALL FOR empty(splat) splat WITH -1
*			ENDIF
		ENDCASE
RETURN

PROCEDURE op_fak
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (ico C(8), o_naz C(29), o_naz1 C(29), cis C(7), ;
			dtv D, dtdph D,dts D, uhr C(8), k_s C(4), d_ban C(20), ;
			d_uct C(20), pred C(48), zap D, pena L, sto L, idkdo C(16), ;
			cas N(10,2), cam N(10,2), caszap N(10,2), ;
			z22 N(10,2), z21 N(10,2), z20 N(10,2), z19 N(10,2), ;
			z15 N(10,2), z14 N(10,2), z10 N(10,2), ;
			z_5 N(10,2), z_0 N(10,2), ;
			d22 N(10,2), d21 N(10,2), d20 N(10,2), d19 N(10,2), ;
			d15 N(10,2), d14 N(10,2), d10 N(10,2), ;
			d_5 N(10,2), ost N(10,2), m_user C(10), ;
			m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON iif(left(cis,2)<"90","20","19")+cis TAG cis2000
		INDEX ON dtoc(dtv,1)+cis TAG dav
		INDEX ON cs(o_naz) TAG odb
		INDEX ON cas TAG cas
		INDEX ON dtos(zap) TAG zap
*		INDEX ON ico TAG ico
		INDEX ON idkdo TAG idkdo
	  CASE m._cfn=3
		RETURN type("FAKTURY.z21")="U" 
	  CASE m._cfn=4

* Rozç¡ýen¡ Ÿ¡sla faktury na 7 znak…
*		REPL ALL cis WITH left(cis,3)+'0'+substr(cis,4,3) FOR right(cis,1)=' '

* Pýev z n¡ na seznam podle fseznam.m_add
		PRIV _se, _ord
		_se=sele()
		SELE fseznam
		_ord=order()
		SET ORDER TO ico
		SELE (m._se)
		SET RELA TO ico INTO fseznam
		SCAN ALL FOR empty(idkdo)
			IF eof('fseznam') OR empty(ico)
				REPL idkdo WITH "________________"
			  ELSE
				REPL idkdo WITH fseznam.m_add
				ENDIF
			ENDSCAN
		SET RELA TO
		SELE fseznam
		SET ORDER TO &_ord
		SELE (m._se)

* NaplnØn¡ proplacenìch Ÿ stek dle datumu zaplacen¡
		REPL ALL FOR !empty(zap) caszap WITH cas
		GO TOP

*		PRIV _se, _ord
*		_se=sele()
*		SELE fakturyu
*		_ord=order()
*		SET ORDER TO cis
*		SELE (_se)
*		SET RELA TO cis INTO fakturyu
*		REPL ALL faktury.o_naz WITH fakturyu.o_naz ;
*			FOR empty(faktury.o_naz) AND !empty(fakturyu.o_naz)
*		SET RELA TO
*		SELE fakturyu
*		SET ORDER TO &_ord
*		SELE (_se)

		ENDCASE
RETURN

PROCEDURE op_fakp
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(7), txt C(100), cas N(10,2), cle C(4), ;
			dph C(2), noe L, ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
	  CASE m._cfn=3
		RETURN type("FAKTURYP.noe")="U" OR len(cis)!=7
	  CASE m._cfn=4

* Rozç¡ýen¡ Ÿ¡sla faktury na 7 znak…
*		REPL ALL cis WITH left(cis,3)+'0'+substr(cis,4,3) FOR right(cis,1)=' '

		ENDCASE
RETURN

PROCEDURE op_faku
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(7), d_naz C(25), d_nazd C(25), ;
			d_uli C(25), d_mes C(25), d_psc C(6), d_dic C(14), ;
			d_ico C(10), d_or C(20), ;
			o_uli C(25), o_mes C(25), o_psc C(6), o_dic C(14), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
	  CASE m._cfn=3
		RETURN len("FAKTURYU.d_mes")!=25
	  CASE m._cfn=4

* Rozç¡ýen¡ Ÿ¡sla faktury na 7 znak…
*		REPL ALL cis WITH left(cis,3)+'0'+substr(cis,4,3) FOR right(cis,1)=' '

		ENDCASE
RETURN
**	Open FPLATBY
PROCEDURE op_plat
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(7), datzap D, cas N(10,2), zbytek N(10,2), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
	  CASE m._cfn=3
		RETURN type("fplatby.zbytek")="U" OR len(cis)!=7
	  CASE m._cfn=4

* Rozç¡ýen¡ Ÿ¡sla faktury na 7 znak…
*		REPL ALL cis WITH left(cis,3)+'0'+substr(cis,4,3) FOR right(cis,1)=' '
		ENDCASE
RETURN

PROCEDURE op_skpol
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(9), cfa C(7), ico C(8), dtv D, idkdo C(16), ;
			cas N(10,2), z22 N(10,2), z_5 N(10,2), z_0 N(10,2), ;
			d22 N(10,2), d_5 N(10,2), ost N(10,2), sleva N(5,2), ;
			doprava C(5), poznamka C(66), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON dtoc(dtv,1)+cis TAG dtv
		INDEX ON cas TAG cas
*		INDEX ON ico TAG ico
		INDEX ON cfa TAG cfa
		INDEX ON idkdo TAG idkdo
	  CASE m._cfn=3
		RETURN type("FSKLPOL.poznamka")="U"
	  CASE m._cfn=4

* Rozç¡ýen¡ Ÿ¡sla faktury na 7 znak…
		REPL ALL cfa WITH left(cfa,3)+'0'+substr(cfa,4,3) ;
			FOR !empty(cfa) AND right(cfa,1)=' '

* Pýev z n¡ na seznam podle fseznam.m_add
		PRIV _se, _ord
		_se=sele()
		SELE fseznam
		_ord=order()
		SET ORDER TO ico
		SELE (m._se)
		SET RELA TO ico INTO fseznam
		SCAN ALL FOR empty(idkdo)
			IF eof('fseznam') OR empty(ico)
				REPL idkdo WITH "________________"
			  ELSE
				REPL idkdo WITH fseznam.m_add
				ENDIF
			ENDSCAN
		SET RELA TO
		SELE fseznam
		SET ORDER TO &_ord
		SELE (m._se)

		WAIT CLEAR
		ENDCASE
RETURN
PROCEDURE op_skcis
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (czb C(8), katc C(4), txt C(70), ;
			cas N(10,2), dph C(2), ;
			ncena N(10,2), nsleva N(2), nspdan N(10,2), nclo N(5,2), ;
			njine N(10,2), nakup N(10,2), stav N(8), minstav N(8), ;
			skup1 C(30), skup2 C(30), akcsleva N(2), ;
			vratobal C(3), proviz N(12,6), ;
			jtxt C(70), jczb C(8), jkus N(6), ;
			vro1 C(70), czb1 C(8), kus1 N(6), ;
			vro2 C(70), czb2 C(8), kus2 N(6), ;
			vro3 C(70), czb3 C(8), kus3 N(6), ;
			vro4 C(70), czb4 C(8), kus4 N(6), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON katc+str(jkus,6) TAG katc
		INDEX ON czb TAG czb
		INDEX ON cs(txt) TAG txt
		INDEX ON cs(left(skup1,5)+left(txt,15)) TAG skup1
		INDEX ON cs(left(skup2,5)+left(txt,15)) TAG skup2
	  CASE m._cfn=3
		RETURN type('FSKLCIS.katc')='U'
	  CASE m._cfn=4
		ENDCASE
RETURN
PROCEDURE op_skpop
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(9), czb C(8), kus N(6), dph C(2), ;
			jcena N(10,2), akcsleva N(2), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
		INDEX ON czb TAG czb
		INDEX ON dph TAG dph DESC
	  CASE m._cfn=3
		RETURN type("FSKLPOLP.akcsleva")='U'
	  CASE m._cfn=4

**	DoplnØn¡ JCENA do vçech polo§ek kde je v JCENA = 0
*		PRIV se
*		se=sele()
*		SELE fsklcis
*		SET ORDER TO czb
*		SELE (m.se)
*		SET RELA TO czb INTO fsklcis
*		REPL ALL jcena WITH fsklcis.cas*kus FOR jcena=0 AND left(cis,1)="1"
*		SET RELA TO

*WAIT WIND NOWA "PýepoŸ¡t v m vìdejky."
*		SELE fsklpol
*		SCAN ALL FOR left(cis,1)="1"
*			sleva=sleva
*
*			PRIV _se, _rc, i
*			_se=sele()
*			SELE fsklpolp
*			_rc=recno()
*			SUM jcena TO z22 FOR fsklpolp.dph="22" AND fsklpol.cis=fsklpolp.cis
*			SUM jcena TO z_5 FOR fsklpolp.dph="_5" AND fsklpol.cis=fsklpolp.cis
*			SUM jcena TO z_0 FOR fsklpolp.dph="_0" AND fsklpol.cis=fsklpolp.cis
*			SUM jcena TO ost FOR fsklpolp.dph="--" AND fsklpol.cis=fsklpolp.cis
*		
*			**	VìpoŸet slevy
*			z22=m.z22*(1-m.sleva/100)
*			z_5=m.z_5*(1-m.sleva/100)
*		
*			d22=ceil(m.z22*2.2)/10
*			d_5=ceil(m.z_5*.5)/10
*			=gorec(m._rc)
*			SELE (m._se)
*			GATHER MEMVAR FIELDS z22, z_5, d22, z_5, z_0, ost
*
*			ENDSCAN
*		WAIT CLEAR
*		SELE (m.se)
		ENDCASE
RETURN
**	Open FMALOP
PROCEDURE op_malop
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(12), czb C(8), kus N(6), dph C(2), ;
			jcena N(10,2), akcsleva N(2), cisvyd C(9), flg L, ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cis TAG cis
	  CASE m._cfn=3
		RETURN type("FMALOP.flg")='U' OR len(cis)!=12
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_nakl
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (dat D, ico C(8), txt C(35), mno C(15), cas N(10), ;
			idkdo C(16), m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON dtos(dat) TAG dat
*		INDEX ON ico TAG ico
		INDEX ON idkdo TAG idkdo
	  CASE m._cfn=3
		RETURN type("fnaklady.idkdo")="U"
	  CASE m._cfn=4
* Pýev z n¡ na seznam podle fseznam.m_add
		PRIV _se, _ord
		_se=sele()
		SELE fseznam
		_ord=order()
		SET ORDER TO ico
		SELE (m._se)
		SET RELA TO ico INTO fseznam
		SCAN ALL FOR empty(idkdo)
			IF eof('fseznam') OR empty(ico)
				REPL idkdo WITH "________________"
			  ELSE
				REPL idkdo WITH fseznam.m_add
				ENDIF
			ENDSCAN
		SET RELA TO
		SELE fseznam
		SET ORDER TO &_ord
		SELE (m._se)

		WAIT CLEAR
		ENDCASE
RETURN

PROCEDURE op_text
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (text C(48), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(text) TAG text
	  CASE m._cfn=3
		RETURN type("FTEXT.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_fclen
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (clen C(4), jmeno C(20), casm N(9), casc N(9), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(clen) TAG clen
	  CASE m._cfn=3
		RETURN type("fclen.casm")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN

PROCEDURE op_ucty
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (banka C(20), ucet C(20), pozn C(20), ;
			m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(banka) TAG banka
	  CASE m._cfn=3
		RETURN type("fucty.m_add")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
PROCEDURE op_deal 
PARA _cfn, _nn
	DO CASE
	  CASE m._cfn=1
		CREAT DBF (m._nn) (cis C(3), jdeal C(25), rcic C(11), cas N(12,2), ;
			del L, m_user C(10), m_date D, m_time C(8), m_add C(16))
	  CASE m._cfn=2
		INDEX ON cs(jdeal) TAG jdeal
		INDEX ON cis TAG cis
	  CASE m._cfn=3
		RETURN type("fdealer.jdeal")="U"
	  CASE m._cfn=4
		ENDCASE
RETURN
***************************************
**	Uzavýe aktu ln¡ zpracov vanou firmu
PROCEDURE cldata
	IF file(m.ggf+"faktury.rei")
		ERASE (m.ggf+"faktury.rei")
		ENDIF
	DO cldata1
RETURN


********************************************************************
****						F U N K C E							****
********************************************************************

PROCEDURE tiskon
	SET ALTE TO &tskf
	SET ALTE ON
	SET CONS OFF
	??m.resetprn
RETURN

PROCEDURE tiskoff
	SET ALTE OFF
	SET ALTE TO
	SET CONS ON
RETURN
**	Obnova obrazovky
PROCEDURE refscr
	DO backgr WITH 'main'
	IF !empty(m.ggf)
		PRIVATE t
		t="éŸtovan  firma: "+alltrim(m.ggjme)
		@ 0,int((scols()-len(m.t))/2) SAY m.t
	ENDIF
RETURN

PROCEDURE winhelp
IF m.gl_usernam="JAKU" AND m.ladeni
	DO viewfile WITH "Prirucka.TEX",0,.T.
  ELSE
	DO viewfile WITH "Prirucka.TEX"
	ENDIF
RETURN

PROCEDURE procent
PARA pdel,pmax,phod
	PRIV a
	a=round(m.pdel/m.pmax*m.phod,0)
RETURN '°'+repl('°',int(a))

