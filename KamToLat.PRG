**************************************
****             LATIN2           ****
**************************************

PARAM FullPath

*SET TALK WIND
*SET TALK OFF
*SET DELE ON
*SET SAFE OFF
*CLOSE ALL
*CLEAR ALL
*CLEAR

*SET LIBRARY TO pprg\foxlib25


*PRIVATE ladeni
*ladeni=.T.


*------------------------------------------------------------*
*---  Nejprve zlikvidujeme fajfky (ASCII=251) z datab†z°  ---*
*------------------------------------------------------------*
*IF !file('up_latdb.dbf')
*	?
*	? "Nahrazeni specialniho znaku"
*
*	DO fajfka WITH soubo_dbf_bez_pripony, polozka, nova_hodnota
*ENDIF


*-------------------------------*
*---  Upgrade dat na LATIN2  ---*
*-------------------------------*
*<BEGIN_CODETABLE>		Nek¢dovat üeÁtinu
PRIVATE xt1, xt2
xt1='ÄÉÖÜáàâäãåçèëíïñóòõúùûü§•¶ß®©™´¨≠µ∂∑∏Ωæ∆«œ–—“”‘’÷◊ÿ›ﬁ‡‚ÂÊÁËÈÍÏÌ¸˝'
xt2='¨‘“õüÿ∑ë÷ííµß¶‡ÖÈÏÊïÌ¸úÂ’ﬁ‚Á˝ÍË/!≥∫øøŸŸ≥∫ÕƒÕƒ¿¿⁄⁄∫Õ∫∫œœœœœœœœœœœœ'
*<END_CODETABLE>


?
? "Upgrade dat na LATIN2"

*- Upgrade pro p˝ek¢dov†n° vÁech datab†z° a .MEM v programu
*  - Datab†ze *.DBF a ktr.ZAL (mimo srcreind.DBF, usersave.DBF [data],
*      pokladni.DBF [PSW] a oper.DBF [heslo])
*  - MEM soubory CFG,MEM,SAV,SET (mem, mimo isklprod.SET a key4ccd.SET)
*  - MEM: config.set ZVLµÊõ
*  - VÁechny PRG p˝ejmenovat na PR% a FX% (ne v MBM3\)
*  - VÁechny FXP starÁ° neß {26.11.99} p˝ejmenovat FX% (ne v MBM3\)

SELE 0

IF file('up_latdb.dbf')
	ERASE up_latdb.dbf
ENDIF

	=perc([p], 0, "Vytv†˝°m seznam datab†z° ...")
	CREATE CURSOR ldb (fil C(92), stav C(1), typ C(3), dir C(80), fname C(8), ext C(4), size N(10))
	INDEX ON fil TAG fil
	*DO dataf_add WITH 'TXT', 'POKLADNA\DATA\', 'KEY4CCD', '.SET'
	*DO dataf_add WITH 'TXT', 'DATA\', 'ERRDBF', '.LOG'
	DO datafiles WITH m.fullpath
	=perc([p], 1/2)
	SELECT ldb
	IF recc()>0
		SET ORDER TO
		COPY TO up_latdb FIELDS stav, typ, dir, fname, ext, size
	ENDIF
	=perc([p], 2/2)
	USE

USE up_latdb ALIAS listdb
=perc([p], 0, "Upgrade dat na LATIN2 ...")
PRIVATE hotovo, celkem, stav, _x1, _x2, _xs
STORE 0 TO hotovo, celkem
SUM size TO celkem FOR stav=' '
SCAN ALL FOR stav=' '
	? ' ', alltr(dir)+alltr(fname)+alltr(ext)
*	IF !ask(-1,-1,"A","Konvertovat soubor: "+alltr(dir)+alltr(fname)+alltr(ext))
*		RETURN 0
*		ENDIF
	stav=''
	DO CASE
	CASE typ='DBF'
		DO cvt_dbf WITH alltr(dir), alltr(fname), alltr(ext)
	CASE typ='MEM'
		_x1=m.xt1
		_x2=m.xt2
		_xs=m.stav
		DO cvt_mem WITH alltr(dir), alltr(fname), alltr(ext)
		stav=m._xs
		RELEASE _x1, _x2, _xs
	CASE typ='TXT'
		DO cvt_txt WITH alltr(dir), alltr(fname), alltr(ext)
	ENDCASE
	SELECT listdb
	REPLACE stav WITH m.stav
	?? "  -", iif(m.stav='*', "OK", iif(m.stav='i', "(reindex)", "---"))
	hotovo=m.hotovo+listdb.size
	=perc([p], m.hotovo/m.celkem)
ENDSCAN
USE
=perc([p])
WAIT WIND "Konverze byla £spÿÁnÿ ukonüena"

RETURN



*---------------------*
*---  Podprogramy  ---*
*---------------------*


********************
PROCEDURE fajfka
PARAM fname, fld, zn

PRIVATE ff, att_dbf, att_fpt, att_cdx
att_dbf=iif(adir(ff, m.fname+'.DBF', 'HS')=1, ff[1,5], 'A')
att_fpt=iif(adir(ff, m.fname+'.FPT', 'HS')=1, ff[1,5], 'A')
att_cdx=iif(adir(ff, m.fname+'.CDX', 'HS')=1, ff[1,5], 'A')
='R'$m.att_dbf AND _setfattr(m.fname+'.DBF', 'A')
='R'$m.att_fpt AND _setfattr(m.fname+'.FPT', 'A')
='R'$m.att_cdx AND _setfattr(m.fname+'.CDX', 'A')
RELEASE ff

PRIVATE flt, t
flt=m.fld+[=']+chr(251)+[']

USE (m.fname) ALIAS db EXCLUSIVE
_tally=0
REPLACE ALL &fld WITH m.zn FOR &flt
t=_tally
USE

IF m.t>0
	? ' ', m.fname, m.t
ENDIF

IF 'R'$m.att_dbf OR (m.t>0 AND !('A'$m.att_dbf))
	=_setfattr(m.fname+'.DBF', m.att_dbf)
ENDIF
IF 'R'$m.att_fpt OR (m.t>0 AND !('A'$m.att_fpt))
	=_setfattr(m.fname+'.FPT', m.att_fpt)
ENDIF
IF 'R'$m.att_cdx OR (m.t>0 AND !('A'$m.att_cdx))
	=_setfattr(m.fname+'.CDX', m.att_cdx)
ENDIF
RETURN





*******************
PROCEDURE datafiles
PARAM dir

WAIT WIND m.dir NOWAIT

* CURSOR ldb: fil, stav, typ, dir, fname, ext, size

PRIVATE i, n, ff, a, fil, stav, typ, fname, ext, size, onerr
onerr=on('error')
stav=''
n=adir(ff, m.dir+'*.*')
IF m.n>0
	=asort(ff)
	FOR i=1 TO m.n
		a=at('.', ff[m.i,1])
		IF m.a>0
			fname=left(ff[m.i,1], m.a-1)
			ext=subst(ff[m.i,1], m.a)
			typ=''
			DO CASE
			CASE m.ext='.DBF'
				typ='DBF'
			CASE inlist(m.ext, '.MEM')
				typ='MEM'
			ENDCASE
			IF !empty(m.typ)
				fil=m.dir+m.fname+m.ext
				size=ff[m.i,2]
				IF !seek(m.fil, 'ldb')
					INSERT INTO ldb FROM MEMVAR
				ENDIF
			ENDIF
		ENDIF
	ENDFOR
ENDIF
RELEASE i, n, ff, a, fil, stav, typ, fname, ext, size

PRIVATE i, n, dd, c
n=adir(dd, m.dir+'*.*', 'D')
c=0
FOR i=1 TO m.n
	IF 'D'$dd[m.i,5] AND left(dd[m.i,1],1)!='.'
		c=m.c+1
		dd[m.c]=dd[m.i,1]
	ENDIF
ENDFOR
IF m.c>0
	DIME dd[m.c]
	=asort(dd)
	FOR i=1 TO m.c
		DO datafiles WITH m.dir+dd[m.i]+'\'
	ENDFOR
ENDIF

WAIT CLEAR
RETURN




********************
PROCEDURE dataf_add
PARAM typ, dir, fname, ext

PRIVATE d, f, ff
d=fullpath(alltr(m.dir))
f=upper(m.d+alltr(m.fname)+alltr(m.ext))
IF adir(ff, m.f, 'HS')!=1 OR seek(m.f, 'ldb')
	RETURN
ENDIF

INSERT INTO ldb (fil, typ, dir, fname, ext, size);
	VALUES (m.f, upper(alltr(m.typ)), m.d,;
	 upper(alltr(m.fname)), upper(alltr(m.ext)), ff[1,2])
RETURN




********************
PROCEDURE cvt_txt
PARAM dir, fname, ext

PRIVATE fi, fo
fi=fopen(m.dir+m.fname+m.ext)
IF m.fi<1
	=usererror("Nelze otevrit: "+m.dir+m.fname+m.ext)
	RETURN
ENDIF
fo=fcreate(m.dir+m.fname+'.TMP')
IF m.fo<1
	=_setfattr(m.dir+m.fname+'.TMP', 'A')
	fo=fcreate(m.dir+m.fname+'.TMP')
ENDIF
IF m.fo<1
	=fclose(m.fi)
	=usererror("Nelze vytvorit: "+m.dir+m.fname+'.TMP')
	RETURN
ENDIF

PRIVATE zmena, bf1, bf2
zmena=.F.
DO WHILE !feof(m.fi)
	bf1=fread(m.fi, 32000)
	bf2=chrtran(m.bf1, m.xt1, m.xt2)
	IF !m.zmena AND m.bf1!=m.bf2
		zmena=.T.
	ENDIF
	IF fwrite(m.fo, m.bf2)!=len(m.bf2)
		=fclose(m.fi), fclose(m.fo)
		ERASE (m.dir+m.fname+'.TMP')
		=usererror("Chyba zapisu: "+m.dir+m.fname+'.TMP')
		RETURN
	ENDIF
ENDDO
=fclose(m.fi), fclose(m.fo)
IF !m.zmena
	ERASE (m.dir+m.fname+'.TMP')
	stav='N'
	RETURN
ENDIF

PRIVATE ff, att
att=iif(adir(ff, m.dir+m.fname+m.ext, 'HS')=1, ff[1,5], 'A')
RELEASE ff
=_setfattr(m.dir+m.fname+m.ext, 'A')

ERASE (m.dir+m.fname+m.ext)

*****************************************************************
*WAIT WIND m.dir+m.fname+m.ext
RENAME (m.dir+m.fname+'.TMP') TO (m.dir+m.fname+m.ext)
=_setfattr(m.dir+m.fname+m.ext, m.att)
stav='*'

RETURN




********************
PROCEDURE cvt_dbf
PARAM dir, fname, ext

PRIVATE oldsel
oldsel=select()

PRIVATE ff, att_dbf, att_fpt, att_cdx
att_dbf=iif(adir(ff, m.dir+m.fname+m.ext,  'HS')=1, ff[1,5], 'A')
att_fpt=iif(adir(ff, m.dir+m.fname+'.FPT', 'HS')=1, ff[1,5], 'A')
att_cdx=iif(adir(ff, m.dir+m.fname+'.CDX', 'HS')=1, ff[1,5], 'A')
='R'$m.att_dbf AND _setfattr(m.dir+m.fname+m.ext, 'A')
='R'$m.att_fpt AND _setfattr(m.dir+m.fname+'.FPT', 'A')
='R'$m.att_cdx AND _setfattr(m.dir+m.fname+'.CDX', 'A')
RELEASE ff


SELECT 0
ON ERROR *
USE (m.dir+m.fname+m.ext) ALIAS db EXCLUSIVE
ON ERROR
IF !used()
	USE (m.dir+m.fname+m.ext) ALIAS db EXCLUSIVE
ENDIF

PRIVATE zmena
zmena=.F.


PRIVATE n, aa, c, mimo, i, f1
mimo='-'
DO CASE
CASE m.fname=='USERS'
	mimo='HESLO'
ENDCASE
n=afields(aa)
c=0
FOR i=1 TO m.n
	IF aa[m.i, 2]$'CM' AND !(aa[m.i, 1]==m.mimo)
		c=m.c+1
		aa[m.c]=aa[m.i, 1]
	ENDIF
ENDFOR
IF m.c>0
	DIME aa[m.c]
	FOR i=1 TO m.c
		f1=aa[m.i]
		=perc([xt], (m.i-1)/m.c, m.fname+': '+m.f1)
		SET DELETED OFF
		_tally=0
		SET TALK WIND
		SET TALK ON
		REPLACE ALL &f1 WITH chrt(&f1, m.xt1, m.xt2);
			FOR &f1 != chrt(&f1, m.xt1, m.xt2) NOOPTIMIZE
		SET TALK OFF
		IF _tally>0
			zmena=.T.
		ENDIF
		SET DELETED ON
	ENDFOR
	=perc([xt])
ENDIF


* Indexy
PRIVATE i, csidx
csidx=.F.
i=1
DO WHILE !empty(tag(m.i))
	IF 'CS('$key(m.i) OR 'CS('$key(m.i) OR 'CSUP('$key(m.i) OR 'CSUP('$key(m.i)
		csidx=.T.
		EXIT
	ENDIF
	i=m.i+1
ENDDO
IF m.csidx
	SET TALK WIND
	SET TALK ON
	REINDEX
	SET TALK OFF
ENDIF


USE IN db
IF setdbfcp(m.dir+m.fname+m.ext, 852)<0
	? "  -- Nelze nastavit k¢dovou str†nku !!!!!"
	?
	WAIT WIND
ENDIF
=_setfattr(m.dir+m.fname+m.ext, m.att_dbf)
=_setfattr(m.dir+m.fname+'.FPT', m.att_fpt)
=_setfattr(m.dir+m.fname+'.CDX', m.att_cdx)

stav=iif(m.zmena, '*', iif(m.csidx, 'i', 'N'))

SELECT (m.oldsel)
RETURN







********************
PROCEDURE cvt_mem
PARAM _d, _f, _e
PRIVATE ALL EXCEPT _x?

PRIVATE _i, _a, _n, _l, _c, _m, _o, _q, _z, _w
STORE 0 TO _a, _n

DO LVar_Mem WITH m._d+m._f+m._e, [_a], m._n, .T.
IF m._n<1
	RETURN
ENDIF

STORE 0 TO _c, _m
_l=_a[1, 1]
FOR _i=1 TO m._n
	IF !('['$_a[m._i, 1])
		_m=max(m._m, len(_a[m._i, 1]))
		DO WHILE _a[m._i, 1]!=m._l AND len(m._l)>0
			_l=left(m._l, len(m._l)-1)
		ENDDO
	ENDIF
	IF _a[m._i, 2]='C'
		_c=m._c+1
	ENDIF
ENDFOR
IF m._c<1 OR empty(m._l)
	RETURN
ENDIF
IF m._m>len(m._l)
	_l=m._l+iif(m._m>=10, '*', repli('?', m._m-len(m._l)))
ENDIF

_o=on('error')
ON ERROR _q=.T.
_q=.F.
RESTORE FROM (m._d+m._f+m._e) ADDITIVE
ON ERROR &_o
IF m._q
	RETURN
ENDIF
RELEASE _o, _q

*	? '      ('+m._l+')'
*	FOR _i=1 TO m._n
*		IF _a[m._i, 2]='C'
*			? '   ', padr(_a[m._i, 1], max(len(_a[m._i, 1]), 10)), '=', '"'+eval(_a[m._i, 1])+'"'
*		ENDIF
*	ENDFOR

_z=.F.
FOR _i=1 TO m._n
	IF _a[m._i, 2]='C'
		_w=eval(_a[m._i, 1])
		IF m._w!=chrt(m._w, m._x1, m._x2)
			&_a[m._i, 1]=chrt(m._w, m._x1, m._x2)
			_z=.T.
		ENDIF
	ENDIF
ENDFOR

IF m._z
	SAVE TO (m._d+m._f+m._e) ALL LIKE &_l
	_xs='*'
ELSE
	_xs='N'
ENDIF
RETURN









*****************************************************
****  Naüten° seznamu promÿnnÏch v souboru .MEM  ****
*****************************************************
PROCEDURE LVar_Mem
PARAM __Fname, __Pole, __Num, __ExpArray
*
*  __Fname    - JmÇno souboru (vüetnÿ cesty a p˝°pony)
*  __Pole     - JmÇno c°lovÇho pole (mus° existovat a bude p˝edimenzov†no)
*  __Num      - Promÿnn† ve kterÇ program vrac° poüet naütenÏch jmen
*  __ExpArray - .T.=rozepsat pole, .F.=nerozepisovat pole 
*
* Sloupce vÏslednÇho pole:
*   1   JmÇno promÿnnÇ   C(1-10) (pro rozpis pole C(1-23))
*   2   Typ promÿnnÇ     C(1)
*       C = Character
*       N = Numeric
*       D = Date
*       L = Logical
*       S = Screen
*       A = Array
*   3   DÇlka promÿnnÇ   N(1-10)  (S:sloupcÖ; A:˝†dkÖ)
*   4   Poüet des.m°st   N(1-3)   (S:˝†dkÖ;   A:sloupcÖ)
*   5   Offset dat v souboru N(1-10)
*   6   DÇlka dat v souboru  N(1-10)


PRIVATE __f
__f=fopen(m.__Fname)
IF m.__f<1
	__Num=-1
	RETURN
ENDIF

PRIVATE __dim, __a, __n
__dim=20
DIME __a[m.__dim, 6]
__n=0

PRIVATE __off, __hd, __name, __typ, __len, __wid, __dec, __larr, __a1, __a2, __i1, __i2
__larr='-'
__off=0
DO WHILE !feof(m.__f)
	__hd=fread(m.__f, 32)
	IF len(m.__hd)=1 AND m.__hd=chr(26)
		EXIT
	ENDIF
	IF len(m.__hd)!=32
		=fclose(m.__f)
		__Num=-2
		RETURN
	ENDIF
	__off=m.__off+len(m.__hd)
	__name=left(m.__hd, min(at(chr(0), m.__hd)-1, 10))
	__typ=subst(m.__hd, 12, 1)
	__wid=asc(subst(m.__hd, 17, 1))
	__dec=asc(subst(m.__hd, 18, 1))
	__inarr=(m.__name==m.__larr)
	DO CASE
	CASE m.__typ='C'
		__len=m.__wid
		__wid=max(m.__wid-1, 0)
	CASE m.__typ='H'
		__typ='C'
		__len=c2num(m.__hd, 13, 4)
		__wid=max(m.__len-1, 0)
	CASE m.__typ='S'
		__len=c2num(m.__hd, 13, 4)
	CASE m.__typ='N'
		__len=8
	CASE m.__typ='D'
		__len=8
		IF m.__wid=0
			__wid=8
		ENDIF
	CASE m.__typ='L'
		__len=1
	CASE m.__typ='A'
		__len=4
		__inarr=.F.
		__larr=m.__name
		STORE 0 TO __a1, __a2, __i1, __i2
	OTHERWISE
		=fclose(m.__f)
		__Num=-2
		RETURN
	ENDCASE

	IF !m.__inarr OR m.__ExpArray
		__n=m.__n+1
		IF m.__n>m.__dim
			__dim=m.__dim+10
			DIME __a[m.__dim, 6]
		ENDIF
		__a[m.__n, 1] = m.__name
		__a[m.__n, 2] = m.__typ
		__a[m.__n, 3] = m.__wid
		__a[m.__n, 4] = m.__dec
		__a[m.__n, 5] = m.__off
		__a[m.__n, 6] = m.__len
		IF m.__inarr
			IF m.__a2>1
				__i1=max(m.__i1, 1)
				__i2=m.__i2+1
				IF m.__i2>m.__a2
					__i2=1
					__i1=m.__i1+1
				ENDIF
				__a[m.__n, 1] = m.__name+'['+stri(m.__i1)+','+stri(m.__i2)+']'
			ELSE
				__i1=m.__i1+1
				__a[m.__n, 1] = m.__name+'['+stri(m.__i1)+']'
			ENDIF
		ENDIF
	ENDIF

	__off=m.__off+m.__len
	IF m.__len>0
		IF m.__typ='A' AND m.__len=4
			__hd=fread(m.__f, 4)
			IF len(m.__hd)=4
				__a1 = c2num(m.__hd, 1, 2)
				__a2 = c2num(m.__hd, 3, 2)
				IF m.__wid=0 AND m.__dec=0
					__a[m.__n, 3] = m.__a1
					__a[m.__n, 4] = m.__a2
				ENDIF
			ENDIF
		ELSE
			=fseek(m.__f, m.__len, 1)
		ENDIF
	ENDIF
ENDDO
RELEASE __off, __hd, __name, __typ, __len, __wid, __dec, __larr, __a1, __a2, __i1, __i2
=fclose(m.__f)

IF m.__n>0
	DIME &__Pole.[m.__n, 6]
	=acopy(__a, &__Pole, 1, m.__n*6)
ENDIF
__Num=m.__n

RETURN
