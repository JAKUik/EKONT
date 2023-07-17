CLOSE ALL
CLEAR ALL

SET LIBRARY TO pprg\foxlib25

SET PRINT TO ppp.log
SET PRINT ON
? "*** Prekodovani programu do LATIN2 ***"
?

*	- P©ek¢dovat cel˜ program
*	  - Textov‚ soubory *.!!!,BAT,DOC,DOK,FRI,PR#,PR-,TXT a soubor mbm.-
*	  - Programy PRG (p©ekl dat jen maj¡-li FXP)
*	  - Screeny SCX
*	  - Reporty FRX
*	  - Def. datab ze FXD,NAZ a datab zi jobtasks.PJX  (mimo datab.fxd !)
*	  - Definice LIST_C  LCC
*	  - Helpy pro LIST_C  HLC
*	  - Helpy pro screeny  SPH


*<BEGIN_CODETABLE>		Nek¢dovat ‡e¨tinu
xt1='€ƒ…†‡ˆ‰Š‹Œ‘’•–—˜›œŸ¤¥¦§¨©ª«¬­µ¶·¸½¾ÆÇÏĞÑÒÓÔÕÖ×ØİŞàâåæçèéêìíüı'
xt2='¬ÔÒ›ŸØ·‘Ö’’µ§¦à…éìæ•íüœåÕŞâçıêè/!³º¿¿ÙÙ³ºÍÄÍÄÀÀÚÚºÍººÏÏÏÏÏÏÏÏÏÏÏÏ'
*<END_CODETABLE>


DO scanfiles WITH ''

zmena=.F.
IF file('mbm.-')
	? 'MBM.-'
	DO cvt_txt WITH '', 'MBM', '.-'
	?? '  -', iif(m.zmena, "OK", '---')
ENDIF

?
? "--- Konec ---"
SET PRINT OFF
SET PRINT TO
RETURN



********************
PROCEDURE scanfiles
PARAM dir

WAIT WIND m.dir NOWAIT

PRIVATE i, n, ff, a, fname, ext, zmena
n=adir(ff, m.dir+'*.*')
IF m.n>0
	=asort(ff)
	FOR i=1 TO m.n
		a=at('.', ff[m.i,1])
		fname=iif(m.a>0, left(ff[m.i,1], m.a-1), ff[m.i,1])
		ext=iif(m.a>0, subst(ff[m.i,1], m.a), '.')
		zmena=.F.
		DO CASE
		CASE m.ext=='.' OR inlist(m.ext, '.!!!', '.BAT', '.DOC', '.DOK', '.FRI', '.PR#', '.PR-', '.TXT', '.TEX', '.ENI')
			? m.dir+m.fname+m.ext
			DO cvt_txt WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
		CASE m.ext='.PRG' AND !(m.fname=='PPP')
			? m.dir+m.fname+m.ext
			DO cvt_txt WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
			IF file(m.dir+m.fname+'.FXP')
				ERASE (m.dir+m.fname+'.ERR')
				COMPILE (m.dir+m.fname+m.ext)
				IF file(m.dir+m.fname+'.ERR')
					?? " - ERROR !!!"
					IF inkey(0)=27
						CANCEL
					ENDIF
				ENDIF
			ENDIF
		CASE m.ext='.SPR'	&& Zatim !!!!
			? m.dir+m.fname+m.ext
			DO cvt_txt WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
			IF file(m.dir+m.fname+'.SPX')
				ERASE (m.dir+m.fname+'.ERR')
				COMPILE (m.dir+m.fname+m.ext)
				IF file(m.dir+m.fname+'.ERR')
					?? " - ERROR !!!"
					IF inkey(0)=27
						CANCEL
					ENDIF
				ENDIF
			ENDIF
		CASE m.ext='.SCX'
			? m.dir+m.fname+m.ext
			DO cvt_dbf WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
		CASE m.ext='.FRX'
			? m.dir+m.fname+m.ext
			DO cvt_dbf WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
		CASE (m.ext='.FXD' AND m.fname!='DATAB') OR m.ext='.NAZ' OR (m.ext='.PJX' AND m.fname='JOBTASKS')
			? m.dir+m.fname+m.ext
			DO cvt_dbf WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
		CASE m.ext='.LCC'
			? m.dir+m.fname+m.ext
			DO cvt_dbf WITH m.dir, m.fname, m.ext
			?? '  -', iif(m.zmena, "OK", '---')
			IF file(m.dir+m.fname+'.LCX')
				DO list_cg WITH m.dir+m.fname+m.ext, '/NOASK'
			ENDIF
		CASE m.ext='.HLC'
		CASE m.ext='.SPH'
		ENDCASE
	ENDFOR
ENDIF
RELEASE i, n, ff, a, fname, ext


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
		DO scanfiles WITH m.dir+dd[m.i]+'\'
	ENDFOR
ENDIF

WAIT CLEAR
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
	=fclose(m.fi)
	=usererror("Nelze vytvorit: "+m.dir+m.fname+'.TMP')
	RETURN
ENDIF

PRIVATE bf1, bf2, cd
zmena=.F.
cd=.T.
DO WHILE !feof(m.fi)
	bf1=fgets(m.fi, 2048)
	IF upper(alltr(chrt(m.bf1,chr(9),'')))='*<END_CODETABLE>'
		cd=.T.
	ENDIF
	IF m.cd
		bf2=chrtran(m.bf1, m.xt1, m.xt2)
		IF !m.zmena AND m.bf1!=m.bf2
			zmena=.T.
		ENDIF
	ELSE
		bf2=m.bf1
	ENDIF
	IF fputs(m.fo, m.bf2)<len(m.bf2)
		=fclose(m.fi), fclose(m.fo)
		ERASE (m.dir+m.fname+'.TMP')
		=usererror("Chyba zapisu: "+m.dir+m.fname+'.TMP')
		RETURN
	ENDIF
	IF upper(alltr(chrt(m.bf1,chr(9),'')))='*<BEGIN_CODETABLE>'
		cd=.F.
	ENDIF
ENDDO
=fclose(m.fi), fclose(m.fo)
IF !m.zmena
	ERASE (m.dir+m.fname+'.TMP')
	RETURN
ENDIF

ERASE (m.dir+m.fname+m.ext)
RENAME (m.dir+m.fname+'.TMP') TO (m.dir+m.fname+m.ext)
RETURN




********************
PROCEDURE cvt_dbf
PARAM dir, fname, ext

SELECT 0
USE (m.dir+m.fname+m.ext) ALIAS db EXCLUSIVE
zmena=.F.

PRIVATE i, n, aa, c, f1, w, j, l, cd
n=afields(aa)
c=0
FOR i=1 TO m.n
	IF aa[m.i, 2]$'CM'
		c=m.c+1
		aa[m.c]=aa[m.i, 1]
	ENDIF
ENDFOR
IF m.c>0
	DIME aa[m.c]
	FOR i=1 TO m.c
		f1=aa[m.i]
		SCAN ALL FOR &f1 != chrt(&f1, m.xt1, m.xt2) NOOPTIMIZE
			IF '*<BEGIN_CODETABLE>'$&f1
				w=''
				n=memlines(&f1)
				cd=.T.
				FOR j=1 TO m.n
					l=mline(&f1, m.j)
					IF upper(alltr(chrt(m.l,chr(9),'')))='*<END_CODETABLE>'
						cd=.T.
					ENDIF
					IF m.cd
						l=chrtran(m.l, m.xt1, m.xt2)
						IF !m.zmena AND !(m.l==mline(&f1, m.j))
							zmena=.T.
						ENDIF
					ENDIF
					IF upper(alltr(chrt(m.l,chr(9),'')))='*<BEGIN_CODETABLE>'
						cd=.F.
					ENDIF
					w=m.w+m.l+iif(m.j<m.n, chr(13)+chr(10), '')
				ENDFOR
				IF m.zmena
					REPLACE &f1 WITH m.w
				ENDIF
				RELEASE w, l
			ELSE
				REPLACE &f1 WITH chrt(&f1, m.xt1, m.xt2)
				zmena=.T.
			ENDIF
		ENDSCAN
	ENDFOR
ENDIF
USE IN db

RETURN
