**	Procedura na vytiçtØn¡ souboru s konverz¡ ý¡d¡c¡ch znak… tisk rny
**	na ESC sekvence
**	vf_lmrg	- O kolik posunou pýed zobrazen¡m sestavu sloupc…
**	edt		- .T. povoluje editaci zobrazen‚ho souboru
* PROCEDURE tisk
PARA vf_lmrg, edt
	IF m.set132_on != -1
		=vmod(m.set132_on)
		ON ERROR err=1405
		RUN cesky
		ON ERROR &_onerr
		ENDIF
	=viewfile(m.tskf, m.vf_lmrg, m.edt)
	IF m.set132_off != -1
		=vmod(m.set132_off)
		ENDIF
	IF ask(-1,-1,"N","Vytisknout sestavu")
		* nacist esc sekvence
		DO TISK_OUT WITH .F.,.T.

*<BEGIN_CODETABLE>
	* KA    Kamenicky->ASCII
	*kod1_ascii=[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«]
	*kod2_ascii=[Ced„DTceELIllŽAEzZo”OuUy™šSLYRtaiounNUOsrrR]

	* KL    Kamenicky->LATIN2
	*kod1_lat2 =[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«]
	*kod2_lat2 =[¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]

	* KAR   Kamenicky->ASCII + konverze ramecku
	*kod1_asc_r=[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚ]
	*kod2_asc_r=[Ced„DTceELIllŽAEzZo”OuUy™šSLYRtaiounNUOsrrR¬­®¯°±²||||++||++++++--|-+||++==|=|=-=-++++||++]

	* KLR   Kamenicky->LATIN2 + konverze ramecku
	*kod1_lat2k=[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚ]
	*kod2_lat2k=[¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè¬­®¯°±²||||++||++++++--|-+||++==|=|=-=-++++||++]

	* KLB   Kamenicky->LATIN2 + korekce ramecku
	*kod1_lat2r=[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚ]
	*kod2_lat2r=[¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè¬­®¯°±²³´³º¿¿¹º»¼ÙÙ¿ÀÁÂÃÄÅ³ºÈÉÊËÌÍÎÍÄÍÄÀÀÚÚºÍÙÚ]

	* KLJ   Kamenicky->LATIN2 + jednoduche ramecky
	*kod1_lat2j=[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚ]
	*kod2_lat2j=[¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè¬­®¯°±²³´´´¿¿´³¿ÙÙÙ¿ÀÁÂÃÄÅÃÃÀÚÁÂÃÄÅÁÁÂÂÀÀÚÚÅÅÙÚ]

	PRIV kodz, kodna
		DO CASE
		  CASE prekodtisk="ASCII"
*			kodz =[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«]
			kodz= [¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]
			kodna=[Ced„DTceELIllŽAEzZo”OuUy™šSLYRtaiounNUOsrrR]
		  CASE prekodtisk="KAM"
*			kodz =[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«]
			kodz= [¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]
			kodna=[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«]
		  CASE prekodtisk="LAT2"
*			kodz =[€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«]
*			kodna=[¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]
			kodz= [¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]
			kodna=[¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]
		  CASE prekodtisk="WIN"
*			kodz= [€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚ]
			kodz= [¬‚Ô„Ò›ŸØ·‘Ö’’Žµ§¦“”à…éì™šæ•íüœ ¡¢£åÕÞâçýêè]
*			kodna=[ÈüéïäÏèìÌÅå¾¾ÄÁÉžŽôöÓùÚýÖÜŠ¼ÝØáíóúòÒùÔšøàÀ¬­®¯°±²||||++||++++++--|-+||++==|=|=-=-++++||++]
			kodna=[ÈüéïäÏèìÌÅå¾¾ÄÁÉžŽôöÓùÚýÖÜŠ¼ÝØáíóúòÒùÔšøàÀ]
		  OTHERWISE
			kodz =""
			kodna=""
		  ENDCASE
*<END_CODETABLE>

		SET CONS OFF
		_pscode=m.resetprn
* konverze + tisk
*		SET PRINT TO (m.imp_output)
	    SET PRINT ON
		x = FOPEN(m.tskf,0)
		DO WHILE !FEOF(x)
			q = FREAD(x,128)
			q = STRTRAN(q,"",cnon)
			q = STRTRAN(q,"",cnoff)
			q = STRTRAN(q,"",ecnon)
			q = STRTRAN(q,"",ecnoff)
			q = STRTRAN(q,"",pica)
			q = STRTRAN(q,"",elit)
			q = STRTRAN(q,"",nlq)
			q = STRTRAN(q,"",draft)
			q = STRTRAN(q,"",dblon)
			q = STRTRAN(q,"",dbloff)
			q = STRTRAN(q,"",resetprn)
		 	q = STRTRAN(q,chr(26),"")
		 	IF !empty(m.kodz)
				q = CHRTRAN(q,m.kodz,m.kodna)
				ENDIF
		 	?? q
			ENDDO
		=FCLOSE(x)
		SET PRINT OFF
		SET PRINT TO
		SET CONS ON
		* nacist opet nahradni znaky
		DO TISK_OUT WITH .F.
	  ELSE
		m.esc=.F.
		ENDIF
RETURN
