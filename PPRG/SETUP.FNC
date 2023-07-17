*	Toto je pot©eba za©adit do ka‘d‚ho programu
********************************************************
****				DEFINICE PROSTžED‹	 			****
**** Definice setupov˜ch promˆnn˜ch jako glob ln¡ch ****
********************************************************
SELE 0
USE setekont
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

DO setekont

**	ALT+F9 - Nastaven¡ tiskov‚ho v˜stupu
DO tisk_out WITH .F.
ON KEY LABEL ALT+F9 DO tisk_out WITH .T.
********************************************************

**	Pomocn‚ p©¡kazy pro nastavov n¡ tisku
WAIT WIND NOWA 'Tiskov˜ v˜stup: '+imp_output+' - '+rtrim(m.&imp_output)

