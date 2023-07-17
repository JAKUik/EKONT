*******************
****  Bud¡Ÿek  ****
*******************
*PROCEDURE budik
PUSH KEY CLEAR

**	Nastaven¡ bud¡Ÿku
PROCEDURE budik
PARA bflg	&&	.T.  Nastaven¡		.F. Vyhodnoceni
	DIME b_typ(5)
	b_typ(1)="Jednou  "
	b_typ(2)="Prc.den "
	b_typ(3)="DennØ   "
	b_typ(4)="TìdnØ   "
	b_typ(5)="MØs¡ŸnØ "

	PRIV _sele, user
	_sele=select()
	user=getenv('USERNAME')
	USE budik ORDER cas
	SET FILT TO budik.user=m.user OR budik.user='ALL'

	DO list_c WITH 6,-1,12,0,;
		[str(rok,2)+' ³ '+left(mline(budik.txt,1),20)],;
		" Seznam bud¡Ÿku pro "+m.user+" ",;
		"//éŸet ³ Text",;
		iif(m.flg,"\ F1-Help    ENTER-VìbØr £Ÿtu   F7-Tý¡dit podle ...",;
				  "\ F1-Help    ENTER-VìbØr analytiky   F7-Tý¡dit podle ..."),;
		iif(m.flg,'','INS:	DO osn_pri WITH .T.		## INS-Pýid n¡ £Ÿtu'),;
		'F4#:	DO osn_pri WITH .F.		## F4-Oprava £Ÿtu',;
		'CR#:	m.toexit=.T.			## ENTER-VìbØr '+iif(m.flg,'£Ÿtu','analytiky')




RETURN

