PARA baze
ON ERROR DO chyba
PRIV n

SET PATH TO pprg

baze=".\data\"+m.baze

wait wind baze

USE &baze
BROW

RETURN



PROCEDURE chyba
WAIT WIND "Chyba Ÿ¡slo: "+strn(error())
RETURN TO MASTER

