// Doplní zprava tvrdé mezery pro formátování
function TmZa(dta,pm)  //dta -> data od Gerta z funkce getFld; pm -> poèet mezer na konec
  {
   var i=1,s="";
   if (pm==null) pm=1;
   for (; i<=pm; i++,s+="&nbsp;") ;
   return dta+s;
  }

// Doplní zleva tvrdé mezery pro formátování
function TmPred(dta,pm)  //dta -> data od Gerta z funkce getFld; pm -> poèet mezer na zaèátek
  {
   var i=1,s="";
   if (pm==null) pm=3;
   for (; i<=pm; i++,s+="&nbsp;") ;
   return s+dta;
  }

// Zjistí verzi prohlížeèe
function getIEVer() 
  {
   var ua=window.navigator.userAgent; 
   var msie=ua.indexOf("MSIE");
   if (msie<=0 || window.navigator.platform!="Win32") return 0;
   return parseInt(ua.substring(msie+5,ua.indexOf(".", msie)));
  }

// Zjistí, jestli to je MSIE 5.5
function isVer55(){
  var ua=window.navigator.userAgent; 
  var msie=ua.indexOf("MSIE");
  var vs=ua.substring(msie+5,ua.indexOf(".", msie)+2);
  return vs=="5.5"? true: false;
}

// Hlášení ve stavovém øádku okna
function StatOn() {status="Pøipravuji sestavu. Vyèkejte, prosím.";} 

// Uvede property status do pùvodního stavu
function StatOff(dta) {if (zmena) status="Hotovo. V tomto formuláøi mùžete nìkteré údaje zmìnit."; else status=""; return dta;} 

function windowInit(){
  // Schová tlaèítko Vytisknout... pøed tiskem
  function window.onbeforeprint() {Tisk.style.display="none";}
  function document.body.onload() {Tisk.style.display="";}

  // Odkryje tlaèítko Vytisknout... po tisku
  function window.onafterprint() {Tisk.style.display="";}
}

// Pøidá na zaèátek sestavy tlaèítko Vytisknout tuto stránku
function ButtonTisk(){
  windowInit();
  with (document){
    write("<div id='Tisk' style='display: none' onClick='window.print()'>");
    write("<form>");
    write("<input type='button' value='Tisk: "+document.title+"'><br>");
    write("</form>");
    write("</div>");
  }
}
