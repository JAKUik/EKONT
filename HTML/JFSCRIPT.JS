//       ini=cellrect  -----------------------------------------------------------------------
function init_cellrect(el,p) {
 var r,rx,ry,hS,w,h,sz,i,s,p,strk,colr;
 w=el.style.pixelWidth; h=el.style.pixelHeight; 
 strk=el.getAttribute("strokeweight"); if (strk==null) strk="1pt";
 colr=el.getAttribute("strokecolor"); if (colr==null) colr="black"; 
 cls2=el.getAttribute("class2"); if (cls2==null) cls2=""; else cls2=" class='"+cls2+"'";
 sz=el.size; if (sz==null) sz=Math.floor(w/20);
 r=el.radius; if (r==null) r=7; ry=Math.floor((r*1000)/h); rx=Math.floor((r*10000)/w);
 hS=el.percent; if (hS==null) hS=20; hS*=10;
 p.style.position="relative"; p.style.top="0"; p.style.left="0";
 s="<table cellpadding=0 cellspacing=0 style='width:100%;height:100%'><tbody><tr>";
 for (i=0;i<sz;i++)
   s+="<td valign=bottom align=center"+cls2+" style='width:"+(100/sz)+"%;height:100%'>&nbsp;</td>";
 s+="</tr></tbody></table>";
 s+="<v:shape style='position:absolute;top:0;left:0;z-index:1;width:"+el.currentStyle.width+
    ";height:"+el.currentStyle.height;
 s+="' coordsize='10000,1000' strokecolor='"+colr+"' strokeweight='"+strk+"' path='m ";
 s+=rx+",0 l "+(10000-rx)+",0 qx 10000,"+ry+" l 10000,"+(1000-ry)+" qy "+(10000-rx)+",1000 l "+
    rx+",1000 qx 0,"+(1000-ry)+" l 0,"+ry+" qy "+rx+",0";
   for (i=1;i<sz;i++) s+=" m "+Math.floor((i*10000)/sz)+",1000 r 0 -"+hS;
 s+=" nf e' />"; p.innerHTML=s; 
}
function bind_cellrect(el,p,txt) {
 var tds=p.firstChild.lastChild.firstChild.children; //.div.table.tbody.tr.
 var i,lTxt=txt.length,lTds=tds.length,n=Math.min(lTds,lTxt),algn=el.style.textAlign;
 for (i=0;i<n;i++) if (algn==""||algn=="left") tds[i].innerText=txt.charAt(i);
  else tds[lTds-1-i].innerText=txt.charAt(lTxt-1-i);
}
//  style="height:..;width:..;text-align:.." size=n radius=n percent=n data="expr" class2=.. 
// radius==pixels size==number of cells percent==%height of delimiter   class2:indiv.characters
//
//       ini=roundrect  -----------------------------------------------------------------------
function init_roundrect(el,p) {
 var r,rx,ry,w,h,s,p,strk,colr;
 w=el.style.pixelWidth; h=el.style.pixelHeight; 
 strk=el.getAttribute("strokeweight"); if (strk==null) strk="1pt";
 colr=el.getAttribute("strokecolor"); if (colr==null) colr="black";
 r=el.radius; if (r==null) r=7; ry=Math.floor((r*1000)/h); rx=Math.floor((r*10000)/w);
 p.style.position="relative"; p.style.top="0"; p.style.left="0";
 s="<v:shape style='position:absolute;top:0;left:0;z-index:1;width:"+el.currentStyle.width+
    ";height:"+el.currentStyle.height;
 s+="' coordsize='10000,1000' strokecolor='"+colr+"' strokeweight='"+strk+"' path='m ";
 s+=rx+",0 l "+(10000-rx)+",0 qx 10000,"+ry+" l 10000,"+(1000-ry)+" qy "+(10000-rx)+",1000 l "+
    rx+",1000 qx 0,"+(1000-ry)+" l 0,"+ry+" qy "+rx+",0 nf e' />";
 p.insertAdjacentHTML("BeforeEnd",s);
}
// style="height:..;weight:.." radius=n                        

