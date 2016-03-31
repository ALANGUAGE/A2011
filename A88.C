/*PLA V.88*/ 
#include ".\INCLUDE\CONST1.C"
#include ".\INCLUDE\VAR1.C"
#include ".\INCLUDE\DOS1.C"
int BCC=1; /*is Borland C, dann kein BSS */
unsigned char testa[]={148,153,129,154,0}; /*"öÖüÜ";*/
int test3( int i, unsigned int u){ 
_AX=0xB800;
}
char Nametemp[16];
int doglob() { int symlen; int j; int isstringarray; isstringarray=0;
  if (GTop >= GMAX) error1("Globale Tabelle voll");
  symlen = strlen1(symbol);
  if (symlen > 15 ) error1("Variable ist laenger als 15 Zeichen");
  if (istoken('[')) { istype='&';
    if (istoken(T_CONST)) {
      if (BCC == FALSE) {prs("\nAData = $\norg "); prnum(orgData);}
      prs("\n"); prs(symbol); 
      if (iswidth=='B') prs(" db "); else prs(" dw "); 
      prnum(lexval); prs(" dup (?)"); 
      if (BCC == FALSE) prs("\norg AData");
      orgData=orgData+lexval; if (iswidth=='W') orgData=orgData+lexval;
      GData[GTop]=lexval; expect(']'); 
    }else { expect(']');
      if (iswidth=='W') error1("Nur ByteArray erlaubt");
      prs("\n"); prs(symbol); prs(" db "); 
      isstringarray=1; strcpy1(Nametemp, symbol);
      expect('='); 
      if (istoken(T_STRING)) { 
        prc(34); prscomment(symbol); prc(34); prs(",0"); 
        symlen=strlen1(symbol); GData[GTop]=symlen; }
      else if (istoken('{' )) { narg=0;
        do { if(narg) prc(','); 
          expect(T_CONST); prnum(lexval); narg++; } 
        while (istoken(',')); expect('}'); }  
      else error1("String oder Zahlenarray erwartet");    
      }; 
  }else { expect('=');
    prs("\n"); prs(symbol); if (istype=='*') prs(" dw ");
    else { if(iswidth=='B') prs(" db "); else   prs(" dw "); }
    if(istoken('-')) prc('-');
    expect(T_CONST); prnum(lexval); }
  GSign [GTop]=issign; GWidth[GTop]=iswidth; GType [GTop]=istype;
  j=GTop*16; pt=&GNameField + j;  
  if (isstringarray) strcpy1(pt, Nametemp); else strcpy1(pt, symbol);
  GTop++; expect(';'); }
int addlocal() { int j;
  if(LTop >= LMAX) error1("Lokale Tabelle voll");
  GSign [LTop]=issign; GWidth[LTop]=iswidth; GType [LTop]=istype;
  j=LTop*16; pt=&GNameField + j;  
  strcpy1(pt, symbol);  LTop++; }
int gettypes(int i) {int j; char c;
  c=GSign [i]; if (c=='S') signi =1;    else signi =0;
  c=GWidth[i]; if (c=='W') widthi=wi=1; else widthi=wi=0;
  c=GType [i]; typei=0; if (c=='*') typei=wi=1; 
  if (c=='&')  typei=2;  
  return i; }  
int adrofname(int i) {int j; int k; j=i*16; k=&GNameField + j; return k; }  
int printName(int i) { i=adrofname(i); prs(i); }
int searchname() { int i; int j;
  i=LStart;while(i<LTop) {j=adrofname(i);if(eqstr(symbol,j))return i; i++;}
  i=0;     while(i<GTop) {j=adrofname(i);if(eqstr(symbol,j))return i; i++;}
  error1("Variable unbekannt"); }
    
int typeName() { int m; /*0=V,1=*,2=&*/
  issign='S';
  if(istoken(T_SIGNED))   issign='S';    if(istoken(T_UNSIGNED)) issign='U';
  if(istoken(T_VOID))     error1("Void ist unbekannt");
  iswidth='W';
  if(istoken(T_CHAR))     iswidth='B';   if(istoken(T_INT))      iswidth='W';
  istype='V'; m=0;
  if(istoken('*'))  { istype='*'; m=1; } if(istoken('&'))  { istype='&'; m=2; }   
  name1(); return m; }
int name1() {if (token!=T_NAME) error1("Name erwartet"); token=getlex(); }

int dofunc() { cloc=&co; strcpy1(fname, symbol);
  prs("\n\n"); prs(symbol); prs(" PROC");
  expect('('); narg=0; LTop=LStart;
  if (istoken(')')==0) { prs("\narg ");
    do { typeName();  addlocal();    
      if (narg) prc(','); prs(symbol); 
      if (istype!='*') {if (iswidth=='B') prs(":byte ");} narg++; }
    while (istoken(','));  expect(')'); }
  expect('{'); /*body*/
  nlocal=0; nreturn=0; nconst=0;
  if (isvariable()) prs("\nlocal ");
  while(isvariable()) {
    do { typeName();    addlocal();
      if (nlocal) prc(',');   prs(symbol); 
      if (istype!='*') {if (iswidth=='B') prs(":byte ");} nlocal++;
      if (istoken('[')){istype='&';GType[LTop]='&';expect(T_CONST);expect(']');
        prs(":BYTE:"); prnum(lexval);  }
      } while (istoken(',')); expect(';'); }
  while(istoken('}')==0)   stmt();
  if (nreturn) prs("\n@@retn:"); prs("\n ret\nENDP"); listproc(); }

int pexpr() {expect('('); iscmp=0; 
  if (token==T_NAME) {if (eqstr(symbol, "_")) {constantexpr(); return;}}   
  expr(); if (iscmp==0) prs("\n or  al, al\n je @@"); 
  expect(')'); }

int constantexpr() { int mode; int id1;int ids;
  token=getlex();   mode=typeName();  
  id1=searchname(); gettypes(id1); ids=signi;
  if (isrelational() ==0) error1("Vergleich erwartet");
  expect(T_CONST);
  prs("; constant expression");
  prs("\ncmp "); printName(id1); prs(", "); prnum(lexval); cmpneg(ids); 
   expect(')');
}  
int expr() { int mode; int id1; int ixarr; int ids;
  if (istoken(T_CONST)) {doconst(); return 1; }
  mode=typeName(); /*0=V,1=*,2=&*/
  if (token=='(')  {docall1(); goto e1; }
  if (isreg()) goto e1;
  id1=searchname(); gettypes(id1); ids=signi;
  ixarr=0;
  if (istoken('[')) { ixarr=searchname(); expect(T_NAME); expect(']');  
    gettypes(ixarr);
    if (widthi==0) error1("Arrayindex muss int sein"); } 
  if (istoken(T_PLUSPLUS  )) {if(mode)error1("Nur var erlaubt");doinc();goto e1;}
  if (istoken(T_MINUSMINUS)) {if(mode)error1("Nur var erlaubt");dodec();goto e1;}
  if (istoken(T_PLUSASS   )) {compoundass("add", mode); goto e1;}
  if (istoken(T_MINUSASS  )) {compoundass("sub", mode); goto e1;}
  if (istoken(T_ANDASS    )) {compoundass("and", mode); goto e1;}
  if (istoken(T_ORASS     )) {compoundass("or" , mode); goto e1;}    
  if (istoken(T_MULASS    )) {error1("nicht implementiert");}
  if (istoken(T_DIVASS    )) {error1("nicht implementiert");}      
  if (istoken('=')) { isconst=expr(); 
     if (isconst) { if(mode==0) {prs("\n;++++ mov  "); printName(id1); 
       prs(", "); prnum(lexval);  } }
     doassign(mode, id1, ixarr); goto e1;} 
  dovar1(mode, "mov", ixarr, id1);
e1:    if (istoken('+')) rterm("add");
  else if (istoken('-')) rterm("sub" );
  else if (istoken('&')) rterm("and" );
  else if (istoken('|')) rterm("or" );  
  else if (istoken(T_LESSLESS)) rterm("shl");
  else if (istoken(T_GREATGREAT)) rterm("shr");  
  else if (istoken('*')) domul (ids);
  else if (istoken('/')) doidiv(ids);
  else if (istoken('%')) domod (ids);
  if (isrelational()) { rterm("cmp"); cmpneg(ids);}
  return 0;
}
int compoundass(char *op, int mode) {
  if(mode) error1("Nur scalar Var erlaubt");
  prnl(); prs(op); prs("  "); prs(symbol); prs(", ");
  expect(T_CONST); prnum(lexval);
}
int dovar1(int mode, int op, int ixarr, int id1) { 
  gettypes(id1);
  if (mode==1) {prs("\n mov bx, "); printName(id1); prnl(); prs(op);
    if(widthi) prs(" ax, [bx]"); 
    else prs(" al, [bx]\n mov ah, 0"); return; }
  if (mode==2){prnl();prs(op);prs(" ax, offset ");printName(id1); return; }
  if (ixarr) {
    prs("\n mov bx, "); printName(ixarr);
    if (wi) prs("\n shl bx, 1");
    prs("\n "); prs(op);
    if (wi) prs(" ax, "); else prs(" al, ");
    printName(id1); prs(" [bx]"); return; }
  prnl();prs(op);
  if(wi) prs(" ax, "); else prs(" al, "); printName(id1);
}
int rterm(char *op) {int mode; int opint; int ixarr; int id1;
  if (istoken(T_CONST)) { prnl(); prs(op); 
    if (wi) prs(" ax, "); else prs(" al, "); prnum(lexval); return;}
  mode=typeName(); id1=searchname(); ixarr=0;
  if (istoken('[')) { ixarr=searchname(); expect(T_NAME); expect(']');  
    gettypes(ixarr);
    if (widthi==0) error1("Arrayindex muss int sein"); } 
  if (eqstr(symbol,"_AX")) return;
  opint=op; dovar1(mode, opint, ixarr, id1);
}
int isreg() {
  if (eqstr(symbol,"_AH")) {doreg("ah"); goto r1;}
  if (eqstr(symbol,"_AL")) {doreg("al"); goto r1;}  
  if (eqstr(symbol,"_AX")) {doreg("ax"); goto r1;}
  if (eqstr(symbol,"_BH")) {doreg("bh"); goto r1;}
  if (eqstr(symbol,"_BL")) {doreg("bl"); goto r1;}
  if (eqstr(symbol,"_BX")) {doreg("bx"); goto r1;}    
  if (eqstr(symbol,"_CH")) {doreg("ch"); goto r1;}
  if (eqstr(symbol,"_CL")) {doreg("cl"); goto r1;}
  if (eqstr(symbol,"_CX")) {doreg("cx"); goto r1;}    
  if (eqstr(symbol,"_DH")) {doreg("dh"); goto r1;}
  if (eqstr(symbol,"_DL")) {doreg("dl"); goto r1;}
  if (eqstr(symbol,"_DX")) {doreg("dx"); goto r1;}  
  if (eqstr(symbol,"_FLAGS")) {doreg("flags"); goto r1;}  
  return 0;   r1: return 1; 
}
int doreg(char *dr) { expect('=');
  prs("\n mov  "); prs(dr); prs(", ");
       if (istoken(T_CONST)) prunsign(lexval);
  else if (istoken(T_NAME )) {
    if (eqstr(symbol,"_DX")) prs("dx");
    else if (eqstr(symbol,"_CX")) prs("cx");
    else prs(symbol); }
  else error1("Nur Zahl oder Var erlaubt"); }

int doinc() { prs("\n inc  "); prs(symbol); }
int dodec() { prs("\n dec  "); prs(symbol); }

int doassign(int mode, int i, int ixarr) {
  gettypes(i);
  if (mode==1) {prs("\n mov  bx, ");printName(i);
    if (widthi) prs("\n mov  [bx], ax"); else  prs("\n mov  [bx], al"); return;}
  if (mode==2) {prs("\n mov  offset ");printName(i); prs(", ax"); return;}
  if (ixarr) {
    prs("\n mov bx, "); printName(ixarr);
    if (wi) prs("\n shl bx, 1");
    prs("\n mov "); printName(i);
    if (wi) prs(" [bx], ax"); else prs(" [bx], al"); return; }  
  prs("\n mov  "); printName(i); 
  if (wi) prs(", ax"); else prs(", al");  
}
int domul(int ids) {
  if (ids) rterm("imul"); else {
  if (istoken(T_CONST)) {prs("\n mov bx, "); prnum(lexval); prs("\n mul bx"); }
  else error1("Bei MUL nur Zahl als Multiplikator erlaubt"); } }
int doidiv(int ids) {
  if (istoken(T_CONST)) {
    prs("\n mov bx, "); prnum(lexval); 
    if (ids) prs("\n cwd\n idiv bx"); else prs("\n mov dx, 0\n div bx"); }
  else error1("Nur Zahl als Divisor erlaubt"); }
int domod(int ids) { doidiv(ids); prs("\n mov ax, dx"); }
int doconst() { prs("\n mov ax, "); prnum(lexval); }

int part1=0;int part2=0;int part3=0;int part4=0;int part5=0;int part6=0;
int parn1=0;int parn2=0;int parn3=0;int parn4=0;int parn5=0;int parn6=0; 
char procname[20]; /*1=CONST,2=String,3=&,4=Name*/
int docall1() {int i; int nargs; int t0; int n0; 
  nargs=0; strcpy1(&procname, symbol);expect('(');
	if (istoken(')') ==0 ) {
	  do { nargs++;
	    if (nargs >6 ) error1("Max. 6 Parameter");  t0=0;
      if(istoken(T_CONST)) {t0=1; n0=lexval; }
      if(istoken(T_STRING)){t0=2; n0=nconst;
        eprs("\n"); eprs(fname); eprc(95);eprnum(nconst);eprs(" db ");
        eprc(34);eprs(symbol);eprc(34);eprs(",0"); nconst++; }
      if(istoken('&'))     {t0=3; name1(); n0=searchname();}
      if(istoken(T_NAME))  {t0=4; n0=searchname();
        p1=&GType; p1=p1+n0; if (*p1=='&') t0=3; }
      if (t0==0) error1("Parameter nicht erkannt (kein * erlaubt)");
      if (nargs==1) {part1=t0; parn1=n0; } if (nargs==2) {part2=t0; parn2=n0; }
      if (nargs==3) {part3=t0; parn3=n0; } if (nargs==4) {part4=t0; parn4=n0; }  
      if (nargs==5) {part5=t0; parn5=n0; } if (nargs==6) {part6=t0; parn6=n0; }
    } while (istoken(','));
  	expect(')');  i=nargs;
    do {
      if (i==1) {t0=part1; n0=parn1; }     if (i==2) {t0=part2; n0=parn2; }
      if (i==3) {t0=part3; n0=parn3; }     if (i==4) {t0=part4; n0=parn4; }      
      if (i==5) {t0=part5; n0=parn5; }     if (i==6) {t0=part6; n0=parn6; }      
      if(t0==1){ prs("\n push "); prnum(n0);}
      if(t0==2){ prs("\n push offset "); prs(fname);prc(95);prnum(n0);}
      if(t0==3){ prs("\n lea  ax, "); printName(n0); prs("\n push ax");}
      if(t0==4){ gettypes(n0); 
        if(wi) { prs("\n push "); printName(n0);}
        else { prs("\n mov al, byte ptr "); printName(n0); 
        prs("\n mov ah, 0\n push ax"); } }
   i--; } while (i > 0);  }
	 prs("\n call "); prs(&procname);
	 if (nargs>0) {prs("\n add  sp, "); nargs=nargs+nargs; prnum(nargs); } }
/**********************************************/
int main() { getarg(); 
  prs("\n.MODEL TINY,C\n.186\n.CODE\nJUMPS\nLOCALS\nSTARTUPCODE");
  /*prs("\nmov ax, cs\nmov ds, ax");*/ prs("\njmp main"); getfirstchar(); 
  parse(); epilog(); }
char *arglen=0x80; char *argv=0x82;
int getarg() { int arglen1; int i; char *c; arglen1=*arglen; 
  if (arglen1) {i=arglen1+0x81; c=i; *c=0; strcpy1(namein, argv); 
    strcpy1(namelst, namein); i=strlen1(namelst); i--; c=&namelst+i; *c='S'; }
  else {strcpy1(namein, "F.C"); strcpy1(namelst, "F.S"); }  
  fdin=open2 (namein);
  if(DOS_ERR !=0){print1("Datei fehlt: "); print1(namein); exit1(1); }
  fdout=creat1(namelst);
  if(DOS_ERR !=0){print1("Datei nicht erzeugbar: ");print1(namelst);exit1(2);}
  prs("\n;Arglen: "); prnum(arglen1); if(arglen1){prs(", Argv: "); prs(argv);}
}  
int parse() { token=getlex(); do { if (token <= 0) return 1;
    if (istoken('#')) {
      if (istoken(T_DEFINE)) dodefine();
      else if (istoken(T_INCLUDE)) doinclude();
      else error1("define oder include erwartet");
      } else{ typeName();  if (token=='(') dofunc();  else doglob(); } 
    } while(1); 
}
int doinclude() { int fdtemp;
  if (token==T_STRING) {  fdtemp=fdin;
  prs("\n;Verarbeite Include-Datei: "); prscomment(symbol);  
  fdin=open2(symbol);
  if (DOS_ERR !=0) {prs("Include-Datei fehlt: "); prscomment(symbol); 
    error1("Stop"); }
  linenoinclude=lineno; lineno=1;
  parse(); lineno=linenoinclude;
  fdin=fdtemp; prs("\n;Zurueck in Hauptdatei: "); prs(namein); 
  getfirstchar(); token=getlex(); }
}
int dodefine() { int symlen; int j;
  expect(T_NAME);
    if (token==T_CONST) { 
      if (GTop >= GMAX) error1("Globale Tabelle (define) voll");
      symlen = strlen1(symbol);
      if (symlen > 15 ) error1("Define-Name ist länger als 15 Zeichen");
      GSign [GTop]='U'; GWidth[GTop]='B'; GType [GTop]='#'; 
      j=GTop*16; pt=&GNameField + j; strcpy1(pt, symbol); 
      prs(";  Define Nr "); prnum(GTop); prs(": ");
      printName(GTop); prc(' '); 
      GData[GTop]=lexval; prc('=');prnum(lexval);  expect(T_CONST); GTop++;  } 
}
int stmt() { int c;
       if(istoken('{'))     {while(istoken('}')==0) stmt();}
  else if(istoken(T_IF))    doif();
  else if(istoken(T_DO))    dodo();
  else if(istoken(T_WHILE)) dowhile();
  else if(istoken(T_GOTO))  {prs("\n jmp @@");name1();prs(symbol);expect(';');}
  else if(token==T_ASM)     {prs("\n"); c=next();
    while(c != '\n') { prc(c);	c=next(); }; token=getlex(); }
  else if(istoken(T_ASMBLOCK)) {prs("\n"); expect('{'); c=next();
    while(c!= '}') { prc(c); c=next(); };   token=getlex(); }
  else if(istoken(T_EMIT))  doemit();
  else if(istoken(';'))     { }
  else if(istoken(T_RETURN)){if (token!=';') expr();
    prs("\n jmp @@retn"); nreturn++; expect(';'); }
  else if(thechar==':')     {prs("\n@@"); /*Label*/
     prs(symbol); prc(':'); expect(T_NAME); expect(':'); }
  else                      {expr(); expect(';'); } }
int doemit() {prs("\n db ");
  L1: token=getlex(); prnum(lexval); token=getlex();
    if (token== ',') {prc(','); goto L1;} expect(')'); }
int cmpneg(int ids) {
       if(iscmp==T_EQ) prs("\n jne @@");         /*ZF=0            */
  else if(iscmp==T_NE) prs("\n je  @@");         /*ZF=1            */
  else if(iscmp==T_LE) if (ids) prs("\n jg  @@");/*ZF=0      SF =OF*/
                           else prs("\n ja  @@");/*ZF=0 CF=0       */
  else if(iscmp==T_GE) if (ids){prs(" ;unsigned : "); prnum(ids);
                               prs("\n jl  @@");/*          SF!=OF*/}
                           else{prs(" ;unsigned : "); prnum(ids);
                               prs("\n jb  @@");/*jb=jc=CF=1      */}
  else if(iscmp=='<' ) prs("\n jge @@");         /*          SF =OF*/
  else if(iscmp=='>' ) prs("\n jle @@");         /*ZF=1 oder SF!=OF*/
  else error1("Vergleich unbekannt in CMPNEG()"); }

int prlabel(int n) {prs("\n@@"); prnum(n); prc(':'); }
int prjump (int n) {prs("\n jmp @@"); prnum(n); }
int doif() {int jdest; int tst; pexpr(); nlabel++; jdest=nlabel;
  prnum(jdest); stmt();
  if (istoken(T_ELSE)) { nlabel++; tst=nlabel;
    prjump(tst); prlabel(jdest); stmt(); prlabel(tst); }
  else prlabel(jdest); }
int dodo() {int jdest; int jtemp;
  nlabel++; jdest=nlabel; prlabel(jdest); stmt();
  expect(T_WHILE); pexpr(); nlabel++; jtemp=nlabel; prnum(jtemp);
  prjump(jdest); prlabel(jtemp); }
int dowhile() {int jdest; int tst; nlabel++; jdest=nlabel;
  prlabel(jdest); pexpr(); nlabel++; tst=nlabel; prnum(tst);
  stmt(); prjump(jdest); prlabel(tst); }
int isrelational() {
  if (token==T_EQ) goto w; if (token==T_NE) goto w;
  if (token==T_LE) goto w; if (token==T_GE) goto w;
  if (token=='<' ) goto w; if (token=='>' ) goto w;
  return 0;  w: iscmp=token; token=getlex(); return 1;}
 
char symboltemp[80];    
int getlex() { char c; char *p; 
g1: c=next(); if (c == 0) return 0; if (c <= ' ') goto g1;
  if (c=='=') {if(thechar=='=') {next(); return T_EQ; }}
  if (c=='!') {if(thechar=='=') {next(); return T_NE; }}
  if (c=='<') {if(thechar=='=') {next(); return T_LE; }}
  if (c=='>') {if(thechar=='=') {next(); return T_GE; }}
  if (c=='<') {if(thechar=='<') {next(); return T_LESSLESS;  }}
  if (c=='>') {if(thechar=='>') {next(); return T_GREATGREAT;}}
  if (c=='+') {if(thechar=='+') {next(); return T_PLUSPLUS;  }}
  if (c=='-') {if(thechar=='-') {next(); return T_MINUSMINUS;}}
  if (c=='+') {if(thechar=='=') {next(); return T_PLUSASS;   }}
  if (c=='-') {if(thechar=='=') {next(); return T_MINUSASS;  }}
  if (c=='&') {if(thechar=='=') {next(); return T_ANDASS;    }}
  if (c=='|') {if(thechar=='=') {next(); return T_ORASS;     }}    
  if (c=='*') {if(thechar=='=') {next(); return T_MULASS;    }}
  if (c=='/') {if(thechar=='=') {next(); return T_DIVASS;    }}        
  if (instr1("()[]{},;*:%-><=+!&|#", c)) return c ;
  if (c == '/') { if (thechar == '*') { 
      g2: c=next(); if (c != '*') goto g2; if (thechar != '/') goto g2;
      c=next(); return getlex(); } else  return '/'; }
  if (c == '"') {getstring(c); return T_STRING;}
  if (digit(c)) { getdigit(c); return T_CONST; }
  if (c==39) { lexval=next();
    if (lexval==92) {lexval=next();
      if (lexval=='n') lexval=10; if (lexval=='t') lexval= 9;
      if (lexval=='0') lexval= 0; } next(); return T_CONST; }
  if (letter(c)) { 
    strcpy1(symboltemp, symbol); p=&symbol;  *p=c;  p++;
    while(letter(thechar)) {c=next(); *p=c;  p++; } 
      *p=0;
    if (eqstr(symbol,"signed"  )) return T_SIGNED;
    if (eqstr(symbol,"unsigned")) return T_UNSIGNED;
    if (eqstr(symbol,"void"    )) return T_VOID;
    if (eqstr(symbol,"int"     )) return T_INT;
    if (eqstr(symbol,"char"    )) return T_CHAR;
    if (eqstr(symbol,"asm"     )) return T_ASM;
    if (eqstr(symbol,"__asm"   )) return T_ASMBLOCK;
    if (eqstr(symbol,"__emit__")) return T_EMIT;
    if (eqstr(symbol,"return"  )) return T_RETURN;
    if (eqstr(symbol,"if"      )) return T_IF;
    if (eqstr(symbol,"else"    )) return T_ELSE;
    if (eqstr(symbol,"while"   )) return T_WHILE;
    if (eqstr(symbol,"do"      )) return T_DO;
    if (eqstr(symbol,"goto"    )) return T_GOTO;
    if (eqstr(symbol,"define"  )) return T_DEFINE;   
    if (eqstr(symbol,"include" )) return T_INCLUDE;   
    if (convertdefine() ) {strcpy1(symbol, symboltemp); return T_CONST;}
    return T_NAME; } error1("Zeichen nicht erkannt"); }

int convertdefine() { int i; int j;   i=0;
  while (i < GTop) {
   j=adrofname(i); 
   if (eqstr(symbol,j)) { if (GType[i]=='#') { prs("\n;define: "); printName(i); 
     prs("= "); lexval=GData[i]; prnum(lexval);  return T_CONST; } }
   i++; } 
   return 0; }
int getdigit(char c) { int i;
    lexval=0; lexval=c-'0'; /*lexval=int hi=0, c=char*/
    if (thechar=='x') thechar='X'; if (thechar=='X') { next();
      while(letter(thechar)) { c=next(); if(c>96) c=c-39;
	if (c>64) c=c-7; c=c-48; lexval=lexval << 4; /* *16 */ 
     i=0; i=c; lexval=lexval+i;}
    }else { while(digit(thechar)) { c=next(); c=c-48; lexval=lexval*10; 
     i=0; i=c; lexval=lexval+i; } } 
}
int getstring(int delim) {int c; char *p;  p=&symbol; c=next();
  while (c != delim) {*p=c; p++; c=next(); } *p=0; }

int next() {char r; r = thechar; globC=r; thechar = fgets1(); return r; }
int istoken(int t) {if (token == t) { token=getlex(); return 1; } return 0;}
int isvariable() {if(token==T_CHAR) return 1;  if(token==T_UNSIGNED) return 1;
                  if(token==T_INT ) return 1;  return 0;}
int expect(int t) {if (istoken(t)==0) { listproc();
  prs("\nErwartet ASCII(dez): "); prnum(t); error1(" nicht gefunden"); } }

int eprc(char c)  {*cloc=c; cloc++; }
int eprs(char *s) {char c;  while(*s) { c=*s; eprc(c); s++; } }
int prc(unsigned char c) {if (c==10) {_AX=13; bios_putc(); }
  _AL=c; bios_putc(); fputc1(c, fdout); }
int prscomment(unsigned char *s) {unsigned char c; while(*s){c=*s;prc(c);s++;}}
int prnl() { prs("\n ");}

int prs(unsigned char *s) {unsigned char c; int com; com=0;
  while(*s) { c=*s; if (c==34) if (com) com=0; else com=1;
    if (c==92) { if (com==0) { s++; c=*s;
          if (c=='n') c=10; if (c=='t') c= 9;
    } } prc(c); s++;  } }
int eprnum(int n){int e; if(n<0) { eprc('-'); n=NULL-n;}
  if (n >= 10) {e=n/10; eprnum(e);}  n=n%10; n=n+'0'; eprc(n); }
int prnum (int n){int e; if(n<0) {  prc('-'); n=NULL-n;}
  if (n >= 10) {e=n/10;  prnum(e);}  n=n%10; n=n+'0'; prc(n); }
int pint (int n){int e; if(n<0) {  prc('-'); n=NULL-n;}
  if (n >= 10) {e=n/10;  pint (e);}  n=n%10; n=n+'0'; prc(n); }  
int prunsign(unsigned int n) { unsigned int e; 
  if ( _ n >= 10) {  
    e=n/10; /*DIV*/
    prunsign(e); }
    n = n % 10; /*unsigned mod*/
    n += '0';
    prc(n); }
int prhex (unsigned int n){int e; prc('0');
  if (n >= 0x1000) e=n >> 12; n &= 0xFFF; prhex1(e);
  if (n >=  0x100) e=n >>  8; n &=  0xFF; prhex1(e);
  if (n >=   0x10) e=n >>  4; n &=   0xF; prhex1(e);
  prhex1(n); prc('h'); }
int prhex1(int n) {if (n <= 10) n +=48; else n += 55; prc(n); }  

int getfirstchar() { fgetsp=&fgetsdest; *fgetsp=0; thechar=fgets1(); }
int fgets1() { char c; c=*fgetsp;
  if (c==0) { printinputline(); if (DOS_NoBytes == 0) return 0;
    fgetsp=&fgetsdest; c=*fgetsp; spalte=0; }
  fgetsp++; spalte++;  return c; }
int printinputline() { fgetsp=&fgetsdest;
  do {DOS_NoBytes=read1(&DOS_ByteRead, fdin);  
  if (DOS_NoBytes == 0) return; 
    *fgetsp=DOS_ByteRead; fgetsp++;} 
  while (DOS_ByteRead != 10); *fgetsp=0; 
  prs("\n\n;-"); prnum(lineno); prc(' '); lineno++; prscomment(&fgetsdest); }
  
#include ".\INCLUDE\BIOS1.C"
#include ".\INCLUDE\STRING1.C"
#include ".\INCLUDE\END1.C"
