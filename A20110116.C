/*A.C 16.1.2011     =eax;    */
/*#define BSS 1   */  
char Version1[]="A.COM V0.8";
char BSS=1;
char NASM=1;
char LIST=1;
#define LSTART        150
#define VARMAX        200
#define GNAMEMAX     3200 /* 16*VARMAX */
#define FUNCMAX       200
#define FNAMEMAX     3200 /* 16*FUNCMAX*/
#define CALLMAX      1200
#define CNAMEMAX    19200 /* 16*CALLMAX*/
#define _   /* Konstantvergleich */
#define T_NAME        256
#define T_CONST       257 
#define T_STRING      258     
#define T_INCLUDE     510     
#define T_DEFINE      511  
#define T_RETURN      512
#define T_IF          513      
#define T_ELSE        514  
#define T_WHILE       515      
#define T_DO          516
#define T_INT         517     
#define T_ASM         518   
#define T_ASMBLOCK    519   
#define T_EMIT        520
#define T_GOTO        521    
#define T_VOID        529  
#define T_CHAR        530       
#define T_SIGNED      531
#define T_UNSIGNED    532
#define T_INTH        600
#define T_IFCARRY     601
#define T_IFZERO      602
#define T_EQ          806
#define T_NE          807
#define T_GE          811
#define T_LE          824
#define T_PLUSPLUS   1219
#define T_MINUSMINUS 1225
#define T_PLUSASS    1230
#define T_MINUSASS   1231
#define T_MULASS     1232
#define T_DIVASS     1233
#define T_ANDASS     1234
#define T_ORASS      1235
#define T_LESSLESS   1240
#define T_GREATGREAT 1241
char co[3000];     char *cloc=0;	   char symbol[128];
int fdin=0;        int fdout=0;
char fname[68];    char namein[68];  char namelst[68];
int token=0;       char globC=0;     int spalte=0;
char thechar=0;    int iscmp=0;      int nconst=0;
int nreturn=0;     int nlabel=0;
int GTop=1;        int LTop=150;
unsigned int lexval=0;               unsigned int orgData=22000;
int typei=0;       char istype ='0'; char GType   [VARMAX]; /* 0=V, 1=*, 2=&,#*/
int signi=0;       char issign ='0'; char GSign   [VARMAX]; /* 0=U, 1=S      */
int widthi=0;      char iswidth='0'; char GWidth  [VARMAX]; /* 0=B, 1=W      */
int wi=0;/*Ptr=1*/ int GData[VARMAX];  char GNameField[GNAMEMAX];
int  FTop=0;                 int  CTop=0;
int  FAdr[FUNCMAX];          char CType[CALLMAX];
int  FCalls[FUNCMAX];
char FNameField[FNAMEMAX];   char CNameField[CNAMEMAX];
char fgetsdest[128];
unsigned char *fgetsp=0; 
unsigned int lineno=1;       unsigned int linenoinclude=1;
unsigned char *pt=0;         unsigned char *p1=0;
int DOS_ERR=0; int DOS_NoBytes=0; char DOS_ByteRead=0;


 int uitoa(unsigned int n, char *s) { int i;  int re;
  i=0;  while(i<5) { *s = '0'; s++; i++; }   *s = 0;   s--;
  do { re = n % 10;  *s = re + '0';  n = n / 10;  s--;  } while (n > 0);
}
char ultoa_str[11];
int ultoa4(int lo1, int hi1, char *s ) {
_ dx = hi1;
_ ax = lo1;
_ di=s;
__asm{
  add di,10     ;add statt mov
  mov byte     [di+1], 0
  mov si,10     ;Divisor
  mov bx,ax     ;AX retten
div1:
  mov ax,dx     ;fuer erste Division
  xor dx,dx
  or  ax,ax     ;Ende High-Teil
  jz  div2
  div si
div2:
  mov cx,ax     ;AX retten High-Teil
  mov ax,bx     ;Low-Anteil
  mov bx,cx     ;BX = Wert High
  div si
  mov cl,dl     ;DX erhalten
  add cl,48
  mov [di],cl   ;statt mov asc_str[di],cl
  dec di
  mov dx,ax     ;Anteil Low
  mov cx,dx     ;DX zwischenspeichern
  mov dx,bx     ;DX = High
  mov bx,cx     ;BX = Low
  mov ax,dx     ;AX = High
  or  ax,bx     ;Ende?
  jnz div1
  inc di        ; neu eingef�hrt
  mov ax,di
}  }

int a(unsigned int i) { if(NASM == 0) prs("offset "); printName(i);}/*adress*/
int v(unsigned int i) { if(NASM) { if (i < LSTART) prc('['); } 
  printName(i); if(NASM) { if (i < LSTART) prc(']'); }   }/*value */
char doglobName[16];
int doglob() { int i; int j; int isstrarr; isstrarr=0;
  if (GTop >= LSTART) error1("Globale Tabelle voll");
  i=strlen(symbol); if (i>15) error1("Variable laenger 15 Zeichen");
  if (istoken('[')) { istype='&';
    if (istoken(T_CONST)) {
      if (BSS) {prs("\nAData = $\norg "); prunsign(orgData);}
      prs("\n"); prs(symbol); 
      if (iswidth=='B') {if (NASM) prs(" resb "); else prs(" db ");} 
                   else {if (NASM) prs(" resw "); else prs(" dw ");} 
      prunsign(lexval); if (NASM==0)prs(" dup (?)"); 
      if (BSS) prs("\norg AData");
      orgData=orgData+lexval; if (iswidth=='W') orgData=orgData+lexval;
      GData[GTop]=lexval; expect(']'); 
    }else { expect(']');
      if (iswidth=='W') error1("Nur ByteArray erlaubt");
      prs("\n"); prs(symbol); prs(" db "); 
      isstrarr=1; strcpy(doglobName, symbol);
      expect('='); 
      if (istoken(T_STRING)) { 
        prc(34); prscomment(symbol); prc(34); prs(",0"); 
        i=strlen(symbol); GData[GTop]=i; }
      else if (istoken('{' )) { i=0;
        do { if(i) prc(','); 
          expect(T_CONST); prunsign(lexval); i=1; } 
        while (istoken(',')); expect('}'); }  
      else error1("String oder Zahlenarray erwartet");    
      }; 
  }else { /*expect('=');*/
    prs("\n"); prs(symbol); if (istype=='*') prs(" dw ");
    else { if(iswidth=='B') prs(" db "); else   prs(" dw "); }
    if(istoken('-')) prc('-');
    if (istoken('=')) {expect(T_CONST); prunsign(lexval); }
    else prunsign(0); }
  GSign [GTop]=issign; GWidth[GTop]=iswidth; GType [GTop]=istype;
  j=GTop*16; pt=&GNameField + j;  
  if (isstrarr) strcpy(pt, doglobName); else strcpy(pt, symbol);
  if (checkName() != 0) error1("Variable schon global definiert");
  GTop++; expect(';'); }

int gettypes(int i) {int j; char c;
  c=GSign [i]; if (c=='S') signi =1;    else signi =0;
  c=GWidth[i]; if (c=='W') {widthi=1;wi=1;} else {widthi=0;wi=0;}
  c=GType [i]; typei=0; if (c=='*') {typei=1;wi=1;} 
  if (c=='&')  typei=2;  
  return i; }  
int adrofname(unsigned int i) {unsigned int j;unsigned int k; j=i*16;
k=&GNameField + j; return k; }
int printName(unsigned int i) {int j;
  if (i < LSTART) { i=adrofname(i); prs(i); }
  else { prs("[bp"); j = GData[i]; if (j>0) prc('+'); pint(j); prc(']'); } 
}
int searchname() { unsigned int i;
  i=checkName(); if (i == 0) error1("Variable unbekannt"); return i; }
int checkName() { unsigned int i; unsigned int j;
  i=LSTART;while(i<LTop) {j=adrofname(i);if(eqstr(symbol,j))return i; i++;}
  i=1;     while(i<GTop) {j=adrofname(i);if(eqstr(symbol,j))return i; i++;}
  return 0;
}    
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

int storecall() { int j;
  if (CTop >= CALLMAX) error1("Call-Tabelle voll");  
    CType[CTop]=1;
    j=CTop*16; pt=&CNameField + j; strcpy(pt, symbol); CTop++;
}
int storefunc() { int j;
  if (FTop >= FUNCMAX) error1("Funktionstabelle voll");  
    FAdr[FTop]=lineno - 1;  FCalls[FTop]=0;
    j=FTop*16; pt=&FNameField + j; strcpy(pt, symbol); FTop++;
}
int addlocal() { int j;
  if(LTop >= VARMAX) error1("Lokale Variablentabelle voll");
  if (checkName() != 0) error1("Variable schon lokal definiert");
  GSign [LTop]=issign; GWidth[LTop]=iswidth; GType [LTop]=istype;
  j=LTop*16; pt=&GNameField + j;  
  strcpy(pt, symbol); }
  
int dofunc() { int nloc; int i; int narg;  
  cloc=&co; 
  i=strlen(symbol); if (i>15) error1("Funktionsname laenger 15 Zeichen");
  strcpy(fname, symbol);
  storefunc();
  prs("\n\n"); prs(symbol); prs(":  ; PROC");
  expect('('); LTop=LSTART;  i=0;
  if (istoken(')')==0) { narg=2; 
    do { typeName();  addlocal(); narg+=2; GData[LTop]=narg; LTop++;}
    while (istoken(','));  expect(')'); }
    
  expect('{'); /*body*/
  nloc=0; nreturn=0; nconst=0; i=0; /*nlabel=0; */
  while(isvariable()) {
    do { typeName();    addlocal(); nloc-=2; GData[LTop]=nloc; 
      if (istoken('[')){istype='&';GType[LTop]='&';expect(T_CONST);expect(']');
        nloc=nloc-lexval; nloc+=2; GData[LTop]=nloc; } LTop++; 
      } while (istoken(',')); expect(';'); }
  listproc(); 
  if (LTop>LSTART){prs(";\n ENTER  "); 
    __asm {neg word [bp-2] } /* = nloc*/ 
  pint (nloc); prs(",0"); }
  while(istoken('}')==0)   stmt();
  if (nreturn) { if (NASM) prs("\n@@:"); else prs("\n@@retn:");}
  if (LTop > LSTART) prs(" LEAVE");
  prs("\n ret"); prs("\n; ENDP"); 
  *cloc=0; prs(co);
}

int pexpr() {expect('('); iscmp=0; 
  if (token==T_NAME) {if (eqstr(symbol, "_")) {constantexpr(); return;}}   
  exprstart(); if (iscmp==0) prs("\n or  al, al\n je @@");  prs(fname);
  expect(')'); }

int constantexpr() { int mode; int id1;int ids;
  token=getlex();   mode=typeName();  
  id1=searchname(); gettypes(id1); ids=signi;
  if (isrelational() ==0) error1("Vergleich erwartet");
  expect(T_CONST);  prs(" ; constant expression");
  prs("\ncmp "); 
  gettypes(id1); if (wi) prs("word"); else prs("byte");
  v(id1); prs(", "); prunsign(lexval); cmpneg(ids);   prs(fname);
  expect(')');
}  

unsigned int FL1LO=0; unsigned int FL1HI=0;
unsigned int FL2LO=0; unsigned int FL2HI=0;

int tell1(int fd) { /*Dateilaenge ermitteln, Standard return long */
  _ bx=fd; _ cx=0; _ dx=0;  _ ax=0x4202;    /*Eingabe CX:DX*/
  DosInt(); _ FL1LO=ax; _ FL1HI=dx;   /*Ausgabe DX:AX*/
  if (DOS_ERR) error1("tell1-Fehler");
}
int seek2(int base, int fd, int hi2, int lo2) {
  dx=lo2; cx=hi2; bx=fd; al=base; ah=0x42; DosInt();
  _ lo2=ax; _ hi2=dx;
  if (DOS_ERR) error1("seek2-Fehler"); }
/*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&*/
int exprstart() { if (eqstr(symbol, "_")) expr2(); else expr(0); }

int expr2() { int mode; int ireg; int id1; int ids; int idw; int idt; 
              int val1; int ireg2;int id2; int ids2;int idw2;int idt2; int i;
  token=getlex();   mode=typeName();
  if (mode) error1("Noch kein & und * im Text erlaubt");
  ireg=checkreg();
  if (ireg == 0) { id1=searchname();
    gettypes(id1); ids=signi; idw=wi; idt=typei;
    if (idt)  error1("Noch kein Array oder Pointer links erlaubt");  }
  
  if (istoken('=') == 0) error1("Zuweisung erwartet");
  if (istoken(T_CONST) ) { val1=lexval; prs(" ; constant expression");
    prs("\nmov ");
    if (idw) prs("word"); else prs("byte");
    v(id1); prs(", "); prunsign(val1);
    if (id1 >= LSTART) { i=adrofname(id1);  prs("; "); prs(i); } return; }
  
  mode=typeName(); 
  ireg2=checkreg();
  if (ireg2) { prs("\nmov ");
    if (ireg) printreg(ireg); else v(id1); prs(", "); printreg(ireg2);return;}
  else { if (mode) error1("Noch kein & und * im Text erlaubt");
    id2=searchname();
    gettypes(id2); ids2=signi; idw2=wi; idt2=typei;
    if (idt2)  error1("Noch kein Array oder Pointer rechts erlaubt");
    prs("\nmov ");
    if (ireg) printreg(ireg); else error1("Mem to Mem nicht erlaubt");
    prs(", ");
    if (idw2) prs("word"); else prs("byte");
    v(id2);
    if (id2 >= LSTART) { i=adrofname(id2);  prs("; "); prs(i); }
    return; }
  error1("Interne Fehlermeldung: Nur Konstante oder Register erlaubt hier");
}

int checkreg() { // >=17 = 16bit, >=47 = 32bit
  if (strlen(symbol) <  2) return 0;
  if (eqstr(symbol,"al")) return 1;   if (eqstr(symbol,"cl")) return 3;
  if (eqstr(symbol,"dl")) return 5;   if (eqstr(symbol,"bl")) return 7;
  if (eqstr(symbol,"ah")) return 9;   if (eqstr(symbol,"ch")) return 11;
  if (eqstr(symbol,"dh")) return 13;  if (eqstr(symbol,"bh")) return 15;
  if (eqstr(symbol,"ax")) return 17;  if (eqstr(symbol,"cx")) return 19;
  if (eqstr(symbol,"dx")) return 21;  if (eqstr(symbol,"bx")) return 23;
  if (eqstr(symbol,"sp")) return 25;  if (eqstr(symbol,"bp")) return 27;
  if (eqstr(symbol,"si")) return 29;  if (eqstr(symbol,"di")) return 31;
  if (eqstr(symbol,"es")) return 33;  if (eqstr(symbol,"cs")) return 35;
  if (eqstr(symbol,"ss")) return 37;  if (eqstr(symbol,"ds")) return 39;
  if (eqstr(symbol,"fs")) return 41;  if (eqstr(symbol,"gs")) return 43;
  // (eqstr(symbol,"ip")) return 45;
  if (strlen(symbol) >   3) return 0;
  if (eqstr(symbol,"eax")) return 47; if (eqstr(symbol,"ecx")) return 50;
  if (eqstr(symbol,"edx")) return 53; if (eqstr(symbol,"ebx")) return 56;
  if (eqstr(symbol,"esp")) return 59; if (eqstr(symbol,"ebp")) return 62;
  if (eqstr(symbol,"esi")) return 65; if (eqstr(symbol,"edi")) return 68;
  if (eqstr(symbol,"cr0")) return 71;
  return 0;   }
char printregstr[]
="*alcldlblahchdhbhaxcxdxbxspbpsidiescsssdsfsgsipeaxecxedxebxespebpesiedicr0";
//          1         2         3         4         5         6         7
// 1 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3
/*void printreg(int i, int mode) {  unsigned int k; unsigned char c;
  if (_ mode == 1) prc('[');
  k = &printregstr + i; c=*k; prc(c); i++;
  k = &printregstr + i; c=*k; prc(c);
  if (i > 47) { i++; k = &printregstr + i; c=*k; prc(c); }
  if (_ mode == 1) prc(']');
} */
printpri(int w) { if (w == 1) printreg(1, 0); if (w == 2) printreg(17, 0);
  if (wi == 4) printreg (47, 0);
}

int printreg(int i) {  unsigned int k; unsigned char c;
  k = &printregstr + i; c=*k; prc(c); i++;
  k = &printregstr + i; c=*k; prc(c); }
    
int evalue=0; int exprtype=10;/*0=V, 4=const left, 3=const right*/
int expr(int isRight)
{ int mode; int id1;     int ixarr; int ixconst;
  int ids;  int isCONST; int i;     unsigned char *p;
  if (istoken(T_CONST)) { evalue=lexval;
    if(isRight){prs("\n mov ax, "); prunsign(lexval);
      prs(";**CONST: "); tell1(fdout); prunsign(FL1HI);prc(':');prunsign(FL1LO);
      prs(", alt: "); pt=ultoa4(FL2LO, FL2HI, ultoa_str); prs(pt);
      return 3;}
    else {prs("\n mov ax, ");prunsign(lexval);
    prs("; CONST Left" ); return 4; }
  }
  mode=typeName(); /*0=V,1=*,2=&*/
  if (token=='(')  {docall1(); goto e1; }
  if (isreg()) goto e1;

  id1=searchname(); gettypes(id1); ids=signi;
  ixarr=0;  ixconst=0;
    if (istoken('[')) { if (istoken(T_CONST)) {
      ixconst=1; ixarr=lexval; expect(']');  }
    else {ixarr=searchname(); expect(T_NAME); expect(']');
    gettypes(ixarr);
    if (widthi==0) error1("Arrayindex muss Zahl oder int sein"); } }
  if (istoken(T_PLUSPLUS  )) {if(mode)error1("Nur var erlaubt");
     prs("\n inc  "); if (wi) prs("word"); else prs("byte");
     v(id1); goto e1;}
  if (istoken(T_MINUSMINUS)) {if(mode)error1("Nur var erlaubt");
     prs("\n dec  "); if (wi) prs("word"); else prs("byte");
     v(id1); goto e1;}
       
  FL2LO = FL1LO; FL2HI = FL1HI; tell1(fdout);     
  if (istoken(T_PLUSASS   )) {compoundass("add", mode, id1); goto e1;}
  if (istoken(T_MINUSASS  )) {compoundass("sub", mode, id1); goto e1;}
  if (istoken(T_ANDASS    )) {compoundass("and", mode, id1); goto e1;}
  if (istoken(T_ORASS     )) {compoundass("or" , mode, id1); goto e1;}    
  if (istoken(T_MULASS    )) {error1("nicht implementiert");}
  if (istoken(T_DIVASS    )) {error1("nicht implementiert");}      

  if (istoken('=')) { exprtype= expr(1); 
    isCONST=0;
    if(exprtype==3){if(mode==0){if(ixarr==0){isCONST=1; } } }
    if(isCONST){
      tell1(fdout);
      prs("\n; ++++ mov  "); if (wi) prs("word"); else prs("byte");
      v(id1); prs(", "); pint(evalue);
      /*prs(";+++ConstExpr");
      prs(", Laenge: ");  prunsign(FL1HI);prc(':');prunsign(FL1LO);
      prs(", alt: "); prunsign(FL2HI);prc(':');prunsign(FL2LO);  */
      /*return 0;*/ }
    doassign(mode, id1, ixarr, ixconst); goto e1;
  }
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

int compoundass(char *op, int mode, int id1) {
  if(mode) error1("Nur scalar Var erlaubt");
  prnl(); prs(op); prs("  "); 
  gettypes(id1); if (wi) prs("word"); else prs("byte");
  v(id1); prs(", ");
  expect(T_CONST); prunsign(lexval);
}
int dovar1(int mode, int op, int ixarr, int id1) { 
  gettypes(id1);
  if (mode==1) {prs("\n mov bx, "); v(id1); prnl(); prs(op);
    if(widthi) prs(" ax, [bx]"); 
    else prs(" al, [bx]\n mov ah, 0"); return; }
  if (mode==2){prnl();prs(op);prs(" ax, "); a(id1); return; }
  if (ixarr) {
    prs("\n mov bx, "); v(ixarr);
    if (wi) prs("\n shl bx, 1");
    prs("\n "); prs(op);
    if (wi) prs(" ax, "); else prs(" al, ");
 /* v(id1); prs(" [bx]"); */ 
    prc('['); printName(id1); prs(" + bx]"); 
    return; }
  prnl();prs(op);
  if(wi) prs(" ax, "); else prs(" al, "); v(id1);
}
int rterm(char *op) {int mode; int opint; int ixarr; int id1;
  if (istoken(T_CONST)) { prnl(); prs(op); 
    if (wi) prs(" ax, "); else prs(" al, "); prunsign(lexval); return;}
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
  if (eqstr(symbol,"_DI")) {doreg("di"); goto r1;}
  if (eqstr(symbol,"_FLAGS")) {doreg("flags"); goto r1;}  
  return 0;   r1: return 1; 
}
int doreg(char *dr) { int i; expect('=');
  prs("\n mov  "); prs(dr); prs(", ");
       if (istoken(T_CONST)) prunsign(lexval);
  else if (istoken(T_NAME )) { i=searchname(); v(i); }
  else error1("Nur Zahl oder Var erlaubt"); }

int doassign(int mode, int i, int ixarr, int ixconst) {
  gettypes(i);
  if (mode==1) {prs("\n mov  bx, ");v(i);
    if (widthi) prs("\n mov  [bx], ax"); else  prs("\n mov  [bx], al"); return;}
  if (mode==2) {prs("\n mov  ");a(i); prs(", ax"); return;}
  if (ixarr) {  prs("\n mov bx, ");
    if(ixconst) prunsign(ixarr); else v(ixarr);
    if (wi) prs("\n shl bx, 1");
    prs("\n mov ["); printName(i);
    if (wi) prs("+bx], ax"); else prs("+bx], al"); return; }  
  if (wi){prs("\n mov ");if(i<LSTART) {prs("word ");
    } v(i); prs(", ax"); }
  else   {prs("\n mov ");if(i<LSTART) {prs("byte ");}
    v(i); prs(", al"); }
}
int domul(int ids) {
  if (ids) rterm("imul"); else {
  if (istoken(T_CONST)) {prs("\n mov bx, "); prunsign(lexval); prs("\n mul bx"); }
  else error1("Bei MUL nur Zahl als Multiplikator erlaubt"); } }
int doidiv(int ids) { int mode; int id1;
  if (istoken(T_CONST)) {
    prs("\n mov bx, "); prunsign(lexval); 
    if (ids) prs("\n cwd\n idiv bx"); else prs("\n mov dx, 0\n div bx"); }
  else {
    mode=typeName(); id1=searchname();
    if (mode) error1("Nur Zahl oder Integer als Divisor erlaubt");
    gettypes(id1);
    if (typei) error1("Nur Int als einfache Variable Divisor erlaubt");
    if (wi==0) error1("Nur Int, kein Byte als Divisor erlaubt");
    prs("\n mov bx, "); v(id1);
    if (ids) prs("\n cwd\n idiv bx"); else prs("\n mov dx, 0\n div bx"); }
}
int domod(int ids) { doidiv(ids); prs("\n mov ax, dx"); }

int docalltype[10]; int docallvalue[10];
char procname[17]; /*1=CONST,2=String,3=&,4=Name*/
int docall1() {int i; int narg; int t0; int n0;
  narg=0; 
  i=strlen(symbol); if (i>15) error1("Funktionsname laenger 15 Zeichen");
  strcpy(&procname, symbol);
  storecall();
  expect('(');
	if (istoken(')') ==0 ) {
	  do { narg++;
	    if (narg >9 ) error1("Max. 9 Parameter");  t0=0;
      if(istoken(T_CONST)) {t0=1; n0=lexval; }
      if(istoken(T_STRING)){t0=2; n0=nconst;
        eprs("\n"); eprs(fname); eprc(95);eprnum(nconst);eprs(" db ");
        eprc(34);eprs(symbol);eprc(34);eprs(",0"); nconst++; }
      if(istoken('&'))     {t0=3; name1(); n0=searchname();}
      if(istoken(T_NAME))  {t0=4; n0=searchname();
        p1=&GType; p1=p1+n0; if (*p1=='&') t0=3; }
      if (t0==0) error1("Parameter nicht erkannt (kein * erlaubt)");
      docalltype [narg] = t0;
      docallvalue[narg] = n0;
    } while (istoken(','));
    
  	expect(')');  i=narg;
    do {
      t0 = docalltype [i];
      n0 = docallvalue[i];     
      if(t0==1){ prs("\n push "); pint(n0);}
      if(t0==2){ prs("\n push "); if(NASM==0) prs("offset ");
        prs(fname);prc(95);pint(n0);}
      if(t0==3){ prs("\n lea  ax, ");  v(n0); 
        prs("\n push ax");}
      if(t0==4){ gettypes(n0); 
        if(wi) { prs("\n push word "); printName(n0);}
        else { prs("\n mov al, byte ");  v(n0);
        prs("\n mov ah, 0\n push ax"); } }
   i--; } while (i > 0);  }
	 prs("\n call "); prs(&procname);
	 if (narg>0) {prs("\n add  sp, "); narg=narg+narg; pint(narg); } }
/**********************************************/
int main() { getarg();   getfirstchar(); parse(); epilog(); }
char *arglen=0x80; char *argv=0x82;
int getarg() { int arglen1; int i; char *c; arglen1=*arglen; 
  if (arglen1) {i=arglen1+0x80; c=i; if(*c == 'n') {i-=2; NASM=1;}
     i++; c=i; *c=0; strcpy(namein, argv);
    strcpy(namelst, namein); i=strlen(namelst); i--; c=&namelst+i; *c='S';
 }
  fdin=open2 (namein);
  if(DOS_ERR){print1("Datei fehlt: "); print1(namein); exit1(1); }
  fdout=creat1(namelst);
  if(DOS_ERR){print1("Datei nicht erzeugbar: ");print1(namelst);exit1(2);}
  prs("\n; "); prs(Version1); 
  prs("\nORG  256 \njmp main"); 
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
int dodefine() { int i; int j;
  if (eqstr(symbol, "BSS"   )) {if(NASM==0)BSS=1; token=getlex(); return; }
  if (eqstr(symbol, "LIST"  )) {LIST=1; token=getlex(); return; }
  if (eqstr(symbol, "NOLIST")) {LIST=0; token=getlex(); return; }
   expect(T_NAME);
  if (token==T_CONST) { 
    if (GTop >= LSTART) error1("Globale Tabelle (define) voll");
    i=strlen(symbol); if (i>15) error1("Define-Name laenger 15 Zeichen");
    GSign [GTop]='U'; GWidth[GTop]='B'; GType [GTop]='#'; 
    j=GTop*16; pt=&GNameField + j; strcpy(pt, symbol); GData[GTop]=lexval;
    expect(T_CONST); GTop++;  } 
}
int stmt() { int c; char cha;
       if(istoken('{'))     {while(istoken('}')==0) stmt();}
  else if(istoken(T_IF))    doif();
  else if(istoken(T_DO))    dodo();
  else if(istoken(T_WHILE)) dowhile();
  else if(istoken(T_GOTO))  {prs("\n jmp @@");name1();prs(symbol);expect(';');}
  else if(token==T_ASM)     {prs("\n"); c=next();
    while(c != '\n') { prc(c);	c=next(); }; token=getlex(); }
  else if(istoken(T_ASMBLOCK)) { if (token== '{' )  { prs("\n"); cha=next();  
    while(cha!= '}') { prc(cha); cha=next(); }
    token=getlex(); }
    else error1("geschweifte oeffnende Klammer erwartet"); }
  else if(istoken(T_INTH))  {prs("\n int  "); expect(T_CONST);
    prunsign(lexval); expect(';');    }  
  else if(istoken(T_IFCARRY))doifcarry();
  else if(istoken(T_IFZERO))doifzero();
  else if(istoken(T_EMIT))   doemit();
  else if(istoken(';'))      { }
  else if(istoken(T_RETURN)) {if (token!=';') exprstart();
    if(NASM)prs("\n jmp @f");else prs("\n jmp @@retn"); nreturn++; expect(';');}
  else if(thechar==':')      {prs("\n@@"); /*Label*/
     prs(symbol); prc(':');  expect(T_NAME); expect(':'); }
  else                       {exprstart(); expect(';'); } }

int doemit() {prs("\n db ");
  L1: token=getlex(); prunsign(lexval); token=getlex();
    if (token== ',') {prc(','); goto L1;} expect(')'); }

int cmpneg(int ids) {
       if(iscmp==T_EQ) prs("\n jne @@");         /*ZF=0            */
  else if(iscmp==T_NE) prs("\n je  @@");         /*ZF=1            */
  else if(iscmp==T_LE) if (ids) prs("\n jg  @@");/*ZF=0      SF =OF*/
                           else prs("\n ja  @@");/*ZF=0 CF=0       */
  else if(iscmp==T_GE) if (ids){prs(" ;unsigned : "); prunsign(ids);
                               prs("\n jl  @@");/*          SF!=OF*/}
                           else{prs(" ;unsigned : "); prunsign(ids);
                               prs("\n jb  @@");/*jb=jc=CF=1      */}
  else if(iscmp=='<' ) prs("\n jge @@");         /*          SF =OF*/
  else if(iscmp=='>' ) prs("\n jle @@");         /*ZF=1 oder SF!=OF*/
  else error1("Vergleich unbekannt in CMPNEG()");  }

int prlabel(int n) {prs("\n@@"); prs(fname); pint(n); prc(':'); }
int prjump (int n) {prs("\n jmp @@"); prs(fname); pint(n); }
int doif() {int jdest; int tst; pexpr(); nlabel++; jdest=nlabel;
  pint(jdest); stmt();
  if (istoken(T_ELSE)) { nlabel++; tst=nlabel;
    prjump(tst); prlabel(jdest); stmt(); prlabel(tst); }
  else prlabel(jdest); }
int doifcarry() {int jdest;  nlabel++; jdest=nlabel;
  prs("\n jae short @@");/*jnc*/ prs(fname);  pint(jdest);
  stmt(); prlabel(jdest); }
int doifzero() {int jdest;  nlabel++; jdest=nlabel;
  prs("\n jne short @@");        prs(fname);  pint(jdest);
  stmt(); prlabel(jdest); }
int dodo() {int jdest; int jtemp;
  nlabel++; jdest=nlabel; prlabel(jdest); stmt();
  expect(T_WHILE); pexpr(); nlabel++; jtemp=nlabel; pint(jtemp);
  prjump(jdest); prlabel(jtemp); }
int dowhile() {int jdest; int tst; nlabel++; jdest=nlabel;
  prlabel(jdest); pexpr(); nlabel++; tst=nlabel; pint(tst);
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
    strcpy(symboltemp, symbol); p=&symbol;  *p=c;  p++;
    while(letter(thechar)) {c=next(); *p=c;  p++; } 
      *p=0;
    if (eqstr(symbol,"signed"  )) return T_SIGNED;
    if (eqstr(symbol,"unsigned")) return T_UNSIGNED;
    if (eqstr(symbol,"void"    )) return T_VOID;
    if (eqstr(symbol,"int"     )) return T_INT;
    if (eqstr(symbol,"inth"    )) return T_INTH;
    if (eqstr(symbol,"char"    )) return T_CHAR;
    if (eqstr(symbol,"asm"     )) return T_ASM;
    if (eqstr(symbol,"__asm"   )) return T_ASMBLOCK;
    if (eqstr(symbol,"__emit__")) return T_EMIT;
    if (eqstr(symbol,"return"  )) return T_RETURN;
    if (eqstr(symbol,"if"      )) return T_IF;
    if (eqstr(symbol,"ifcarry" )) return T_IFCARRY;
    if (eqstr(symbol,"ifzero"  )) return T_IFZERO;
    if (eqstr(symbol,"else"    )) return T_ELSE;
    if (eqstr(symbol,"while"   )) return T_WHILE;
    if (eqstr(symbol,"do"      )) return T_DO;
    if (eqstr(symbol,"goto"    )) return T_GOTO;
    if (eqstr(symbol,"define"  )) return T_DEFINE;   
    if (eqstr(symbol,"include" )) return T_INCLUDE;   
    if (convertdefine() ) {strcpy(symbol, symboltemp); return T_CONST;}
    return T_NAME; } error1("Zeichen nicht erkannt"); }

int convertdefine() { int i; int j;   i=0;
  while (i < GTop) {
   j=adrofname(i); 
   if (eqstr(symbol,j)) { if (GType[i]=='#') { lexval=GData[i];
   return T_CONST; } }
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
int expect(int t) {if (istoken(t)==0) { *cloc=0; prs(co); listproc();
  prs("\nErwartet ASCII(dez): "); pint(t); error1(" nicht gefunden"); } }

int eprc(char c)  {*cloc=c; cloc++; }
int eprs(char *s) {char c;  while(*s) { c=*s; eprc(c); s++; } }
int prc(unsigned char c) { if (LIST) { if (c==10) {ax=13; bios_putc(); }
  al=c; bios_putc(); } fputc1(c, fdout); }
int prscomment(unsigned char *s) {unsigned char c; while(*s){c=*s;prc(c);s++;}}
int prnl() { prs("\n ");}

int prs(unsigned char *s) {unsigned char c; int com; com=0;
  while(*s) { c=*s; if (c==34) if (com) com=0; else com=1;
    if (c==92) { if (com==0) { s++; c=*s;
          if (c=='n') c=10; if (c=='t') c= 9;
    } } prc(c); s++;  } }
int eprnum(int n){int e; if(n<0) { eprc('-'); __asm{neg word [bp+4] }  }
  if (n >= 10) {e=n/10; eprnum(e);}  n=n%10; n=n+'0'; eprc(n); }
int pint (int n){int e; if(n<0) {  prc('-'); __asm{neg word [bp+4] }  }
  if (n >= 10) {e=n/10;  pint(e);}  n=n%10; n=n+'0'; prc(n); }
int prunsign(unsigned int n) { unsigned int e; 
  if ( _ n >= 10) {  e=n/10; /*DIV*/ prunsign(e); }
    n = n % 10; /*unsigned mod*/   n += '0'; prc(n); }
int printint5(unsigned int j)  {
  if (j<10000) prc(32); if (j<1000) prc(32);  if (j<100) prc(32);
   if (j<10) prc(32);  prunsign(j); }
int printhex8(unsigned int c) { unsigned int nib;
  nib = c >> 4; nib += 48; if (nib > 57)nib += 7; al=nib; bios_putc();
  nib = c & 15; nib += 48; if (nib > 57)nib += 7; al=nib; bios_putc();
}
int printhex16(unsigned int i) {unsigned int half;
  half = i >>  8; printhex8(half);
  half = i & 255; printhex8(half);
}

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
  prs("\n\n;-"); prunsign(lineno); prc(' '); lineno++; prscomment(&fgetsdest); }
  
/* Bildschirm int 10h */
int bios_putc() { ah=0x0E; bx=0; __emit__(0xCD,0x10); }
int print1(char *s) {char c;  while(*s) { c=*s; al=c; bios_putc(); s++; } }
/*STRING.C*/
int strlen(char *s) { int c; c=0; while (*s!=0) {s++; c++;} return c; }
int strcpy(char *s, char*t) {do { *s=*t; s++; t++; } while (*t!=0); *s=0; }
int instr1(char *s, char c) { while(*s) { if (*s==c) return 1; s++;} return 0;}
int instr2(char *s, char c) { while(*s) { if (*s==c) return s; s++;} return 0;}
int digit(char c){ if(c<'0') return 0; if(c>'9') return 0; return 1; }
int letter(char c) { if (digit(c)) return 1; if (c=='_') return 1;
  if (c> 'z') return 0; if (c< '@') return 0;
  if (c> 'Z') { if (c< 'a') return 0; }  return 1; }
int eqstr(char *p, char *q) { while(*p) {
    if (*p != *q) return 0; p++; q++; }
    if(*q) return 0; return 1; }
int strstr1 (char *s, char *t) { char c; int len; char sc; /*untested*/
  len=strlen(t); if (len == 0) return 0;
  c=*t;  t++;
  do {  do { sc = *s; s++; if (sc == 0) return 0; } while (sc != c);
    } while (strncmp1 (s, t, len) != 0 );
  s--; return s; }
int strncmp1(char *s, char *t, int n) { /*untested*/
  while (*s == *t) {
    if (*s == 0) return 0;  if ( n == 0) return 0;
    s++;  t++;  n--;  }     if (n) return 1;
  return 0; }
  
char ext;
int getchar1(){_ ext=0;ax=0x0C08;inth 0x21;ifzero{ext++;ax=0x0800;inth 0x21;}}
int DosInt() { inth 0x21; ifcarry DOS_ERR++; }
int open2 (char *s) { dx=s; ax=0x3D02;  DosInt(); }
int creat1(char *s) { dx=s; cx=0; ax=0x3C00; DosInt(); }
int read1 (char *s, int fd) {dx=s;cx=1;bx=fd;ax=0x3F00; DosInt(); }
int read2 (char *s, int fd, int len){dx=s;cx=len;bx=fd;ax=0x3F00; DosInt();}
int fputc1(char *n, int fd) { __asm{lea dx, [bp+4]}; /* = *n */
  cx=1; bx=fd; ax=0x4000; DosInt(); }
int fclose1(int fd) { bx=fd; ax=0x3E00; inth 0x21; }
int exit1(char c) { ah=0x4C; al=c; inth 0x21; }

int end1(int n) {fclose1(fdin); fclose1(fdout); exit1(n); }
int error1(char *s) { LIST=1;  *cloc=0; prs(co); listproc();
  prs("\n********** Fehler in Spalte: "); pint(spalte);
  prs(", in Zeile: "); lineno--;  prunsign(lineno);
  prs("\nToken: "); prunsign(token); prs(", globC: "); prc(globC);
  prs(", thechar: "); pint(thechar); prs(", symbol: "); prs(symbol);
  prs("\n********** Fehler : "); prs(s); end1(1); }
int listproc() {int i; 
  if (LTop > LSTART) {
  prs("\n;Funktion : "); prs(fname); 
  prs(", Anzahl lokaler Var: "); i=LTop - LSTART; prunsign(i); 
  prs("\n; # type sign widt Lokale Variablen"); 
    i=LSTART; 
    while (i < LTop) { listvar(i); i++; } } 
}
int listcall() { int i;
  prs("\n\n;   #  name   Liste der CALLs");
  i=0;  while (i< CTop) { calllisting(i); i++; } }
int calllisting(int i) {int j; char c;
  prs("\n;"); printint5(i); prc(32);
  /*c=CType [i]; if(c==1)prs("CALL ");*/
  j=i*16; pt=&CNameField + j;  prs(pt);
}
int countcalls(int f) { unsigned int j; unsigned int c;
  j=f*16; pt=&FNameField + j;  
  c=0;  while (c < CTop) {   
  j=c*16; p1=&CNameField + j;
  if (eqstr(pt,p1))  FCalls[f] = FCalls[f] + 1;   
  c++; }    
}
int listfunc() { int i;
  prs("\n\n\n;   # Calls Line  Name   Liste der Funktionen");
  i=0;  while (i < FTop) { countcalls (i); i++; } 
  i=0;  while (i < FTop) { funclisting(i); i++; } }
int funclisting(int i) {int j; 
  prs("\n;");    printint5(i);
  j = FCalls[i]; printint5(j);
  j = FAdr[i];   printint5(j); prc(32); prc(32);
  j=i*16; pt=&FNameField + j;  prs(pt);
}
int epilog() {unsigned int i; 
  prs("\n \n;   # type sign with name  Globale Variablen");
  i=1;
  while (i< GTop) { listvar(i); i++; }  listcall(); listfunc(); 
  LIST=1;
  prs("\n\n; ==================== Zeilen : "); prunsign(lineno);
  prs(",  HeapEnd: "); prunsign(orgData);
  prs("\n; Input: "); prs(&namein);
  prs(",  List: ");   prs(&namelst);
  prs(",  Ende des Programms: "); prs(namein);
  if(NASM==0)prs("\nEND");end1(0);}
int listvar(unsigned int i) {unsigned int j; char c;
  prs("\n;"); printint5(i); prc(32);
  c=GType [i]; if(c=='V')prs("var ");   if(c=='*')prs("ptr "); 
               if(c=='&')prs("arr ");   if(c=='#')prs("def ");  
  c=GSign [i]; if(c=='S')prs("sign "); if(c=='U')prs("unsg ");
  c=GWidth[i]; if(c=='B')prs("byte " );if(c=='W')prs("word " );
  j=i*16; pt=&GNameField + j;  prs(pt);  
  if(GType[i]=='#') { prc('='); j=GData[i]; prunsign(j); }
  if(GType[i]=='&') { prc('['); j=GData[i]; prunsign(j); prc(']');}  
  if (i >= LSTART) { prs(" = bp"); j=GData[i]; 
    if (j > 0) prc('+'); pint(j);  }
}
/* while(expr) stmt; do stmt while(expr); FOR: i=0; while(i<10){stmt; i++;}*/
/* DIV SI= dx:ax/si= ax,Rest dx;  dividend / divisor = quotient, remainder */