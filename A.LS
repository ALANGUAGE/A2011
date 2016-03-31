     1                                  
     2                                  ; F.COM CComp V0.6, Arglen: 4, Argv: A.C, Source: A.C, Output asm: A.S
     3                                  ;PROTECTED: 0 V86 OFF.  32bit: ON. 
     4                                  org  256 ; NASM ON
     5 00000000 E9BD2F                  jmp main
     6                                  
     7                                  ;-1 //   simplexpr-test-prleft-prrigth   22.05.2011
     8                                  
     9                                  
    10                                  ;-2 char Version1[]="F.COM CComp V0.6";
    11                                  
    12 00000003 462E434F4D2043436F-     Version1 db "F.COM CComp V0.6",0
    13 0000000C 6D702056302E3600   
    14                                  
    15                                  ;-3 #define BSS
    16                                  
    17                                  
    18                                  ;-4 #define ARCHIVE "AR.C"
    19                                  
    20                                  ;Use archive file: AR.C
    21                                  
    22                                  ;-5 #define ORGDATA     30000
    23                                  
    24                                  
    25                                  ;-6 char BSS=1;  char NASM=1;  char PROTECTED=0;     char LIST=0;
    26                                  
    27 00000014 01                      BSS db 1
    28 00000015 01                      NASM db 1
    29 00000016 00                      PROTECTED db 0
    30 00000017 00                      LIST db 0
    31                                  
    32                                  ;-7 #define LSTART        200
    33                                  
    34                                  
    35                                  ;-8 #define VARMAX        300
    36                                  
    37                                  
    38                                  ;-9 #define FUNCMAX       300
    39                                  
    40                                  
    41                                  ;-10 #define CALLMAX      2000
    42                                  
    43                                  
    44                                  ;-11 #define IDLENMAX       16
    45                                  
    46                                  
    47                                  ;-12 #define _  // Konstantvergleich
    48                                  
    49                                  
    50                                  ;-13 #define T_NAME        256
    51                                  
    52                                  
    53                                  ;-14 #define T_CONST       257 
    54                                  
    55                                  
    56                                  ;-15 #define T_STRING      258     
    57                                  
    58                                  
    59                                  ;-16 #define T_INCLUDE     510     
    60                                  
    61                                  
    62                                  ;-17 #define T_DEFINE      511  
    63                                  
    64                                  
    65                                  ;-18 #define T_RETURN      512
    66                                  
    67                                  
    68                                  ;-19 #define T_IF          513      
    69                                  
    70                                  
    71                                  ;-20 #define T_ELSE        514  
    72                                  
    73                                  
    74                                  ;-21 #define T_WHILE       515      
    75                                  
    76                                  
    77                                  ;-22 #define T_DO          516
    78                                  
    79                                  
    80                                  ;-23 #define T_INT         517     
    81                                  
    82                                  
    83                                  ;-24 #define T_ASM         518   
    84                                  
    85                                  
    86                                  ;-25 #define T_ASMBLOCK    519   
    87                                  
    88                                  
    89                                  ;-26 #define T_EMIT        520
    90                                  
    91                                  
    92                                  ;-27 #define T_GOTO        521    
    93                                  
    94                                  
    95                                  ;-28 #define T_VOID        529  
    96                                  
    97                                  
    98                                  ;-29 #define T_CHAR        530       
    99                                  
   100                                  
   101                                  ;-30 #define T_SIGNED      531
   102                                  
   103                                  
   104                                  ;-31 #define T_UNSIGNED    532
   105                                  
   106                                  
   107                                  ;-32 #define T_LONG        533
   108                                  
   109                                  
   110                                  ;-33 #define T_SHORT       534
   111                                  
   112                                  
   113                                  ;-34 #define T_UINT32      545
   114                                  
   115                                  
   116                                  ;-35 #define T_INTH        600
   117                                  
   118                                  
   119                                  ;-36 #define T_IFCARRY     601
   120                                  
   121                                  
   122                                  ;-37 #define T_IFZERO      602
   123                                  
   124                                  
   125                                  ;-38 #define T_EQ          806
   126                                  
   127                                  
   128                                  ;-39 #define T_NE          807
   129                                  
   130                                  
   131                                  ;-40 #define T_GE          811
   132                                  
   133                                  
   134                                  ;-41 #define T_LE          824
   135                                  
   136                                  
   137                                  ;-42 #define T_PLUSPLUS   1219
   138                                  
   139                                  
   140                                  ;-43 #define T_MINUSMINUS 1225
   141                                  
   142                                  
   143                                  ;-44 #define T_PLUSASS    1230
   144                                  
   145                                  
   146                                  ;-45 #define T_MINUSASS   1231
   147                                  
   148                                  
   149                                  ;-46 #define T_MULASS     1232
   150                                  
   151                                  
   152                                  ;-47 #define T_DIVASS     1233
   153                                  
   154                                  
   155                                  ;-48 #define T_ANDASS     1234
   156                                  
   157                                  
   158                                  ;-49 #define T_ORASS      1235
   159                                  
   160                                  
   161                                  ;-50 #define T_LESSLESS   1240
   162                                  
   163                                  
   164                                  ;-51 #define T_GREATGREAT 1241
   165                                  
   166                                  
   167                                  ;-52 #define COMAX        3000
   168                                  
   169                                  
   170                                  ;-53 #define CMDLENMAX      67
   171                                  
   172                                  
   173                                  ;-54 unsigned long LDATAORIG=2000000;     unsigned long ldata=2000000; long LONG0=0;
   174                                  
   175 00000018 80841E00                LDATAORIG dd 2000000
   176 0000001C 80841E00                ldata dd 2000000
   177 00000020 00000000                LONG0 dd 0
   178                                  
   179                                  ;-55 unsigned long ORGDATAORIG=30000;     unsigned long orgData=30000;
   180                                  
   181 00000024 30750000                ORGDATAORIG dd 30000
   182 00000028 30750000                orgData dd 30000
   183                                  
   184                                  ;-56 char co[COMAX];    int maxco=0;      int maxco1=0;
   185                                  
   186                                  section .bss
   187                                  absolute 30000
   188 00007530 <res 00000BB8>          co resb 3000
   189                                  section .text
   190 0000002C 0000                    maxco dw 0
   191 0000002E 0000                    maxco1 dw 0
   192                                  
   193                                  ;-57 char coname[CMDLENMAX];              char *cloc=0;
   194                                  
   195                                  section .bss
   196                                  absolute 33000
   197 000080E8 <res 00000043>          coname resb 67
   198                                  section .text
   199 00000030 0000                    cloc dw 0
   200                                  
   201                                  ;-58 char locname[CMDLENMAX]; int maxloc=0;
   202                                  
   203                                  section .bss
   204                                  absolute 33067
   205 0000812B <res 00000043>          locname resb 67
   206                                  section .text
   207 00000032 0000                    maxloc dw 0
   208                                  
   209                                  ;-59 int fdin=0;        int fdout=0;      char symbol[128];
   210                                  
   211 00000034 0000                    fdin dw 0
   212 00000036 0000                    fdout dw 0
   213                                  section .bss
   214                                  absolute 33134
   215 0000816E <res 00000080>          symbol resb 128
   216                                  section .text
   217                                  
   218                                  ;-60 char fname[CMDLENMAX];               char namein[CMDLENMAX];
   219                                  
   220                                  section .bss
   221                                  absolute 33262
   222 000081EE <res 00000043>          fname resb 67
   223                                  section .text
   224                                  section .bss
   225                                  absolute 33329
   226 00008231 <res 00000043>          namein resb 67
   227                                  section .text
   228                                  
   229                                  ;-61 char namelst[CMDLENMAX];             char archivename[]="AR.C";
   230                                  
   231                                  section .bss
   232                                  absolute 33396
   233 00008274 <res 00000043>          namelst resb 67
   234                                  section .text
   235 00000038 41522E4300              archivename db "AR.C",0
   236                                  
   237                                  ;-62 int token=0;       char globC=0;     int spalte=0;
   238                                  
   239 0000003D 0000                    token dw 0
   240 0000003F 00                      globC db 0
   241 00000040 0000                    spalte dw 0
   242                                  
   243                                  ;-63 char thechar=0;    int iscmp=0;      int nconst=0;
   244                                  
   245 00000042 00                      thechar db 0
   246 00000043 0000                    iscmp dw 0
   247 00000045 0000                    nconst dw 0
   248                                  
   249                                  ;-64 int nreturn=0;     int nlabel=0;     int callrecursive=0;
   250                                  
   251 00000047 0000                    nreturn dw 0
   252 00000049 0000                    nlabel dw 0
   253 0000004B 0000                    callrecursive dw 0
   254                                  
   255                                  ;-65 unsigned long lexvalL;  char islong; char isldata=0;
   256                                  
   257 0000004D 00000000                lexvalL dd 0
   258 00000051 00                      islong db 0
   259 00000052 00                      isldata db 0
   260                                  
   261                                  ;-66 int typei;        char istype;  char GType [VARMAX]; // 0=V, 1=*, 2=&,#
   262                                  
   263 00000053 0000                    typei dw 0
   264 00000055 00                      istype db 0
   265                                  section .bss
   266                                  absolute 33463
   267 000082B7 <res 0000012C>          GType resb 300
   268                                  section .text
   269                                  
   270                                  ;-67 int signi;        char issign;  char GSign [VARMAX]; // 0=U, 1=S
   271                                  
   272 00000056 0000                    signi dw 0
   273 00000058 00                      issign db 0
   274                                  section .bss
   275                                  absolute 33763
   276 000083E3 <res 0000012C>          GSign resb 300
   277                                  section .text
   278                                  
   279                                  ;-68 int widthi;int wi;char iswidth; char GWidth[VARMAX]; // 0, 1, 2, 4
   280                                  
   281 00000059 0000                    widthi dw 0
   282 0000005B 0000                    wi dw 0
   283 0000005D 00                      iswidth db 0
   284                                  section .bss
   285                                  absolute 34063
   286 0000850F <res 0000012C>          GWidth resb 300
   287                                  section .text
   288                                  
   289                                  ;-69 int  GTop=1;        int LTop=150;
   290                                  
   291 0000005E 0100                    GTop dw 1
   292 00000060 9600                    LTop dw 150
   293                                  
   294                                  ;-70 int  GLino   [VARMAX];     // lineno
   295                                  
   296                                  section .bss
   297                                  absolute 34363
   298 0000863B <res 00000258>          GLino resw 300
   299                                  section .text
   300                                  
   301                                  ;-71 int  GUsed   [VARMAX];     // how manytimes is var used
   302                                  
   303                                  section .bss
   304                                  absolute 34963
   305 00008893 <res 00000258>          GUsed resw 300
   306                                  section .text
   307                                  
   308                                  ;-72 long GData   [VARMAX];     // init data or local addr or array len or define no
   309                                  
   310                                  section .bss
   311                                  absolute 35563
   312 00008AEB <res 000004B0>          GData resd 300
   313                                  section .text
   314                                  
   315                                  ;-73 long GAdr    [VARMAX];     // Address 32 bit
   316                                  
   317                                  section .bss
   318                                  absolute 36763
   319 00008F9B <res 000004B0>          GAdr resd 300
   320                                  section .text
   321                                  
   322                                  ;-74 int  GName   [VARMAX];     // start of var name in list
   323                                  
   324                                  section .bss
   325                                  absolute 37963
   326 0000944B <res 00000258>          GName resw 300
   327                                  section .text
   328                                  
   329                                  ;-75 int  FTop=0;               // top of function list
   330                                  
   331 00000062 0000                    FTop dw 0
   332                                  
   333                                  ;-76 char FWidth  [FUNCMAX];    // iswidth
   334                                  
   335                                  section .bss
   336                                  absolute 38563
   337 000096A3 <res 0000012C>          FWidth resb 300
   338                                  section .text
   339                                  
   340                                  ;-77 int  FLino   [FUNCMAX];    // lineno
   341                                  
   342                                  section .bss
   343                                  absolute 38863
   344 000097CF <res 00000258>          FLino resw 300
   345                                  section .text
   346                                  
   347                                  ;-78 int  FCalls  [FUNCMAX];    // no of calls for this function
   348                                  
   349                                  section .bss
   350                                  absolute 39463
   351 00009A27 <res 00000258>          FCalls resw 300
   352                                  section .text
   353                                  
   354                                  ;-79 int  FName   [FUNCMAX];    // start of function name in list
   355                                  
   356                                  section .bss
   357                                  absolute 40063
   358 00009C7F <res 00000258>          FName resw 300
   359                                  section .text
   360                                  
   361                                  ;-80 int  CTop=0;               // top of call list
   362                                  
   363 00000064 0000                    CTop dw 0
   364                                  
   365                                  ;-81 int  CName   [CALLMAX];    // CNameTop
   366                                  
   367                                  section .bss
   368                                  absolute 40663
   369 00009ED7 <res 00000FA0>          CName resw 2000
   370                                  section .text
   371                                  
   372                                  ;-82 char CUnres  [CALLMAX];    // 0 = unresolved call
   373                                  
   374                                  section .bss
   375                                  absolute 44663
   376 0000AE77 <res 000007D0>          CUnres resb 2000
   377                                  section .text
   378                                  
   379                                  ;-83 char Ls[]="12345678901";                      //must be in low memory
   380                                  
   381 00000066 313233343536373839-     Ls db "12345678901",0
   382 0000006F 303100             
   383                                  
   384                                  ;-84 char NA[]="12345678901234567890123456789012"; //must be in low memory
   385                                  
   386 00000072 313233343536373839-     NA db "12345678901234567890123456789012",0
   387 0000007B 303132333435363738-
   388 00000084 393031323334353637-
   389 0000008D 383930313200       
   390                                  
   391                                  ;-85 char NB[]="12345678901234567890123456789012"; //must be in low memory
   392                                  
   393 00000093 313233343536373839-     NB db "12345678901234567890123456789012",0
   394 0000009C 303132333435363738-
   395 000000A5 393031323334353637-
   396 000000AE 383930313200       
   397                                  
   398                                  ;-86 char fgetsdest[128];         unsigned char *CNameTop=0;
   399                                  
   400                                  section .bss
   401                                  absolute 46663
   402 0000B647 <res 00000080>          fgetsdest resb 128
   403                                  section .text
   404 000000B4 0000                    CNameTop dw 0
   405                                  
   406                                  ;-87 unsigned char *fgetsp=0;     unsigned int segE;
   407                                  
   408 000000B6 0000                    fgetsp dw 0
   409 000000B8 0000                    segE dw 0
   410                                  
   411                                  ;-88 unsigned int lineno=1;       unsigned int linenoinclude=1;
   412                                  
   413 000000BA 0100                    lineno dw 1
   414 000000BC 0100                    linenoinclude dw 1
   415                                  
   416                                  ;-89 unsigned char *pt=0;         unsigned char *p1=0;
   417                                  
   418 000000BE 0000                    pt dw 0
   419 000000C0 0000                    p1 dw 0
   420                                  
   421                                  ;-90 int DOS_ERR=0; int DOS_NoBytes=0; char DOS_ByteRead=0;
   422                                  
   423 000000C2 0000                    DOS_ERR dw 0
   424 000000C4 0000                    DOS_NoBytes dw 0
   425 000000C6 00                      DOS_ByteRead db 0
   426                                  
   427                                  ;-91 
   428                                  
   429                                  
   430                                  ;-92 long varL1!; int vari1!; unsigned int vari2![3];
   431                                  
   432                                  ;p32  adr: 2000000, varL1 [4]
   433                                  ;p32  adr: 2000004, vari1 [2]
   434                                  ;p32  adr: 2000006, vari2 [6]
   435                                  
   436                                  ;-93 char varc1!; long varL2!;
   437                                  
   438                                  ;p32  adr: 2000012, varc1 [1]
   439                                  ;p32  adr: 2000013, varL2 [4]
   440                                  
   441                                  ;-94 long arrL1![3]; long varL4!; char chc2!;
   442                                  
   443                                  ;p32  adr: 2000017, arrL1 [12]
   444                                  ;p32  adr: 2000029, varL4 [4]
   445                                  ;p32  adr: 2000033, chc2 [1]
   446                                  
   447                                  ;-95 void ttt(long par1, char *s)
   448                                  
   449                                  
   450                                  ttt:  ; *** PROC ***
   451                                  
   452                                  ;-96   {   }
   453                                  
   454                                  ;Function : ttt, Number of local variables: 2
   455                                  ;   # type sign width line used address name   list of local variables
   456                                  ;  200 var sign dwrd    95 NULL bp+4    par1
   457                                  ;  201 ptr sign byte    95 NULL bp+8    s;
   458 000000C7 C8000000                 ENTER  0,0
   459                                  
   460                                  ;-97 void a(unsigned int i) { if(NASM == 0) prs("offset "); printName(i);}//address
   461 000000CB C9                       LEAVE
   462 000000CC C3                       ret
   463                                  ; ENDP
   464                                  
   465                                  a:  ; *** PROC ***
   466                                  ;Function : a, Number of local variables: 1
   467                                  ;   # type sign width line used address name   list of local variables
   468                                  ;  200 var unsg word    97 NULL bp+4    i;
   469 000000CD C8000000                 ENTER  0,0
   470 000000D1 A0[1500]                 mov al, [NASM]
   471 000000D4 3C00                     cmp al, 0
   472 000000D6 0F850900                 jne @@a1
   473 000000DA 68[EE00]                 push a_0
   474 000000DD E8B24D                   call prs
   475 000000E0 83C402                   add  sp, 2
   476                                  @@a1:
   477 000000E3 FF7604                   push word [bp+4]
   478 000000E6 E82309                   call printName
   479 000000E9 83C402                   add  sp, 2
   480                                  
   481                                  ;-98 void v(unsigned int i) { if(NASM) { if (i < LSTART) prc('['); }
   482 000000EC C9                       LEAVE
   483 000000ED C3                       ret
   484                                  ; ENDP
   485 000000EE 6F66667365742000        a_0 db "offset ",0
   486                                  
   487                                  v:  ; *** PROC ***
   488                                  ;Function : v, Number of local variables: 1
   489                                  ;   # type sign width line used address name   list of local variables
   490                                  ;  200 var unsg word    98 NULL bp+4    i;
   491 000000F6 C8000000                 ENTER  0,0
   492 000000FA A0[1500]                 mov al, [NASM]
   493 000000FD 08C0                     or  al, al
   494 000000FF 0F841200                 je @@v2
   495 00000103 8B4604                   mov ax, [bp+4]
   496 00000106 3DC800                   cmp ax, 200
   497 00000109 0F8D0800                 jge @@v3
   498 0000010D 6A5B                     push 91
   499 0000010F E8114D                   call prc
   500 00000112 83C402                   add  sp, 2
   501                                  @@v3:
   502                                  
   503                                  ;-99   printName(i); if(NASM) { if (i < LSTART) prc(']'); }   }//value
   504                                  
   505                                  @@v2:
   506 00000115 FF7604                   push word [bp+4]
   507 00000118 E8F108                   call printName
   508 0000011B 83C402                   add  sp, 2
   509 0000011E A0[1500]                 mov al, [NASM]
   510 00000121 08C0                     or  al, al
   511 00000123 0F841200                 je @@v4
   512 00000127 8B4604                   mov ax, [bp+4]
   513 0000012A 3DC800                   cmp ax, 200
   514 0000012D 0F8D0800                 jge @@v5
   515 00000131 6A5D                     push 93
   516 00000133 E8ED4C                   call prc
   517 00000136 83C402                   add  sp, 2
   518                                  @@v5:
   519                                  @@v4:
   520                                  
   521                                  ;-100 void checknamelen() { int i;    i=strlen(symbol);
   522 00000139 C9                       LEAVE
   523 0000013A C3                       ret
   524                                  ; ENDP
   525                                  
   526                                  checknamelen:  ; *** PROC ***
   527                                  ;Function : checknamelen, Number of local variables: 1
   528                                  ;   # type sign width line used address name   list of local variables
   529                                  ;  200 var sign word   100 NULL bp-2    i;
   530 0000013B C8020000                 ENTER  2,0
   531 0000013F 8D066E81                 lea  ax, [symbol]
   532 00000143 50                       push ax
   533 00000144 E81A60                   call strlen
   534 00000147 83C402                   add  sp, 2
   535 0000014A 8946FE                   mov word [bp-2], ax
   536                                  
   537                                  ;-101   if (i >= IDLENMAX) error1("Item name is too long in characters)"); }
   538                                  
   539 0000014D 8B46FE                   mov ax, [bp-2]
   540 00000150 83F810                   cmp ax, 16 ;unsigned : 1
   541 00000153 0F8C0900                 jl  @@checknamelen6
   542 00000157 68[6201]                 push checknamelen_0
   543 0000015A E83650                   call error1
   544 0000015D 83C402                   add  sp, 2
   545                                  @@checknamelen6:
   546                                  
   547                                  ;-102 void pwi(int i) {  // print width
   548 00000160 C9                       LEAVE
   549 00000161 C3                       ret
   550                                  ; ENDP
   551 00000162 4974656D206E616D65-     checknamelen_0 db "Item name is too long in characters)",0
   552 0000016B 20697320746F6F206C-
   553 00000174 6F6E6720696E206368-
   554 0000017D 617261637465727329-
   555 00000186 00                 
   556                                  
   557                                  pwi:  ; *** PROC ***
   558                                  
   559                                  ;-103   if (i == 1) prs("byte ");    if (i == 2) prs("word ");
   560                                  
   561                                  ;Function : pwi, Number of local variables: 1
   562                                  ;   # type sign width line used address name   list of local variables
   563                                  ;  200 var sign word   102 NULL bp+4    i;
   564 00000187 C8000000                 ENTER  0,0
   565 0000018B 8B4604                   mov ax, [bp+4]
   566 0000018E 83F801                   cmp ax, 1
   567 00000191 0F850900                 jne @@pwi7
   568 00000195 68[D801]                 push pwi_0
   569 00000198 E8F74C                   call prs
   570 0000019B 83C402                   add  sp, 2
   571                                  @@pwi7:
   572 0000019E 8B4604                   mov ax, [bp+4]
   573 000001A1 83F802                   cmp ax, 2
   574 000001A4 0F850900                 jne @@pwi8
   575 000001A8 68[DE01]                 push pwi_1
   576 000001AB E8E44C                   call prs
   577 000001AE 83C402                   add  sp, 2
   578                                  
   579                                  ;-104   if (i == 4) prs("dword ");   if (NASM == 0) prs("ptr ");
   580                                  
   581                                  @@pwi8:
   582 000001B1 8B4604                   mov ax, [bp+4]
   583 000001B4 83F804                   cmp ax, 4
   584 000001B7 0F850900                 jne @@pwi9
   585 000001BB 68[E401]                 push pwi_2
   586 000001BE E8D14C                   call prs
   587 000001C1 83C402                   add  sp, 2
   588                                  @@pwi9:
   589 000001C4 A0[1500]                 mov al, [NASM]
   590 000001C7 3C00                     cmp al, 0
   591 000001C9 0F850900                 jne @@pwi10
   592 000001CD 68[EB01]                 push pwi_3
   593 000001D0 E8BF4C                   call prs
   594 000001D3 83C402                   add  sp, 2
   595                                  
   596                                  ;-105 }
   597                                  
   598                                  @@pwi10:
   599                                  
   600                                  ;-106 void storeName() {int i;
   601 000001D6 C9                       LEAVE
   602 000001D7 C3                       ret
   603                                  ; ENDP
   604 000001D8 627974652000            pwi_0 db "byte ",0
   605 000001DE 776F72642000            pwi_1 db "word ",0
   606 000001E4 64776F72642000          pwi_2 db "dword ",0
   607 000001EB 7074722000              pwi_3 db "ptr ",0
   608                                  
   609                                  storeName:  ; *** PROC ***
   610                                  
   611                                  ;-107   i=strlen(symbol); to_far(CNameTop, symbol); CNameTop=CNameTop+i; CNameTop++;
   612                                  
   613                                  ;Function : storeName, Number of local variables: 1
   614                                  ;   # type sign width line used address name   list of local variables
   615                                  ;  200 var sign word   106 NULL bp-2    i;
   616 000001F0 C8020000                 ENTER  2,0
   617 000001F4 8D066E81                 lea  ax, [symbol]
   618 000001F8 50                       push ax
   619 000001F9 E8655F                   call strlen
   620 000001FC 83C402                   add  sp, 2
   621 000001FF 8946FE                   mov word [bp-2], ax
   622 00000202 8D066E81                 lea  ax, [symbol]
   623 00000206 50                       push ax
   624 00000207 FF36[B400]               push word [CNameTop]
   625 0000020B E85B5C                   call to_far
   626 0000020E 83C404                   add  sp, 4
   627 00000211 A1[B400]                 mov ax, [CNameTop]
   628 00000214 0346FE                   add ax, [bp-2]
   629 00000217 A3[B400]                 mov word [CNameTop], ax
   630 0000021A FF06[B400]               inc  word [CNameTop]
   631                                  
   632                                  ;-108 }
   633                                  
   634                                  
   635                                  ;-109 char doglobName[IDLENMAX];
   636 0000021E C9                       LEAVE
   637 0000021F C3                       ret
   638                                  ; ENDP
   639                                  section .bss
   640                                  absolute 46791
   641 0000B6C7 <res 00000010>          doglobName resb 16
   642                                  section .text
   643                                  
   644                                  ;-110 void doLdata() {
   645                                  
   646                                  
   647                                  doLdata:  ; *** PROC ***
   648                                  
   649                                  ;-111   if (GTop >= LSTART) error1("Global table full");
   650                                  
   651 00000220 A1[5E00]                 mov ax, [GTop]
   652 00000223 3DC800                   cmp ax, 200 ;unsigned : 1
   653 00000226 0F8C0900                 jl  @@doLdata11
   654 0000022A 68[8703]                 push doLdata_0
   655 0000022D E8634F                   call error1
   656 00000230 83C402                   add  sp, 2
   657                                  
   658                                  ;-112   if (iswidth == 0) error1("no VOID as var type");
   659                                  
   660                                  @@doLdata11:
   661 00000233 A0[5D00]                 mov al, [iswidth]
   662 00000236 3C00                     cmp al, 0
   663 00000238 0F850900                 jne @@doLdata12
   664 0000023C 68[9903]                 push doLdata_1
   665 0000023F E8514F                   call error1
   666 00000242 83C402                   add  sp, 2
   667                                  
   668                                  ;-113   checknamelen();
   669                                  
   670                                  @@doLdata12:
   671 00000245 E8F3FE                   call checknamelen
   672                                  
   673                                  ;-114   if (checkName() != 0) error1("Global variable already defined");
   674                                  
   675 00000248 E87608                   call checkName
   676 0000024B 3C00                     cmp al, 0
   677 0000024D 0F840900                 je  @@doLdata13
   678 00000251 68[AD03]                 push doLdata_2
   679 00000254 E83C4F                   call error1
   680 00000257 83C402                   add  sp, 2
   681                                  
   682                                  ;-115   GAdr[GTop]=ldata;
   683                                  
   684                                  @@doLdata13:
   685 0000025A 66A1[1C00]               mov eax, [ldata]
   686 0000025E 8B1E[5E00]               mov bx, [GTop]
   687 00000262 C1E302                   shl bx, 2
   688 00000265 6689879B8F               mov [GAdr+bx], eax
   689                                  
   690                                  ;-116   eax=0;  lexvalL=iswidth; // Cast B2L
   691                                  
   692 0000026A 66B800000000             mov  eax, 0
   693 00000270 A0[5D00]                 mov al, [iswidth]
   694 00000273 66A3[4D00]               mov dword [lexvalL], eax
   695                                  
   696                                  ;-117   if (istoken('[')) { istype='&';
   697                                  
   698 00000277 6A5B                     push 91
   699 00000279 E8D04A                   call istoken
   700 0000027C 83C402                   add  sp, 2
   701 0000027F 08C0                     or  al, al
   702 00000281 0F844300                 je @@doLdata14
   703 00000285 66B826000000             mov eax, 38
   704 0000028B A2[5500]                 mov byte [istype], al
   705                                  
   706                                  ;-118     expect(T_CONST);
   707                                  
   708 0000028E 680101                   push 257
   709 00000291 E8E04A                   call expect
   710 00000294 83C402                   add  sp, 2
   711                                  
   712                                  ;-119     if (iswidth==2) lexvalL=lexvalL << 1;
   713                                  
   714 00000297 A0[5D00]                 mov al, [iswidth]
   715 0000029A 3C02                     cmp al, 2
   716 0000029C 0F850B00                 jne @@doLdata15
   717 000002A0 66A1[4D00]               mov eax, [lexvalL]
   718 000002A4 66D1E0                   shl eax, 1
   719 000002A7 66A3[4D00]               mov dword [lexvalL], eax
   720                                  
   721                                  ;-120     if (iswidth==4) lexvalL=lexvalL << 2;
   722                                  
   723                                  @@doLdata15:
   724 000002AB A0[5D00]                 mov al, [iswidth]
   725 000002AE 3C04                     cmp al, 4
   726 000002B0 0F850C00                 jne @@doLdata16
   727 000002B4 66A1[4D00]               mov eax, [lexvalL]
   728 000002B8 66C1E002                 shl eax, 2
   729 000002BC 66A3[4D00]               mov dword [lexvalL], eax
   730                                  
   731                                  ;-121     expect(']');
   732                                  
   733                                  @@doLdata16:
   734 000002C0 6A5D                     push 93
   735 000002C2 E8AF4A                   call expect
   736 000002C5 83C402                   add  sp, 2
   737                                  
   738                                  ;-122   }
   739                                  
   740                                  
   741                                  ;-123   prs("\n;p32  adr: "); prL(ldata); prs(", "); prs(symbol);
   742                                  
   743                                  @@doLdata14:
   744 000002C8 68[CD03]                 push doLdata_3
   745 000002CB E8C44B                   call prs
   746 000002CE 83C402                   add  sp, 2
   747 000002D1 66FF36[1C00]             push dword [ldata]
   748 000002D6 E86C5C                   call prL
   749 000002D9 83C404                   add  sp, 4
   750 000002DC 68[DB03]                 push doLdata_4
   751 000002DF E8B04B                   call prs
   752 000002E2 83C402                   add  sp, 2
   753 000002E5 8D066E81                 lea  ax, [symbol]
   754 000002E9 50                       push ax
   755 000002EA E8A54B                   call prs
   756 000002ED 83C402                   add  sp, 2
   757                                  
   758                                  ;-124   prs(" ["); prL(lexvalL); prc(']');
   759                                  
   760 000002F0 68[DE03]                 push doLdata_5
   761 000002F3 E89C4B                   call prs
   762 000002F6 83C402                   add  sp, 2
   763 000002F9 66FF36[4D00]             push dword [lexvalL]
   764 000002FE E8445C                   call prL
   765 00000301 83C404                   add  sp, 4
   766 00000304 6A5D                     push 93
   767 00000306 E81A4B                   call prc
   768 00000309 83C402                   add  sp, 2
   769                                  
   770                                  ;-125   ldata=ldata+lexvalL; GData[GTop]=lexvalL;
   771                                  
   772 0000030C 66A1[1C00]               mov eax, [ldata]
   773 00000310 660306[4D00]             add eax, [lexvalL]
   774 00000315 66A3[1C00]               mov dword [ldata], eax
   775 00000319 66A1[4D00]               mov eax, [lexvalL]
   776 0000031D 8B1E[5E00]               mov bx, [GTop]
   777 00000321 C1E302                   shl bx, 2
   778 00000324 668987EB8A               mov [GData+bx], eax
   779                                  
   780                                  ;-126   GSign[GTop]=issign;  GWidth[GTop]=iswidth; GType[GTop]=istype;
   781                                  
   782 00000329 A0[5800]                 mov al, [issign]
   783 0000032C 8B1E[5E00]               mov bx, [GTop]
   784 00000330 8887E383                 mov [GSign+bx], al
   785 00000334 A0[5D00]                 mov al, [iswidth]
   786 00000337 8B1E[5E00]               mov bx, [GTop]
   787 0000033B 88870F85                 mov [GWidth+bx], al
   788 0000033F A0[5500]                 mov al, [istype]
   789 00000342 8B1E[5E00]               mov bx, [GTop]
   790 00000346 8887B782                 mov [GType+bx], al
   791                                  
   792                                  ;-127   GLino [GTop]=lineno-1; GUsed [GTop]=0;
   793                                  
   794 0000034A A1[BA00]                 mov ax, [lineno]
   795 0000034D 83E801                   sub ax, 1
   796 00000350 8B1E[5E00]               mov bx, [GTop]
   797 00000354 D1E3                     shl bx, 1
   798 00000356 89873B86                 mov [GLino+bx], ax
   799 0000035A 66B800000000             mov eax, 0
   800 00000360 8B1E[5E00]               mov bx, [GTop]
   801 00000364 D1E3                     shl bx, 1
   802 00000366 89879388                 mov [GUsed+bx], ax
   803                                  
   804                                  ;-128 //  if (isstrarr) strcpy(symbol, doglobName);
   805                                  
   806                                  
   807                                  ;-129   GName[GTop] = CNameTop;
   808                                  
   809 0000036A A1[B400]                 mov ax, [CNameTop]
   810 0000036D 8B1E[5E00]               mov bx, [GTop]
   811 00000371 D1E3                     shl bx, 1
   812 00000373 89874B94                 mov [GName+bx], ax
   813                                  
   814                                  ;-130   storeName();
   815                                  
   816 00000377 E876FE                   call storeName
   817                                  
   818                                  ;-131   GTop++; expect(';');
   819                                  
   820 0000037A FF06[5E00]               inc  word [GTop]
   821 0000037E 6A3B                     push 59
   822 00000380 E8F149                   call expect
   823 00000383 83C402                   add  sp, 2
   824                                  
   825                                  ;-132 }
   826                                  
   827                                  
   828                                  ;-133 void doglob() { int i; int j; int isstrarr; isstrarr=0;
   829                                  
   830 00000386 C3                       ret
   831                                  ; ENDP
   832 00000387 476C6F62616C207461-     doLdata_0 db "Global table full",0
   833 00000390 626C652066756C6C00 
   834 00000399 6E6F20564F49442061-     doLdata_1 db "no VOID as var type",0
   835 000003A2 732076617220747970-
   836 000003AB 6500               
   837 000003AD 476C6F62616C207661-     doLdata_2 db "Global variable already defined",0
   838 000003B6 726961626C6520616C-
   839 000003BF 726561647920646566-
   840 000003C8 696E656400         
   841 000003CD 5C6E3B703332202061-     doLdata_3 db "\n;p32  adr: ",0
   842 000003D6 64723A2000         
   843 000003DB 2C2000                  doLdata_4 db ", ",0
   844 000003DE 205B00                  doLdata_5 db " [",0
   845                                  
   846                                  doglob:  ; *** PROC ***
   847                                  ;Function : doglob, Number of local variables: 3
   848                                  ;   # type sign width line used address name   list of local variables
   849                                  ;  200 var sign word   133 NULL bp-2    i
   850                                  ;  201 var sign word   133 NULL bp-4    j
   851                                  ;  202 var sign word   133 NULL bp-6    isstrarr;
   852 000003E1 C8060000                 ENTER  6,0
   853 000003E5 66B800000000             mov eax, 0
   854 000003EB 8946FA                   mov word [bp-6], ax
   855                                  
   856                                  ;-134   if (GTop >= LSTART) error1("Global table full");
   857                                  
   858 000003EE A1[5E00]                 mov ax, [GTop]
   859 000003F1 3DC800                   cmp ax, 200 ;unsigned : 1
   860 000003F4 0F8C0900                 jl  @@doglob17
   861 000003F8 68[1408]                 push doglob_0
   862 000003FB E8954D                   call error1
   863 000003FE 83C402                   add  sp, 2
   864                                  
   865                                  ;-135   if (iswidth == 0) error1("no VOID as var type");
   866                                  
   867                                  @@doglob17:
   868 00000401 A0[5D00]                 mov al, [iswidth]
   869 00000404 3C00                     cmp al, 0
   870 00000406 0F850900                 jne @@doglob18
   871 0000040A 68[2608]                 push doglob_1
   872 0000040D E8834D                   call error1
   873 00000410 83C402                   add  sp, 2
   874                                  
   875                                  ;-136   checknamelen();
   876                                  
   877                                  @@doglob18:
   878 00000413 E825FD                   call checknamelen
   879                                  
   880                                  ;-137   if (checkName() != 0) error1("Global variable already defined");
   881                                  
   882 00000416 E8A806                   call checkName
   883 00000419 3C00                     cmp al, 0
   884 0000041B 0F840900                 je  @@doglob19
   885 0000041F 68[3A08]                 push doglob_2
   886 00000422 E86E4D                   call error1
   887 00000425 83C402                   add  sp, 2
   888                                  
   889                                  ;-138   GAdr[GTop]=LONG0;
   890                                  
   891                                  @@doglob19:
   892 00000428 66A1[2000]               mov eax, [LONG0]
   893 0000042C 8B1E[5E00]               mov bx, [GTop]
   894 00000430 C1E302                   shl bx, 2
   895 00000433 6689879B8F               mov [GAdr+bx], eax
   896                                  
   897                                  ;-139   if (istoken('[')) { istype='&';
   898                                  
   899 00000438 6A5B                     push 91
   900 0000043A E80F49                   call istoken
   901 0000043D 83C402                   add  sp, 2
   902 00000440 08C0                     or  al, al
   903 00000442 0F84B402                 je @@doglob20
   904 00000446 66B826000000             mov eax, 38
   905 0000044C A2[5500]                 mov byte [istype], al
   906                                  
   907                                  ;-140       if (istoken(T_CONST)) {
   908                                  
   909 0000044F 680101                   push 257
   910 00000452 E8F748                   call istoken
   911 00000455 83C402                   add  sp, 2
   912 00000458 08C0                     or  al, al
   913 0000045A 0F847C01                 je @@doglob21
   914                                  
   915                                  ;-141       if (BSS) { if (NASM) {prs("\nsection .bss\nabsolute ");
   916                                  
   917 0000045E A0[1400]                 mov al, [BSS]
   918 00000461 08C0                     or  al, al
   919 00000463 0F844400                 je @@doglob22
   920 00000467 A0[1500]                 mov al, [NASM]
   921 0000046A 08C0                     or  al, al
   922 0000046C 0F842700                 je @@doglob23
   923 00000470 68[5A08]                 push doglob_3
   924 00000473 E81C4A                   call prs
   925 00000476 83C402                   add  sp, 2
   926                                  
   927                                  ;-142         GAdr[GTop]=orgData;
   928                                  
   929 00000479 66A1[2800]               mov eax, [orgData]
   930 0000047D 8B1E[5E00]               mov bx, [GTop]
   931 00000481 C1E302                   shl bx, 2
   932 00000484 6689879B8F               mov [GAdr+bx], eax
   933                                  
   934                                  ;-143         prunsign1(orgData); }
   935                                  
   936 00000489 66FF36[2800]             push dword [orgData]
   937 0000048E E86A4B                   call prunsign1
   938 00000491 83C404                   add  sp, 4
   939                                  
   940                                  ;-144         else {prs("\nAData = $\norg "); prunsign1(orgData);} }
   941                                  
   942 00000494 E91400                   jmp @@doglob24
   943                                  @@doglob23:
   944 00000497 68[7408]                 push doglob_4
   945 0000049A E8F549                   call prs
   946 0000049D 83C402                   add  sp, 2
   947 000004A0 66FF36[2800]             push dword [orgData]
   948 000004A5 E8534B                   call prunsign1
   949 000004A8 83C404                   add  sp, 4
   950                                  @@doglob24:
   951                                  
   952                                  ;-145       prs("\n"); prs(symbol); 
   953                                  
   954                                  @@doglob22:
   955 000004AB 68[8608]                 push doglob_5
   956 000004AE E8E149                   call prs
   957 000004B1 83C402                   add  sp, 2
   958 000004B4 8D066E81                 lea  ax, [symbol]
   959 000004B8 50                       push ax
   960 000004B9 E8D649                   call prs
   961 000004BC 83C402                   add  sp, 2
   962                                  
   963                                  ;-146       if (iswidth==1) {if (NASM) prs(" resb "); else prs(" db ");}
   964                                  
   965 000004BF A0[5D00]                 mov al, [iswidth]
   966 000004C2 3C01                     cmp al, 1
   967 000004C4 0F851E00                 jne @@doglob25
   968 000004C8 A0[1500]                 mov al, [NASM]
   969 000004CB 08C0                     or  al, al
   970 000004CD 0F840C00                 je @@doglob26
   971 000004D1 68[8908]                 push doglob_6
   972 000004D4 E8BB49                   call prs
   973 000004D7 83C402                   add  sp, 2
   974 000004DA E90900                   jmp @@doglob27
   975                                  @@doglob26:
   976 000004DD 68[9008]                 push doglob_7
   977 000004E0 E8AF49                   call prs
   978 000004E3 83C402                   add  sp, 2
   979                                  @@doglob27:
   980                                  
   981                                  ;-147       if (iswidth==2) {if (NASM) prs(" resw "); else prs(" dw ");}
   982                                  
   983                                  @@doglob25:
   984 000004E6 A0[5D00]                 mov al, [iswidth]
   985 000004E9 3C02                     cmp al, 2
   986 000004EB 0F851E00                 jne @@doglob28
   987 000004EF A0[1500]                 mov al, [NASM]
   988 000004F2 08C0                     or  al, al
   989 000004F4 0F840C00                 je @@doglob29
   990 000004F8 68[9508]                 push doglob_8
   991 000004FB E89449                   call prs
   992 000004FE 83C402                   add  sp, 2
   993 00000501 E90900                   jmp @@doglob30
   994                                  @@doglob29:
   995 00000504 68[9C08]                 push doglob_9
   996 00000507 E88849                   call prs
   997 0000050A 83C402                   add  sp, 2
   998                                  @@doglob30:
   999                                  
  1000                                  ;-148       if (iswidth==4) {if (NASM) prs(" resd "); else prs(" dd ");}
  1001                                  
  1002                                  @@doglob28:
  1003 0000050D A0[5D00]                 mov al, [iswidth]
  1004 00000510 3C04                     cmp al, 4
  1005 00000512 0F851E00                 jne @@doglob31
  1006 00000516 A0[1500]                 mov al, [NASM]
  1007 00000519 08C0                     or  al, al
  1008 0000051B 0F840C00                 je @@doglob32
  1009 0000051F 68[A108]                 push doglob_10
  1010 00000522 E86D49                   call prs
  1011 00000525 83C402                   add  sp, 2
  1012 00000528 E90900                   jmp @@doglob33
  1013                                  @@doglob32:
  1014 0000052B 68[A808]                 push doglob_11
  1015 0000052E E86149                   call prs
  1016 00000531 83C402                   add  sp, 2
  1017                                  @@doglob33:
  1018                                  
  1019                                  ;-149       prL(lexvalL); if (NASM==0)prs(" dup (?)");
  1020                                  
  1021                                  @@doglob31:
  1022 00000534 66FF36[4D00]             push dword [lexvalL]
  1023 00000539 E8095A                   call prL
  1024 0000053C 83C404                   add  sp, 4
  1025 0000053F A0[1500]                 mov al, [NASM]
  1026 00000542 3C00                     cmp al, 0
  1027 00000544 0F850900                 jne @@doglob34
  1028 00000548 68[AD08]                 push doglob_12
  1029 0000054B E84449                   call prs
  1030 0000054E 83C402                   add  sp, 2
  1031                                  
  1032                                  ;-150       if (BSS) {if(NASM) prs("\nsection .text");
  1033                                  
  1034                                  @@doglob34:
  1035 00000551 A0[1400]                 mov al, [BSS]
  1036 00000554 08C0                     or  al, al
  1037 00000556 0F841E00                 je @@doglob35
  1038 0000055A A0[1500]                 mov al, [NASM]
  1039 0000055D 08C0                     or  al, al
  1040 0000055F 0F840C00                 je @@doglob36
  1041 00000563 68[B608]                 push doglob_13
  1042 00000566 E82949                   call prs
  1043 00000569 83C402                   add  sp, 2
  1044                                  
  1045                                  ;-151         else prs("\norg AData"); }
  1046                                  
  1047 0000056C E90900                   jmp @@doglob37
  1048                                  @@doglob36:
  1049 0000056F 68[C608]                 push doglob_14
  1050 00000572 E81D49                   call prs
  1051 00000575 83C402                   add  sp, 2
  1052                                  @@doglob37:
  1053                                  
  1054                                  ;-152       orgData=orgData+lexvalL;
  1055                                  
  1056                                  @@doglob35:
  1057 00000578 66A1[2800]               mov eax, [orgData]
  1058 0000057C 660306[4D00]             add eax, [lexvalL]
  1059 00000581 66A3[2800]               mov dword [orgData], eax
  1060                                  
  1061                                  ;-153       if (iswidth==2) orgData=orgData+lexvalL;
  1062                                  
  1063 00000585 A0[5D00]                 mov al, [iswidth]
  1064 00000588 3C02                     cmp al, 2
  1065 0000058A 0F850D00                 jne @@doglob38
  1066 0000058E 66A1[2800]               mov eax, [orgData]
  1067 00000592 660306[4D00]             add eax, [lexvalL]
  1068 00000597 66A3[2800]               mov dword [orgData], eax
  1069                                  
  1070                                  ;-154       if (iswidth==4) {i= lexvalL * 3; orgData=orgData + i;}
  1071                                  
  1072                                  @@doglob38:
  1073 0000059B A0[5D00]                 mov al, [iswidth]
  1074 0000059E 3C04                     cmp al, 4
  1075 000005A0 0F851B00                 jne @@doglob39
  1076 000005A4 66A1[4D00]               mov eax, [lexvalL]
  1077 000005A8 66BB03000000             mov ebx, 3
  1078 000005AE 66F7E3                   mul ebx
  1079 000005B1 8946FE                   mov word [bp-2], ax
  1080 000005B4 66A1[2800]               mov eax, [orgData]
  1081 000005B8 0346FE                   add ax, [bp-2]
  1082 000005BB 66A3[2800]               mov dword [orgData], eax
  1083                                  
  1084                                  ;-155       GData[GTop]=lexvalL; expect(']');
  1085                                  
  1086                                  @@doglob39:
  1087 000005BF 66A1[4D00]               mov eax, [lexvalL]
  1088 000005C3 8B1E[5E00]               mov bx, [GTop]
  1089 000005C7 C1E302                   shl bx, 2
  1090 000005CA 668987EB8A               mov [GData+bx], eax
  1091 000005CF 6A5D                     push 93
  1092 000005D1 E8A047                   call expect
  1093 000005D4 83C402                   add  sp, 2
  1094                                  
  1095                                  ;-156     }else { expect(']');
  1096                                  
  1097 000005D7 E91D01                   jmp @@doglob40
  1098                                  @@doglob21:
  1099 000005DA 6A5D                     push 93
  1100 000005DC E89547                   call expect
  1101 000005DF 83C402                   add  sp, 2
  1102                                  
  1103                                  ;-157       if (iswidth != 1) error1("Only ByteArray allowed");
  1104                                  
  1105 000005E2 A0[5D00]                 mov al, [iswidth]
  1106 000005E5 3C01                     cmp al, 1
  1107 000005E7 0F840900                 je  @@doglob41
  1108 000005EB 68[D208]                 push doglob_15
  1109 000005EE E8A24B                   call error1
  1110 000005F1 83C402                   add  sp, 2
  1111                                  
  1112                                  ;-158       prs("\n"); prs(symbol); prs(" db "); 
  1113                                  
  1114                                  @@doglob41:
  1115 000005F4 68[E908]                 push doglob_16
  1116 000005F7 E89848                   call prs
  1117 000005FA 83C402                   add  sp, 2
  1118 000005FD 8D066E81                 lea  ax, [symbol]
  1119 00000601 50                       push ax
  1120 00000602 E88D48                   call prs
  1121 00000605 83C402                   add  sp, 2
  1122 00000608 68[EC08]                 push doglob_17
  1123 0000060B E88448                   call prs
  1124 0000060E 83C402                   add  sp, 2
  1125                                  
  1126                                  ;-159       isstrarr=1; strcpy(doglobName, symbol);
  1127                                  
  1128 00000611 66B801000000             mov eax, 1
  1129 00000617 8946FA                   mov word [bp-6], ax
  1130 0000061A 8D066E81                 lea  ax, [symbol]
  1131 0000061E 50                       push ax
  1132 0000061F 8D06C7B6                 lea  ax, [doglobName]
  1133 00000623 50                       push ax
  1134 00000624 E8655B                   call strcpy
  1135 00000627 83C404                   add  sp, 4
  1136                                  
  1137                                  ;-160       expect('='); 
  1138                                  
  1139 0000062A 6A3D                     push 61
  1140 0000062C E84547                   call expect
  1141 0000062F 83C402                   add  sp, 2
  1142                                  
  1143                                  ;-161       if (istoken(T_STRING)) { 
  1144                                  
  1145 00000632 680201                   push 258
  1146 00000635 E81447                   call istoken
  1147 00000638 83C402                   add  sp, 2
  1148 0000063B 08C0                     or  al, al
  1149 0000063D 0F844400                 je @@doglob42
  1150                                  
  1151                                  ;-162         prc(34); prscomment(symbol); prc(34); prs(",0"); 
  1152                                  
  1153 00000641 6A22                     push 34
  1154 00000643 E8DD47                   call prc
  1155 00000646 83C402                   add  sp, 2
  1156 00000649 8D066E81                 lea  ax, [symbol]
  1157 0000064D 50                       push ax
  1158 0000064E E80548                   call prscomment
  1159 00000651 83C402                   add  sp, 2
  1160 00000654 6A22                     push 34
  1161 00000656 E8CA47                   call prc
  1162 00000659 83C402                   add  sp, 2
  1163 0000065C 68[F108]                 push doglob_18
  1164 0000065F E83048                   call prs
  1165 00000662 83C402                   add  sp, 2
  1166                                  
  1167                                  ;-163         i=strlen(symbol); GData[GTop]=i; }
  1168                                  
  1169 00000665 8D066E81                 lea  ax, [symbol]
  1170 00000669 50                       push ax
  1171 0000066A E8F45A                   call strlen
  1172 0000066D 83C402                   add  sp, 2
  1173 00000670 8946FE                   mov word [bp-2], ax
  1174 00000673 8B46FE                   mov ax, [bp-2]
  1175 00000676 8B1E[5E00]               mov bx, [GTop]
  1176 0000067A C1E302                   shl bx, 2
  1177 0000067D 668987EB8A               mov [GData+bx], eax
  1178                                  
  1179                                  ;-164       else if (istoken('{' )) { i=0;
  1180                                  
  1181 00000682 E97200                   jmp @@doglob43
  1182                                  @@doglob42:
  1183 00000685 6A7B                     push 123
  1184 00000687 E8C246                   call istoken
  1185 0000068A 83C402                   add  sp, 2
  1186 0000068D 08C0                     or  al, al
  1187 0000068F 0F845B00                 je @@doglob44
  1188 00000693 66B800000000             mov eax, 0
  1189 00000699 8946FE                   mov word [bp-2], ax
  1190                                  
  1191                                  ;-165         do { if(i) prc(','); 
  1192                                  
  1193                                  @@doglob45:
  1194 0000069C 8B46FE                   mov ax, [bp-2]
  1195 0000069F 08C0                     or  al, al
  1196 000006A1 0F840800                 je @@doglob46
  1197 000006A5 6A2C                     push 44
  1198 000006A7 E87947                   call prc
  1199 000006AA 83C402                   add  sp, 2
  1200                                  
  1201                                  ;-166           expect(T_CONST); prL(lexvalL); i++; }
  1202                                  
  1203                                  @@doglob46:
  1204 000006AD 680101                   push 257
  1205 000006B0 E8C146                   call expect
  1206 000006B3 83C402                   add  sp, 2
  1207 000006B6 66FF36[4D00]             push dword [lexvalL]
  1208 000006BB E88758                   call prL
  1209 000006BE 83C404                   add  sp, 4
  1210 000006C1 FF46FE                   inc  word [bp-2]
  1211                                  
  1212                                  ;-167         while (istoken(',')); expect('}'); GData[GTop]=i; }
  1213                                  
  1214 000006C4 6A2C                     push 44
  1215 000006C6 E88346                   call istoken
  1216 000006C9 83C402                   add  sp, 2
  1217 000006CC 08C0                     or  al, al
  1218 000006CE 0F840200                 je @@doglob47
  1219 000006D2 EBC8                     jmp @@doglob45
  1220                                  @@doglob47:
  1221 000006D4 6A7D                     push 125
  1222 000006D6 E89B46                   call expect
  1223 000006D9 83C402                   add  sp, 2
  1224 000006DC 8B46FE                   mov ax, [bp-2]
  1225 000006DF 8B1E[5E00]               mov bx, [GTop]
  1226 000006E3 C1E302                   shl bx, 2
  1227 000006E6 668987EB8A               mov [GData+bx], eax
  1228                                  
  1229                                  ;-168       else error1("String or number array expected");
  1230                                  
  1231 000006EB E90900                   jmp @@doglob48
  1232                                  @@doglob44:
  1233 000006EE 68[F408]                 push doglob_19
  1234 000006F1 E89F4A                   call error1
  1235 000006F4 83C402                   add  sp, 2
  1236                                  
  1237                                  ;-169       }; 
  1238                                  
  1239                                  @@doglob48:
  1240                                  @@doglob43:
  1241                                  @@doglob40:
  1242                                  
  1243                                  ;-170     } else { //expect('=');
  1244                                  
  1245 000006F7 E9A200                   jmp @@doglob49
  1246                                  @@doglob20:
  1247                                  
  1248                                  ;-171     prs("\n"); prs(symbol); if (istype=='*') prs(" dw ");
  1249                                  
  1250 000006FA 68[1409]                 push doglob_20
  1251 000006FD E89247                   call prs
  1252 00000700 83C402                   add  sp, 2
  1253 00000703 8D066E81                 lea  ax, [symbol]
  1254 00000707 50                       push ax
  1255 00000708 E88747                   call prs
  1256 0000070B 83C402                   add  sp, 2
  1257 0000070E A0[5500]                 mov al, [istype]
  1258 00000711 3C2A                     cmp al, 42
  1259 00000713 0F850C00                 jne @@doglob50
  1260 00000717 68[1709]                 push doglob_21
  1261 0000071A E87547                   call prs
  1262 0000071D 83C402                   add  sp, 2
  1263                                  
  1264                                  ;-172     else {if (iswidth==1) prs(" db ");
  1265                                  
  1266 00000720 E93600                   jmp @@doglob51
  1267                                  @@doglob50:
  1268 00000723 A0[5D00]                 mov al, [iswidth]
  1269 00000726 3C01                     cmp al, 1
  1270 00000728 0F850900                 jne @@doglob52
  1271 0000072C 68[1C09]                 push doglob_22
  1272 0000072F E86047                   call prs
  1273 00000732 83C402                   add  sp, 2
  1274                                  
  1275                                  ;-173           if (iswidth==2) prs(" dw ");
  1276                                  
  1277                                  @@doglob52:
  1278 00000735 A0[5D00]                 mov al, [iswidth]
  1279 00000738 3C02                     cmp al, 2
  1280 0000073A 0F850900                 jne @@doglob53
  1281 0000073E 68[2109]                 push doglob_23
  1282 00000741 E84E47                   call prs
  1283 00000744 83C402                   add  sp, 2
  1284                                  
  1285                                  ;-174           if (iswidth==4) prs(" dd ");  }
  1286                                  
  1287                                  @@doglob53:
  1288 00000747 A0[5D00]                 mov al, [iswidth]
  1289 0000074A 3C04                     cmp al, 4
  1290 0000074C 0F850900                 jne @@doglob54
  1291 00000750 68[2609]                 push doglob_24
  1292 00000753 E83C47                   call prs
  1293 00000756 83C402                   add  sp, 2
  1294                                  @@doglob54:
  1295                                  
  1296                                  ;-175     if(istoken('-')) prc('-');
  1297                                  
  1298                                  @@doglob51:
  1299 00000759 6A2D                     push 45
  1300 0000075B E8EE45                   call istoken
  1301 0000075E 83C402                   add  sp, 2
  1302 00000761 08C0                     or  al, al
  1303 00000763 0F840800                 je @@doglob55
  1304 00000767 6A2D                     push 45
  1305 00000769 E8B746                   call prc
  1306 0000076C 83C402                   add  sp, 2
  1307                                  
  1308                                  ;-176     if (istoken('=')) {expect(T_CONST); prL(lexvalL); }
  1309                                  
  1310                                  @@doglob55:
  1311 0000076F 6A3D                     push 61
  1312 00000771 E8D845                   call istoken
  1313 00000774 83C402                   add  sp, 2
  1314 00000777 08C0                     or  al, al
  1315 00000779 0F841700                 je @@doglob56
  1316 0000077D 680101                   push 257
  1317 00000780 E8F145                   call expect
  1318 00000783 83C402                   add  sp, 2
  1319 00000786 66FF36[4D00]             push dword [lexvalL]
  1320 0000078B E8B757                   call prL
  1321 0000078E 83C404                   add  sp, 4
  1322                                  
  1323                                  ;-177     else prunsign1(0); }
  1324                                  
  1325 00000791 E90800                   jmp @@doglob57
  1326                                  @@doglob56:
  1327 00000794 6A00                     push 0
  1328 00000796 E86248                   call prunsign1
  1329 00000799 83C402                   add  sp, 2
  1330                                  @@doglob57:
  1331                                  
  1332                                  ;-178   GSign[GTop]=issign; GWidth[GTop]=iswidth; GType[GTop]=istype;
  1333                                  
  1334                                  @@doglob49:
  1335 0000079C A0[5800]                 mov al, [issign]
  1336 0000079F 8B1E[5E00]               mov bx, [GTop]
  1337 000007A3 8887E383                 mov [GSign+bx], al
  1338 000007A7 A0[5D00]                 mov al, [iswidth]
  1339 000007AA 8B1E[5E00]               mov bx, [GTop]
  1340 000007AE 88870F85                 mov [GWidth+bx], al
  1341 000007B2 A0[5500]                 mov al, [istype]
  1342 000007B5 8B1E[5E00]               mov bx, [GTop]
  1343 000007B9 8887B782                 mov [GType+bx], al
  1344                                  
  1345                                  ;-179   GLino [GTop]=lineno-1; GUsed [GTop]=0;
  1346                                  
  1347 000007BD A1[BA00]                 mov ax, [lineno]
  1348 000007C0 83E801                   sub ax, 1
  1349 000007C3 8B1E[5E00]               mov bx, [GTop]
  1350 000007C7 D1E3                     shl bx, 1
  1351 000007C9 89873B86                 mov [GLino+bx], ax
  1352 000007CD 66B800000000             mov eax, 0
  1353 000007D3 8B1E[5E00]               mov bx, [GTop]
  1354 000007D7 D1E3                     shl bx, 1
  1355 000007D9 89879388                 mov [GUsed+bx], ax
  1356                                  
  1357                                  ;-180   if (isstrarr) strcpy(symbol, doglobName);
  1358                                  
  1359 000007DD 8B46FA                   mov ax, [bp-6]
  1360 000007E0 08C0                     or  al, al
  1361 000007E2 0F841000                 je @@doglob58
  1362 000007E6 8D06C7B6                 lea  ax, [doglobName]
  1363 000007EA 50                       push ax
  1364 000007EB 8D066E81                 lea  ax, [symbol]
  1365 000007EF 50                       push ax
  1366 000007F0 E89959                   call strcpy
  1367 000007F3 83C404                   add  sp, 4
  1368                                  
  1369                                  ;-181   GName[GTop] = CNameTop;
  1370                                  
  1371                                  @@doglob58:
  1372 000007F6 A1[B400]                 mov ax, [CNameTop]
  1373 000007F9 8B1E[5E00]               mov bx, [GTop]
  1374 000007FD D1E3                     shl bx, 1
  1375 000007FF 89874B94                 mov [GName+bx], ax
  1376                                  
  1377                                  ;-182   storeName();
  1378                                  
  1379 00000803 E8EAF9                   call storeName
  1380                                  
  1381                                  ;-183   GTop++; expect(';');
  1382                                  
  1383 00000806 FF06[5E00]               inc  word [GTop]
  1384 0000080A 6A3B                     push 59
  1385 0000080C E86545                   call expect
  1386 0000080F 83C402                   add  sp, 2
  1387                                  
  1388                                  ;-184 }
  1389                                  
  1390                                  
  1391                                  ;-185 int gettypes(int i) {int j; char c;
  1392 00000812 C9                       LEAVE
  1393 00000813 C3                       ret
  1394                                  ; ENDP
  1395 00000814 476C6F62616C207461-     doglob_0 db "Global table full",0
  1396 0000081D 626C652066756C6C00 
  1397 00000826 6E6F20564F49442061-     doglob_1 db "no VOID as var type",0
  1398 0000082F 732076617220747970-
  1399 00000838 6500               
  1400 0000083A 476C6F62616C207661-     doglob_2 db "Global variable already defined",0
  1401 00000843 726961626C6520616C-
  1402 0000084C 726561647920646566-
  1403 00000855 696E656400         
  1404 0000085A 5C6E73656374696F6E-     doglob_3 db "\nsection .bss\nabsolute ",0
  1405 00000863 202E6273735C6E6162-
  1406 0000086C 736F6C7574652000   
  1407 00000874 5C6E4144617461203D-     doglob_4 db "\nAData = $\norg ",0
  1408 0000087D 20245C6E6F72672000 
  1409 00000886 5C6E00                  doglob_5 db "\n",0
  1410 00000889 20726573622000          doglob_6 db " resb ",0
  1411 00000890 2064622000              doglob_7 db " db ",0
  1412 00000895 20726573772000          doglob_8 db " resw ",0
  1413 0000089C 2064772000              doglob_9 db " dw ",0
  1414 000008A1 20726573642000          doglob_10 db " resd ",0
  1415 000008A8 2064642000              doglob_11 db " dd ",0
  1416 000008AD 2064757020283F2900      doglob_12 db " dup (?)",0
  1417 000008B6 5C6E73656374696F6E-     doglob_13 db "\nsection .text",0
  1418 000008BF 202E7465787400     
  1419 000008C6 5C6E6F726720414461-     doglob_14 db "\norg AData",0
  1420 000008CF 746100             
  1421 000008D2 4F6E6C792042797465-     doglob_15 db "Only ByteArray allowed",0
  1422 000008DB 417272617920616C6C-
  1423 000008E4 6F77656400         
  1424 000008E9 5C6E00                  doglob_16 db "\n",0
  1425 000008EC 2064622000              doglob_17 db " db ",0
  1426 000008F1 2C3000                  doglob_18 db ",0",0
  1427 000008F4 537472696E67206F72-     doglob_19 db "String or number array expected",0
  1428 000008FD 206E756D6265722061-
  1429 00000906 727261792065787065-
  1430 0000090F 6374656400         
  1431 00000914 5C6E00                  doglob_20 db "\n",0
  1432 00000917 2064772000              doglob_21 db " dw ",0
  1433 0000091C 2064622000              doglob_22 db " db ",0
  1434 00000921 2064772000              doglob_23 db " dw ",0
  1435 00000926 2064642000              doglob_24 db " dd ",0
  1436                                  
  1437                                  gettypes:  ; *** PROC ***
  1438                                  
  1439                                  ;-186   c=GSign [i]; if (c=='S') signi =1;    else signi =0;
  1440                                  
  1441                                  ;Function : gettypes, Number of local variables: 3
  1442                                  ;   # type sign width line used address name   list of local variables
  1443                                  ;  200 var sign word   185 NULL bp+4    i
  1444                                  ;  201 var sign word   185 NULL bp-2    j
  1445                                  ;  202 var sign byte   185 NULL bp-4    c;
  1446 0000092B C8040000                 ENTER  4,0
  1447 0000092F 8B5E04                   mov bx, [bp+4]
  1448 00000932 8A87E383                 mov al, [GSign + bx]
  1449 00000936 8846FC                   mov byte [bp-4], al
  1450 00000939 8A46FC                   mov al, [bp-4]
  1451 0000093C 3C53                     cmp al, 83
  1452 0000093E 0F850C00                 jne @@gettypes59
  1453 00000942 66B801000000             mov eax, 1
  1454 00000948 A3[5600]                 mov word [signi], ax
  1455 0000094B E90900                   jmp @@gettypes60
  1456                                  @@gettypes59:
  1457 0000094E 66B800000000             mov eax, 0
  1458 00000954 A3[5600]                 mov word [signi], ax
  1459                                  
  1460                                  ;-187   c=GWidth[i]; widthi=0;wi=0;
  1461                                  
  1462                                  @@gettypes60:
  1463 00000957 8B5E04                   mov bx, [bp+4]
  1464 0000095A 8A870F85                 mov al, [GWidth + bx]
  1465 0000095E 8846FC                   mov byte [bp-4], al
  1466 00000961 66B800000000             mov eax, 0
  1467 00000967 A3[5900]                 mov word [widthi], ax
  1468 0000096A 66B800000000             mov eax, 0
  1469 00000970 A3[5B00]                 mov word [wi], ax
  1470                                  
  1471                                  ;-188   if (c==1) {widthi=1;wi=1;}
  1472                                  
  1473 00000973 8A46FC                   mov al, [bp-4]
  1474 00000976 3C01                     cmp al, 1
  1475 00000978 0F851200                 jne @@gettypes61
  1476 0000097C 66B801000000             mov eax, 1
  1477 00000982 A3[5900]                 mov word [widthi], ax
  1478 00000985 66B801000000             mov eax, 1
  1479 0000098B A3[5B00]                 mov word [wi], ax
  1480                                  
  1481                                  ;-189   if (c==2) {widthi=2;wi=2;}
  1482                                  
  1483                                  @@gettypes61:
  1484 0000098E 8A46FC                   mov al, [bp-4]
  1485 00000991 3C02                     cmp al, 2
  1486 00000993 0F851200                 jne @@gettypes62
  1487 00000997 66B802000000             mov eax, 2
  1488 0000099D A3[5900]                 mov word [widthi], ax
  1489 000009A0 66B802000000             mov eax, 2
  1490 000009A6 A3[5B00]                 mov word [wi], ax
  1491                                  
  1492                                  ;-190   if (c==4) {widthi=4;wi=4;}
  1493                                  
  1494                                  @@gettypes62:
  1495 000009A9 8A46FC                   mov al, [bp-4]
  1496 000009AC 3C04                     cmp al, 4
  1497 000009AE 0F851200                 jne @@gettypes63
  1498 000009B2 66B804000000             mov eax, 4
  1499 000009B8 A3[5900]                 mov word [widthi], ax
  1500 000009BB 66B804000000             mov eax, 4
  1501 000009C1 A3[5B00]                 mov word [wi], ax
  1502                                  
  1503                                  ;-191   c=GType [i]; typei=0; if (c=='*') {typei=1;wi=2;}
  1504                                  
  1505                                  @@gettypes63:
  1506 000009C4 8B5E04                   mov bx, [bp+4]
  1507 000009C7 8A87B782                 mov al, [GType + bx]
  1508 000009CB 8846FC                   mov byte [bp-4], al
  1509 000009CE 66B800000000             mov eax, 0
  1510 000009D4 A3[5300]                 mov word [typei], ax
  1511 000009D7 8A46FC                   mov al, [bp-4]
  1512 000009DA 3C2A                     cmp al, 42
  1513 000009DC 0F851200                 jne @@gettypes64
  1514 000009E0 66B801000000             mov eax, 1
  1515 000009E6 A3[5300]                 mov word [typei], ax
  1516 000009E9 66B802000000             mov eax, 2
  1517 000009EF A3[5B00]                 mov word [wi], ax
  1518                                  
  1519                                  ;-192   if (c=='&')  typei=2;  
  1520                                  
  1521                                  @@gettypes64:
  1522 000009F2 8A46FC                   mov al, [bp-4]
  1523 000009F5 3C26                     cmp al, 38
  1524 000009F7 0F850900                 jne @@gettypes65
  1525 000009FB 66B802000000             mov eax, 2
  1526 00000A01 A3[5300]                 mov word [typei], ax
  1527                                  
  1528                                  ;-193   return i; }  
  1529                                  
  1530                                  @@gettypes65:
  1531 00000A04 8B4604                   mov ax, [bp+4]
  1532 00000A07 E90000                   jmp @@retn
  1533                                  
  1534                                  ;-194 
  1535                                  
  1536                                  
  1537                                  ;-195 void printName(unsigned int i) {int j;
  1538                                  
  1539 00000A0A C9                      @@retn: LEAVE
  1540 00000A0B C3                       ret
  1541                                  ; ENDP
  1542                                  
  1543                                  printName:  ; *** PROC ***
  1544                                  
  1545                                  ;-196   if (i < LSTART) { loadName(NA, i); prs(NA); }
  1546                                  
  1547                                  ;Function : printName, Number of local variables: 2
  1548                                  ;   # type sign width line used address name   list of local variables
  1549                                  ;  200 var unsg word   195 NULL bp+4    i
  1550                                  ;  201 var sign word   195 NULL bp-2    j;
  1551 00000A0C C8020000                 ENTER  2,0
  1552 00000A10 8B4604                   mov ax, [bp+4]
  1553 00000A13 3DC800                   cmp ax, 200
  1554 00000A16 0F8D1C00                 jge @@printName66
  1555 00000A1A FF7604                   push word [bp+4]
  1556 00000A1D 8D06[7200]               lea  ax, [NA]
  1557 00000A21 50                       push ax
  1558 00000A22 E81C54                   call loadName
  1559 00000A25 83C404                   add  sp, 4
  1560 00000A28 8D06[7200]               lea  ax, [NA]
  1561 00000A2C 50                       push ax
  1562 00000A2D E86244                   call prs
  1563 00000A30 83C402                   add  sp, 2
  1564                                  
  1565                                  ;-197   else { prs("[bp"); j = GData[i]; if (j>0) prc('+'); pint1(j); prc(']'); }
  1566                                  
  1567 00000A33 E93A00                   jmp @@printName67
  1568                                  @@printName66:
  1569 00000A36 68[720A]                 push printName_0
  1570 00000A39 E85644                   call prs
  1571 00000A3C 83C402                   add  sp, 2
  1572 00000A3F 8B5E04                   mov bx, [bp+4]
  1573 00000A42 C1E302                   shl bx, 2
  1574 00000A45 668B87EB8A               mov eax, [GData + bx]
  1575 00000A4A 8946FE                   mov word [bp-2], ax
  1576 00000A4D 8B46FE                   mov ax, [bp-2]
  1577 00000A50 83F800                   cmp ax, 0
  1578 00000A53 0F8E0800                 jle @@printName68
  1579 00000A57 6A2B                     push 43
  1580 00000A59 E8C743                   call prc
  1581 00000A5C 83C402                   add  sp, 2
  1582                                  @@printName68:
  1583 00000A5F FF76FE                   push word [bp-2]
  1584 00000A62 E83345                   call pint1
  1585 00000A65 83C402                   add  sp, 2
  1586 00000A68 6A5D                     push 93
  1587 00000A6A E8B643                   call prc
  1588 00000A6D 83C402                   add  sp, 2
  1589                                  
  1590                                  ;-198 }
  1591                                  
  1592                                  @@printName67:
  1593                                  
  1594                                  ;-199 int searchname() { unsigned int i;
  1595 00000A70 C9                       LEAVE
  1596 00000A71 C3                       ret
  1597                                  ; ENDP
  1598 00000A72 5B627000                printName_0 db "[bp",0
  1599                                  
  1600                                  searchname:  ; *** PROC ***
  1601                                  
  1602                                  ;-200   i=checkName(); if (i == 0) error1("Variable unknown");
  1603                                  
  1604                                  ;Function : searchname, Number of local variables: 1
  1605                                  ;   # type sign width line used address name   list of local variables
  1606                                  ;  200 var unsg word   199 NULL bp-2    i;
  1607 00000A76 C8020000                 ENTER  2,0
  1608 00000A7A E84400                   call checkName
  1609 00000A7D 8946FE                   mov word [bp-2], ax
  1610 00000A80 8B46FE                   mov ax, [bp-2]
  1611 00000A83 83F800                   cmp ax, 0
  1612 00000A86 0F850900                 jne @@searchname69
  1613 00000A8A 68[B00A]                 push searchname_0
  1614 00000A8D E80347                   call error1
  1615 00000A90 83C402                   add  sp, 2
  1616                                  
  1617                                  ;-201   GUsed[i]=GUsed[i] + 1; return i;
  1618                                  
  1619                                  @@searchname69:
  1620 00000A93 8B5EFE                   mov bx, [bp-2]
  1621 00000A96 D1E3                     shl bx, 1
  1622 00000A98 8B879388                 mov ax, [GUsed + bx]
  1623 00000A9C 83C001                   add ax, 1
  1624 00000A9F 8B5EFE                   mov bx, [bp-2]
  1625 00000AA2 D1E3                     shl bx, 1
  1626 00000AA4 89879388                 mov [GUsed+bx], ax
  1627 00000AA8 8B46FE                   mov ax, [bp-2]
  1628 00000AAB E90000                   jmp @@retn
  1629                                  
  1630                                  ;-202 }
  1631                                  
  1632                                  
  1633                                  ;-203 int checkName() { unsigned int i; unsigned int j;
  1634                                  
  1635 00000AAE C9                      @@retn: LEAVE
  1636 00000AAF C3                       ret
  1637                                  ; ENDP
  1638 00000AB0 5661726961626C6520-     searchname_0 db "Variable unknown",0
  1639 00000AB9 756E6B6E6F776E00   
  1640                                  
  1641                                  checkName:  ; *** PROC ***
  1642                                  
  1643                                  ;-204   i=LSTART;while(i<LTop){loadName(NA,i);if(eqstr(symbol,NA))return i;i++;}
  1644                                  
  1645                                  ;Function : checkName, Number of local variables: 2
  1646                                  ;   # type sign width line used address name   list of local variables
  1647                                  ;  200 var unsg word   203 NULL bp-2    i
  1648                                  ;  201 var unsg word   203 NULL bp-4    j;
  1649 00000AC1 C8040000                 ENTER  4,0
  1650 00000AC5 66B8C8000000             mov eax, 200
  1651 00000ACB 8946FE                   mov word [bp-2], ax
  1652                                  @@checkName70:
  1653 00000ACE 8B46FE                   mov ax, [bp-2]
  1654 00000AD1 3B06[6000]               cmp ax, [LTop]
  1655 00000AD5 0F8D2F00                 jge @@checkName71
  1656 00000AD9 FF76FE                   push word [bp-2]
  1657 00000ADC 8D06[7200]               lea  ax, [NA]
  1658 00000AE0 50                       push ax
  1659 00000AE1 E85D53                   call loadName
  1660 00000AE4 83C404                   add  sp, 4
  1661 00000AE7 8D06[7200]               lea  ax, [NA]
  1662 00000AEB 50                       push ax
  1663 00000AEC 8D066E81                 lea  ax, [symbol]
  1664 00000AF0 50                       push ax
  1665 00000AF1 E8EC57                   call eqstr
  1666 00000AF4 83C404                   add  sp, 4
  1667 00000AF7 08C0                     or  al, al
  1668 00000AF9 0F840600                 je @@checkName72
  1669 00000AFD 8B46FE                   mov ax, [bp-2]
  1670 00000B00 E95100                   jmp @@retn
  1671                                  @@checkName72:
  1672 00000B03 FF46FE                   inc  word [bp-2]
  1673                                  
  1674                                  ;-205   i=1;     while(i<GTop){loadName(NA,i);if(eqstr(symbol,NA))return i;i++;}
  1675                                  
  1676 00000B06 EBC6                     jmp @@checkName70
  1677                                  @@checkName71:
  1678 00000B08 66B801000000             mov eax, 1
  1679 00000B0E 8946FE                   mov word [bp-2], ax
  1680                                  @@checkName73:
  1681 00000B11 8B46FE                   mov ax, [bp-2]
  1682 00000B14 3B06[5E00]               cmp ax, [GTop]
  1683 00000B18 0F8D2F00                 jge @@checkName74
  1684 00000B1C FF76FE                   push word [bp-2]
  1685 00000B1F 8D06[7200]               lea  ax, [NA]
  1686 00000B23 50                       push ax
  1687 00000B24 E81A53                   call loadName
  1688 00000B27 83C404                   add  sp, 4
  1689 00000B2A 8D06[7200]               lea  ax, [NA]
  1690 00000B2E 50                       push ax
  1691 00000B2F 8D066E81                 lea  ax, [symbol]
  1692 00000B33 50                       push ax
  1693 00000B34 E8A957                   call eqstr
  1694 00000B37 83C404                   add  sp, 4
  1695 00000B3A 08C0                     or  al, al
  1696 00000B3C 0F840600                 je @@checkName75
  1697 00000B40 8B46FE                   mov ax, [bp-2]
  1698 00000B43 E90E00                   jmp @@retn
  1699                                  @@checkName75:
  1700 00000B46 FF46FE                   inc  word [bp-2]
  1701                                  
  1702                                  ;-206   return 0;
  1703                                  
  1704 00000B49 EBC6                     jmp @@checkName73
  1705                                  @@checkName74:
  1706 00000B4B 66B800000000             mov eax, 0
  1707 00000B51 E90000                   jmp @@retn
  1708                                  
  1709                                  ;-207 }    
  1710                                  
  1711                                  
  1712                                  ;-208 int typeName() { int m; //0=V,1=*,2=&
  1713                                  
  1714 00000B54 C9                      @@retn: LEAVE
  1715 00000B55 C3                       ret
  1716                                  ; ENDP
  1717                                  
  1718                                  typeName:  ; *** PROC ***
  1719                                  
  1720                                  ;-209   issign='S';
  1721                                  
  1722                                  ;Function : typeName, Number of local variables: 1
  1723                                  ;   # type sign width line used address name   list of local variables
  1724                                  ;  200 var sign word   208 NULL bp-2    m;
  1725 00000B56 C8020000                 ENTER  2,0
  1726 00000B5A 66B853000000             mov eax, 83
  1727 00000B60 A2[5800]                 mov byte [issign], al
  1728                                  
  1729                                  ;-210   if(istoken(T_SIGNED))   issign='S';  if(istoken(T_UNSIGNED)) issign='U';
  1730                                  
  1731 00000B63 681302                   push 531
  1732 00000B66 E8E341                   call istoken
  1733 00000B69 83C402                   add  sp, 2
  1734 00000B6C 08C0                     or  al, al
  1735 00000B6E 0F840900                 je @@typeName76
  1736 00000B72 66B853000000             mov eax, 83
  1737 00000B78 A2[5800]                 mov byte [issign], al
  1738                                  @@typeName76:
  1739 00000B7B 681402                   push 532
  1740 00000B7E E8CB41                   call istoken
  1741 00000B81 83C402                   add  sp, 2
  1742 00000B84 08C0                     or  al, al
  1743 00000B86 0F840900                 je @@typeName77
  1744 00000B8A 66B855000000             mov eax, 85
  1745 00000B90 A2[5800]                 mov byte [issign], al
  1746                                  
  1747                                  ;-211   iswidth=2;                           if(istoken(T_VOID))     iswidth=0;
  1748                                  
  1749                                  @@typeName77:
  1750 00000B93 66B802000000             mov eax, 2
  1751 00000B99 A2[5D00]                 mov byte [iswidth], al
  1752 00000B9C 681102                   push 529
  1753 00000B9F E8AA41                   call istoken
  1754 00000BA2 83C402                   add  sp, 2
  1755 00000BA5 08C0                     or  al, al
  1756 00000BA7 0F840900                 je @@typeName78
  1757 00000BAB 66B800000000             mov eax, 0
  1758 00000BB1 A2[5D00]                 mov byte [iswidth], al
  1759                                  
  1760                                  ;-212   if(istoken(T_CHAR))     iswidth=1;   if(istoken(T_INT))      iswidth=2;
  1761                                  
  1762                                  @@typeName78:
  1763 00000BB4 681202                   push 530
  1764 00000BB7 E89241                   call istoken
  1765 00000BBA 83C402                   add  sp, 2
  1766 00000BBD 08C0                     or  al, al
  1767 00000BBF 0F840900                 je @@typeName79
  1768 00000BC3 66B801000000             mov eax, 1
  1769 00000BC9 A2[5D00]                 mov byte [iswidth], al
  1770                                  @@typeName79:
  1771 00000BCC 680502                   push 517
  1772 00000BCF E87A41                   call istoken
  1773 00000BD2 83C402                   add  sp, 2
  1774 00000BD5 08C0                     or  al, al
  1775 00000BD7 0F840900                 je @@typeName80
  1776 00000BDB 66B802000000             mov eax, 2
  1777 00000BE1 A2[5D00]                 mov byte [iswidth], al
  1778                                  
  1779                                  ;-213   if(istoken(T_SHORT))    iswidth=2;   if(istoken(T_LONG))     iswidth=4;
  1780                                  
  1781                                  @@typeName80:
  1782 00000BE4 681602                   push 534
  1783 00000BE7 E86241                   call istoken
  1784 00000BEA 83C402                   add  sp, 2
  1785 00000BED 08C0                     or  al, al
  1786 00000BEF 0F840900                 je @@typeName81
  1787 00000BF3 66B802000000             mov eax, 2
  1788 00000BF9 A2[5D00]                 mov byte [iswidth], al
  1789                                  @@typeName81:
  1790 00000BFC 681502                   push 533
  1791 00000BFF E84A41                   call istoken
  1792 00000C02 83C402                   add  sp, 2
  1793 00000C05 08C0                     or  al, al
  1794 00000C07 0F840900                 je @@typeName82
  1795 00000C0B 66B804000000             mov eax, 4
  1796 00000C11 A2[5D00]                 mov byte [iswidth], al
  1797                                  
  1798                                  ;-214   if(istoken(T_UINT32)) { iswidth=4; issign='U'; }
  1799                                  
  1800                                  @@typeName82:
  1801 00000C14 682102                   push 545
  1802 00000C17 E83241                   call istoken
  1803 00000C1A 83C402                   add  sp, 2
  1804 00000C1D 08C0                     or  al, al
  1805 00000C1F 0F841200                 je @@typeName83
  1806 00000C23 66B804000000             mov eax, 4
  1807 00000C29 A2[5D00]                 mov byte [iswidth], al
  1808 00000C2C 66B855000000             mov eax, 85
  1809 00000C32 A2[5800]                 mov byte [issign], al
  1810                                  
  1811                                  ;-215   istype='V'; m=0;
  1812                                  
  1813                                  @@typeName83:
  1814 00000C35 66B856000000             mov eax, 86
  1815 00000C3B A2[5500]                 mov byte [istype], al
  1816 00000C3E 66B800000000             mov eax, 0
  1817 00000C44 8946FE                   mov word [bp-2], ax
  1818                                  
  1819                                  ;-216   if(istoken('*'))  { istype='*'; m=1; } if(istoken('&'))  {istype='&'; m=2;}
  1820                                  
  1821 00000C47 6A2A                     push 42
  1822 00000C49 E80041                   call istoken
  1823 00000C4C 83C402                   add  sp, 2
  1824 00000C4F 08C0                     or  al, al
  1825 00000C51 0F841200                 je @@typeName84
  1826 00000C55 66B82A000000             mov eax, 42
  1827 00000C5B A2[5500]                 mov byte [istype], al
  1828 00000C5E 66B801000000             mov eax, 1
  1829 00000C64 8946FE                   mov word [bp-2], ax
  1830                                  @@typeName84:
  1831 00000C67 6A26                     push 38
  1832 00000C69 E8E040                   call istoken
  1833 00000C6C 83C402                   add  sp, 2
  1834 00000C6F 08C0                     or  al, al
  1835 00000C71 0F841200                 je @@typeName85
  1836 00000C75 66B826000000             mov eax, 38
  1837 00000C7B A2[5500]                 mov byte [istype], al
  1838 00000C7E 66B802000000             mov eax, 2
  1839 00000C84 8946FE                   mov word [bp-2], ax
  1840                                  
  1841                                  ;-217   name1(); return m; }
  1842                                  
  1843                                  @@typeName85:
  1844 00000C87 E80800                   call name1
  1845 00000C8A 8B46FE                   mov ax, [bp-2]
  1846 00000C8D E90000                   jmp @@retn
  1847                                  
  1848                                  ;-218 void name1() {if (token!=T_NAME) error1("Name expected"); token=getlex(); }
  1849                                  
  1850 00000C90 C9                      @@retn: LEAVE
  1851 00000C91 C3                       ret
  1852                                  ; ENDP
  1853                                  
  1854                                  name1:  ; *** PROC ***
  1855 00000C92 A1[3D00]                 mov ax, [token]
  1856 00000C95 3D0001                   cmp ax, 256
  1857 00000C98 0F840900                 je  @@name186
  1858 00000C9C 68[AC0C]                 push name1_0
  1859 00000C9F E8F144                   call error1
  1860 00000CA2 83C402                   add  sp, 2
  1861                                  @@name186:
  1862 00000CA5 E80438                   call getlex
  1863 00000CA8 A3[3D00]                 mov word [token], ax
  1864                                  
  1865                                  ;-219 
  1866                                  
  1867                                  
  1868                                  ;-220 void storecall() { int i;
  1869                                  
  1870 00000CAB C3                       ret
  1871                                  ; ENDP
  1872 00000CAC 4E616D652065787065-     name1_0 db "Name expected",0
  1873 00000CB5 6374656400         
  1874                                  
  1875                                  storecall:  ; *** PROC ***
  1876                                  
  1877                                  ;-221   if (CTop >= CALLMAX) error1("Call table full");
  1878                                  
  1879                                  ;Function : storecall, Number of local variables: 1
  1880                                  ;   # type sign width line used address name   list of local variables
  1881                                  ;  200 var sign word   220 NULL bp-2    i;
  1882 00000CBA C8020000                 ENTER  2,0
  1883 00000CBE A1[6400]                 mov ax, [CTop]
  1884 00000CC1 3DD007                   cmp ax, 2000 ;unsigned : 1
  1885 00000CC4 0F8C0900                 jl  @@storecall87
  1886 00000CC8 68[080D]                 push storecall_0
  1887 00000CCB E8C544                   call error1
  1888 00000CCE 83C402                   add  sp, 2
  1889                                  
  1890                                  ;-222   if (CNameTop >= 65468) error1("Call name table fuill");
  1891                                  
  1892                                  @@storecall87:
  1893 00000CD1 A1[B400]                 mov ax, [CNameTop]
  1894 00000CD4 3DBCFF                   cmp ax, 65468 ;unsigned : 0
  1895 00000CD7 0F820900                 jb  @@storecall88
  1896 00000CDB 68[180D]                 push storecall_1
  1897 00000CDE E8B244                   call error1
  1898 00000CE1 83C402                   add  sp, 2
  1899                                  
  1900                                  ;-223   CUnres[CTop]=0;  CName [CTop]=CNameTop;
  1901                                  
  1902                                  @@storecall88:
  1903 00000CE4 66B800000000             mov eax, 0
  1904 00000CEA 8B1E[6400]               mov bx, [CTop]
  1905 00000CEE 888777AE                 mov [CUnres+bx], al
  1906 00000CF2 A1[B400]                 mov ax, [CNameTop]
  1907 00000CF5 8B1E[6400]               mov bx, [CTop]
  1908 00000CF9 D1E3                     shl bx, 1
  1909 00000CFB 8987D79E                 mov [CName+bx], ax
  1910                                  
  1911                                  ;-224   storeName();
  1912                                  
  1913 00000CFF E8EEF4                   call storeName
  1914                                  
  1915                                  ;-225   CTop++;
  1916                                  
  1917 00000D02 FF06[6400]               inc  word [CTop]
  1918                                  
  1919                                  ;-226 }
  1920                                  
  1921                                  
  1922                                  ;-227 void storefunc() { int i;
  1923 00000D06 C9                       LEAVE
  1924 00000D07 C3                       ret
  1925                                  ; ENDP
  1926 00000D08 43616C6C207461626C-     storecall_0 db "Call table full",0
  1927 00000D11 652066756C6C00     
  1928 00000D18 43616C6C206E616D65-     storecall_1 db "Call name table fuill",0
  1929 00000D21 207461626C65206675-
  1930 00000D2A 696C6C00           
  1931                                  
  1932                                  storefunc:  ; *** PROC ***
  1933                                  
  1934                                  ;-228   if (FTop >= FUNCMAX) error1("Function table full");
  1935                                  
  1936                                  ;Function : storefunc, Number of local variables: 1
  1937                                  ;   # type sign width line used address name   list of local variables
  1938                                  ;  200 var sign word   227 NULL bp-2    i;
  1939 00000D2E C8020000                 ENTER  2,0
  1940 00000D32 A1[6200]                 mov ax, [FTop]
  1941 00000D35 3D2C01                   cmp ax, 300 ;unsigned : 1
  1942 00000D38 0F8C0900                 jl  @@storefunc89
  1943 00000D3C 68[860D]                 push storefunc_0
  1944 00000D3F E85144                   call error1
  1945 00000D42 83C402                   add  sp, 2
  1946                                  
  1947                                  ;-229   FLino[FTop]=lineno - 1;  FCalls[FTop]=0;   FWidth[FTop]=iswidth;
  1948                                  
  1949                                  @@storefunc89:
  1950 00000D45 A1[BA00]                 mov ax, [lineno]
  1951 00000D48 83E801                   sub ax, 1
  1952 00000D4B 8B1E[6200]               mov bx, [FTop]
  1953 00000D4F D1E3                     shl bx, 1
  1954 00000D51 8987CF97                 mov [FLino+bx], ax
  1955 00000D55 66B800000000             mov eax, 0
  1956 00000D5B 8B1E[6200]               mov bx, [FTop]
  1957 00000D5F D1E3                     shl bx, 1
  1958 00000D61 8987279A                 mov [FCalls+bx], ax
  1959 00000D65 A0[5D00]                 mov al, [iswidth]
  1960 00000D68 8B1E[6200]               mov bx, [FTop]
  1961 00000D6C 8887A396                 mov [FWidth+bx], al
  1962                                  
  1963                                  ;-230   FName[FTop]=CNameTop;
  1964                                  
  1965 00000D70 A1[B400]                 mov ax, [CNameTop]
  1966 00000D73 8B1E[6200]               mov bx, [FTop]
  1967 00000D77 D1E3                     shl bx, 1
  1968 00000D79 89877F9C                 mov [FName+bx], ax
  1969                                  
  1970                                  ;-231   storeName();
  1971                                  
  1972 00000D7D E870F4                   call storeName
  1973                                  
  1974                                  ;-232   FTop++;
  1975                                  
  1976 00000D80 FF06[6200]               inc  word [FTop]
  1977                                  
  1978                                  ;-233 }
  1979                                  
  1980                                  
  1981                                  ;-234 void addlocal() { if(LTop >= VARMAX) error1("Local variable table full");
  1982 00000D84 C9                       LEAVE
  1983 00000D85 C3                       ret
  1984                                  ; ENDP
  1985 00000D86 46756E6374696F6E20-     storefunc_0 db "Function table full",0
  1986 00000D8F 7461626C652066756C-
  1987 00000D98 6C00               
  1988                                  
  1989                                  addlocal:  ; *** PROC ***
  1990 00000D9A A1[6000]                 mov ax, [LTop]
  1991 00000D9D 3D2C01                   cmp ax, 300 ;unsigned : 1
  1992 00000DA0 0F8C0900                 jl  @@addlocal90
  1993 00000DA4 68[120E]                 push addlocal_0
  1994 00000DA7 E8E943                   call error1
  1995 00000DAA 83C402                   add  sp, 2
  1996                                  
  1997                                  ;-235   if (checkName() != 0) error1("Local variable already defined");
  1998                                  
  1999                                  @@addlocal90:
  2000 00000DAD E811FD                   call checkName
  2001 00000DB0 83F800                   cmp ax, 0
  2002 00000DB3 0F840900                 je  @@addlocal91
  2003 00000DB7 68[2C0E]                 push addlocal_1
  2004 00000DBA E8D643                   call error1
  2005 00000DBD 83C402                   add  sp, 2
  2006                                  
  2007                                  ;-236   GSign[LTop]=issign; GWidth[LTop]=iswidth; GType[LTop]=istype;
  2008                                  
  2009                                  @@addlocal91:
  2010 00000DC0 A0[5800]                 mov al, [issign]
  2011 00000DC3 8B1E[6000]               mov bx, [LTop]
  2012 00000DC7 8887E383                 mov [GSign+bx], al
  2013 00000DCB A0[5D00]                 mov al, [iswidth]
  2014 00000DCE 8B1E[6000]               mov bx, [LTop]
  2015 00000DD2 88870F85                 mov [GWidth+bx], al
  2016 00000DD6 A0[5500]                 mov al, [istype]
  2017 00000DD9 8B1E[6000]               mov bx, [LTop]
  2018 00000DDD 8887B782                 mov [GType+bx], al
  2019                                  
  2020                                  ;-237   GLino [LTop]=lineno-1; GUsed[LTop]=0;
  2021                                  
  2022 00000DE1 A1[BA00]                 mov ax, [lineno]
  2023 00000DE4 83E801                   sub ax, 1
  2024 00000DE7 8B1E[6000]               mov bx, [LTop]
  2025 00000DEB D1E3                     shl bx, 1
  2026 00000DED 89873B86                 mov [GLino+bx], ax
  2027 00000DF1 66B800000000             mov eax, 0
  2028 00000DF7 8B1E[6000]               mov bx, [LTop]
  2029 00000DFB D1E3                     shl bx, 1
  2030 00000DFD 89879388                 mov [GUsed+bx], ax
  2031                                  
  2032                                  ;-238   GName[LTop]=CNameTop;
  2033                                  
  2034 00000E01 A1[B400]                 mov ax, [CNameTop]
  2035 00000E04 8B1E[6000]               mov bx, [LTop]
  2036 00000E08 D1E3                     shl bx, 1
  2037 00000E0A 89874B94                 mov [GName+bx], ax
  2038                                  
  2039                                  ;-239   storeName();
  2040                                  
  2041 00000E0E E8DFF3                   call storeName
  2042                                  
  2043                                  ;-240 }
  2044                                  
  2045                                  
  2046                                  ;-241 int checkFunction() { unsigned int i; unsigned int j; i=0;
  2047                                  
  2048 00000E11 C3                       ret
  2049                                  ; ENDP
  2050 00000E12 4C6F63616C20766172-     addlocal_0 db "Local variable table full",0
  2051 00000E1B 6961626C6520746162-
  2052 00000E24 6C652066756C6C00   
  2053 00000E2C 4C6F63616C20766172-     addlocal_1 db "Local variable already defined",0
  2054 00000E35 6961626C6520616C72-
  2055 00000E3E 656164792064656669-
  2056 00000E47 6E656400           
  2057                                  
  2058                                  checkFunction:  ; *** PROC ***
  2059                                  ;Function : checkFunction, Number of local variables: 2
  2060                                  ;   # type sign width line used address name   list of local variables
  2061                                  ;  200 var unsg word   241 NULL bp-2    i
  2062                                  ;  201 var unsg word   241 NULL bp-4    j;
  2063 00000E4B C8040000                 ENTER  4,0
  2064 00000E4F 66B800000000             mov eax, 0
  2065 00000E55 8946FE                   mov word [bp-2], ax
  2066                                  
  2067                                  ;-242   while (i < FTop) {
  2068                                  
  2069                                  @@checkFunction92:
  2070 00000E58 8B46FE                   mov ax, [bp-2]
  2071 00000E5B 3B06[6200]               cmp ax, [FTop]
  2072 00000E5F 0F8D3B00                 jge @@checkFunction93
  2073                                  
  2074                                  ;-243     j=FName[i]; from_far(NA, j);
  2075                                  
  2076 00000E63 8B5EFE                   mov bx, [bp-2]
  2077 00000E66 D1E3                     shl bx, 1
  2078 00000E68 8B877F9C                 mov ax, [FName + bx]
  2079 00000E6C 8946FC                   mov word [bp-4], ax
  2080 00000E6F FF76FC                   push word [bp-4]
  2081 00000E72 8D06[7200]               lea  ax, [NA]
  2082 00000E76 50                       push ax
  2083 00000E77 E80F50                   call from_far
  2084 00000E7A 83C404                   add  sp, 4
  2085                                  
  2086                                  ;-244     if(eqstr(symbol, NA))return i; i++;}
  2087                                  
  2088 00000E7D 8D06[7200]               lea  ax, [NA]
  2089 00000E81 50                       push ax
  2090 00000E82 8D066E81                 lea  ax, [symbol]
  2091 00000E86 50                       push ax
  2092 00000E87 E85654                   call eqstr
  2093 00000E8A 83C404                   add  sp, 4
  2094 00000E8D 08C0                     or  al, al
  2095 00000E8F 0F840600                 je @@checkFunction94
  2096 00000E93 8B46FE                   mov ax, [bp-2]
  2097 00000E96 E90E00                   jmp @@retn
  2098                                  @@checkFunction94:
  2099 00000E99 FF46FE                   inc  word [bp-2]
  2100                                  
  2101                                  ;-245   return 0;
  2102                                  
  2103 00000E9C EBBA                     jmp @@checkFunction92
  2104                                  @@checkFunction93:
  2105 00000E9E 66B800000000             mov eax, 0
  2106 00000EA4 E90000                   jmp @@retn
  2107                                  
  2108                                  ;-246 }
  2109                                  
  2110                                  
  2111                                  ;-247 void dofunc() { int nloc; int narg; int i;
  2112                                  
  2113 00000EA7 C9                      @@retn: LEAVE
  2114 00000EA8 C3                       ret
  2115                                  ; ENDP
  2116                                  
  2117                                  dofunc:  ; *** PROC ***
  2118                                  
  2119                                  ;-248   cloc=&co; 
  2120                                  
  2121                                  ;Function : dofunc, Number of local variables: 3
  2122                                  ;   # type sign width line used address name   list of local variables
  2123                                  ;  200 var sign word   247 NULL bp-2    nloc
  2124                                  ;  201 var sign word   247 NULL bp-4    narg
  2125                                  ;  202 var sign word   247 NULL bp-6    i;
  2126 00000EA9 C8060000                 ENTER  6,0
  2127 00000EAD B83075                   mov ax, co
  2128 00000EB0 A3[3000]                 mov word [cloc], ax
  2129                                  
  2130                                  ;-249   checknamelen();
  2131                                  
  2132 00000EB3 E885F2                   call checknamelen
  2133                                  
  2134                                  ;-250   strcpy(fname, symbol);
  2135                                  
  2136 00000EB6 8D066E81                 lea  ax, [symbol]
  2137 00000EBA 50                       push ax
  2138 00000EBB 8D06EE81                 lea  ax, [fname]
  2139 00000EBF 50                       push ax
  2140 00000EC0 E8C952                   call strcpy
  2141 00000EC3 83C404                   add  sp, 4
  2142                                  
  2143                                  ;-251   if (checkFunction() ) error1("Function already defined");
  2144                                  
  2145 00000EC6 E882FF                   call checkFunction
  2146 00000EC9 08C0                     or  al, al
  2147 00000ECB 0F840900                 je @@dofunc95
  2148 00000ECF 68[1411]                 push dofunc_0
  2149 00000ED2 E8BE42                   call error1
  2150 00000ED5 83C402                   add  sp, 2
  2151                                  
  2152                                  ;-252   storefunc();
  2153                                  
  2154                                  @@dofunc95:
  2155 00000ED8 E853FE                   call storefunc
  2156                                  
  2157                                  ;-253   prs("\n\n"); prs(symbol); prs(":  ; *** PROC ***");
  2158                                  
  2159 00000EDB 68[2D11]                 push dofunc_1
  2160 00000EDE E8B13F                   call prs
  2161 00000EE1 83C402                   add  sp, 2
  2162 00000EE4 8D066E81                 lea  ax, [symbol]
  2163 00000EE8 50                       push ax
  2164 00000EE9 E8A63F                   call prs
  2165 00000EEC 83C402                   add  sp, 2
  2166 00000EEF 68[3211]                 push dofunc_2
  2167 00000EF2 E89D3F                   call prs
  2168 00000EF5 83C402                   add  sp, 2
  2169                                  
  2170                                  ;-254   expect('(');  LTop=LSTART;
  2171                                  
  2172 00000EF8 6A28                     push 40
  2173 00000EFA E8773E                   call expect
  2174 00000EFD 83C402                   add  sp, 2
  2175 00000F00 66B8C8000000             mov eax, 200
  2176 00000F06 A3[6000]                 mov word [LTop], ax
  2177                                  
  2178                                  ;-255   if (istoken(')')==0) { narg=4; // return address and old bp value
  2179                                  
  2180 00000F09 6A29                     push 41
  2181 00000F0B E83E3E                   call istoken
  2182 00000F0E 83C402                   add  sp, 2
  2183 00000F11 83F800                   cmp ax, 0
  2184 00000F14 0F855400                 jne @@dofunc96
  2185 00000F18 66B804000000             mov eax, 4
  2186 00000F1E 8946FC                   mov word [bp-4], ax
  2187                                  
  2188                                  ;-256     do { typeName();  addlocal();
  2189                                  
  2190                                  @@dofunc97:
  2191 00000F21 E832FC                   call typeName
  2192 00000F24 E873FE                   call addlocal
  2193                                  
  2194                                  ;-257          GData[LTop]=narg;
  2195                                  
  2196 00000F27 8B46FC                   mov ax, [bp-4]
  2197 00000F2A 8B1E[6000]               mov bx, [LTop]
  2198 00000F2E C1E302                   shl bx, 2
  2199 00000F31 668987EB8A               mov [GData+bx], eax
  2200                                  
  2201                                  ;-258          if (iswidth < 2) narg+=2; else narg=narg+iswidth;
  2202                                  
  2203 00000F36 A0[5D00]                 mov al, [iswidth]
  2204 00000F39 3C02                     cmp al, 2
  2205 00000F3B 0F8D0700                 jge @@dofunc98
  2206 00000F3F 8346FC02                 add  word [bp-4], 2
  2207 00000F43 E90A00                   jmp @@dofunc99
  2208                                  @@dofunc98:
  2209 00000F46 8B46FC                   mov ax, [bp-4]
  2210 00000F49 0206[5D00]               add al, [iswidth]
  2211 00000F4D 8946FC                   mov word [bp-4], ax
  2212                                  
  2213                                  ;-259          LTop++; }
  2214                                  
  2215                                  @@dofunc99:
  2216 00000F50 FF06[6000]               inc  word [LTop]
  2217                                  
  2218                                  ;-260     while (istoken(','));  expect(')'); }
  2219                                  
  2220 00000F54 6A2C                     push 44
  2221 00000F56 E8F33D                   call istoken
  2222 00000F59 83C402                   add  sp, 2
  2223 00000F5C 08C0                     or  al, al
  2224 00000F5E 0F840200                 je @@dofunc100
  2225 00000F62 EBBD                     jmp @@dofunc97
  2226                                  @@dofunc100:
  2227 00000F64 6A29                     push 41
  2228 00000F66 E80B3E                   call expect
  2229 00000F69 83C402                   add  sp, 2
  2230                                  
  2231                                  ;-261     
  2232                                  
  2233                                  
  2234                                  ;-262   expect('{'); /*body*/
  2235                                  
  2236                                  @@dofunc96:
  2237 00000F6C 6A7B                     push 123
  2238 00000F6E E8033E                   call expect
  2239 00000F71 83C402                   add  sp, 2
  2240                                  
  2241                                  ;-263   nloc=0; nreturn=0; nconst=0;
  2242                                  
  2243 00000F74 66B800000000             mov eax, 0
  2244 00000F7A 8946FE                   mov word [bp-2], ax
  2245 00000F7D 66B800000000             mov eax, 0
  2246 00000F83 A3[4700]                 mov word [nreturn], ax
  2247 00000F86 66B800000000             mov eax, 0
  2248 00000F8C A3[4500]                 mov word [nconst], ax
  2249                                  
  2250                                  ;-264   while(isvariable()) {
  2251                                  
  2252                                  @@dofunc101:
  2253 00000F8F E8E201                   call isvariable
  2254 00000F92 08C0                     or  al, al
  2255 00000F94 0F849400                 je @@dofunc102
  2256                                  
  2257                                  ;-265     do {typeName(); checknamelen(); addlocal();
  2258                                  
  2259                                  @@dofunc103:
  2260 00000F98 E8BBFB                   call typeName
  2261 00000F9B E89DF1                   call checknamelen
  2262 00000F9E E8F9FD                   call addlocal
  2263                                  
  2264                                  ;-266         _ i= 2; if(_ iswidth > 2)  { ax=iswidth; /*cast b2w*/ _ i=ax; }
  2265                                  
  2266 00000FA1 C746FA0200              mov word [bp-6], 2; i
  2267 00000FA6 803E[5D00]02            cmp byte [iswidth], 2
  2268 00000FAB 0F8E0800                 jle @@dofunc104
  2269 00000FAF 0FBE06[5D00]             movsx  ax, byte [iswidth]
  2270 00000FB4 8946FA                  mov [bp-6], ax
  2271                                  
  2272                                  ;-267         if (istoken('[')){istype='&';GType[LTop]='&';expect(T_CONST);
  2273                                  
  2274                                  @@dofunc104:
  2275 00000FB7 6A5B                     push 91
  2276 00000FB9 E8903D                   call istoken
  2277 00000FBC 83C402                   add  sp, 2
  2278 00000FBF 08C0                     or  al, al
  2279 00000FC1 0F842F00                 je @@dofunc105
  2280 00000FC5 66B826000000             mov eax, 38
  2281 00000FCB A2[5500]                 mov byte [istype], al
  2282 00000FCE 66B826000000             mov eax, 38
  2283 00000FD4 8B1E[6000]               mov bx, [LTop]
  2284 00000FD8 8887B782                 mov [GType+bx], al
  2285 00000FDC 680101                   push 257
  2286 00000FDF E8923D                   call expect
  2287 00000FE2 83C402                   add  sp, 2
  2288                                  
  2289                                  ;-268             expect(']'); i=lexvalL; }
  2290                                  
  2291 00000FE5 6A5D                     push 93
  2292 00000FE7 E88A3D                   call expect
  2293 00000FEA 83C402                   add  sp, 2
  2294 00000FED 66A1[4D00]               mov eax, [lexvalL]
  2295 00000FF1 8946FA                   mov word [bp-6], ax
  2296                                  
  2297                                  ;-269         nloc=nloc - i;  GData[LTop]=nloc;
  2298                                  
  2299                                  @@dofunc105:
  2300 00000FF4 8B46FE                   mov ax, [bp-2]
  2301 00000FF7 2B46FA                   sub ax, [bp-6]
  2302 00000FFA 8946FE                   mov word [bp-2], ax
  2303 00000FFD 8B46FE                   mov ax, [bp-2]
  2304 00001000 8B1E[6000]               mov bx, [LTop]
  2305 00001004 C1E302                   shl bx, 2
  2306 00001007 668987EB8A               mov [GData+bx], eax
  2307                                  
  2308                                  ;-270         LTop++;
  2309                                  
  2310 0000100C FF06[6000]               inc  word [LTop]
  2311                                  
  2312                                  ;-271       } while (istoken(',')); expect(';'); }
  2313                                  
  2314 00001010 6A2C                     push 44
  2315 00001012 E8373D                   call istoken
  2316 00001015 83C402                   add  sp, 2
  2317 00001018 08C0                     or  al, al
  2318 0000101A 0F840300                 je @@dofunc106
  2319 0000101E E977FF                   jmp @@dofunc103
  2320                                  @@dofunc106:
  2321 00001021 6A3B                     push 59
  2322 00001023 E84E3D                   call expect
  2323 00001026 83C402                   add  sp, 2
  2324                                  
  2325                                  ;-272   listproc(); 
  2326                                  
  2327 00001029 E963FF                   jmp @@dofunc101
  2328                                  @@dofunc102:
  2329 0000102C E86F42                   call listproc
  2330                                  
  2331                                  ;-273   if (LTop>LSTART){prs(";\n ENTER  "); 
  2332                                  
  2333 0000102F A1[6000]                 mov ax, [LTop]
  2334 00001032 3DC800                   cmp ax, 200
  2335 00001035 0F8E2700                 jle @@dofunc107
  2336 00001039 68[4411]                 push dofunc_3
  2337 0000103C E8533E                   call prs
  2338 0000103F 83C402                   add  sp, 2
  2339                                  
  2340                                  ;-274     nloc=mkneg(nloc); pint1 (nloc); prs(",0"); }
  2341                                  
  2342 00001042 FF76FE                   push word [bp-2]
  2343 00001045 E86850                   call mkneg
  2344 00001048 83C402                   add  sp, 2
  2345 0000104B 8946FE                   mov word [bp-2], ax
  2346 0000104E FF76FE                   push word [bp-2]
  2347 00001051 E8443F                   call pint1
  2348 00001054 83C402                   add  sp, 2
  2349 00001057 68[5011]                 push dofunc_4
  2350 0000105A E8353E                   call prs
  2351 0000105D 83C402                   add  sp, 2
  2352                                  
  2353                                  ;-275   while(istoken('}')==0)   stmt();
  2354                                  
  2355                                  @@dofunc107:
  2356                                  @@dofunc108:
  2357 00001060 6A7D                     push 125
  2358 00001062 E8E73C                   call istoken
  2359 00001065 83C402                   add  sp, 2
  2360 00001068 83F800                   cmp ax, 0
  2361 0000106B 0F850500                 jne @@dofunc109
  2362 0000106F E8CA2D                   call stmt
  2363                                  
  2364                                  ;-276   if (nreturn) { prs("\n@@retn:");}
  2365                                  
  2366 00001072 EBEC                     jmp @@dofunc108
  2367                                  @@dofunc109:
  2368 00001074 A1[4700]                 mov ax, [nreturn]
  2369 00001077 08C0                     or  al, al
  2370 00001079 0F840900                 je @@dofunc110
  2371 0000107D 68[5311]                 push dofunc_5
  2372 00001080 E80F3E                   call prs
  2373 00001083 83C402                   add  sp, 2
  2374                                  
  2375                                  ;-277   if (LTop > LSTART) prs(" LEAVE");
  2376                                  
  2377                                  @@dofunc110:
  2378 00001086 A1[6000]                 mov ax, [LTop]
  2379 00001089 3DC800                   cmp ax, 200
  2380 0000108C 0F8E0900                 jle @@dofunc111
  2381 00001090 68[5D11]                 push dofunc_6
  2382 00001093 E8FC3D                   call prs
  2383 00001096 83C402                   add  sp, 2
  2384                                  
  2385                                  ;-278   prs("\n ret"); prs("\n; ENDP"); 
  2386                                  
  2387                                  @@dofunc111:
  2388 00001099 68[6411]                 push dofunc_7
  2389 0000109C E8F33D                   call prs
  2390 0000109F 83C402                   add  sp, 2
  2391 000010A2 68[6B11]                 push dofunc_8
  2392 000010A5 E8EA3D                   call prs
  2393 000010A8 83C402                   add  sp, 2
  2394                                  
  2395                                  ;-279   *cloc=0; prs(co); maxco1=strlen(co);
  2396                                  
  2397 000010AB 66B800000000             mov eax, 0
  2398 000010B1 8B1E[3000]               mov  bx, [cloc]
  2399 000010B5 8807                     mov  [bx], al
  2400 000010B7 8D063075                 lea  ax, [co]
  2401 000010BB 50                       push ax
  2402 000010BC E8D33D                   call prs
  2403 000010BF 83C402                   add  sp, 2
  2404 000010C2 8D063075                 lea  ax, [co]
  2405 000010C6 50                       push ax
  2406 000010C7 E89750                   call strlen
  2407 000010CA 83C402                   add  sp, 2
  2408 000010CD A3[2E00]                 mov word [maxco1], ax
  2409                                  
  2410                                  ;-280   if (maxco1 > maxco) {maxco=maxco1; strcpy(coname, fname); }
  2411                                  
  2412 000010D0 A1[2E00]                 mov ax, [maxco1]
  2413 000010D3 3B06[2C00]               cmp ax, [maxco]
  2414 000010D7 0F8E1600                 jle @@dofunc112
  2415 000010DB A1[2E00]                 mov ax, [maxco1]
  2416 000010DE A3[2C00]                 mov word [maxco], ax
  2417 000010E1 8D06EE81                 lea  ax, [fname]
  2418 000010E5 50                       push ax
  2419 000010E6 8D06E880                 lea  ax, [coname]
  2420 000010EA 50                       push ax
  2421 000010EB E89E50                   call strcpy
  2422 000010EE 83C404                   add  sp, 4
  2423                                  
  2424                                  ;-281   if (LTop  > maxloc) {maxloc=LTop; strcpy(locname, fname); }
  2425                                  
  2426                                  @@dofunc112:
  2427 000010F1 A1[6000]                 mov ax, [LTop]
  2428 000010F4 3B06[3200]               cmp ax, [maxloc]
  2429 000010F8 0F8E1600                 jle @@dofunc113
  2430 000010FC A1[6000]                 mov ax, [LTop]
  2431 000010FF A3[3200]                 mov word [maxloc], ax
  2432 00001102 8D06EE81                 lea  ax, [fname]
  2433 00001106 50                       push ax
  2434 00001107 8D062B81                 lea  ax, [locname]
  2435 0000110B 50                       push ax
  2436 0000110C E87D50                   call strcpy
  2437 0000110F 83C404                   add  sp, 4
  2438                                  
  2439                                  ;-282 }
  2440                                  
  2441                                  @@dofunc113:
  2442                                  
  2443                                  ;-283 int isvariable() {
  2444 00001112 C9                       LEAVE
  2445 00001113 C3                       ret
  2446                                  ; ENDP
  2447 00001114 46756E6374696F6E20-     dofunc_0 db "Function already defined",0
  2448 0000111D 616C72656164792064-
  2449 00001126 6566696E656400     
  2450 0000112D 5C6E5C6E00              dofunc_1 db "\n\n",0
  2451 00001132 3A20203B202A2A2A20-     dofunc_2 db ":  ; *** PROC ***",0
  2452 0000113B 50524F43202A2A2A00 
  2453 00001144 3B5C6E20454E544552-     dofunc_3 db ";\n ENTER  ",0
  2454 0000114D 202000             
  2455 00001150 2C3000                  dofunc_4 db ",0",0
  2456 00001153 5C6E40407265746E3A-     dofunc_5 db "\n@@retn:",0
  2457 0000115C 00                 
  2458 0000115D 204C4541564500          dofunc_6 db " LEAVE",0
  2459 00001164 5C6E2072657400          dofunc_7 db "\n ret",0
  2460 0000116B 5C6E3B20454E445000      dofunc_8 db "\n; ENDP",0
  2461                                  
  2462                                  isvariable:  ; *** PROC ***
  2463                                  
  2464                                  ;-284   if(token==T_SIGNED) goto v1;   if(token==T_UNSIGNED) goto v1;
  2465                                  
  2466 00001174 A1[3D00]                 mov ax, [token]
  2467 00001177 3D1302                   cmp ax, 531
  2468 0000117A 0F850300                 jne @@isvariable114
  2469 0000117E E95700                   jmp @@v1
  2470                                  @@isvariable114:
  2471 00001181 A1[3D00]                 mov ax, [token]
  2472 00001184 3D1402                   cmp ax, 532
  2473 00001187 0F850300                 jne @@isvariable115
  2474 0000118B E94A00                   jmp @@v1
  2475                                  
  2476                                  ;-285   if(token==T_CHAR)   goto v1;   if(token==T_INT)      goto v1;
  2477                                  
  2478                                  @@isvariable115:
  2479 0000118E A1[3D00]                 mov ax, [token]
  2480 00001191 3D1202                   cmp ax, 530
  2481 00001194 0F850300                 jne @@isvariable116
  2482 00001198 E93D00                   jmp @@v1
  2483                                  @@isvariable116:
  2484 0000119B A1[3D00]                 mov ax, [token]
  2485 0000119E 3D0502                   cmp ax, 517
  2486 000011A1 0F850300                 jne @@isvariable117
  2487 000011A5 E93000                   jmp @@v1
  2488                                  
  2489                                  ;-286   if(token==T_SHORT)  goto v1;   if(token==T_LONG)     goto v1;
  2490                                  
  2491                                  @@isvariable117:
  2492 000011A8 A1[3D00]                 mov ax, [token]
  2493 000011AB 3D1602                   cmp ax, 534
  2494 000011AE 0F850300                 jne @@isvariable118
  2495 000011B2 E92300                   jmp @@v1
  2496                                  @@isvariable118:
  2497 000011B5 A1[3D00]                 mov ax, [token]
  2498 000011B8 3D1502                   cmp ax, 533
  2499 000011BB 0F850300                 jne @@isvariable119
  2500 000011BF E91600                   jmp @@v1
  2501                                  
  2502                                  ;-287   if(token==T_UINT32) goto v1;   return 0;  v1: return 1;
  2503                                  
  2504                                  @@isvariable119:
  2505 000011C2 A1[3D00]                 mov ax, [token]
  2506 000011C5 3D2102                   cmp ax, 545
  2507 000011C8 0F850300                 jne @@isvariable120
  2508 000011CC E90900                   jmp @@v1
  2509                                  @@isvariable120:
  2510 000011CF 66B800000000             mov eax, 0
  2511 000011D5 E90900                   jmp @@retn
  2512                                  @@v1:
  2513 000011D8 66B801000000             mov eax, 1
  2514 000011DE E90000                   jmp @@retn
  2515                                  
  2516                                  ;-288 }
  2517                                  
  2518                                  
  2519                                  ;-289 int sd; //side: 0=left, 1=middle, 2=rigth
  2520                                  
  2521                                  @@retn:
  2522 000011E1 C3                       ret
  2523                                  ; ENDP
  2524 000011E2 0000                    sd dw 0
  2525                                  
  2526                                  ;-290 char un; //1=unary, 2=binary, 3=assign, 4=mul, 5=mod, 6=div
  2527                                  
  2528 000011E4 00                      un db 0
  2529                                  
  2530                                  ;-291 char ty; // 1const, 2call, 3reg, 4var, 5arr
  2531                                  
  2532 000011E5 00                      ty db 0
  2533                                  
  2534                                  ;-292 char md[3];  int mod1; int mod2; //typename       issign,iswidth  0=m, 1=*, 2=&
  2535                                  
  2536                                  section .bss
  2537                                  absolute 46807
  2538 0000B6D7 <res 00000003>          md resb 3
  2539                                  section .text
  2540 000011E6 0000                    mod1 dw 0
  2541 000011E8 0000                    mod2 dw 0
  2542                                  
  2543                                  ;-293 int  ir[3];  int irg1; int irg2; //checkreg       0=noreg, reg-nr
  2544                                  
  2545                                  section .bss
  2546                                  absolute 46810
  2547 0000B6DA <res 00000006>          ir resw 3
  2548                                  section .text
  2549 000011EA 0000                    irg1 dw 0
  2550 000011EC 0000                    irg2 dw 0
  2551                                  
  2552                                  ;-294 int  ix[3];  int idx1; int idx2; //searchname     var-nr
  2553                                  
  2554                                  section .bss
  2555                                  absolute 46816
  2556 0000B6E0 <res 00000006>          ix resw 3
  2557                                  section .text
  2558 000011EE 0000                    idx1 dw 0
  2559 000011F0 0000                    idx2 dw 0
  2560                                  
  2561                                  ;-295 char is[3];  int ids1; int ids2; //gettypes(idx1) signi: 0=U, 1=S
  2562                                  
  2563                                  section .bss
  2564                                  absolute 46822
  2565 0000B6E6 <res 00000003>          is resb 3
  2566                                  section .text
  2567 000011F2 0000                    ids1 dw 0
  2568 000011F4 0000                    ids2 dw 0
  2569                                  
  2570                                  ;-296 char iw[3];  int idw1; int idw2; //gettypes(idx1) wi   : 1,2,4
  2571                                  
  2572                                  section .bss
  2573                                  absolute 46825
  2574 0000B6E9 <res 00000003>          iw resb 3
  2575                                  section .text
  2576 000011F6 0000                    idw1 dw 0
  2577 000011F8 0000                    idw2 dw 0
  2578                                  
  2579                                  ;-297 char it[3];  int idt1; int idt2; //gettypes(idx1) typei: 0=mem, 1=*, 2=&
  2580                                  
  2581                                  section .bss
  2582                                  absolute 46828
  2583 0000B6EC <res 00000003>          it resb 3
  2584                                  section .text
  2585 000011FA 0000                    idt1 dw 0
  2586 000011FC 0000                    idt2 dw 0
  2587                                  
  2588                                  ;-298 long ia[3];  int ida1; int ida2; //array const or varnr
  2589                                  
  2590                                  section .bss
  2591                                  absolute 46831
  2592 0000B6EF <res 0000000C>          ia resd 3
  2593                                  section .text
  2594 000011FE 0000                    ida1 dw 0
  2595 00001200 0000                    ida2 dw 0
  2596                                  
  2597                                  ;-299 char ic[3];  int idac1;int idac2;//array is const
  2598                                  
  2599                                  section .bss
  2600                                  absolute 46843
  2601 0000B6FB <res 00000003>          ic resb 3
  2602                                  section .text
  2603 00001202 0000                    idac1 dw 0
  2604 00001204 0000                    idac2 dw 0
  2605                                  
  2606                                  ;-300 long iv[3];  long val1;long val2;// = lexvalL
  2607                                  
  2608                                  section .bss
  2609                                  absolute 46846
  2610 0000B6FE <res 0000000C>          iv resd 3
  2611                                  section .text
  2612 00001206 00000000                val1 dd 0
  2613 0000120A 00000000                val2 dd 0
  2614                                  
  2615                                  ;-301 
  2616                                  
  2617                                  
  2618                                  ;-302 
  2619                                  
  2620                                  
  2621                                  ;-303 int pexpr() {expect('('); iscmp=0; 
  2622                                  
  2623                                  
  2624                                  pexpr:  ; *** PROC ***
  2625 0000120E 6A28                     push 40
  2626 00001210 E8613B                   call expect
  2627 00001213 83C402                   add  sp, 2
  2628 00001216 66B800000000             mov eax, 0
  2629 0000121C A3[4300]                 mov word [iscmp], ax
  2630                                  
  2631                                  ;-304   if (token==T_NAME) {if (eqstr(symbol, "_")) {constantexpr(); return;}
  2632                                  
  2633 0000121F A1[3D00]                 mov ax, [token]
  2634 00001222 3D0001                   cmp ax, 256
  2635 00001225 0F853400                 jne @@pexpr121
  2636 00001229 68[8712]                 push pexpr_0
  2637 0000122C 8D066E81                 lea  ax, [symbol]
  2638 00001230 50                       push ax
  2639 00001231 E8AC50                   call eqstr
  2640 00001234 83C404                   add  sp, 4
  2641 00001237 08C0                     or  al, al
  2642 00001239 0F840600                 je @@pexpr122
  2643 0000123D E85F00                   call constantexpr
  2644 00001240 E94300                   jmp @@retn
  2645                                  
  2646                                  ;-305     irg1=checkreg();
  2647                                  
  2648                                  @@pexpr122:
  2649 00001243 E88B08                   call checkreg
  2650 00001246 A3[EA11]                 mov word [irg1], ax
  2651                                  
  2652                                  ;-306     if (irg1) { doreg1(1); return; }  }
  2653                                  
  2654 00001249 A1[EA11]                 mov ax, [irg1]
  2655 0000124C 08C0                     or  al, al
  2656 0000124E 0F840B00                 je @@pexpr123
  2657 00001252 6A01                     push 1
  2658 00001254 E8A506                   call doreg1
  2659 00001257 83C402                   add  sp, 2
  2660 0000125A E92900                   jmp @@retn
  2661                                  @@pexpr123:
  2662                                  
  2663                                  ;-307   exprstart(); if (iscmp==0) prs("\n or  al, al\n je @@");  prs(fname);
  2664                                  
  2665                                  @@pexpr121:
  2666 0000125D E80001                   call exprstart
  2667 00001260 A1[4300]                 mov ax, [iscmp]
  2668 00001263 83F800                   cmp ax, 0
  2669 00001266 0F850900                 jne @@pexpr124
  2670 0000126A 68[8912]                 push pexpr_1
  2671 0000126D E8223C                   call prs
  2672 00001270 83C402                   add  sp, 2
  2673                                  @@pexpr124:
  2674 00001273 8D06EE81                 lea  ax, [fname]
  2675 00001277 50                       push ax
  2676 00001278 E8173C                   call prs
  2677 0000127B 83C402                   add  sp, 2
  2678                                  
  2679                                  ;-308   expect(')'); }
  2680                                  
  2681 0000127E 6A29                     push 41
  2682 00001280 E8F13A                   call expect
  2683 00001283 83C402                   add  sp, 2
  2684                                  
  2685                                  ;-309 
  2686                                  
  2687                                  
  2688                                  ;-310 void constantexpr() { int mode; int id1;int ids;
  2689                                  
  2690                                  @@retn:
  2691 00001286 C3                       ret
  2692                                  ; ENDP
  2693 00001287 5F00                    pexpr_0 db "_",0
  2694 00001289 5C6E206F722020616C-     pexpr_1 db "\n or  al, al\n je @@",0
  2695 00001292 2C20616C5C6E206A65-
  2696 0000129B 20404000           
  2697                                  
  2698                                  constantexpr:  ; *** PROC ***
  2699                                  
  2700                                  ;-311   token=getlex();   mode=typeName();  
  2701                                  
  2702                                  ;Function : constantexpr, Number of local variables: 3
  2703                                  ;   # type sign width line used address name   list of local variables
  2704                                  ;  200 var sign word   310 NULL bp-2    mode
  2705                                  ;  201 var sign word   310 NULL bp-4    id1
  2706                                  ;  202 var sign word   310 NULL bp-6    ids;
  2707 0000129F C8060000                 ENTER  6,0
  2708 000012A3 E80632                   call getlex
  2709 000012A6 A3[3D00]                 mov word [token], ax
  2710 000012A9 E8AAF8                   call typeName
  2711 000012AC 8946FE                   mov word [bp-2], ax
  2712                                  
  2713                                  ;-312   id1=searchname(); gettypes(id1); ids=signi;
  2714                                  
  2715 000012AF E8C4F7                   call searchname
  2716 000012B2 8946FC                   mov word [bp-4], ax
  2717 000012B5 FF76FC                   push word [bp-4]
  2718 000012B8 E870F6                   call gettypes
  2719 000012BB 83C402                   add  sp, 2
  2720 000012BE A1[5600]                 mov ax, [signi]
  2721 000012C1 8946FA                   mov word [bp-6], ax
  2722                                  
  2723                                  ;-313   if (isrelational() ==0) error1("Relational expression expected");
  2724                                  
  2725 000012C4 E87831                   call isrelational
  2726 000012C7 83F800                   cmp ax, 0
  2727 000012CA 0F850900                 jne @@constantexpr125
  2728 000012CE 68[3713]                 push constantexpr_0
  2729 000012D1 E8BF3E                   call error1
  2730 000012D4 83C402                   add  sp, 2
  2731                                  
  2732                                  ;-314   expect(T_CONST);
  2733                                  
  2734                                  @@constantexpr125:
  2735 000012D7 680101                   push 257
  2736 000012DA E8973A                   call expect
  2737 000012DD 83C402                   add  sp, 2
  2738                                  
  2739                                  ;-315   prs("\ncmp "); 
  2740                                  
  2741 000012E0 68[5613]                 push constantexpr_1
  2742 000012E3 E8AC3B                   call prs
  2743 000012E6 83C402                   add  sp, 2
  2744                                  
  2745                                  ;-316   gettypes(id1);  pwi(wi);  v(id1);
  2746                                  
  2747 000012E9 FF76FC                   push word [bp-4]
  2748 000012EC E83CF6                   call gettypes
  2749 000012EF 83C402                   add  sp, 2
  2750 000012F2 FF36[5B00]               push word [wi]
  2751 000012F6 E88EEE                   call pwi
  2752 000012F9 83C402                   add  sp, 2
  2753 000012FC FF76FC                   push word [bp-4]
  2754 000012FF E8F4ED                   call v
  2755 00001302 83C402                   add  sp, 2
  2756                                  
  2757                                  ;-317   prs(", "); prL(lexvalL); cmpneg(ids);   prs(fname);
  2758                                  
  2759 00001305 68[5D13]                 push constantexpr_2
  2760 00001308 E8873B                   call prs
  2761 0000130B 83C402                   add  sp, 2
  2762 0000130E 66FF36[4D00]             push dword [lexvalL]
  2763 00001313 E82F4C                   call prL
  2764 00001316 83C404                   add  sp, 4
  2765 00001319 FF76FA                   push word [bp-6]
  2766 0000131C E8D12D                   call cmpneg
  2767 0000131F 83C402                   add  sp, 2
  2768 00001322 8D06EE81                 lea  ax, [fname]
  2769 00001326 50                       push ax
  2770 00001327 E8683B                   call prs
  2771 0000132A 83C402                   add  sp, 2
  2772                                  
  2773                                  ;-318   expect(')');
  2774                                  
  2775 0000132D 6A29                     push 41
  2776 0000132F E8423A                   call expect
  2777 00001332 83C402                   add  sp, 2
  2778                                  
  2779                                  ;-319 }  
  2780                                  
  2781                                  
  2782                                  ;-320 void exprstart() { if (eqstr(symbol, "_")) simplexpr(); else expr(); }
  2783 00001335 C9                       LEAVE
  2784 00001336 C3                       ret
  2785                                  ; ENDP
  2786 00001337 52656C6174696F6E61-     constantexpr_0 db "Relational expression expected",0
  2787 00001340 6C2065787072657373-
  2788 00001349 696F6E206578706563-
  2789 00001352 74656400           
  2790 00001356 5C6E636D702000          constantexpr_1 db "\ncmp ",0
  2791 0000135D 2C2000                  constantexpr_2 db ", ",0
  2792                                  
  2793                                  exprstart:  ; *** PROC ***
  2794 00001360 68[7E13]                 push exprstart_0
  2795 00001363 8D066E81                 lea  ax, [symbol]
  2796 00001367 50                       push ax
  2797 00001368 E8754F                   call eqstr
  2798 0000136B 83C404                   add  sp, 4
  2799 0000136E 08C0                     or  al, al
  2800 00001370 0F840600                 je @@exprstart126
  2801 00001374 E80900                   call simplexpr
  2802 00001377 E90300                   jmp @@exprstart127
  2803                                  @@exprstart126:
  2804 0000137A E8970C                   call expr
  2805                                  @@exprstart127:
  2806                                  
  2807                                  ;-321 // 1const, 2call, 3reg, 4var, 5arr
  2808                                  
  2809                                  
  2810                                  ;-322 int simplexpr() {  int i;
  2811                                  
  2812 0000137D C3                       ret
  2813                                  ; ENDP
  2814 0000137E 5F00                    exprstart_0 db "_",0
  2815                                  
  2816                                  simplexpr:  ; *** PROC ***
  2817                                  
  2818                                  ;-323   sd=0;
  2819                                  
  2820                                  ;Function : simplexpr, Number of local variables: 1
  2821                                  ;   # type sign width line used address name   list of local variables
  2822                                  ;  200 var sign word   322 NULL bp-2    i;
  2823 00001380 C8020000                 ENTER  2,0
  2824 00001384 66B800000000             mov eax, 0
  2825 0000138A A3[E211]                 mov word [sd], ax
  2826                                  
  2827                                  ;-324   token=getlex();
  2828                                  
  2829 0000138D E81C31                   call getlex
  2830 00001390 A3[3D00]                 mov word [token], ax
  2831                                  
  2832                                  ;-325   if (istoken(T_CONST)) { prs("\n mov eax, "); prL(lexvalL); return; }// 1const
  2833                                  
  2834 00001393 680101                   push 257
  2835 00001396 E8B339                   call istoken
  2836 00001399 83C402                   add  sp, 2
  2837 0000139C 08C0                     or  al, al
  2838 0000139E 0F841700                 je @@simplexpr128
  2839 000013A2 68[2916]                 push simplexpr_0
  2840 000013A5 E8EA3A                   call prs
  2841 000013A8 83C402                   add  sp, 2
  2842 000013AB 66FF36[4D00]             push dword [lexvalL]
  2843 000013B0 E8924B                   call prL
  2844 000013B3 83C404                   add  sp, 4
  2845 000013B6 E96E02                   jmp @@retn
  2846                                  
  2847                                  ;-326   mod1=typeName();
  2848                                  
  2849                                  @@simplexpr128:
  2850 000013B9 E89AF7                   call typeName
  2851 000013BC A3[E611]                 mov word [mod1], ax
  2852                                  
  2853                                  ;-327   if (token=='(')  {ids1=issign; idw1=iswidth; docall1(); return; }   // 2call
  2854                                  
  2855 000013BF A1[3D00]                 mov ax, [token]
  2856 000013C2 83F828                   cmp ax, 40
  2857 000013C5 0F851200                 jne @@simplexpr129
  2858 000013C9 A0[5800]                 mov al, [issign]
  2859 000013CC A3[F211]                 mov word [ids1], ax
  2860 000013CF A0[5D00]                 mov al, [iswidth]
  2861 000013D2 A3[F611]                 mov word [idw1], ax
  2862 000013D5 E88717                   call docall1
  2863 000013D8 E94C02                   jmp @@retn
  2864                                  
  2865                                  ;-328   if (mod1 == 2) error1(" & is not allowed in left side");
  2866                                  
  2867                                  @@simplexpr129:
  2868 000013DB A1[E611]                 mov ax, [mod1]
  2869 000013DE 83F802                   cmp ax, 2
  2870 000013E1 0F850900                 jne @@simplexpr130
  2871 000013E5 68[3616]                 push simplexpr_1
  2872 000013E8 E8A83D                   call error1
  2873 000013EB 83C402                   add  sp, 2
  2874                                  
  2875                                  ;-329   irg1=checkreg();
  2876                                  
  2877                                  @@simplexpr130:
  2878 000013EE E8E006                   call checkreg
  2879 000013F1 A3[EA11]                 mov word [irg1], ax
  2880                                  
  2881                                  ;-330   if (irg1)       {doreg1(0); return; }                               // 3reg
  2882                                  
  2883 000013F4 A1[EA11]                 mov ax, [irg1]
  2884 000013F7 08C0                     or  al, al
  2885 000013F9 0F840B00                 je @@simplexpr131
  2886 000013FD 6A00                     push 0
  2887 000013FF E8FA04                   call doreg1
  2888 00001402 83C402                   add  sp, 2
  2889 00001405 E91F02                   jmp @@retn
  2890                                  
  2891                                  ;-331   if (irg1 == 0) { idx1=searchname();
  2892                                  
  2893                                  @@simplexpr131:
  2894 00001408 A1[EA11]                 mov ax, [irg1]
  2895 0000140B 83F800                   cmp ax, 0
  2896 0000140E 0F853500                 jne @@simplexpr132
  2897 00001412 E861F6                   call searchname
  2898 00001415 A3[EE11]                 mov word [idx1], ax
  2899                                  
  2900                                  ;-332     gettypes(idx1); ids1=signi; idw1=wi; idt1=typei;                  // 4var
  2901                                  
  2902 00001418 FF36[EE11]               push word [idx1]
  2903 0000141C E80CF5                   call gettypes
  2904 0000141F 83C402                   add  sp, 2
  2905 00001422 A1[5600]                 mov ax, [signi]
  2906 00001425 A3[F211]                 mov word [ids1], ax
  2907 00001428 A1[5B00]                 mov ax, [wi]
  2908 0000142B A3[F611]                 mov word [idw1], ax
  2909 0000142E A1[5300]                 mov ax, [typei]
  2910 00001431 A3[FA11]                 mov word [idt1], ax
  2911                                  
  2912                                  ;-333     if (idt1==2)  error1("Array not allowed left side"); }//////      // 5arr
  2913                                  
  2914 00001434 A1[FA11]                 mov ax, [idt1]
  2915 00001437 83F802                   cmp ax, 2
  2916 0000143A 0F850900                 jne @@simplexpr133
  2917 0000143E 68[5516]                 push simplexpr_2
  2918 00001441 E84F3D                   call error1
  2919 00001444 83C402                   add  sp, 2
  2920                                  @@simplexpr133:
  2921                                  
  2922                                  ;-334 
  2923                                  
  2924                                  
  2925                                  ;-335   if (isrelational()) { error1("Relational not implemented yet"); ////////
  2926                                  
  2927                                  @@simplexpr132:
  2928 00001447 E8F52F                   call isrelational
  2929 0000144A 08C0                     or  al, al
  2930 0000144C 0F840900                 je @@simplexpr134
  2931 00001450 68[7116]                 push simplexpr_3
  2932 00001453 E83D3D                   call error1
  2933 00001456 83C402                   add  sp, 2
  2934                                  
  2935                                  ;-336   }
  2936                                  
  2937                                  
  2938                                  ;-337   if (istoken('=') == 0) error1("Assign expected");
  2939                                  
  2940                                  @@simplexpr134:
  2941 00001459 6A3D                     push 61
  2942 0000145B E8EE38                   call istoken
  2943 0000145E 83C402                   add  sp, 2
  2944 00001461 83F800                   cmp ax, 0
  2945 00001464 0F850900                 jne @@simplexpr135
  2946 00001468 68[9016]                 push simplexpr_4
  2947 0000146B E8253D                   call error1
  2948 0000146E 83C402                   add  sp, 2
  2949                                  
  2950                                  ;-338   if (istoken(T_CONST) ) { val2=lexvalL;
  2951                                  
  2952                                  @@simplexpr135:
  2953 00001471 680101                   push 257
  2954 00001474 E8D538                   call istoken
  2955 00001477 83C402                   add  sp, 2
  2956 0000147A 08C0                     or  al, al
  2957 0000147C 0F846900                 je @@simplexpr136
  2958 00001480 66A1[4D00]               mov eax, [lexvalL]
  2959 00001484 66A3[0A12]               mov dword [val2], eax
  2960                                  
  2961                                  ;-339     prs("\nmov ");
  2962                                  
  2963 00001488 68[A016]                 push simplexpr_5
  2964 0000148B E8043A                   call prs
  2965 0000148E 83C402                   add  sp, 2
  2966                                  
  2967                                  ;-340     pwi(idw1);   v(idx1);
  2968                                  
  2969 00001491 FF36[F611]               push word [idw1]
  2970 00001495 E8EFEC                   call pwi
  2971 00001498 83C402                   add  sp, 2
  2972 0000149B FF36[EE11]               push word [idx1]
  2973 0000149F E854EC                   call v
  2974 000014A2 83C402                   add  sp, 2
  2975                                  
  2976                                  ;-341     prs(", "); prL(val2);
  2977                                  
  2978 000014A5 68[A716]                 push simplexpr_6
  2979 000014A8 E8E739                   call prs
  2980 000014AB 83C402                   add  sp, 2
  2981 000014AE 66FF36[0A12]             push dword [val2]
  2982 000014B3 E88F4A                   call prL
  2983 000014B6 83C404                   add  sp, 4
  2984                                  
  2985                                  ;-342     if (idx1 >= LSTART) { prs("; ");
  2986                                  
  2987 000014B9 A1[EE11]                 mov ax, [idx1]
  2988 000014BC 3DC800                   cmp ax, 200 ;unsigned : 1
  2989 000014BF 0F8C2300                 jl  @@simplexpr137
  2990 000014C3 68[AA16]                 push simplexpr_7
  2991 000014C6 E8C939                   call prs
  2992 000014C9 83C402                   add  sp, 2
  2993                                  
  2994                                  ;-343       loadName(NA, idx1); prs(NA); } return; }
  2995                                  
  2996 000014CC FF36[EE11]               push word [idx1]
  2997 000014D0 8D06[7200]               lea  ax, [NA]
  2998 000014D4 50                       push ax
  2999 000014D5 E86949                   call loadName
  3000 000014D8 83C404                   add  sp, 4
  3001 000014DB 8D06[7200]               lea  ax, [NA]
  3002 000014DF 50                       push ax
  3003 000014E0 E8AF39                   call prs
  3004 000014E3 83C402                   add  sp, 2
  3005                                  @@simplexpr137:
  3006 000014E6 E93E01                   jmp @@retn
  3007                                  
  3008                                  ;-344   
  3009                                  
  3010                                  
  3011                                  ;-345   mod2=typeName(); irg2=checkreg();
  3012                                  
  3013                                  @@simplexpr136:
  3014 000014E9 E86AF6                   call typeName
  3015 000014EC A3[E811]                 mov word [mod2], ax
  3016 000014EF E8DF05                   call checkreg
  3017 000014F2 A3[EC11]                 mov word [irg2], ax
  3018                                  
  3019                                  ;-346   if (irg2) { prs("\nmov ");
  3020                                  
  3021 000014F5 A1[EC11]                 mov ax, [irg2]
  3022 000014F8 08C0                     or  al, al
  3023 000014FA 0F845400                 je @@simplexpr138
  3024 000014FE 68[AD16]                 push simplexpr_8
  3025 00001501 E88E39                   call prs
  3026 00001504 83C402                   add  sp, 2
  3027                                  
  3028                                  ;-347     if (irg1) printreg(irg1, mod1);
  3029                                  
  3030 00001507 A1[EA11]                 mov ax, [irg1]
  3031 0000150A 08C0                     or  al, al
  3032 0000150C 0F841100                 je @@simplexpr139
  3033 00001510 FF36[E611]               push word [mod1]
  3034 00001514 FF36[EA11]               push word [irg1]
  3035 00001518 E82A0A                   call printreg
  3036 0000151B 83C404                   add  sp, 4
  3037                                  
  3038                                  ;-348     else { pwi(idx1); v(idx1); }
  3039                                  
  3040 0000151E E91400                   jmp @@simplexpr140
  3041                                  @@simplexpr139:
  3042 00001521 FF36[EE11]               push word [idx1]
  3043 00001525 E85FEC                   call pwi
  3044 00001528 83C402                   add  sp, 2
  3045 0000152B FF36[EE11]               push word [idx1]
  3046 0000152F E8C4EB                   call v
  3047 00001532 83C402                   add  sp, 2
  3048                                  
  3049                                  ;-349     prs(", "); printreg(irg2, mod2);return;}
  3050                                  
  3051                                  @@simplexpr140:
  3052 00001535 68[B416]                 push simplexpr_9
  3053 00001538 E85739                   call prs
  3054 0000153B 83C402                   add  sp, 2
  3055 0000153E FF36[E811]               push word [mod2]
  3056 00001542 FF36[EC11]               push word [irg2]
  3057 00001546 E8FC09                   call printreg
  3058 00001549 83C404                   add  sp, 4
  3059 0000154C E9D800                   jmp @@retn
  3060                                  
  3061                                  ;-350   else {
  3062                                  
  3063 0000154F E9D500                   jmp @@simplexpr141
  3064                                  @@simplexpr138:
  3065                                  
  3066                                  ;-351     if (mod2 == 2) error1("& not allowed");
  3067                                  
  3068 00001552 A1[E811]                 mov ax, [mod2]
  3069 00001555 83F802                   cmp ax, 2
  3070 00001558 0F850900                 jne @@simplexpr142
  3071 0000155C 68[B716]                 push simplexpr_10
  3072 0000155F E8313C                   call error1
  3073 00001562 83C402                   add  sp, 2
  3074                                  
  3075                                  ;-352     
  3076                                  
  3077                                  
  3078                                  ;-353     idx2=searchname();
  3079                                  
  3080                                  @@simplexpr142:
  3081 00001565 E80EF5                   call searchname
  3082 00001568 A3[F011]                 mov word [idx2], ax
  3083                                  
  3084                                  ;-354     gettypes(idx2); ids2=signi; idw2=wi; idt2=typei;
  3085                                  
  3086 0000156B FF36[F011]               push word [idx2]
  3087 0000156F E8B9F3                   call gettypes
  3088 00001572 83C402                   add  sp, 2
  3089 00001575 A1[5600]                 mov ax, [signi]
  3090 00001578 A3[F411]                 mov word [ids2], ax
  3091 0000157B A1[5B00]                 mov ax, [wi]
  3092 0000157E A3[F811]                 mov word [idw2], ax
  3093 00001581 A1[5300]                 mov ax, [typei]
  3094 00001584 A3[FC11]                 mov word [idt2], ax
  3095                                  
  3096                                  ;-355     if (idt2 == 1)  error1("Array right side not implemented"); ///////
  3097                                  
  3098 00001587 A1[FC11]                 mov ax, [idt2]
  3099 0000158A 83F801                   cmp ax, 1
  3100 0000158D 0F850900                 jne @@simplexpr143
  3101 00001591 68[C516]                 push simplexpr_11
  3102 00001594 E8FC3B                   call error1
  3103 00001597 83C402                   add  sp, 2
  3104                                  
  3105                                  ;-356     prs("\nmov ");
  3106                                  
  3107                                  @@simplexpr143:
  3108 0000159A 68[E616]                 push simplexpr_12
  3109 0000159D E8F238                   call prs
  3110 000015A0 83C402                   add  sp, 2
  3111                                  
  3112                                  ;-357     if (irg1) printreg(irg1, mod1);
  3113                                  
  3114 000015A3 A1[EA11]                 mov ax, [irg1]
  3115 000015A6 08C0                     or  al, al
  3116 000015A8 0F841100                 je @@simplexpr144
  3117 000015AC FF36[E611]               push word [mod1]
  3118 000015B0 FF36[EA11]               push word [irg1]
  3119 000015B4 E88E09                   call printreg
  3120 000015B7 83C404                   add  sp, 4
  3121                                  
  3122                                  ;-358     else error1("Mem to Mem not allowed by x86-CPU");
  3123                                  
  3124 000015BA E90900                   jmp @@simplexpr145
  3125                                  @@simplexpr144:
  3126 000015BD 68[ED16]                 push simplexpr_13
  3127 000015C0 E8D03B                   call error1
  3128 000015C3 83C402                   add  sp, 2
  3129                                  
  3130                                  ;-359     prs(", ");
  3131                                  
  3132                                  @@simplexpr145:
  3133 000015C6 68[0F17]                 push simplexpr_14
  3134 000015C9 E8C638                   call prs
  3135 000015CC 83C402                   add  sp, 2
  3136                                  
  3137                                  ;-360     if (mod2 == 2) a(idx2);
  3138                                  
  3139 000015CF A1[E811]                 mov ax, [mod2]
  3140 000015D2 83F802                   cmp ax, 2
  3141 000015D5 0F850D00                 jne @@simplexpr146
  3142 000015D9 FF36[F011]               push word [idx2]
  3143 000015DD E8EDEA                   call a
  3144 000015E0 83C402                   add  sp, 2
  3145                                  
  3146                                  ;-361     else {
  3147                                  
  3148 000015E3 E94100                   jmp @@simplexpr147
  3149                                  @@simplexpr146:
  3150                                  
  3151                                  ;-362       pwi(idw2);  v(idx2);
  3152                                  
  3153 000015E6 FF36[F811]               push word [idw2]
  3154 000015EA E89AEB                   call pwi
  3155 000015ED 83C402                   add  sp, 2
  3156 000015F0 FF36[F011]               push word [idx2]
  3157 000015F4 E8FFEA                   call v
  3158 000015F7 83C402                   add  sp, 2
  3159                                  
  3160                                  ;-363       if (idx2 >= LSTART) { prs("; ");
  3161                                  
  3162 000015FA A1[F011]                 mov ax, [idx2]
  3163 000015FD 3DC800                   cmp ax, 200 ;unsigned : 1
  3164 00001600 0F8C2300                 jl  @@simplexpr148
  3165 00001604 68[1217]                 push simplexpr_15
  3166 00001607 E88838                   call prs
  3167 0000160A 83C402                   add  sp, 2
  3168                                  
  3169                                  ;-364        loadName(NA, idx2); prs(NA); }  }
  3170                                  
  3171 0000160D FF36[F011]               push word [idx2]
  3172 00001611 8D06[7200]               lea  ax, [NA]
  3173 00001615 50                       push ax
  3174 00001616 E82848                   call loadName
  3175 00001619 83C404                   add  sp, 4
  3176 0000161C 8D06[7200]               lea  ax, [NA]
  3177 00001620 50                       push ax
  3178 00001621 E86E38                   call prs
  3179 00001624 83C402                   add  sp, 2
  3180                                  @@simplexpr148:
  3181                                  
  3182                                  ;-365   }
  3183                                  
  3184                                  @@simplexpr147:
  3185                                  
  3186                                  ;-366 }
  3187                                  
  3188                                  @@simplexpr141:
  3189                                  
  3190                                  ;-367 char ops[6];
  3191                                  
  3192 00001627 C9                      @@retn: LEAVE
  3193 00001628 C3                       ret
  3194                                  ; ENDP
  3195 00001629 5C6E206D6F76206561-     simplexpr_0 db "\n mov eax, ",0
  3196 00001632 782C2000           
  3197 00001636 2026206973206E6F74-     simplexpr_1 db " & is not allowed in left side",0
  3198 0000163F 20616C6C6F77656420-
  3199 00001648 696E206C6566742073-
  3200 00001651 69646500           
  3201 00001655 4172726179206E6F74-     simplexpr_2 db "Array not allowed left side",0
  3202 0000165E 20616C6C6F77656420-
  3203 00001667 6C6566742073696465-
  3204 00001670 00                 
  3205 00001671 52656C6174696F6E61-     simplexpr_3 db "Relational not implemented yet",0
  3206 0000167A 6C206E6F7420696D70-
  3207 00001683 6C656D656E74656420-
  3208 0000168C 79657400           
  3209 00001690 41737369676E206578-     simplexpr_4 db "Assign expected",0
  3210 00001699 70656374656400     
  3211 000016A0 5C6E6D6F762000          simplexpr_5 db "\nmov ",0
  3212 000016A7 2C2000                  simplexpr_6 db ", ",0
  3213 000016AA 3B2000                  simplexpr_7 db "; ",0
  3214 000016AD 5C6E6D6F762000          simplexpr_8 db "\nmov ",0
  3215 000016B4 2C2000                  simplexpr_9 db ", ",0
  3216 000016B7 26206E6F7420616C6C-     simplexpr_10 db "& not allowed",0
  3217 000016C0 6F77656400         
  3218 000016C5 417272617920726967-     simplexpr_11 db "Array right side not implemented",0
  3219 000016CE 68742073696465206E-
  3220 000016D7 6F7420696D706C656D-
  3221 000016E0 656E74656400       
  3222 000016E6 5C6E6D6F762000          simplexpr_12 db "\nmov ",0
  3223 000016ED 4D656D20746F204D65-     simplexpr_13 db "Mem to Mem not allowed by x86-CPU",0
  3224 000016F6 6D206E6F7420616C6C-
  3225 000016FF 6F7765642062792078-
  3226 00001708 38362D43505500     
  3227 0000170F 2C2000                  simplexpr_14 db ", ",0
  3228 00001712 3B2000                  simplexpr_15 db "; ",0
  3229                                  section .bss
  3230                                  absolute 46858
  3231 0000B70A <res 00000006>          ops resb 6
  3232                                  section .text
  3233                                  
  3234                                  ;-368 int getop() { int i;              *ops=0;       _ un=0;
  3235                                  
  3236                                  
  3237                                  getop:  ; *** PROC ***
  3238                                  ;Function : getop, Number of local variables: 1
  3239                                  ;   # type sign width line used address name   list of local variables
  3240                                  ;  200 var sign word   368 NULL bp-2    i;
  3241 00001715 C8020000                 ENTER  2,0
  3242 00001719 66B800000000             mov eax, 0
  3243 0000171F 8B1E0AB7                 mov  bx, [ops]
  3244 00001723 8807                     mov  [bx], al
  3245 00001725 C606[E411]00            mov byte [un], 0
  3246                                  
  3247                                  ;-369   if (istoken('='))         {strcpy(ops, "mov");_ un=2;}
  3248                                  
  3249 0000172A 6A3D                     push 61
  3250 0000172C E81D36                   call istoken
  3251 0000172F 83C402                   add  sp, 2
  3252 00001732 08C0                     or  al, al
  3253 00001734 0F841300                 je @@getop149
  3254 00001738 68[A118]                 push getop_0
  3255 0000173B 8D060AB7                 lea  ax, [ops]
  3256 0000173F 50                       push ax
  3257 00001740 E8494A                   call strcpy
  3258 00001743 83C404                   add  sp, 4
  3259 00001746 C606[E411]02            mov byte [un], 2
  3260                                  
  3261                                  ;-370   if (istoken(T_PLUSASS))   {strcpy(ops, "add");_ un=2;}
  3262                                  
  3263                                  @@getop149:
  3264 0000174B 68CE04                   push 1230
  3265 0000174E E8FB35                   call istoken
  3266 00001751 83C402                   add  sp, 2
  3267 00001754 08C0                     or  al, al
  3268 00001756 0F841300                 je @@getop150
  3269 0000175A 68[A518]                 push getop_1
  3270 0000175D 8D060AB7                 lea  ax, [ops]
  3271 00001761 50                       push ax
  3272 00001762 E8274A                   call strcpy
  3273 00001765 83C404                   add  sp, 4
  3274 00001768 C606[E411]02            mov byte [un], 2
  3275                                  
  3276                                  ;-371   if (istoken(T_MINUSASS))  {strcpy(ops, "sub");_ un=2;}
  3277                                  
  3278                                  @@getop150:
  3279 0000176D 68CF04                   push 1231
  3280 00001770 E8D935                   call istoken
  3281 00001773 83C402                   add  sp, 2
  3282 00001776 08C0                     or  al, al
  3283 00001778 0F841300                 je @@getop151
  3284 0000177C 68[A918]                 push getop_2
  3285 0000177F 8D060AB7                 lea  ax, [ops]
  3286 00001783 50                       push ax
  3287 00001784 E8054A                   call strcpy
  3288 00001787 83C404                   add  sp, 4
  3289 0000178A C606[E411]02            mov byte [un], 2
  3290                                  
  3291                                  ;-372   if (istoken(T_ANDASS))    {strcpy(ops, "and");_ un=2;}
  3292                                  
  3293                                  @@getop151:
  3294 0000178F 68D204                   push 1234
  3295 00001792 E8B735                   call istoken
  3296 00001795 83C402                   add  sp, 2
  3297 00001798 08C0                     or  al, al
  3298 0000179A 0F841300                 je @@getop152
  3299 0000179E 68[AD18]                 push getop_3
  3300 000017A1 8D060AB7                 lea  ax, [ops]
  3301 000017A5 50                       push ax
  3302 000017A6 E8E349                   call strcpy
  3303 000017A9 83C404                   add  sp, 4
  3304 000017AC C606[E411]02            mov byte [un], 2
  3305                                  
  3306                                  ;-373   if (istoken(T_ORASS))     {strcpy(ops, "or" );_ un=2;}
  3307                                  
  3308                                  @@getop152:
  3309 000017B1 68D304                   push 1235
  3310 000017B4 E89535                   call istoken
  3311 000017B7 83C402                   add  sp, 2
  3312 000017BA 08C0                     or  al, al
  3313 000017BC 0F841300                 je @@getop153
  3314 000017C0 68[B118]                 push getop_4
  3315 000017C3 8D060AB7                 lea  ax, [ops]
  3316 000017C7 50                       push ax
  3317 000017C8 E8C149                   call strcpy
  3318 000017CB 83C404                   add  sp, 4
  3319 000017CE C606[E411]02            mov byte [un], 2
  3320                                  
  3321                                  ;-374   if (istoken(T_LESSLESS))  {strcpy(ops, "shl");_ un=2;}
  3322                                  
  3323                                  @@getop153:
  3324 000017D3 68D804                   push 1240
  3325 000017D6 E87335                   call istoken
  3326 000017D9 83C402                   add  sp, 2
  3327 000017DC 08C0                     or  al, al
  3328 000017DE 0F841300                 je @@getop154
  3329 000017E2 68[B418]                 push getop_5
  3330 000017E5 8D060AB7                 lea  ax, [ops]
  3331 000017E9 50                       push ax
  3332 000017EA E89F49                   call strcpy
  3333 000017ED 83C404                   add  sp, 4
  3334 000017F0 C606[E411]02            mov byte [un], 2
  3335                                  
  3336                                  ;-375   if (istoken(T_GREATGREAT)){strcpy(ops, "shr");_ un=2;}
  3337                                  
  3338                                  @@getop154:
  3339 000017F5 68D904                   push 1241
  3340 000017F8 E85135                   call istoken
  3341 000017FB 83C402                   add  sp, 2
  3342 000017FE 08C0                     or  al, al
  3343 00001800 0F841300                 je @@getop155
  3344 00001804 68[B818]                 push getop_6
  3345 00001807 8D060AB7                 lea  ax, [ops]
  3346 0000180B 50                       push ax
  3347 0000180C E87D49                   call strcpy
  3348 0000180F 83C404                   add  sp, 4
  3349 00001812 C606[E411]02            mov byte [un], 2
  3350                                  
  3351                                  ;-376   if (istoken(T_PLUSPLUS))  {strcpy(ops, "inc");_ un=1;}
  3352                                  
  3353                                  @@getop155:
  3354 00001817 68C304                   push 1219
  3355 0000181A E82F35                   call istoken
  3356 0000181D 83C402                   add  sp, 2
  3357 00001820 08C0                     or  al, al
  3358 00001822 0F841300                 je @@getop156
  3359 00001826 68[BC18]                 push getop_7
  3360 00001829 8D060AB7                 lea  ax, [ops]
  3361 0000182D 50                       push ax
  3362 0000182E E85B49                   call strcpy
  3363 00001831 83C404                   add  sp, 4
  3364 00001834 C606[E411]01            mov byte [un], 1
  3365                                  
  3366                                  ;-377   if (istoken(T_MINUSMINUS)){strcpy(ops, "dec");_ un=1;}
  3367                                  
  3368                                  @@getop156:
  3369 00001839 68C904                   push 1225
  3370 0000183C E80D35                   call istoken
  3371 0000183F 83C402                   add  sp, 2
  3372 00001842 08C0                     or  al, al
  3373 00001844 0F841300                 je @@getop157
  3374 00001848 68[C018]                 push getop_8
  3375 0000184B 8D060AB7                 lea  ax, [ops]
  3376 0000184F 50                       push ax
  3377 00001850 E83949                   call strcpy
  3378 00001853 83C404                   add  sp, 4
  3379 00001856 C606[E411]01            mov byte [un], 1
  3380                                  
  3381                                  ;-378   if (istoken(T_MULASS))    {strcpy(ops, "mul");_ un=1;}//reg only
  3382                                  
  3383                                  @@getop157:
  3384 0000185B 68D004                   push 1232
  3385 0000185E E8EB34                   call istoken
  3386 00001861 83C402                   add  sp, 2
  3387 00001864 08C0                     or  al, al
  3388 00001866 0F841300                 je @@getop158
  3389 0000186A 68[C418]                 push getop_9
  3390 0000186D 8D060AB7                 lea  ax, [ops]
  3391 00001871 50                       push ax
  3392 00001872 E81749                   call strcpy
  3393 00001875 83C404                   add  sp, 4
  3394 00001878 C606[E411]01            mov byte [un], 1
  3395                                  
  3396                                  ;-379   if (istoken(T_DIVASS))    {strcpy(ops, "div");_ un=1;}//reg only
  3397                                  
  3398                                  @@getop158:
  3399 0000187D 68D104                   push 1233
  3400 00001880 E8C934                   call istoken
  3401 00001883 83C402                   add  sp, 2
  3402 00001886 08C0                     or  al, al
  3403 00001888 0F841300                 je @@getop159
  3404 0000188C 68[C818]                 push getop_10
  3405 0000188F 8D060AB7                 lea  ax, [ops]
  3406 00001893 50                       push ax
  3407 00001894 E8F548                   call strcpy
  3408 00001897 83C404                   add  sp, 4
  3409 0000189A C606[E411]01            mov byte [un], 1
  3410                                  
  3411                                  ;-380 /*  if (istoken('+'))         {strcpy(ops, "add");_ un=3;}
  3412                                  
  3413                                  
  3414                                  ;-381   if (istoken('-'))         {strcpy(ops, "sub");_ un=3;}
  3415                                  
  3416                                  
  3417                                  ;-382   if (istoken('&'))         {strcpy(ops, "and");_ un=3;}
  3418                                  
  3419                                  
  3420                                  ;-383   if (istoken('|'))         {strcpy(ops, "or" );_ un=3;}
  3421                                  
  3422                                  
  3423                                  ;-384   if (istoken('*'))                             _ un=4;
  3424                                  
  3425                                  
  3426                                  ;-385   if (istoken('%'))                             _ un=5;
  3427                                  
  3428                                  
  3429                                  ;-386   if (istoken('/'))                             _ un=6;     */
  3430                                  
  3431                                  
  3432                                  ;-387 }
  3433                                  
  3434                                  @@getop159:
  3435                                  
  3436                                  ;-388 int prleftreg() { prnl(); prs(ops); prs("  "); printreg(irg1, mod1); }
  3437 0000189F C9                       LEAVE
  3438 000018A0 C3                       ret
  3439                                  ; ENDP
  3440 000018A1 6D6F7600                getop_0 db "mov",0
  3441 000018A5 61646400                getop_1 db "add",0
  3442 000018A9 73756200                getop_2 db "sub",0
  3443 000018AD 616E6400                getop_3 db "and",0
  3444 000018B1 6F7200                  getop_4 db "or",0
  3445 000018B4 73686C00                getop_5 db "shl",0
  3446 000018B8 73687200                getop_6 db "shr",0
  3447 000018BC 696E6300                getop_7 db "inc",0
  3448 000018C0 64656300                getop_8 db "dec",0
  3449 000018C4 6D756C00                getop_9 db "mul",0
  3450 000018C8 64697600                getop_10 db "div",0
  3451                                  
  3452                                  prleftreg:  ; *** PROC ***
  3453 000018CC E8B535                   call prnl
  3454 000018CF 8D060AB7                 lea  ax, [ops]
  3455 000018D3 50                       push ax
  3456 000018D4 E8BB35                   call prs
  3457 000018D7 83C402                   add  sp, 2
  3458 000018DA 68[F218]                 push prleftreg_0
  3459 000018DD E8B235                   call prs
  3460 000018E0 83C402                   add  sp, 2
  3461 000018E3 FF36[E611]               push word [mod1]
  3462 000018E7 FF36[EA11]               push word [irg1]
  3463 000018EB E85706                   call printreg
  3464 000018EE 83C404                   add  sp, 4
  3465                                  
  3466                                  ;-389 int prleftvar() { }
  3467                                  
  3468 000018F1 C3                       ret
  3469                                  ; ENDP
  3470 000018F2 202000                  prleftreg_0 db "  ",0
  3471                                  
  3472                                  prleftvar:  ; *** PROC ***
  3473                                  
  3474                                  ;-390 
  3475                                  
  3476                                  
  3477                                  ;-391 int printvar(int i, int mode) {  }
  3478                                  
  3479 000018F5 C3                       ret
  3480                                  ; ENDP
  3481                                  
  3482                                  printvar:  ; *** PROC ***
  3483                                  ;Function : printvar, Number of local variables: 2
  3484                                  ;   # type sign width line used address name   list of local variables
  3485                                  ;  200 var sign word   391 NULL bp+4    i
  3486                                  ;  201 var sign word   391 NULL bp+6    mode;
  3487 000018F6 C8000000                 ENTER  0,0
  3488                                  
  3489                                  ;-392 
  3490                                  
  3491                                  
  3492                                  ;-393 int doreg1(int iscmp1) { int i; char sz;
  3493 000018FA C9                       LEAVE
  3494 000018FB C3                       ret
  3495                                  ; ENDP
  3496                                  
  3497                                  doreg1:  ; *** PROC ***
  3498                                  
  3499                                  ;-394   getop();
  3500                                  
  3501                                  ;Function : doreg1, Number of local variables: 3
  3502                                  ;   # type sign width line used address name   list of local variables
  3503                                  ;  200 var sign word   393 NULL bp+4    iscmp1
  3504                                  ;  201 var sign word   393 NULL bp-2    i
  3505                                  ;  202 var sign byte   393 NULL bp-4    sz;
  3506 000018FC C8040000                 ENTER  4,0
  3507 00001900 E812FE                   call getop
  3508                                  
  3509                                  ;-395   if (iscmp1 == 1) {
  3510                                  
  3511 00001903 8B4604                   mov ax, [bp+4]
  3512 00001906 83F801                   cmp ax, 1
  3513 00001909 0F852700                 jne @@doreg1160
  3514                                  
  3515                                  ;-396       token=getlex();
  3516                                  
  3517 0000190D E89C2B                   call getlex
  3518 00001910 A3[3D00]                 mov word [token], ax
  3519                                  
  3520                                  ;-397       if (isrelational() ==0) error1("Relational expected");
  3521                                  
  3522 00001913 E8292B                   call isrelational
  3523 00001916 83F800                   cmp ax, 0
  3524 00001919 0F850900                 jne @@doreg1161
  3525 0000191D 68[A01A]                 push doreg1_0
  3526 00001920 E87038                   call error1
  3527 00001923 83C402                   add  sp, 2
  3528                                  
  3529                                  ;-398       strcpy(ops, "cmp");
  3530                                  
  3531                                  @@doreg1161:
  3532 00001926 68[B41A]                 push doreg1_1
  3533 00001929 8D060AB7                 lea  ax, [ops]
  3534 0000192D 50                       push ax
  3535 0000192E E85B48                   call strcpy
  3536 00001931 83C404                   add  sp, 4
  3537                                  
  3538                                  ;-399       }
  3539                                  
  3540                                  
  3541                                  ;-400   if (un==1) { prleftreg(); return; }
  3542                                  
  3543                                  @@doreg1160:
  3544 00001934 A0[E411]                 mov al, [un]
  3545 00001937 3C01                     cmp al, 1
  3546 00001939 0F850600                 jne @@doreg1162
  3547 0000193D E88CFF                   call prleftreg
  3548 00001940 E95B01                   jmp @@retn
  3549                                  
  3550                                  ;-401 
  3551                                  
  3552                                  
  3553                                  ;-402   if (istoken(T_CONST)) {prleftreg(); prs(", "); prL(lexvalL); goto reg1;}
  3554                                  
  3555                                  @@doreg1162:
  3556 00001943 680101                   push 257
  3557 00001946 E80334                   call istoken
  3558 00001949 83C402                   add  sp, 2
  3559 0000194C 08C0                     or  al, al
  3560 0000194E 0F841A00                 je @@doreg1163
  3561 00001952 E877FF                   call prleftreg
  3562 00001955 68[B81A]                 push doreg1_2
  3563 00001958 E83735                   call prs
  3564 0000195B 83C402                   add  sp, 2
  3565 0000195E 66FF36[4D00]             push dword [lexvalL]
  3566 00001963 E8DF45                   call prL
  3567 00001966 83C404                   add  sp, 4
  3568 00001969 E90D01                   jmp @@reg1
  3569                                  
  3570                                  ;-403   
  3571                                  
  3572                                  
  3573                                  ;-404   mod2=typeName(); irg2=checkreg();
  3574                                  
  3575                                  @@doreg1163:
  3576 0000196C E8E7F1                   call typeName
  3577 0000196F A3[E811]                 mov word [mod2], ax
  3578 00001972 E85C01                   call checkreg
  3579 00001975 A3[EC11]                 mov word [irg2], ax
  3580                                  
  3581                                  ;-405   if (irg2) {prleftreg(); prs(", "); printreg(irg2, mod2); goto reg1;}
  3582                                  
  3583 00001978 A1[EC11]                 mov ax, [irg2]
  3584 0000197B 08C0                     or  al, al
  3585 0000197D 0F841D00                 je @@doreg1164
  3586 00001981 E848FF                   call prleftreg
  3587 00001984 68[BB1A]                 push doreg1_3
  3588 00001987 E80835                   call prs
  3589 0000198A 83C402                   add  sp, 2
  3590 0000198D FF36[E811]               push word [mod2]
  3591 00001991 FF36[EC11]               push word [irg2]
  3592 00001995 E8AD05                   call printreg
  3593 00001998 83C404                   add  sp, 4
  3594 0000199B E9DB00                   jmp @@reg1
  3595                                  
  3596                                  ;-406 
  3597                                  
  3598                                  
  3599                                  ;-407   i=searchname();
  3600                                  
  3601                                  @@doreg1164:
  3602 0000199E E8D5F0                   call searchname
  3603 000019A1 8946FE                   mov word [bp-2], ax
  3604                                  
  3605                                  ;-408   gettypes(i);
  3606                                  
  3607 000019A4 FF76FE                   push word [bp-2]
  3608 000019A7 E881EF                   call gettypes
  3609 000019AA 83C402                   add  sp, 2
  3610                                  
  3611                                  ;-409   ids2=signi; idw2=wi; idt2=typei;
  3612                                  
  3613 000019AD A1[5600]                 mov ax, [signi]
  3614 000019B0 A3[F411]                 mov word [ids2], ax
  3615 000019B3 A1[5B00]                 mov ax, [wi]
  3616 000019B6 A3[F811]                 mov word [idw2], ax
  3617 000019B9 A1[5300]                 mov ax, [typei]
  3618 000019BC A3[FC11]                 mov word [idt2], ax
  3619                                  
  3620                                  ;-410   if (eqstr(ops,"mov")) {_ sz=0;
  3621                                  
  3622 000019BF 68[BE1A]                 push doreg1_4
  3623 000019C2 8D060AB7                 lea  ax, [ops]
  3624 000019C6 50                       push ax
  3625 000019C7 E81649                   call eqstr
  3626 000019CA 83C404                   add  sp, 4
  3627 000019CD 08C0                     or  al, al
  3628 000019CF 0F842E00                 je @@doreg1165
  3629 000019D3 C646FC00                mov byte [bp-4], 0; sz
  3630                                  
  3631                                  ;-411     if (_ irg1 >= 17) if (_ idw2==1) sz++;  // cast byte to word or dword
  3632                                  
  3633 000019D7 833E[EA11]11            cmp word [irg1], 17 ;unsigned : 1
  3634 000019DC 0F8C0C00                 jl  @@doreg1166
  3635 000019E0 833E[F811]01            cmp word [idw2], 1
  3636 000019E5 0F850300                 jne @@doreg1167
  3637 000019E9 FE46FC                   inc  byte [bp-4]
  3638                                  
  3639                                  ;-412     if (_ irg1 >= 47) if (_ idw2==2) sz++; }// cast word to dword
  3640                                  
  3641                                  @@doreg1167:
  3642                                  @@doreg1166:
  3643 000019EC 833E[EA11]2F            cmp word [irg1], 47 ;unsigned : 1
  3644 000019F1 0F8C0C00                 jl  @@doreg1168
  3645 000019F5 833E[F811]02            cmp word [idw2], 2
  3646 000019FA 0F850300                 jne @@doreg1169
  3647 000019FE FE46FC                   inc  byte [bp-4]
  3648                                  @@doreg1169:
  3649                                  @@doreg1168:
  3650                                  
  3651                                  ;-413   if (sz) { if (ids2) strcpy(ops,"movsx"); else strcpy(ops,"movzx"); }
  3652                                  
  3653                                  @@doreg1165:
  3654 00001A01 8A46FC                   mov al, [bp-4]
  3655 00001A04 08C0                     or  al, al
  3656 00001A06 0F842800                 je @@doreg1170
  3657 00001A0A A1[F411]                 mov ax, [ids2]
  3658 00001A0D 08C0                     or  al, al
  3659 00001A0F 0F841100                 je @@doreg1171
  3660 00001A13 68[C21A]                 push doreg1_5
  3661 00001A16 8D060AB7                 lea  ax, [ops]
  3662 00001A1A 50                       push ax
  3663 00001A1B E86E47                   call strcpy
  3664 00001A1E 83C404                   add  sp, 4
  3665 00001A21 E90E00                   jmp @@doreg1172
  3666                                  @@doreg1171:
  3667 00001A24 68[C81A]                 push doreg1_6
  3668 00001A27 8D060AB7                 lea  ax, [ops]
  3669 00001A2B 50                       push ax
  3670 00001A2C E85D47                   call strcpy
  3671 00001A2F 83C404                   add  sp, 4
  3672                                  @@doreg1172:
  3673                                  
  3674                                  ;-414 
  3675                                  
  3676                                  
  3677                                  ;-415   prleftreg(); prs(", ");
  3678                                  
  3679                                  @@doreg1170:
  3680 00001A32 E897FE                   call prleftreg
  3681 00001A35 68[CE1A]                 push doreg1_7
  3682 00001A38 E85734                   call prs
  3683 00001A3B 83C402                   add  sp, 2
  3684                                  
  3685                                  ;-416   
  3686                                  
  3687                                  
  3688                                  ;-417 //  if (sz) { if (_ idw2==1) prs("byte "); else prs("word "); }
  3689                                  
  3690                                  
  3691                                  ;-418   if (sz) pwi(idw2);
  3692                                  
  3693 00001A3E 8A46FC                   mov al, [bp-4]
  3694 00001A41 08C0                     or  al, al
  3695 00001A43 0F840A00                 je @@doreg1173
  3696 00001A47 FF36[F811]               push word [idw2]
  3697 00001A4B E839E7                   call pwi
  3698 00001A4E 83C402                   add  sp, 2
  3699                                  
  3700                                  ;-419   
  3701                                  
  3702                                  
  3703                                  ;-420   if (mod2 == 2) a(i);
  3704                                  
  3705                                  @@doreg1173:
  3706 00001A51 A1[E811]                 mov ax, [mod2]
  3707 00001A54 83F802                   cmp ax, 2
  3708 00001A57 0F850C00                 jne @@doreg1174
  3709 00001A5B FF76FE                   push word [bp-2]
  3710 00001A5E E86CE6                   call a
  3711 00001A61 83C402                   add  sp, 2
  3712                                  
  3713                                  ;-421   else  {pwi(i); v(i); }
  3714                                  
  3715 00001A64 E91200                   jmp @@doreg1175
  3716                                  @@doreg1174:
  3717 00001A67 FF76FE                   push word [bp-2]
  3718 00001A6A E81AE7                   call pwi
  3719 00001A6D 83C402                   add  sp, 2
  3720 00001A70 FF76FE                   push word [bp-2]
  3721 00001A73 E880E6                   call v
  3722 00001A76 83C402                   add  sp, 2
  3723                                  
  3724                                  ;-422   reg1: if (iscmp1 == 1) {cmpneg(0); prs(fname); expect(')'); }
  3725                                  
  3726                                  @@doreg1175:
  3727                                  @@reg1:
  3728 00001A79 8B4604                   mov ax, [bp+4]
  3729 00001A7C 83F801                   cmp ax, 1
  3730 00001A7F 0F851B00                 jne @@doreg1176
  3731 00001A83 6A00                     push 0
  3732 00001A85 E86826                   call cmpneg
  3733 00001A88 83C402                   add  sp, 2
  3734 00001A8B 8D06EE81                 lea  ax, [fname]
  3735 00001A8F 50                       push ax
  3736 00001A90 E8FF33                   call prs
  3737 00001A93 83C402                   add  sp, 2
  3738 00001A96 6A29                     push 41
  3739 00001A98 E8D932                   call expect
  3740 00001A9B 83C402                   add  sp, 2
  3741                                  
  3742                                  ;-423 }
  3743                                  
  3744                                  @@doreg1176:
  3745                                  
  3746                                  ;-424 int checkreg() { // >=17 = 16bit, >=47 = 32bit
  3747                                  
  3748 00001A9E C9                      @@retn: LEAVE
  3749 00001A9F C3                       ret
  3750                                  ; ENDP
  3751 00001AA0 52656C6174696F6E61-     doreg1_0 db "Relational expected",0
  3752 00001AA9 6C2065787065637465-
  3753 00001AB2 6400               
  3754 00001AB4 636D7000                doreg1_1 db "cmp",0
  3755 00001AB8 2C2000                  doreg1_2 db ", ",0
  3756 00001ABB 2C2000                  doreg1_3 db ", ",0
  3757 00001ABE 6D6F7600                doreg1_4 db "mov",0
  3758 00001AC2 6D6F76737800            doreg1_5 db "movsx",0
  3759 00001AC8 6D6F767A7800            doreg1_6 db "movzx",0
  3760 00001ACE 2C2000                  doreg1_7 db ", ",0
  3761                                  
  3762                                  checkreg:  ; *** PROC ***
  3763                                  
  3764                                  ;-425   if (strlen(symbol) <  2) return 0;
  3765                                  
  3766 00001AD1 8D066E81                 lea  ax, [symbol]
  3767 00001AD5 50                       push ax
  3768 00001AD6 E88846                   call strlen
  3769 00001AD9 83C402                   add  sp, 2
  3770 00001ADC 83F802                   cmp ax, 2
  3771 00001ADF 0F8D0900                 jge @@checkreg177
  3772 00001AE3 66B800000000             mov eax, 0
  3773 00001AE9 E9A703                   jmp @@retn
  3774                                  
  3775                                  ;-426   if (eqstr(symbol,"al")) return 1;   if (eqstr(symbol,"cl")) return 3;
  3776                                  
  3777                                  @@checkreg177:
  3778 00001AEC 68[941E]                 push checkreg_0
  3779 00001AEF 8D066E81                 lea  ax, [symbol]
  3780 00001AF3 50                       push ax
  3781 00001AF4 E8E947                   call eqstr
  3782 00001AF7 83C404                   add  sp, 4
  3783 00001AFA 08C0                     or  al, al
  3784 00001AFC 0F840900                 je @@checkreg178
  3785 00001B00 66B801000000             mov eax, 1
  3786 00001B06 E98A03                   jmp @@retn
  3787                                  @@checkreg178:
  3788 00001B09 68[971E]                 push checkreg_1
  3789 00001B0C 8D066E81                 lea  ax, [symbol]
  3790 00001B10 50                       push ax
  3791 00001B11 E8CC47                   call eqstr
  3792 00001B14 83C404                   add  sp, 4
  3793 00001B17 08C0                     or  al, al
  3794 00001B19 0F840900                 je @@checkreg179
  3795 00001B1D 66B803000000             mov eax, 3
  3796 00001B23 E96D03                   jmp @@retn
  3797                                  
  3798                                  ;-427   if (eqstr(symbol,"dl")) return 5;   if (eqstr(symbol,"bl")) return 7;
  3799                                  
  3800                                  @@checkreg179:
  3801 00001B26 68[9A1E]                 push checkreg_2
  3802 00001B29 8D066E81                 lea  ax, [symbol]
  3803 00001B2D 50                       push ax
  3804 00001B2E E8AF47                   call eqstr
  3805 00001B31 83C404                   add  sp, 4
  3806 00001B34 08C0                     or  al, al
  3807 00001B36 0F840900                 je @@checkreg180
  3808 00001B3A 66B805000000             mov eax, 5
  3809 00001B40 E95003                   jmp @@retn
  3810                                  @@checkreg180:
  3811 00001B43 68[9D1E]                 push checkreg_3
  3812 00001B46 8D066E81                 lea  ax, [symbol]
  3813 00001B4A 50                       push ax
  3814 00001B4B E89247                   call eqstr
  3815 00001B4E 83C404                   add  sp, 4
  3816 00001B51 08C0                     or  al, al
  3817 00001B53 0F840900                 je @@checkreg181
  3818 00001B57 66B807000000             mov eax, 7
  3819 00001B5D E93303                   jmp @@retn
  3820                                  
  3821                                  ;-428   if (eqstr(symbol,"ah")) return 9;   if (eqstr(symbol,"ch")) return 11;
  3822                                  
  3823                                  @@checkreg181:
  3824 00001B60 68[A01E]                 push checkreg_4
  3825 00001B63 8D066E81                 lea  ax, [symbol]
  3826 00001B67 50                       push ax
  3827 00001B68 E87547                   call eqstr
  3828 00001B6B 83C404                   add  sp, 4
  3829 00001B6E 08C0                     or  al, al
  3830 00001B70 0F840900                 je @@checkreg182
  3831 00001B74 66B809000000             mov eax, 9
  3832 00001B7A E91603                   jmp @@retn
  3833                                  @@checkreg182:
  3834 00001B7D 68[A31E]                 push checkreg_5
  3835 00001B80 8D066E81                 lea  ax, [symbol]
  3836 00001B84 50                       push ax
  3837 00001B85 E85847                   call eqstr
  3838 00001B88 83C404                   add  sp, 4
  3839 00001B8B 08C0                     or  al, al
  3840 00001B8D 0F840900                 je @@checkreg183
  3841 00001B91 66B80B000000             mov eax, 11
  3842 00001B97 E9F902                   jmp @@retn
  3843                                  
  3844                                  ;-429   if (eqstr(symbol,"dh")) return 13;  if (eqstr(symbol,"bh")) return 15;
  3845                                  
  3846                                  @@checkreg183:
  3847 00001B9A 68[A61E]                 push checkreg_6
  3848 00001B9D 8D066E81                 lea  ax, [symbol]
  3849 00001BA1 50                       push ax
  3850 00001BA2 E83B47                   call eqstr
  3851 00001BA5 83C404                   add  sp, 4
  3852 00001BA8 08C0                     or  al, al
  3853 00001BAA 0F840900                 je @@checkreg184
  3854 00001BAE 66B80D000000             mov eax, 13
  3855 00001BB4 E9DC02                   jmp @@retn
  3856                                  @@checkreg184:
  3857 00001BB7 68[A91E]                 push checkreg_7
  3858 00001BBA 8D066E81                 lea  ax, [symbol]
  3859 00001BBE 50                       push ax
  3860 00001BBF E81E47                   call eqstr
  3861 00001BC2 83C404                   add  sp, 4
  3862 00001BC5 08C0                     or  al, al
  3863 00001BC7 0F840900                 je @@checkreg185
  3864 00001BCB 66B80F000000             mov eax, 15
  3865 00001BD1 E9BF02                   jmp @@retn
  3866                                  
  3867                                  ;-430   if (eqstr(symbol,"ax")) return 17;  if (eqstr(symbol,"cx")) return 19;
  3868                                  
  3869                                  @@checkreg185:
  3870 00001BD4 68[AC1E]                 push checkreg_8
  3871 00001BD7 8D066E81                 lea  ax, [symbol]
  3872 00001BDB 50                       push ax
  3873 00001BDC E80147                   call eqstr
  3874 00001BDF 83C404                   add  sp, 4
  3875 00001BE2 08C0                     or  al, al
  3876 00001BE4 0F840900                 je @@checkreg186
  3877 00001BE8 66B811000000             mov eax, 17
  3878 00001BEE E9A202                   jmp @@retn
  3879                                  @@checkreg186:
  3880 00001BF1 68[AF1E]                 push checkreg_9
  3881 00001BF4 8D066E81                 lea  ax, [symbol]
  3882 00001BF8 50                       push ax
  3883 00001BF9 E8E446                   call eqstr
  3884 00001BFC 83C404                   add  sp, 4
  3885 00001BFF 08C0                     or  al, al
  3886 00001C01 0F840900                 je @@checkreg187
  3887 00001C05 66B813000000             mov eax, 19
  3888 00001C0B E98502                   jmp @@retn
  3889                                  
  3890                                  ;-431   if (eqstr(symbol,"dx")) return 21;  if (eqstr(symbol,"bx")) return 23;
  3891                                  
  3892                                  @@checkreg187:
  3893 00001C0E 68[B21E]                 push checkreg_10
  3894 00001C11 8D066E81                 lea  ax, [symbol]
  3895 00001C15 50                       push ax
  3896 00001C16 E8C746                   call eqstr
  3897 00001C19 83C404                   add  sp, 4
  3898 00001C1C 08C0                     or  al, al
  3899 00001C1E 0F840900                 je @@checkreg188
  3900 00001C22 66B815000000             mov eax, 21
  3901 00001C28 E96802                   jmp @@retn
  3902                                  @@checkreg188:
  3903 00001C2B 68[B51E]                 push checkreg_11
  3904 00001C2E 8D066E81                 lea  ax, [symbol]
  3905 00001C32 50                       push ax
  3906 00001C33 E8AA46                   call eqstr
  3907 00001C36 83C404                   add  sp, 4
  3908 00001C39 08C0                     or  al, al
  3909 00001C3B 0F840900                 je @@checkreg189
  3910 00001C3F 66B817000000             mov eax, 23
  3911 00001C45 E94B02                   jmp @@retn
  3912                                  
  3913                                  ;-432   if (eqstr(symbol,"sp")) return 25;  if (eqstr(symbol,"bp")) return 27;
  3914                                  
  3915                                  @@checkreg189:
  3916 00001C48 68[B81E]                 push checkreg_12
  3917 00001C4B 8D066E81                 lea  ax, [symbol]
  3918 00001C4F 50                       push ax
  3919 00001C50 E88D46                   call eqstr
  3920 00001C53 83C404                   add  sp, 4
  3921 00001C56 08C0                     or  al, al
  3922 00001C58 0F840900                 je @@checkreg190
  3923 00001C5C 66B819000000             mov eax, 25
  3924 00001C62 E92E02                   jmp @@retn
  3925                                  @@checkreg190:
  3926 00001C65 68[BB1E]                 push checkreg_13
  3927 00001C68 8D066E81                 lea  ax, [symbol]
  3928 00001C6C 50                       push ax
  3929 00001C6D E87046                   call eqstr
  3930 00001C70 83C404                   add  sp, 4
  3931 00001C73 08C0                     or  al, al
  3932 00001C75 0F840900                 je @@checkreg191
  3933 00001C79 66B81B000000             mov eax, 27
  3934 00001C7F E91102                   jmp @@retn
  3935                                  
  3936                                  ;-433   if (eqstr(symbol,"si")) return 29;  if (eqstr(symbol,"di")) return 31;
  3937                                  
  3938                                  @@checkreg191:
  3939 00001C82 68[BE1E]                 push checkreg_14
  3940 00001C85 8D066E81                 lea  ax, [symbol]
  3941 00001C89 50                       push ax
  3942 00001C8A E85346                   call eqstr
  3943 00001C8D 83C404                   add  sp, 4
  3944 00001C90 08C0                     or  al, al
  3945 00001C92 0F840900                 je @@checkreg192
  3946 00001C96 66B81D000000             mov eax, 29
  3947 00001C9C E9F401                   jmp @@retn
  3948                                  @@checkreg192:
  3949 00001C9F 68[C11E]                 push checkreg_15
  3950 00001CA2 8D066E81                 lea  ax, [symbol]
  3951 00001CA6 50                       push ax
  3952 00001CA7 E83646                   call eqstr
  3953 00001CAA 83C404                   add  sp, 4
  3954 00001CAD 08C0                     or  al, al
  3955 00001CAF 0F840900                 je @@checkreg193
  3956 00001CB3 66B81F000000             mov eax, 31
  3957 00001CB9 E9D701                   jmp @@retn
  3958                                  
  3959                                  ;-434   if (eqstr(symbol,"es")) return 33;  if (eqstr(symbol,"cs")) return 35;
  3960                                  
  3961                                  @@checkreg193:
  3962 00001CBC 68[C41E]                 push checkreg_16
  3963 00001CBF 8D066E81                 lea  ax, [symbol]
  3964 00001CC3 50                       push ax
  3965 00001CC4 E81946                   call eqstr
  3966 00001CC7 83C404                   add  sp, 4
  3967 00001CCA 08C0                     or  al, al
  3968 00001CCC 0F840900                 je @@checkreg194
  3969 00001CD0 66B821000000             mov eax, 33
  3970 00001CD6 E9BA01                   jmp @@retn
  3971                                  @@checkreg194:
  3972 00001CD9 68[C71E]                 push checkreg_17
  3973 00001CDC 8D066E81                 lea  ax, [symbol]
  3974 00001CE0 50                       push ax
  3975 00001CE1 E8FC45                   call eqstr
  3976 00001CE4 83C404                   add  sp, 4
  3977 00001CE7 08C0                     or  al, al
  3978 00001CE9 0F840900                 je @@checkreg195
  3979 00001CED 66B823000000             mov eax, 35
  3980 00001CF3 E99D01                   jmp @@retn
  3981                                  
  3982                                  ;-435   if (eqstr(symbol,"ss")) return 37;  if (eqstr(symbol,"ds")) return 39;
  3983                                  
  3984                                  @@checkreg195:
  3985 00001CF6 68[CA1E]                 push checkreg_18
  3986 00001CF9 8D066E81                 lea  ax, [symbol]
  3987 00001CFD 50                       push ax
  3988 00001CFE E8DF45                   call eqstr
  3989 00001D01 83C404                   add  sp, 4
  3990 00001D04 08C0                     or  al, al
  3991 00001D06 0F840900                 je @@checkreg196
  3992 00001D0A 66B825000000             mov eax, 37
  3993 00001D10 E98001                   jmp @@retn
  3994                                  @@checkreg196:
  3995 00001D13 68[CD1E]                 push checkreg_19
  3996 00001D16 8D066E81                 lea  ax, [symbol]
  3997 00001D1A 50                       push ax
  3998 00001D1B E8C245                   call eqstr
  3999 00001D1E 83C404                   add  sp, 4
  4000 00001D21 08C0                     or  al, al
  4001 00001D23 0F840900                 je @@checkreg197
  4002 00001D27 66B827000000             mov eax, 39
  4003 00001D2D E96301                   jmp @@retn
  4004                                  
  4005                                  ;-436   if (eqstr(symbol,"fs")) return 41;  if (eqstr(symbol,"gs")) return 43;
  4006                                  
  4007                                  @@checkreg197:
  4008 00001D30 68[D01E]                 push checkreg_20
  4009 00001D33 8D066E81                 lea  ax, [symbol]
  4010 00001D37 50                       push ax
  4011 00001D38 E8A545                   call eqstr
  4012 00001D3B 83C404                   add  sp, 4
  4013 00001D3E 08C0                     or  al, al
  4014 00001D40 0F840900                 je @@checkreg198
  4015 00001D44 66B829000000             mov eax, 41
  4016 00001D4A E94601                   jmp @@retn
  4017                                  @@checkreg198:
  4018 00001D4D 68[D31E]                 push checkreg_21
  4019 00001D50 8D066E81                 lea  ax, [symbol]
  4020 00001D54 50                       push ax
  4021 00001D55 E88845                   call eqstr
  4022 00001D58 83C404                   add  sp, 4
  4023 00001D5B 08C0                     or  al, al
  4024 00001D5D 0F840900                 je @@checkreg199
  4025 00001D61 66B82B000000             mov eax, 43
  4026 00001D67 E92901                   jmp @@retn
  4027                                  
  4028                                  ;-437   // (eqstr(symbol,"ip")) return 45;
  4029                                  
  4030                                  
  4031                                  ;-438   if (strlen(symbol) >   3) return 0;
  4032                                  
  4033                                  @@checkreg199:
  4034 00001D6A 8D066E81                 lea  ax, [symbol]
  4035 00001D6E 50                       push ax
  4036 00001D6F E8EF43                   call strlen
  4037 00001D72 83C402                   add  sp, 2
  4038 00001D75 83F803                   cmp ax, 3
  4039 00001D78 0F8E0900                 jle @@checkreg200
  4040 00001D7C 66B800000000             mov eax, 0
  4041 00001D82 E90E01                   jmp @@retn
  4042                                  
  4043                                  ;-439   if (eqstr(symbol,"eax")) return 47; if (eqstr(symbol,"ecx")) return 50;
  4044                                  
  4045                                  @@checkreg200:
  4046 00001D85 68[D61E]                 push checkreg_22
  4047 00001D88 8D066E81                 lea  ax, [symbol]
  4048 00001D8C 50                       push ax
  4049 00001D8D E85045                   call eqstr
  4050 00001D90 83C404                   add  sp, 4
  4051 00001D93 08C0                     or  al, al
  4052 00001D95 0F840900                 je @@checkreg201
  4053 00001D99 66B82F000000             mov eax, 47
  4054 00001D9F E9F100                   jmp @@retn
  4055                                  @@checkreg201:
  4056 00001DA2 68[DA1E]                 push checkreg_23
  4057 00001DA5 8D066E81                 lea  ax, [symbol]
  4058 00001DA9 50                       push ax
  4059 00001DAA E83345                   call eqstr
  4060 00001DAD 83C404                   add  sp, 4
  4061 00001DB0 08C0                     or  al, al
  4062 00001DB2 0F840900                 je @@checkreg202
  4063 00001DB6 66B832000000             mov eax, 50
  4064 00001DBC E9D400                   jmp @@retn
  4065                                  
  4066                                  ;-440   if (eqstr(symbol,"edx")) return 53; if (eqstr(symbol,"ebx")) return 56;
  4067                                  
  4068                                  @@checkreg202:
  4069 00001DBF 68[DE1E]                 push checkreg_24
  4070 00001DC2 8D066E81                 lea  ax, [symbol]
  4071 00001DC6 50                       push ax
  4072 00001DC7 E81645                   call eqstr
  4073 00001DCA 83C404                   add  sp, 4
  4074 00001DCD 08C0                     or  al, al
  4075 00001DCF 0F840900                 je @@checkreg203
  4076 00001DD3 66B835000000             mov eax, 53
  4077 00001DD9 E9B700                   jmp @@retn
  4078                                  @@checkreg203:
  4079 00001DDC 68[E21E]                 push checkreg_25
  4080 00001DDF 8D066E81                 lea  ax, [symbol]
  4081 00001DE3 50                       push ax
  4082 00001DE4 E8F944                   call eqstr
  4083 00001DE7 83C404                   add  sp, 4
  4084 00001DEA 08C0                     or  al, al
  4085 00001DEC 0F840900                 je @@checkreg204
  4086 00001DF0 66B838000000             mov eax, 56
  4087 00001DF6 E99A00                   jmp @@retn
  4088                                  
  4089                                  ;-441   if (eqstr(symbol,"esp")) return 59; if (eqstr(symbol,"ebp")) return 62;
  4090                                  
  4091                                  @@checkreg204:
  4092 00001DF9 68[E61E]                 push checkreg_26
  4093 00001DFC 8D066E81                 lea  ax, [symbol]
  4094 00001E00 50                       push ax
  4095 00001E01 E8DC44                   call eqstr
  4096 00001E04 83C404                   add  sp, 4
  4097 00001E07 08C0                     or  al, al
  4098 00001E09 0F840900                 je @@checkreg205
  4099 00001E0D 66B83B000000             mov eax, 59
  4100 00001E13 E97D00                   jmp @@retn
  4101                                  @@checkreg205:
  4102 00001E16 68[EA1E]                 push checkreg_27
  4103 00001E19 8D066E81                 lea  ax, [symbol]
  4104 00001E1D 50                       push ax
  4105 00001E1E E8BF44                   call eqstr
  4106 00001E21 83C404                   add  sp, 4
  4107 00001E24 08C0                     or  al, al
  4108 00001E26 0F840900                 je @@checkreg206
  4109 00001E2A 66B83E000000             mov eax, 62
  4110 00001E30 E96000                   jmp @@retn
  4111                                  
  4112                                  ;-442   if (eqstr(symbol,"esi")) return 65; if (eqstr(symbol,"edi")) return 68;
  4113                                  
  4114                                  @@checkreg206:
  4115 00001E33 68[EE1E]                 push checkreg_28
  4116 00001E36 8D066E81                 lea  ax, [symbol]
  4117 00001E3A 50                       push ax
  4118 00001E3B E8A244                   call eqstr
  4119 00001E3E 83C404                   add  sp, 4
  4120 00001E41 08C0                     or  al, al
  4121 00001E43 0F840900                 je @@checkreg207
  4122 00001E47 66B841000000             mov eax, 65
  4123 00001E4D E94300                   jmp @@retn
  4124                                  @@checkreg207:
  4125 00001E50 68[F21E]                 push checkreg_29
  4126 00001E53 8D066E81                 lea  ax, [symbol]
  4127 00001E57 50                       push ax
  4128 00001E58 E88544                   call eqstr
  4129 00001E5B 83C404                   add  sp, 4
  4130 00001E5E 08C0                     or  al, al
  4131 00001E60 0F840900                 je @@checkreg208
  4132 00001E64 66B844000000             mov eax, 68
  4133 00001E6A E92600                   jmp @@retn
  4134                                  
  4135                                  ;-443   if (eqstr(symbol,"cr0")) return 71;
  4136                                  
  4137                                  @@checkreg208:
  4138 00001E6D 68[F61E]                 push checkreg_30
  4139 00001E70 8D066E81                 lea  ax, [symbol]
  4140 00001E74 50                       push ax
  4141 00001E75 E86844                   call eqstr
  4142 00001E78 83C404                   add  sp, 4
  4143 00001E7B 08C0                     or  al, al
  4144 00001E7D 0F840900                 je @@checkreg209
  4145 00001E81 66B847000000             mov eax, 71
  4146 00001E87 E90900                   jmp @@retn
  4147                                  
  4148                                  ;-444   return 0;   }
  4149                                  
  4150                                  @@checkreg209:
  4151 00001E8A 66B800000000             mov eax, 0
  4152 00001E90 E90000                   jmp @@retn
  4153                                  
  4154                                  ;-445 char printregstr[]
  4155                                  
  4156                                  @@retn:
  4157 00001E93 C3                       ret
  4158                                  ; ENDP
  4159 00001E94 616C00                  checkreg_0 db "al",0
  4160 00001E97 636C00                  checkreg_1 db "cl",0
  4161 00001E9A 646C00                  checkreg_2 db "dl",0
  4162 00001E9D 626C00                  checkreg_3 db "bl",0
  4163 00001EA0 616800                  checkreg_4 db "ah",0
  4164 00001EA3 636800                  checkreg_5 db "ch",0
  4165 00001EA6 646800                  checkreg_6 db "dh",0
  4166 00001EA9 626800                  checkreg_7 db "bh",0
  4167 00001EAC 617800                  checkreg_8 db "ax",0
  4168 00001EAF 637800                  checkreg_9 db "cx",0
  4169 00001EB2 647800                  checkreg_10 db "dx",0
  4170 00001EB5 627800                  checkreg_11 db "bx",0
  4171 00001EB8 737000                  checkreg_12 db "sp",0
  4172 00001EBB 627000                  checkreg_13 db "bp",0
  4173 00001EBE 736900                  checkreg_14 db "si",0
  4174 00001EC1 646900                  checkreg_15 db "di",0
  4175 00001EC4 657300                  checkreg_16 db "es",0
  4176 00001EC7 637300                  checkreg_17 db "cs",0
  4177 00001ECA 737300                  checkreg_18 db "ss",0
  4178 00001ECD 647300                  checkreg_19 db "ds",0
  4179 00001ED0 667300                  checkreg_20 db "fs",0
  4180 00001ED3 677300                  checkreg_21 db "gs",0
  4181 00001ED6 65617800                checkreg_22 db "eax",0
  4182 00001EDA 65637800                checkreg_23 db "ecx",0
  4183 00001EDE 65647800                checkreg_24 db "edx",0
  4184 00001EE2 65627800                checkreg_25 db "ebx",0
  4185 00001EE6 65737000                checkreg_26 db "esp",0
  4186 00001EEA 65627000                checkreg_27 db "ebp",0
  4187 00001EEE 65736900                checkreg_28 db "esi",0
  4188 00001EF2 65646900                checkreg_29 db "edi",0
  4189 00001EF6 63723000                checkreg_30 db "cr0",0
  4190                                  
  4191                                  ;-446 ="*alcldlblahchdhbhaxcxdxbxspbpsidiescsssdsfsgsipeaxecxedxebxespebpesiedicr0";
  4192                                  
  4193 00001EFA 2A616C636C646C626C-     printregstr db "*alcldlblahchdhbhaxcxdxbxspbpsidiescsssdsfsgsipeaxecxedxebxespebpesiedicr0",0
  4194 00001F03 616863686468626861-
  4195 00001F0C 786378647862787370-
  4196 00001F15 627073696469657363-
  4197 00001F1E 737373647366736773-
  4198 00001F27 697065617865637865-
  4199 00001F30 647865627865737065-
  4200 00001F39 627065736965646963-
  4201 00001F42 723000             
  4202                                  
  4203                                  ;-447 //          1         2         3         4         5         6         7
  4204                                  
  4205                                  
  4206                                  ;-448 // 1 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3 5 7 901 3
  4207                                  
  4208                                  
  4209                                  ;-449 void printreg(int i, int mode) {  unsigned int k; unsigned char c;
  4210                                  
  4211                                  
  4212                                  printreg:  ; *** PROC ***
  4213                                  
  4214                                  ;-450   if (_ mode == 1) prc('[');
  4215                                  
  4216                                  ;Function : printreg, Number of local variables: 4
  4217                                  ;   # type sign width line used address name   list of local variables
  4218                                  ;  200 var sign word   449 NULL bp+4    i
  4219                                  ;  201 var sign word   449 NULL bp+6    mode
  4220                                  ;  202 var unsg word   449 NULL bp-2    k
  4221                                  ;  203 var unsg byte   449 NULL bp-4    c;
  4222 00001F45 C8040000                 ENTER  4,0
  4223 00001F49 837E0601                cmp word [bp+6], 1
  4224 00001F4D 0F850800                 jne @@printreg210
  4225 00001F51 6A5B                     push 91
  4226 00001F53 E8CD2E                   call prc
  4227 00001F56 83C402                   add  sp, 2
  4228                                  
  4229                                  ;-451   k = &printregstr + i; c=*k; prc(c); i++;
  4230                                  
  4231                                  @@printreg210:
  4232 00001F59 B8[FA1E]                 mov ax, printregstr
  4233 00001F5C 034604                   add ax, [bp+4]
  4234 00001F5F 8946FE                   mov word [bp-2], ax
  4235 00001F62 8B5EFE                   mov bx, [bp-2]
  4236 00001F65 8B07                     mov ax, [bx]
  4237 00001F67 8846FC                   mov byte [bp-4], al
  4238 00001F6A 8A46FC                   mov al, byte [bp-4]
  4239 00001F6D B400                     mov ah, 0
  4240 00001F6F 50                       push ax
  4241 00001F70 E8B02E                   call prc
  4242 00001F73 83C402                   add  sp, 2
  4243 00001F76 FF4604                   inc  word [bp+4]
  4244                                  
  4245                                  ;-452   k = &printregstr + i; c=*k; prc(c);
  4246                                  
  4247 00001F79 B8[FA1E]                 mov ax, printregstr
  4248 00001F7C 034604                   add ax, [bp+4]
  4249 00001F7F 8946FE                   mov word [bp-2], ax
  4250 00001F82 8B5EFE                   mov bx, [bp-2]
  4251 00001F85 8B07                     mov ax, [bx]
  4252 00001F87 8846FC                   mov byte [bp-4], al
  4253 00001F8A 8A46FC                   mov al, byte [bp-4]
  4254 00001F8D B400                     mov ah, 0
  4255 00001F8F 50                       push ax
  4256 00001F90 E8902E                   call prc
  4257 00001F93 83C402                   add  sp, 2
  4258                                  
  4259                                  ;-453   if (i > 47) { i++; k = &printregstr + i; c=*k; prc(c); }
  4260                                  
  4261 00001F96 8B4604                   mov ax, [bp+4]
  4262 00001F99 83F82F                   cmp ax, 47
  4263 00001F9C 0F8E2000                 jle @@printreg211
  4264 00001FA0 FF4604                   inc  word [bp+4]
  4265 00001FA3 B8[FA1E]                 mov ax, printregstr
  4266 00001FA6 034604                   add ax, [bp+4]
  4267 00001FA9 8946FE                   mov word [bp-2], ax
  4268 00001FAC 8B5EFE                   mov bx, [bp-2]
  4269 00001FAF 8B07                     mov ax, [bx]
  4270 00001FB1 8846FC                   mov byte [bp-4], al
  4271 00001FB4 8A46FC                   mov al, byte [bp-4]
  4272 00001FB7 B400                     mov ah, 0
  4273 00001FB9 50                       push ax
  4274 00001FBA E8662E                   call prc
  4275 00001FBD 83C402                   add  sp, 2
  4276                                  
  4277                                  ;-454   if (_ mode == 1) prc(']');
  4278                                  
  4279                                  @@printreg211:
  4280 00001FC0 837E0601                cmp word [bp+6], 1
  4281 00001FC4 0F850800                 jne @@printreg212
  4282 00001FC8 6A5D                     push 93
  4283 00001FCA E8562E                   call prc
  4284 00001FCD 83C402                   add  sp, 2
  4285                                  
  4286                                  ;-455 }
  4287                                  
  4288                                  @@printreg212:
  4289                                  
  4290                                  ;-456 printpri(int w) { if (w == 1) printreg(1, 0); if (w == 2) printreg(17, 0);
  4291 00001FD0 C9                       LEAVE
  4292 00001FD1 C3                       ret
  4293                                  ; ENDP
  4294                                  
  4295                                  printpri:  ; *** PROC ***
  4296                                  ;Function : printpri, Number of local variables: 1
  4297                                  ;   # type sign width line used address name   list of local variables
  4298                                  ;  200 var sign word   456 NULL bp+4    w;
  4299 00001FD2 C8000000                 ENTER  0,0
  4300 00001FD6 8B4604                   mov ax, [bp+4]
  4301 00001FD9 83F801                   cmp ax, 1
  4302 00001FDC 0F850A00                 jne @@printpri213
  4303 00001FE0 6A00                     push 0
  4304 00001FE2 6A01                     push 1
  4305 00001FE4 E85EFF                   call printreg
  4306 00001FE7 83C404                   add  sp, 4
  4307                                  @@printpri213:
  4308 00001FEA 8B4604                   mov ax, [bp+4]
  4309 00001FED 83F802                   cmp ax, 2
  4310 00001FF0 0F850A00                 jne @@printpri214
  4311 00001FF4 6A00                     push 0
  4312 00001FF6 6A11                     push 17
  4313 00001FF8 E84AFF                   call printreg
  4314 00001FFB 83C404                   add  sp, 4
  4315                                  
  4316                                  ;-457   if (wi == 4) printreg (47, 0);
  4317                                  
  4318                                  @@printpri214:
  4319 00001FFE A1[5B00]                 mov ax, [wi]
  4320 00002001 83F804                   cmp ax, 4
  4321 00002004 0F850A00                 jne @@printpri215
  4322 00002008 6A00                     push 0
  4323 0000200A 6A2F                     push 47
  4324 0000200C E836FF                   call printreg
  4325 0000200F 83C404                   add  sp, 4
  4326                                  
  4327                                  ;-458 }
  4328                                  
  4329                                  @@printpri215:
  4330                                  
  4331                                  ;-459 
  4332                                  
  4333                                  
  4334                                  ;-460 
  4335                                  
  4336                                  
  4337                                  ;-461 
  4338                                  
  4339                                  
  4340                                  ;-462 int expr()
  4341 00002012 C9                       LEAVE
  4342 00002013 C3                       ret
  4343                                  ; ENDP
  4344                                  
  4345                                  expr:  ; *** PROC ***
  4346                                  
  4347                                  ;-463 { int mode; int id1;     int ixarr; int ixconst;
  4348                                  
  4349                                  
  4350                                  ;-464   int ids;  int isCONST; int i;     unsigned char *p;
  4351                                  
  4352                                  
  4353                                  ;-465   if (istoken(T_CONST)) { prs("\n mov eax, "); prL(lexvalL); return; }
  4354                                  
  4355                                  ;Function : expr, Number of local variables: 8
  4356                                  ;   # type sign width line used address name   list of local variables
  4357                                  ;  200 var sign word   463 NULL bp-2    mode
  4358                                  ;  201 var sign word   463 NULL bp-4    id1
  4359                                  ;  202 var sign word   463 NULL bp-6    ixarr
  4360                                  ;  203 var sign word   463 NULL bp-8    ixconst
  4361                                  ;  204 var sign word   464 NULL bp-10    ids
  4362                                  ;  205 var sign word   464 NULL bp-12    isCONST
  4363                                  ;  206 var sign word   464 NULL bp-14    i
  4364                                  ;  207 ptr unsg byte   464 NULL bp-16    p;
  4365 00002014 C8100000                 ENTER  16,0
  4366 00002018 680101                   push 257
  4367 0000201B E82E2D                   call istoken
  4368 0000201E 83C402                   add  sp, 2
  4369 00002021 08C0                     or  al, al
  4370 00002023 0F841700                 je @@expr216
  4371 00002027 68[8123]                 push expr_0
  4372 0000202A E8652E                   call prs
  4373 0000202D 83C402                   add  sp, 2
  4374 00002030 66FF36[4D00]             push dword [lexvalL]
  4375 00002035 E80D3F                   call prL
  4376 00002038 83C404                   add  sp, 4
  4377 0000203B E94103                   jmp @@retn
  4378                                  
  4379                                  ;-466   mode=typeName(); /*0=V,1=*,2=&*/
  4380                                  
  4381                                  @@expr216:
  4382 0000203E E815EB                   call typeName
  4383 00002041 8946FE                   mov word [bp-2], ax
  4384                                  
  4385                                  ;-467   mod1=mode;
  4386                                  
  4387 00002044 8B46FE                   mov ax, [bp-2]
  4388 00002047 A3[E611]                 mov word [mod1], ax
  4389                                  
  4390                                  ;-468   irg1=checkreg();
  4391                                  
  4392 0000204A E884FA                   call checkreg
  4393 0000204D A3[EA11]                 mov word [irg1], ax
  4394                                  
  4395                                  ;-469   if (irg1) { doreg1(0); return; }
  4396                                  
  4397 00002050 A1[EA11]                 mov ax, [irg1]
  4398 00002053 08C0                     or  al, al
  4399 00002055 0F840B00                 je @@expr217
  4400 00002059 6A00                     push 0
  4401 0000205B E89EF8                   call doreg1
  4402 0000205E 83C402                   add  sp, 2
  4403 00002061 E91B03                   jmp @@retn
  4404                                  
  4405                                  ;-470   if (token=='(')  {docall1(); goto e1; }
  4406                                  
  4407                                  @@expr217:
  4408 00002064 A1[3D00]                 mov ax, [token]
  4409 00002067 83F828                   cmp ax, 40
  4410 0000206A 0F850600                 jne @@expr218
  4411 0000206E E8EE0A                   call docall1
  4412 00002071 E9FE01                   jmp @@e1
  4413                                  
  4414                                  ;-471 
  4415                                  
  4416                                  
  4417                                  ;-472   id1=searchname(); gettypes(id1); ids=signi;  ixarr=0;  ixconst=0;
  4418                                  
  4419                                  @@expr218:
  4420 00002074 E8FFE9                   call searchname
  4421 00002077 8946FC                   mov word [bp-4], ax
  4422 0000207A FF76FC                   push word [bp-4]
  4423 0000207D E8ABE8                   call gettypes
  4424 00002080 83C402                   add  sp, 2
  4425 00002083 A1[5600]                 mov ax, [signi]
  4426 00002086 8946F6                   mov word [bp-10], ax
  4427 00002089 66B800000000             mov eax, 0
  4428 0000208F 8946FA                   mov word [bp-6], ax
  4429 00002092 66B800000000             mov eax, 0
  4430 00002098 8946F8                   mov word [bp-8], ax
  4431                                  
  4432                                  ;-473     if (istoken('[')) { if (istoken(T_CONST)) {
  4433                                  
  4434 0000209B 6A5B                     push 91
  4435 0000209D E8AC2C                   call istoken
  4436 000020A0 83C402                   add  sp, 2
  4437 000020A3 08C0                     or  al, al
  4438 000020A5 0F845D00                 je @@expr219
  4439 000020A9 680101                   push 257
  4440 000020AC E89D2C                   call istoken
  4441 000020AF 83C402                   add  sp, 2
  4442 000020B2 08C0                     or  al, al
  4443 000020B4 0F841B00                 je @@expr220
  4444                                  
  4445                                  ;-474       ixconst=1; ixarr=lexvalL; expect(']');  }
  4446                                  
  4447 000020B8 66B801000000             mov eax, 1
  4448 000020BE 8946F8                   mov word [bp-8], ax
  4449 000020C1 66A1[4D00]               mov eax, [lexvalL]
  4450 000020C5 8946FA                   mov word [bp-6], ax
  4451 000020C8 6A5D                     push 93
  4452 000020CA E8A72C                   call expect
  4453 000020CD 83C402                   add  sp, 2
  4454                                  
  4455                                  ;-475     else {ixarr=searchname(); expect(T_NAME); expect(']');
  4456                                  
  4457 000020D0 E93300                   jmp @@expr221
  4458                                  @@expr220:
  4459 000020D3 E8A0E9                   call searchname
  4460 000020D6 8946FA                   mov word [bp-6], ax
  4461 000020D9 680001                   push 256
  4462 000020DC E8952C                   call expect
  4463 000020DF 83C402                   add  sp, 2
  4464 000020E2 6A5D                     push 93
  4465 000020E4 E88D2C                   call expect
  4466 000020E7 83C402                   add  sp, 2
  4467                                  
  4468                                  ;-476     gettypes(ixarr);
  4469                                  
  4470 000020EA FF76FA                   push word [bp-6]
  4471 000020ED E83BE8                   call gettypes
  4472 000020F0 83C402                   add  sp, 2
  4473                                  
  4474                                  ;-477     if (widthi != 2) error1("Array index must be number or int"); } }
  4475                                  
  4476 000020F3 A1[5900]                 mov ax, [widthi]
  4477 000020F6 83F802                   cmp ax, 2
  4478 000020F9 0F840900                 je  @@expr222
  4479 000020FD 68[8E23]                 push expr_1
  4480 00002100 E89030                   call error1
  4481 00002103 83C402                   add  sp, 2
  4482                                  @@expr222:
  4483                                  @@expr221:
  4484                                  
  4485                                  ;-478   if (istoken(T_PLUSPLUS  )) {if(mode)error1("Only var allowed");
  4486                                  
  4487                                  @@expr219:
  4488 00002106 68C304                   push 1219
  4489 00002109 E8402C                   call istoken
  4490 0000210C 83C402                   add  sp, 2
  4491 0000210F 08C0                     or  al, al
  4492 00002111 0F843100                 je @@expr223
  4493 00002115 8B46FE                   mov ax, [bp-2]
  4494 00002118 08C0                     or  al, al
  4495 0000211A 0F840900                 je @@expr224
  4496 0000211E 68[B023]                 push expr_2
  4497 00002121 E86F30                   call error1
  4498 00002124 83C402                   add  sp, 2
  4499                                  
  4500                                  ;-479      prs("\n inc  ");  pwi(wi); v(id1); goto e1;}
  4501                                  
  4502                                  @@expr224:
  4503 00002127 68[C123]                 push expr_3
  4504 0000212A E8652D                   call prs
  4505 0000212D 83C402                   add  sp, 2
  4506 00002130 FF36[5B00]               push word [wi]
  4507 00002134 E850E0                   call pwi
  4508 00002137 83C402                   add  sp, 2
  4509 0000213A FF76FC                   push word [bp-4]
  4510 0000213D E8B6DF                   call v
  4511 00002140 83C402                   add  sp, 2
  4512 00002143 E92C01                   jmp @@e1
  4513                                  
  4514                                  ;-480   if (istoken(T_MINUSMINUS)) {if(mode)error1("Only Var allowed");
  4515                                  
  4516                                  @@expr223:
  4517 00002146 68C904                   push 1225
  4518 00002149 E8002C                   call istoken
  4519 0000214C 83C402                   add  sp, 2
  4520 0000214F 08C0                     or  al, al
  4521 00002151 0F843100                 je @@expr225
  4522 00002155 8B46FE                   mov ax, [bp-2]
  4523 00002158 08C0                     or  al, al
  4524 0000215A 0F840900                 je @@expr226
  4525 0000215E 68[CA23]                 push expr_4
  4526 00002161 E82F30                   call error1
  4527 00002164 83C402                   add  sp, 2
  4528                                  
  4529                                  ;-481      prs("\n dec  ");  pwi(wi); v(id1); goto e1;}
  4530                                  
  4531                                  @@expr226:
  4532 00002167 68[DB23]                 push expr_5
  4533 0000216A E8252D                   call prs
  4534 0000216D 83C402                   add  sp, 2
  4535 00002170 FF36[5B00]               push word [wi]
  4536 00002174 E810E0                   call pwi
  4537 00002177 83C402                   add  sp, 2
  4538 0000217A FF76FC                   push word [bp-4]
  4539 0000217D E876DF                   call v
  4540 00002180 83C402                   add  sp, 2
  4541 00002183 E9EC00                   jmp @@e1
  4542                                  
  4543                                  ;-482        
  4544                                  
  4545                                  
  4546                                  ;-483   if (istoken(T_PLUSASS   )) {compoundass("add", mode, id1); goto e1;}
  4547                                  
  4548                                  @@expr225:
  4549 00002186 68CE04                   push 1230
  4550 00002189 E8C02B                   call istoken
  4551 0000218C 83C402                   add  sp, 2
  4552 0000218F 08C0                     or  al, al
  4553 00002191 0F841200                 je @@expr227
  4554 00002195 FF76FC                   push word [bp-4]
  4555 00002198 FF76FE                   push word [bp-2]
  4556 0000219B 68[E423]                 push expr_6
  4557 0000219E E89102                   call compoundass
  4558 000021A1 83C406                   add  sp, 6
  4559 000021A4 E9CB00                   jmp @@e1
  4560                                  
  4561                                  ;-484   if (istoken(T_MINUSASS  )) {compoundass("sub", mode, id1); goto e1;}
  4562                                  
  4563                                  @@expr227:
  4564 000021A7 68CF04                   push 1231
  4565 000021AA E89F2B                   call istoken
  4566 000021AD 83C402                   add  sp, 2
  4567 000021B0 08C0                     or  al, al
  4568 000021B2 0F841200                 je @@expr228
  4569 000021B6 FF76FC                   push word [bp-4]
  4570 000021B9 FF76FE                   push word [bp-2]
  4571 000021BC 68[E823]                 push expr_7
  4572 000021BF E87002                   call compoundass
  4573 000021C2 83C406                   add  sp, 6
  4574 000021C5 E9AA00                   jmp @@e1
  4575                                  
  4576                                  ;-485   if (istoken(T_ANDASS    )) {compoundass("and", mode, id1); goto e1;}
  4577                                  
  4578                                  @@expr228:
  4579 000021C8 68D204                   push 1234
  4580 000021CB E87E2B                   call istoken
  4581 000021CE 83C402                   add  sp, 2
  4582 000021D1 08C0                     or  al, al
  4583 000021D3 0F841200                 je @@expr229
  4584 000021D7 FF76FC                   push word [bp-4]
  4585 000021DA FF76FE                   push word [bp-2]
  4586 000021DD 68[EC23]                 push expr_8
  4587 000021E0 E84F02                   call compoundass
  4588 000021E3 83C406                   add  sp, 6
  4589 000021E6 E98900                   jmp @@e1
  4590                                  
  4591                                  ;-486   if (istoken(T_ORASS     )) {compoundass("or" , mode, id1); goto e1;}    
  4592                                  
  4593                                  @@expr229:
  4594 000021E9 68D304                   push 1235
  4595 000021EC E85D2B                   call istoken
  4596 000021EF 83C402                   add  sp, 2
  4597 000021F2 08C0                     or  al, al
  4598 000021F4 0F841200                 je @@expr230
  4599 000021F8 FF76FC                   push word [bp-4]
  4600 000021FB FF76FE                   push word [bp-2]
  4601 000021FE 68[F023]                 push expr_9
  4602 00002201 E82E02                   call compoundass
  4603 00002204 83C406                   add  sp, 6
  4604 00002207 E96800                   jmp @@e1
  4605                                  
  4606                                  ;-487   if (istoken(T_MULASS    )) {error1("not implemented");}
  4607                                  
  4608                                  @@expr230:
  4609 0000220A 68D004                   push 1232
  4610 0000220D E83C2B                   call istoken
  4611 00002210 83C402                   add  sp, 2
  4612 00002213 08C0                     or  al, al
  4613 00002215 0F840900                 je @@expr231
  4614 00002219 68[F323]                 push expr_10
  4615 0000221C E8742F                   call error1
  4616 0000221F 83C402                   add  sp, 2
  4617                                  
  4618                                  ;-488   if (istoken(T_DIVASS    )) {error1("not implemented");}
  4619                                  
  4620                                  @@expr231:
  4621 00002222 68D104                   push 1233
  4622 00002225 E8242B                   call istoken
  4623 00002228 83C402                   add  sp, 2
  4624 0000222B 08C0                     or  al, al
  4625 0000222D 0F840900                 je @@expr232
  4626 00002231 68[0324]                 push expr_11
  4627 00002234 E85C2F                   call error1
  4628 00002237 83C402                   add  sp, 2
  4629                                  
  4630                                  ;-489 
  4631                                  
  4632                                  
  4633                                  ;-490   if (istoken('=')) { expr();
  4634                                  
  4635                                  @@expr232:
  4636 0000223A 6A3D                     push 61
  4637 0000223C E80D2B                   call istoken
  4638 0000223F 83C402                   add  sp, 2
  4639 00002242 08C0                     or  al, al
  4640 00002244 0F841800                 je @@expr233
  4641 00002248 E8C9FD                   call expr
  4642                                  
  4643                                  ;-491   doassign(mode, id1, ixarr, ixconst); goto e1;  }
  4644                                  
  4645 0000224B FF76F8                   push word [bp-8]
  4646 0000224E FF76FA                   push word [bp-6]
  4647 00002251 FF76FC                   push word [bp-4]
  4648 00002254 FF76FE                   push word [bp-2]
  4649 00002257 E81605                   call doassign
  4650 0000225A 83C408                   add  sp, 8
  4651 0000225D E91200                   jmp @@e1
  4652                                  
  4653                                  ;-492   dovar1(mode, "mov", ixarr, id1);
  4654                                  
  4655                                  @@expr233:
  4656 00002260 FF76FC                   push word [bp-4]
  4657 00002263 FF76FA                   push word [bp-6]
  4658 00002266 68[1324]                 push expr_12
  4659 00002269 FF76FE                   push word [bp-2]
  4660 0000226C E84702                   call dovar1
  4661 0000226F 83C408                   add  sp, 8
  4662                                  
  4663                                  ;-493   
  4664                                  
  4665                                  
  4666                                  ;-494 e1:    if (istoken('+')) rterm("add");
  4667                                  
  4668                                  @@e1:
  4669 00002272 6A2B                     push 43
  4670 00002274 E8D52A                   call istoken
  4671 00002277 83C402                   add  sp, 2
  4672 0000227A 08C0                     or  al, al
  4673 0000227C 0F840C00                 je @@expr234
  4674 00002280 68[1724]                 push expr_13
  4675 00002283 E8FD03                   call rterm
  4676 00002286 83C402                   add  sp, 2
  4677                                  
  4678                                  ;-495   else if (istoken('-')) rterm("sub" );
  4679                                  
  4680 00002289 E9CF00                   jmp @@expr235
  4681                                  @@expr234:
  4682 0000228C 6A2D                     push 45
  4683 0000228E E8BB2A                   call istoken
  4684 00002291 83C402                   add  sp, 2
  4685 00002294 08C0                     or  al, al
  4686 00002296 0F840C00                 je @@expr236
  4687 0000229A 68[1B24]                 push expr_14
  4688 0000229D E8E303                   call rterm
  4689 000022A0 83C402                   add  sp, 2
  4690                                  
  4691                                  ;-496   else if (istoken('&')) rterm("and" );
  4692                                  
  4693 000022A3 E9B500                   jmp @@expr237
  4694                                  @@expr236:
  4695 000022A6 6A26                     push 38
  4696 000022A8 E8A12A                   call istoken
  4697 000022AB 83C402                   add  sp, 2
  4698 000022AE 08C0                     or  al, al
  4699 000022B0 0F840C00                 je @@expr238
  4700 000022B4 68[1F24]                 push expr_15
  4701 000022B7 E8C903                   call rterm
  4702 000022BA 83C402                   add  sp, 2
  4703                                  
  4704                                  ;-497   else if (istoken('|')) rterm("or" );  
  4705                                  
  4706 000022BD E99B00                   jmp @@expr239
  4707                                  @@expr238:
  4708 000022C0 6A7C                     push 124
  4709 000022C2 E8872A                   call istoken
  4710 000022C5 83C402                   add  sp, 2
  4711 000022C8 08C0                     or  al, al
  4712 000022CA 0F840C00                 je @@expr240
  4713 000022CE 68[2324]                 push expr_16
  4714 000022D1 E8AF03                   call rterm
  4715 000022D4 83C402                   add  sp, 2
  4716                                  
  4717                                  ;-498   else if (istoken(T_LESSLESS)) rterm("shl");
  4718                                  
  4719 000022D7 E98100                   jmp @@expr241
  4720                                  @@expr240:
  4721 000022DA 68D804                   push 1240
  4722 000022DD E86C2A                   call istoken
  4723 000022E0 83C402                   add  sp, 2
  4724 000022E3 08C0                     or  al, al
  4725 000022E5 0F840C00                 je @@expr242
  4726 000022E9 68[2624]                 push expr_17
  4727 000022EC E89403                   call rterm
  4728 000022EF 83C402                   add  sp, 2
  4729                                  
  4730                                  ;-499   else if (istoken(T_GREATGREAT)) rterm("shr");  
  4731                                  
  4732 000022F2 E96600                   jmp @@expr243
  4733                                  @@expr242:
  4734 000022F5 68D904                   push 1241
  4735 000022F8 E8512A                   call istoken
  4736 000022FB 83C402                   add  sp, 2
  4737 000022FE 08C0                     or  al, al
  4738 00002300 0F840C00                 je @@expr244
  4739 00002304 68[2A24]                 push expr_18
  4740 00002307 E87903                   call rterm
  4741 0000230A 83C402                   add  sp, 2
  4742                                  
  4743                                  ;-500   else if (istoken('*')) domul (ids);
  4744                                  
  4745 0000230D E94B00                   jmp @@expr245
  4746                                  @@expr244:
  4747 00002310 6A2A                     push 42
  4748 00002312 E8372A                   call istoken
  4749 00002315 83C402                   add  sp, 2
  4750 00002318 08C0                     or  al, al
  4751 0000231A 0F840C00                 je @@expr246
  4752 0000231E FF76F6                   push word [bp-10]
  4753 00002321 E8CD05                   call domul
  4754 00002324 83C402                   add  sp, 2
  4755                                  
  4756                                  ;-501   else if (istoken('/')) doidiv(ids);
  4757                                  
  4758 00002327 E93100                   jmp @@expr247
  4759                                  @@expr246:
  4760 0000232A 6A2F                     push 47
  4761 0000232C E81D2A                   call istoken
  4762 0000232F 83C402                   add  sp, 2
  4763 00002332 08C0                     or  al, al
  4764 00002334 0F840C00                 je @@expr248
  4765 00002338 FF76F6                   push word [bp-10]
  4766 0000233B E85706                   call doidiv
  4767 0000233E 83C402                   add  sp, 2
  4768                                  
  4769                                  ;-502   else if (istoken('%')) domod (ids);
  4770                                  
  4771 00002341 E91700                   jmp @@expr249
  4772                                  @@expr248:
  4773 00002344 6A25                     push 37
  4774 00002346 E8032A                   call istoken
  4775 00002349 83C402                   add  sp, 2
  4776 0000234C 08C0                     or  al, al
  4777 0000234E 0F840900                 je @@expr250
  4778 00002352 FF76F6                   push word [bp-10]
  4779 00002355 E8E107                   call domod
  4780 00002358 83C402                   add  sp, 2
  4781                                  
  4782                                  ;-503   if (isrelational()) { rterm("cmp"); cmpneg(ids);}
  4783                                  
  4784                                  @@expr250:
  4785                                  @@expr249:
  4786                                  @@expr247:
  4787                                  @@expr245:
  4788                                  @@expr243:
  4789                                  @@expr241:
  4790                                  @@expr239:
  4791                                  @@expr237:
  4792                                  @@expr235:
  4793 0000235B E8E120                   call isrelational
  4794 0000235E 08C0                     or  al, al
  4795 00002360 0F841200                 je @@expr251
  4796 00002364 68[2E24]                 push expr_19
  4797 00002367 E81903                   call rterm
  4798 0000236A 83C402                   add  sp, 2
  4799 0000236D FF76F6                   push word [bp-10]
  4800 00002370 E87D1D                   call cmpneg
  4801 00002373 83C402                   add  sp, 2
  4802                                  
  4803                                  ;-504   return 0;
  4804                                  
  4805                                  @@expr251:
  4806 00002376 66B800000000             mov eax, 0
  4807 0000237C E90000                   jmp @@retn
  4808                                  
  4809                                  ;-505 }
  4810                                  
  4811                                  
  4812                                  ;-506 
  4813                                  
  4814                                  
  4815                                  ;-507 int compoundass(char *op, int mode, int id1) {
  4816                                  
  4817 0000237F C9                      @@retn: LEAVE
  4818 00002380 C3                       ret
  4819                                  ; ENDP
  4820 00002381 5C6E206D6F76206561-     expr_0 db "\n mov eax, ",0
  4821 0000238A 782C2000           
  4822 0000238E 417272617920696E64-     expr_1 db "Array index must be number or int",0
  4823 00002397 6578206D7573742062-
  4824 000023A0 65206E756D62657220-
  4825 000023A9 6F7220696E7400     
  4826 000023B0 4F6E6C792076617220-     expr_2 db "Only var allowed",0
  4827 000023B9 616C6C6F77656400   
  4828 000023C1 5C6E20696E63202000      expr_3 db "\n inc  ",0
  4829 000023CA 4F6E6C792056617220-     expr_4 db "Only Var allowed",0
  4830 000023D3 616C6C6F77656400   
  4831 000023DB 5C6E20646563202000      expr_5 db "\n dec  ",0
  4832 000023E4 61646400                expr_6 db "add",0
  4833 000023E8 73756200                expr_7 db "sub",0
  4834 000023EC 616E6400                expr_8 db "and",0
  4835 000023F0 6F7200                  expr_9 db "or",0
  4836 000023F3 6E6F7420696D706C65-     expr_10 db "not implemented",0
  4837 000023FC 6D656E74656400     
  4838 00002403 6E6F7420696D706C65-     expr_11 db "not implemented",0
  4839 0000240C 6D656E74656400     
  4840 00002413 6D6F7600                expr_12 db "mov",0
  4841 00002417 61646400                expr_13 db "add",0
  4842 0000241B 73756200                expr_14 db "sub",0
  4843 0000241F 616E6400                expr_15 db "and",0
  4844 00002423 6F7200                  expr_16 db "or",0
  4845 00002426 73686C00                expr_17 db "shl",0
  4846 0000242A 73687200                expr_18 db "shr",0
  4847 0000242E 636D7000                expr_19 db "cmp",0
  4848                                  
  4849                                  compoundass:  ; *** PROC ***
  4850                                  
  4851                                  ;-508   if(mode) error1("only scalar Var allowed");
  4852                                  
  4853                                  ;Function : compoundass, Number of local variables: 3
  4854                                  ;   # type sign width line used address name   list of local variables
  4855                                  ;  200 ptr sign byte   507 NULL bp+4    op
  4856                                  ;  201 var sign word   507 NULL bp+6    mode
  4857                                  ;  202 var sign word   507 NULL bp+8    id1;
  4858 00002432 C8000000                 ENTER  0,0
  4859 00002436 8B4606                   mov ax, [bp+6]
  4860 00002439 08C0                     or  al, al
  4861 0000243B 0F840900                 je @@compoundass252
  4862 0000243F 68[9824]                 push compoundass_0
  4863 00002442 E84E2D                   call error1
  4864 00002445 83C402                   add  sp, 2
  4865                                  
  4866                                  ;-509   prnl(); prs(op); prs("  "); 
  4867                                  
  4868                                  @@compoundass252:
  4869 00002448 E8392A                   call prnl
  4870 0000244B FF7604                   push word [bp+4]
  4871 0000244E E8412A                   call prs
  4872 00002451 83C402                   add  sp, 2
  4873 00002454 68[B024]                 push compoundass_1
  4874 00002457 E8382A                   call prs
  4875 0000245A 83C402                   add  sp, 2
  4876                                  
  4877                                  ;-510   gettypes(id1); pwi(wi); v(id1); prs(", ");
  4878                                  
  4879 0000245D FF7608                   push word [bp+8]
  4880 00002460 E8C8E4                   call gettypes
  4881 00002463 83C402                   add  sp, 2
  4882 00002466 FF36[5B00]               push word [wi]
  4883 0000246A E81ADD                   call pwi
  4884 0000246D 83C402                   add  sp, 2
  4885 00002470 FF7608                   push word [bp+8]
  4886 00002473 E880DC                   call v
  4887 00002476 83C402                   add  sp, 2
  4888 00002479 68[B324]                 push compoundass_2
  4889 0000247C E8132A                   call prs
  4890 0000247F 83C402                   add  sp, 2
  4891                                  
  4892                                  ;-511   expect(T_CONST); prL(lexvalL);
  4893                                  
  4894 00002482 680101                   push 257
  4895 00002485 E8EC28                   call expect
  4896 00002488 83C402                   add  sp, 2
  4897 0000248B 66FF36[4D00]             push dword [lexvalL]
  4898 00002490 E8B23A                   call prL
  4899 00002493 83C404                   add  sp, 4
  4900                                  
  4901                                  ;-512 }
  4902                                  
  4903                                  
  4904                                  ;-513 int dovar1(int mode, int op, int ixarr, int id1) { 
  4905 00002496 C9                       LEAVE
  4906 00002497 C3                       ret
  4907                                  ; ENDP
  4908 00002498 6F6E6C79207363616C-     compoundass_0 db "only scalar Var allowed",0
  4909 000024A1 61722056617220616C-
  4910 000024AA 6C6F77656400       
  4911 000024B0 202000                  compoundass_1 db "  ",0
  4912 000024B3 2C2000                  compoundass_2 db ", ",0
  4913                                  
  4914                                  dovar1:  ; *** PROC ***
  4915                                  
  4916                                  ;-514   gettypes(id1);
  4917                                  
  4918                                  ;Function : dovar1, Number of local variables: 4
  4919                                  ;   # type sign width line used address name   list of local variables
  4920                                  ;  200 var sign word   513 NULL bp+4    mode
  4921                                  ;  201 var sign word   513 NULL bp+6    op
  4922                                  ;  202 var sign word   513 NULL bp+8    ixarr
  4923                                  ;  203 var sign word   513 NULL bp+10    id1;
  4924 000024B6 C8000000                 ENTER  0,0
  4925 000024BA FF760A                   push word [bp+10]
  4926 000024BD E86BE4                   call gettypes
  4927 000024C0 83C402                   add  sp, 2
  4928                                  
  4929                                  ;-515   if (mode==1) {prs("\n mov bx, "); v(id1); prnl(); prs(op);
  4930                                  
  4931 000024C3 8B4604                   mov ax, [bp+4]
  4932 000024C6 83F801                   cmp ax, 1
  4933 000024C9 0F855A00                 jne @@dovar1253
  4934 000024CD 68[0F26]                 push dovar1_0
  4935 000024D0 E8BF29                   call prs
  4936 000024D3 83C402                   add  sp, 2
  4937 000024D6 FF760A                   push word [bp+10]
  4938 000024D9 E81ADC                   call v
  4939 000024DC 83C402                   add  sp, 2
  4940 000024DF E8A229                   call prnl
  4941 000024E2 FF7606                   push word [bp+6]
  4942 000024E5 E8AA29                   call prs
  4943 000024E8 83C402                   add  sp, 2
  4944                                  
  4945                                  ;-516     if(widthi == 1) prs(" al, [bx]\n mov ah, 0");
  4946                                  
  4947 000024EB A1[5900]                 mov ax, [widthi]
  4948 000024EE 83F801                   cmp ax, 1
  4949 000024F1 0F850900                 jne @@dovar1254
  4950 000024F5 68[1B26]                 push dovar1_1
  4951 000024F8 E89729                   call prs
  4952 000024FB 83C402                   add  sp, 2
  4953                                  
  4954                                  ;-517     if(widthi == 2) prs(" ax, [bx]");
  4955                                  
  4956                                  @@dovar1254:
  4957 000024FE A1[5900]                 mov ax, [widthi]
  4958 00002501 83F802                   cmp ax, 2
  4959 00002504 0F850900                 jne @@dovar1255
  4960 00002508 68[3126]                 push dovar1_2
  4961 0000250B E88429                   call prs
  4962 0000250E 83C402                   add  sp, 2
  4963                                  
  4964                                  ;-518     if(widthi == 4) prs(" eax, [bx]");
  4965                                  
  4966                                  @@dovar1255:
  4967 00002511 A1[5900]                 mov ax, [widthi]
  4968 00002514 83F804                   cmp ax, 4
  4969 00002517 0F850900                 jne @@dovar1256
  4970 0000251B 68[3B26]                 push dovar1_3
  4971 0000251E E87129                   call prs
  4972 00002521 83C402                   add  sp, 2
  4973                                  
  4974                                  ;-519     return; }
  4975                                  
  4976                                  @@dovar1256:
  4977 00002524 E9E600                   jmp @@retn
  4978                                  
  4979                                  ;-520   if (mode==2){prnl();prs(op);prs(" ax, "); a(id1); return; }
  4980                                  
  4981                                  @@dovar1253:
  4982 00002527 8B4604                   mov ax, [bp+4]
  4983 0000252A 83F802                   cmp ax, 2
  4984 0000252D 0F852100                 jne @@dovar1257
  4985 00002531 E85029                   call prnl
  4986 00002534 FF7606                   push word [bp+6]
  4987 00002537 E85829                   call prs
  4988 0000253A 83C402                   add  sp, 2
  4989 0000253D 68[4626]                 push dovar1_4
  4990 00002540 E84F29                   call prs
  4991 00002543 83C402                   add  sp, 2
  4992 00002546 FF760A                   push word [bp+10]
  4993 00002549 E881DB                   call a
  4994 0000254C 83C402                   add  sp, 2
  4995 0000254F E9BB00                   jmp @@retn
  4996                                  
  4997                                  ;-521   if (ixarr) {
  4998                                  
  4999                                  @@dovar1257:
  5000 00002552 8B4608                   mov ax, [bp+8]
  5001 00002555 08C0                     or  al, al
  5002 00002557 0F848200                 je @@dovar1258
  5003                                  
  5004                                  ;-522     prs("\n mov bx, "); v(ixarr);
  5005                                  
  5006 0000255B 68[4C26]                 push dovar1_5
  5007 0000255E E83129                   call prs
  5008 00002561 83C402                   add  sp, 2
  5009 00002564 FF7608                   push word [bp+8]
  5010 00002567 E88CDB                   call v
  5011 0000256A 83C402                   add  sp, 2
  5012                                  
  5013                                  ;-523     if (wi==2) prs("\n shl bx, 1");
  5014                                  
  5015 0000256D A1[5B00]                 mov ax, [wi]
  5016 00002570 83F802                   cmp ax, 2
  5017 00002573 0F850900                 jne @@dovar1259
  5018 00002577 68[5826]                 push dovar1_6
  5019 0000257A E81529                   call prs
  5020 0000257D 83C402                   add  sp, 2
  5021                                  
  5022                                  ;-524     if (wi==4) prs("\n shl bx, 2");
  5023                                  
  5024                                  @@dovar1259:
  5025 00002580 A1[5B00]                 mov ax, [wi]
  5026 00002583 83F804                   cmp ax, 4
  5027 00002586 0F850900                 jne @@dovar1260
  5028 0000258A 68[6526]                 push dovar1_7
  5029 0000258D E80229                   call prs
  5030 00002590 83C402                   add  sp, 2
  5031                                  
  5032                                  ;-525     prs("\n "); prs(op); prc(' '); printpri(wi); prs(", ");
  5033                                  
  5034                                  @@dovar1260:
  5035 00002593 68[7226]                 push dovar1_8
  5036 00002596 E8F928                   call prs
  5037 00002599 83C402                   add  sp, 2
  5038 0000259C FF7606                   push word [bp+6]
  5039 0000259F E8F028                   call prs
  5040 000025A2 83C402                   add  sp, 2
  5041 000025A5 6A20                     push 32
  5042 000025A7 E87928                   call prc
  5043 000025AA 83C402                   add  sp, 2
  5044 000025AD FF36[5B00]               push word [wi]
  5045 000025B1 E81EFA                   call printpri
  5046 000025B4 83C402                   add  sp, 2
  5047 000025B7 68[7626]                 push dovar1_9
  5048 000025BA E8D528                   call prs
  5049 000025BD 83C402                   add  sp, 2
  5050                                  
  5051                                  ;-526 // v(id1); prs(" [bx]");
  5052                                  
  5053                                  
  5054                                  ;-527     prc('['); printName(id1); prs(" + bx]"); return; }
  5055                                  
  5056 000025C0 6A5B                     push 91
  5057 000025C2 E85E28                   call prc
  5058 000025C5 83C402                   add  sp, 2
  5059 000025C8 FF760A                   push word [bp+10]
  5060 000025CB E83EE4                   call printName
  5061 000025CE 83C402                   add  sp, 2
  5062 000025D1 68[7926]                 push dovar1_10
  5063 000025D4 E8BB28                   call prs
  5064 000025D7 83C402                   add  sp, 2
  5065 000025DA E93000                   jmp @@retn
  5066                                  
  5067                                  ;-528   prnl();prs(op); prc(' '); printpri(wi); prs(", ");  v(id1);
  5068                                  
  5069                                  @@dovar1258:
  5070 000025DD E8A428                   call prnl
  5071 000025E0 FF7606                   push word [bp+6]
  5072 000025E3 E8AC28                   call prs
  5073 000025E6 83C402                   add  sp, 2
  5074 000025E9 6A20                     push 32
  5075 000025EB E83528                   call prc
  5076 000025EE 83C402                   add  sp, 2
  5077 000025F1 FF36[5B00]               push word [wi]
  5078 000025F5 E8DAF9                   call printpri
  5079 000025F8 83C402                   add  sp, 2
  5080 000025FB 68[8026]                 push dovar1_11
  5081 000025FE E89128                   call prs
  5082 00002601 83C402                   add  sp, 2
  5083 00002604 FF760A                   push word [bp+10]
  5084 00002607 E8ECDA                   call v
  5085 0000260A 83C402                   add  sp, 2
  5086                                  
  5087                                  ;-529 }
  5088                                  
  5089                                  
  5090                                  ;-530 int rterm(char *op) {int mode; int opint; int ixarr; int id1;
  5091                                  
  5092 0000260D C9                      @@retn: LEAVE
  5093 0000260E C3                       ret
  5094                                  ; ENDP
  5095 0000260F 5C6E206D6F76206278-     dovar1_0 db "\n mov bx, ",0
  5096 00002618 2C2000             
  5097 0000261B 20616C2C205B62785D-     dovar1_1 db " al, [bx]\n mov ah, 0",0
  5098 00002624 5C6E206D6F76206168-
  5099 0000262D 2C203000           
  5100 00002631 2061782C205B62785D-     dovar1_2 db " ax, [bx]",0
  5101 0000263A 00                 
  5102 0000263B 206561782C205B6278-     dovar1_3 db " eax, [bx]",0
  5103 00002644 5D00               
  5104 00002646 2061782C2000            dovar1_4 db " ax, ",0
  5105 0000264C 5C6E206D6F76206278-     dovar1_5 db "\n mov bx, ",0
  5106 00002655 2C2000             
  5107 00002658 5C6E2073686C206278-     dovar1_6 db "\n shl bx, 1",0
  5108 00002661 2C203100           
  5109 00002665 5C6E2073686C206278-     dovar1_7 db "\n shl bx, 2",0
  5110 0000266E 2C203200           
  5111 00002672 5C6E2000                dovar1_8 db "\n ",0
  5112 00002676 2C2000                  dovar1_9 db ", ",0
  5113 00002679 202B2062785D00          dovar1_10 db " + bx]",0
  5114 00002680 2C2000                  dovar1_11 db ", ",0
  5115                                  
  5116                                  rterm:  ; *** PROC ***
  5117                                  
  5118                                  ;-531   if (istoken(T_CONST)) { prnl(); prs(op); prc(' '); printpri(wi); prs(", ");
  5119                                  
  5120                                  ;Function : rterm, Number of local variables: 5
  5121                                  ;   # type sign width line used address name   list of local variables
  5122                                  ;  200 ptr sign byte   530 NULL bp+4    op
  5123                                  ;  201 var sign word   530 NULL bp-2    mode
  5124                                  ;  202 var sign word   530 NULL bp-4    opint
  5125                                  ;  203 var sign word   530 NULL bp-6    ixarr
  5126                                  ;  204 var sign word   530 NULL bp-8    id1;
  5127 00002683 C8080000                 ENTER  8,0
  5128 00002687 680101                   push 257
  5129 0000268A E8BF26                   call istoken
  5130 0000268D 83C402                   add  sp, 2
  5131 00002690 08C0                     or  al, al
  5132 00002692 0F843500                 je @@rterm261
  5133 00002696 E8EB27                   call prnl
  5134 00002699 FF7604                   push word [bp+4]
  5135 0000269C E8F327                   call prs
  5136 0000269F 83C402                   add  sp, 2
  5137 000026A2 6A20                     push 32
  5138 000026A4 E87C27                   call prc
  5139 000026A7 83C402                   add  sp, 2
  5140 000026AA FF36[5B00]               push word [wi]
  5141 000026AE E821F9                   call printpri
  5142 000026B1 83C402                   add  sp, 2
  5143 000026B4 68[5227]                 push rterm_0
  5144 000026B7 E8D827                   call prs
  5145 000026BA 83C402                   add  sp, 2
  5146                                  
  5147                                  ;-532     prL(lexvalL); return;}
  5148                                  
  5149 000026BD 66FF36[4D00]             push dword [lexvalL]
  5150 000026C2 E88038                   call prL
  5151 000026C5 83C404                   add  sp, 4
  5152 000026C8 E98500                   jmp @@retn
  5153                                  
  5154                                  ;-533   mode=typeName(); id1=searchname(); ixarr=0;
  5155                                  
  5156                                  @@rterm261:
  5157 000026CB E888E4                   call typeName
  5158 000026CE 8946FE                   mov word [bp-2], ax
  5159 000026D1 E8A2E3                   call searchname
  5160 000026D4 8946F8                   mov word [bp-8], ax
  5161 000026D7 66B800000000             mov eax, 0
  5162 000026DD 8946FA                   mov word [bp-6], ax
  5163                                  
  5164                                  ;-534   if (istoken('[')) { ixarr=searchname(); expect(T_NAME); expect(']');  
  5165                                  
  5166 000026E0 6A5B                     push 91
  5167 000026E2 E86726                   call istoken
  5168 000026E5 83C402                   add  sp, 2
  5169 000026E8 08C0                     or  al, al
  5170 000026EA 0F843300                 je @@rterm262
  5171 000026EE E885E3                   call searchname
  5172 000026F1 8946FA                   mov word [bp-6], ax
  5173 000026F4 680001                   push 256
  5174 000026F7 E87A26                   call expect
  5175 000026FA 83C402                   add  sp, 2
  5176 000026FD 6A5D                     push 93
  5177 000026FF E87226                   call expect
  5178 00002702 83C402                   add  sp, 2
  5179                                  
  5180                                  ;-535     gettypes(ixarr);
  5181                                  
  5182 00002705 FF76FA                   push word [bp-6]
  5183 00002708 E820E2                   call gettypes
  5184 0000270B 83C402                   add  sp, 2
  5185                                  
  5186                                  ;-536     if (widthi != 2) error1("Array index must be int"); }
  5187                                  
  5188 0000270E A1[5900]                 mov ax, [widthi]
  5189 00002711 83F802                   cmp ax, 2
  5190 00002714 0F840900                 je  @@rterm263
  5191 00002718 68[5527]                 push rterm_1
  5192 0000271B E8752A                   call error1
  5193 0000271E 83C402                   add  sp, 2
  5194                                  @@rterm263:
  5195                                  
  5196                                  ;-537   if (eqstr(symbol,"ax")) return;
  5197                                  
  5198                                  @@rterm262:
  5199 00002721 68[6D27]                 push rterm_2
  5200 00002724 8D066E81                 lea  ax, [symbol]
  5201 00002728 50                       push ax
  5202 00002729 E8B43B                   call eqstr
  5203 0000272C 83C404                   add  sp, 4
  5204 0000272F 08C0                     or  al, al
  5205 00002731 0F840300                 je @@rterm264
  5206 00002735 E91800                   jmp @@retn
  5207                                  
  5208                                  ;-538   opint=op; dovar1(mode, opint, ixarr, id1);
  5209                                  
  5210                                  @@rterm264:
  5211 00002738 8B4604                   mov ax, [bp+4]
  5212 0000273B 8946FC                   mov word [bp-4], ax
  5213 0000273E FF76F8                   push word [bp-8]
  5214 00002741 FF76FA                   push word [bp-6]
  5215 00002744 FF76FC                   push word [bp-4]
  5216 00002747 FF76FE                   push word [bp-2]
  5217 0000274A E869FD                   call dovar1
  5218 0000274D 83C408                   add  sp, 8
  5219                                  
  5220                                  ;-539 }
  5221                                  
  5222                                  
  5223                                  ;-540 int doassign(int mode, int i, int ixarr, int ixconst) {
  5224                                  
  5225 00002750 C9                      @@retn: LEAVE
  5226 00002751 C3                       ret
  5227                                  ; ENDP
  5228 00002752 2C2000                  rterm_0 db ", ",0
  5229 00002755 417272617920696E64-     rterm_1 db "Array index must be int",0
  5230 0000275E 6578206D7573742062-
  5231 00002767 6520696E7400       
  5232 0000276D 617800                  rterm_2 db "ax",0
  5233                                  
  5234                                  doassign:  ; *** PROC ***
  5235                                  
  5236                                  ;-541   gettypes(i);
  5237                                  
  5238                                  ;Function : doassign, Number of local variables: 4
  5239                                  ;   # type sign width line used address name   list of local variables
  5240                                  ;  200 var sign word   540 NULL bp+4    mode
  5241                                  ;  201 var sign word   540 NULL bp+6    i
  5242                                  ;  202 var sign word   540 NULL bp+8    ixarr
  5243                                  ;  203 var sign word   540 NULL bp+10    ixconst;
  5244 00002770 C8000000                 ENTER  0,0
  5245 00002774 FF7606                   push word [bp+6]
  5246 00002777 E8B1E1                   call gettypes
  5247 0000277A 83C402                   add  sp, 2
  5248                                  
  5249                                  ;-542   if (mode==1) {prs("\n mov  bx, ");v(i);
  5250                                  
  5251 0000277D 8B4604                   mov ax, [bp+4]
  5252 00002780 83F801                   cmp ax, 1
  5253 00002783 0F852800                 jne @@doassign265
  5254 00002787 68[8628]                 push doassign_0
  5255 0000278A E80527                   call prs
  5256 0000278D 83C402                   add  sp, 2
  5257 00002790 FF7606                   push word [bp+6]
  5258 00002793 E860D9                   call v
  5259 00002796 83C402                   add  sp, 2
  5260                                  
  5261                                  ;-543     prs("\n mov  [bx], "); printpri(widthi); return;}
  5262                                  
  5263 00002799 68[9328]                 push doassign_1
  5264 0000279C E8F326                   call prs
  5265 0000279F 83C402                   add  sp, 2
  5266 000027A2 FF36[5900]               push word [widthi]
  5267 000027A6 E829F8                   call printpri
  5268 000027A9 83C402                   add  sp, 2
  5269 000027AC E9D500                   jmp @@retn
  5270                                  
  5271                                  ;-544   if (mode==2) {prs("\n mov  ");a(i); prs(", ax"); return;}
  5272                                  
  5273                                  @@doassign265:
  5274 000027AF 8B4604                   mov ax, [bp+4]
  5275 000027B2 83F802                   cmp ax, 2
  5276 000027B5 0F851E00                 jne @@doassign266
  5277 000027B9 68[A228]                 push doassign_2
  5278 000027BC E8D326                   call prs
  5279 000027BF 83C402                   add  sp, 2
  5280 000027C2 FF7606                   push word [bp+6]
  5281 000027C5 E805D9                   call a
  5282 000027C8 83C402                   add  sp, 2
  5283 000027CB 68[AB28]                 push doassign_3
  5284 000027CE E8C126                   call prs
  5285 000027D1 83C402                   add  sp, 2
  5286 000027D4 E9AD00                   jmp @@retn
  5287                                  
  5288                                  ;-545   if (ixarr) {  prs("\n mov bx, ");
  5289                                  
  5290                                  @@doassign266:
  5291 000027D7 8B4608                   mov ax, [bp+8]
  5292 000027DA 08C0                     or  al, al
  5293 000027DC 0F847500                 je @@doassign267
  5294 000027E0 68[B028]                 push doassign_4
  5295 000027E3 E8AC26                   call prs
  5296 000027E6 83C402                   add  sp, 2
  5297                                  
  5298                                  ;-546     if(ixconst) prunsign1(ixarr); else v(ixarr);
  5299                                  
  5300 000027E9 8B460A                   mov ax, [bp+10]
  5301 000027EC 08C0                     or  al, al
  5302 000027EE 0F840C00                 je @@doassign268
  5303 000027F2 FF7608                   push word [bp+8]
  5304 000027F5 E80328                   call prunsign1
  5305 000027F8 83C402                   add  sp, 2
  5306 000027FB E90900                   jmp @@doassign269
  5307                                  @@doassign268:
  5308 000027FE FF7608                   push word [bp+8]
  5309 00002801 E8F2D8                   call v
  5310 00002804 83C402                   add  sp, 2
  5311                                  
  5312                                  ;-547     if (wi==2) prs("\n shl bx, 1");
  5313                                  
  5314                                  @@doassign269:
  5315 00002807 A1[5B00]                 mov ax, [wi]
  5316 0000280A 83F802                   cmp ax, 2
  5317 0000280D 0F850900                 jne @@doassign270
  5318 00002811 68[BC28]                 push doassign_5
  5319 00002814 E87B26                   call prs
  5320 00002817 83C402                   add  sp, 2
  5321                                  
  5322                                  ;-548     if (wi==4) prs("\n shl bx, 2");
  5323                                  
  5324                                  @@doassign270:
  5325 0000281A A1[5B00]                 mov ax, [wi]
  5326 0000281D 83F804                   cmp ax, 4
  5327 00002820 0F850900                 jne @@doassign271
  5328 00002824 68[C928]                 push doassign_6
  5329 00002827 E86826                   call prs
  5330 0000282A 83C402                   add  sp, 2
  5331                                  
  5332                                  ;-549     prs("\n mov ["); printName(i); prs("+bx], "); printpri(wi); return; }
  5333                                  
  5334                                  @@doassign271:
  5335 0000282D 68[D628]                 push doassign_7
  5336 00002830 E85F26                   call prs
  5337 00002833 83C402                   add  sp, 2
  5338 00002836 FF7606                   push word [bp+6]
  5339 00002839 E8D0E1                   call printName
  5340 0000283C 83C402                   add  sp, 2
  5341 0000283F 68[DF28]                 push doassign_8
  5342 00002842 E84D26                   call prs
  5343 00002845 83C402                   add  sp, 2
  5344 00002848 FF36[5B00]               push word [wi]
  5345 0000284C E883F7                   call printpri
  5346 0000284F 83C402                   add  sp, 2
  5347 00002852 E92F00                   jmp @@retn
  5348                                  
  5349                                  ;-550   prs("\n mov "); pwi(wi); v(i); prs(", "); printpri(wi);
  5350                                  
  5351                                  @@doassign267:
  5352 00002855 68[E628]                 push doassign_9
  5353 00002858 E83726                   call prs
  5354 0000285B 83C402                   add  sp, 2
  5355 0000285E FF36[5B00]               push word [wi]
  5356 00002862 E822D9                   call pwi
  5357 00002865 83C402                   add  sp, 2
  5358 00002868 FF7606                   push word [bp+6]
  5359 0000286B E888D8                   call v
  5360 0000286E 83C402                   add  sp, 2
  5361 00002871 68[EE28]                 push doassign_10
  5362 00002874 E81B26                   call prs
  5363 00002877 83C402                   add  sp, 2
  5364 0000287A FF36[5B00]               push word [wi]
  5365 0000287E E851F7                   call printpri
  5366 00002881 83C402                   add  sp, 2
  5367                                  
  5368                                  ;-551 }
  5369                                  
  5370                                  
  5371                                  ;-552 int domul(int ids) {
  5372                                  
  5373 00002884 C9                      @@retn: LEAVE
  5374 00002885 C3                       ret
  5375                                  ; ENDP
  5376 00002886 5C6E206D6F76202062-     doassign_0 db "\n mov  bx, ",0
  5377 0000288F 782C2000           
  5378 00002893 5C6E206D6F7620205B-     doassign_1 db "\n mov  [bx], ",0
  5379 0000289C 62785D2C2000       
  5380 000028A2 5C6E206D6F76202000      doassign_2 db "\n mov  ",0
  5381 000028AB 2C20617800              doassign_3 db ", ax",0
  5382 000028B0 5C6E206D6F76206278-     doassign_4 db "\n mov bx, ",0
  5383 000028B9 2C2000             
  5384 000028BC 5C6E2073686C206278-     doassign_5 db "\n shl bx, 1",0
  5385 000028C5 2C203100           
  5386 000028C9 5C6E2073686C206278-     doassign_6 db "\n shl bx, 2",0
  5387 000028D2 2C203200           
  5388 000028D6 5C6E206D6F76205B00      doassign_7 db "\n mov [",0
  5389 000028DF 2B62785D2C2000          doassign_8 db "+bx], ",0
  5390 000028E6 5C6E206D6F762000        doassign_9 db "\n mov ",0
  5391 000028EE 2C2000                  doassign_10 db ", ",0
  5392                                  
  5393                                  domul:  ; *** PROC ***
  5394                                  
  5395                                  ;-553   if (ids) rterm("imul"); else {
  5396                                  
  5397                                  ;Function : domul, Number of local variables: 1
  5398                                  ;   # type sign width line used address name   list of local variables
  5399                                  ;  200 var sign word   552 NULL bp+4    ids;
  5400 000028F1 C8000000                 ENTER  0,0
  5401 000028F5 8B4604                   mov ax, [bp+4]
  5402 000028F8 08C0                     or  al, al
  5403 000028FA 0F840C00                 je @@domul272
  5404 000028FE 68[4429]                 push domul_0
  5405 00002901 E87FFD                   call rterm
  5406 00002904 83C402                   add  sp, 2
  5407 00002907 E93800                   jmp @@domul273
  5408                                  @@domul272:
  5409                                  
  5410                                  ;-554   if (istoken(T_CONST)) {
  5411                                  
  5412 0000290A 680101                   push 257
  5413 0000290D E83C24                   call istoken
  5414 00002910 83C402                   add  sp, 2
  5415 00002913 08C0                     or  al, al
  5416 00002915 0F842000                 je @@domul274
  5417                                  
  5418                                  ;-555       prs("\n mov ebx, "); prL(lexvalL); prs("\n mul ebx"); }
  5419                                  
  5420 00002919 68[4929]                 push domul_1
  5421 0000291C E87325                   call prs
  5422 0000291F 83C402                   add  sp, 2
  5423 00002922 66FF36[4D00]             push dword [lexvalL]
  5424 00002927 E81B36                   call prL
  5425 0000292A 83C404                   add  sp, 4
  5426 0000292D 68[5629]                 push domul_2
  5427 00002930 E85F25                   call prs
  5428 00002933 83C402                   add  sp, 2
  5429                                  
  5430                                  ;-556   else error1("with MUL only const number as multiplicator allowed"); } }
  5431                                  
  5432 00002936 E90900                   jmp @@domul275
  5433                                  @@domul274:
  5434 00002939 68[6129]                 push domul_3
  5435 0000293C E85428                   call error1
  5436 0000293F 83C402                   add  sp, 2
  5437                                  @@domul275:
  5438                                  @@domul273:
  5439                                  
  5440                                  ;-557 int doidiv(int ids) { int mode; int id1;
  5441 00002942 C9                       LEAVE
  5442 00002943 C3                       ret
  5443                                  ; ENDP
  5444 00002944 696D756C00              domul_0 db "imul",0
  5445 00002949 5C6E206D6F76206562-     domul_1 db "\n mov ebx, ",0
  5446 00002952 782C2000           
  5447 00002956 5C6E206D756C206562-     domul_2 db "\n mul ebx",0
  5448 0000295F 7800               
  5449 00002961 77697468204D554C20-     domul_3 db "with MUL only const number as multiplicator allowed",0
  5450 0000296A 6F6E6C7920636F6E73-
  5451 00002973 74206E756D62657220-
  5452 0000297C 6173206D756C746970-
  5453 00002985 6C696361746F722061-
  5454 0000298E 6C6C6F77656400     
  5455                                  
  5456                                  doidiv:  ; *** PROC ***
  5457                                  
  5458                                  ;-558   if (istoken(T_CONST)) {
  5459                                  
  5460                                  ;Function : doidiv, Number of local variables: 3
  5461                                  ;   # type sign width line used address name   list of local variables
  5462                                  ;  200 var sign word   557 NULL bp+4    ids
  5463                                  ;  201 var sign word   557 NULL bp-2    mode
  5464                                  ;  202 var sign word   557 NULL bp-4    id1;
  5465 00002995 C8040000                 ENTER  4,0
  5466 00002999 680101                   push 257
  5467 0000299C E8AD23                   call istoken
  5468 0000299F 83C402                   add  sp, 2
  5469 000029A2 08C0                     or  al, al
  5470 000029A4 0F843500                 je @@doidiv276
  5471                                  
  5472                                  ;-559     prs("\n mov bx, "); prL(lexvalL);
  5473                                  
  5474 000029A8 68[5B2A]                 push doidiv_0
  5475 000029AB E8E424                   call prs
  5476 000029AE 83C402                   add  sp, 2
  5477 000029B1 66FF36[4D00]             push dword [lexvalL]
  5478 000029B6 E88C35                   call prL
  5479 000029B9 83C404                   add  sp, 4
  5480                                  
  5481                                  ;-560     if (ids) prs("\n cwd\n idiv bx"); else prs("\n mov dx, 0\n div bx"); }
  5482                                  
  5483 000029BC 8B4604                   mov ax, [bp+4]
  5484 000029BF 08C0                     or  al, al
  5485 000029C1 0F840C00                 je @@doidiv277
  5486 000029C5 68[672A]                 push doidiv_1
  5487 000029C8 E8C724                   call prs
  5488 000029CB 83C402                   add  sp, 2
  5489 000029CE E90900                   jmp @@doidiv278
  5490                                  @@doidiv277:
  5491 000029D1 68[782A]                 push doidiv_2
  5492 000029D4 E8BB24                   call prs
  5493 000029D7 83C402                   add  sp, 2
  5494                                  @@doidiv278:
  5495                                  
  5496                                  ;-561   else {
  5497                                  
  5498 000029DA E97C00                   jmp @@doidiv279
  5499                                  @@doidiv276:
  5500                                  
  5501                                  ;-562     mode=typeName(); id1=searchname();
  5502                                  
  5503 000029DD E876E1                   call typeName
  5504 000029E0 8946FE                   mov word [bp-2], ax
  5505 000029E3 E890E0                   call searchname
  5506 000029E6 8946FC                   mov word [bp-4], ax
  5507                                  
  5508                                  ;-563     if (mode) error1("only const number or int as divisor allowed");
  5509                                  
  5510 000029E9 8B46FE                   mov ax, [bp-2]
  5511 000029EC 08C0                     or  al, al
  5512 000029EE 0F840900                 je @@doidiv280
  5513 000029F2 68[8E2A]                 push doidiv_3
  5514 000029F5 E89B27                   call error1
  5515 000029F8 83C402                   add  sp, 2
  5516                                  
  5517                                  ;-564     gettypes(id1);
  5518                                  
  5519                                  @@doidiv280:
  5520 000029FB FF76FC                   push word [bp-4]
  5521 000029FE E82ADF                   call gettypes
  5522 00002A01 83C402                   add  sp, 2
  5523                                  
  5524                                  ;-565     if (typei) error1("only int as simple var divisor allowed");
  5525                                  
  5526 00002A04 A1[5300]                 mov ax, [typei]
  5527 00002A07 08C0                     or  al, al
  5528 00002A09 0F840900                 je @@doidiv281
  5529 00002A0D 68[BA2A]                 push doidiv_4
  5530 00002A10 E88027                   call error1
  5531 00002A13 83C402                   add  sp, 2
  5532                                  
  5533                                  ;-566     if (wi!=2) error1("only int, no byte as divisor allowed");
  5534                                  
  5535                                  @@doidiv281:
  5536 00002A16 A1[5B00]                 mov ax, [wi]
  5537 00002A19 83F802                   cmp ax, 2
  5538 00002A1C 0F840900                 je  @@doidiv282
  5539 00002A20 68[E12A]                 push doidiv_5
  5540 00002A23 E86D27                   call error1
  5541 00002A26 83C402                   add  sp, 2
  5542                                  
  5543                                  ;-567     prs("\n mov bx, "); v(id1);
  5544                                  
  5545                                  @@doidiv282:
  5546 00002A29 68[062B]                 push doidiv_6
  5547 00002A2C E86324                   call prs
  5548 00002A2F 83C402                   add  sp, 2
  5549 00002A32 FF76FC                   push word [bp-4]
  5550 00002A35 E8BED6                   call v
  5551 00002A38 83C402                   add  sp, 2
  5552                                  
  5553                                  ;-568     if (ids) prs("\n cwd\n idiv bx"); else prs("\n mov dx, 0\n div bx"); }
  5554                                  
  5555 00002A3B 8B4604                   mov ax, [bp+4]
  5556 00002A3E 08C0                     or  al, al
  5557 00002A40 0F840C00                 je @@doidiv283
  5558 00002A44 68[122B]                 push doidiv_7
  5559 00002A47 E84824                   call prs
  5560 00002A4A 83C402                   add  sp, 2
  5561 00002A4D E90900                   jmp @@doidiv284
  5562                                  @@doidiv283:
  5563 00002A50 68[232B]                 push doidiv_8
  5564 00002A53 E83C24                   call prs
  5565 00002A56 83C402                   add  sp, 2
  5566                                  @@doidiv284:
  5567                                  
  5568                                  ;-569 }
  5569                                  
  5570                                  @@doidiv279:
  5571                                  
  5572                                  ;-570 int domod(int ids) { doidiv(ids); prs("\n mov ax, dx"); }
  5573 00002A59 C9                       LEAVE
  5574 00002A5A C3                       ret
  5575                                  ; ENDP
  5576 00002A5B 5C6E206D6F76206278-     doidiv_0 db "\n mov bx, ",0
  5577 00002A64 2C2000             
  5578 00002A67 5C6E206377645C6E20-     doidiv_1 db "\n cwd\n idiv bx",0
  5579 00002A70 6964697620627800   
  5580 00002A78 5C6E206D6F76206478-     doidiv_2 db "\n mov dx, 0\n div bx",0
  5581 00002A81 2C20305C6E20646976-
  5582 00002A8A 20627800           
  5583 00002A8E 6F6E6C7920636F6E73-     doidiv_3 db "only const number or int as divisor allowed",0
  5584 00002A97 74206E756D62657220-
  5585 00002AA0 6F7220696E74206173-
  5586 00002AA9 2064697669736F7220-
  5587 00002AB2 616C6C6F77656400   
  5588 00002ABA 6F6E6C7920696E7420-     doidiv_4 db "only int as simple var divisor allowed",0
  5589 00002AC3 61732073696D706C65-
  5590 00002ACC 207661722064697669-
  5591 00002AD5 736F7220616C6C6F77-
  5592 00002ADE 656400             
  5593 00002AE1 6F6E6C7920696E742C-     doidiv_5 db "only int, no byte as divisor allowed",0
  5594 00002AEA 206E6F206279746520-
  5595 00002AF3 61732064697669736F-
  5596 00002AFC 7220616C6C6F776564-
  5597 00002B05 00                 
  5598 00002B06 5C6E206D6F76206278-     doidiv_6 db "\n mov bx, ",0
  5599 00002B0F 2C2000             
  5600 00002B12 5C6E206377645C6E20-     doidiv_7 db "\n cwd\n idiv bx",0
  5601 00002B1B 6964697620627800   
  5602 00002B23 5C6E206D6F76206478-     doidiv_8 db "\n mov dx, 0\n div bx",0
  5603 00002B2C 2C20305C6E20646976-
  5604 00002B35 20627800           
  5605                                  
  5606                                  domod:  ; *** PROC ***
  5607                                  ;Function : domod, Number of local variables: 1
  5608                                  ;   # type sign width line used address name   list of local variables
  5609                                  ;  200 var sign word   570 NULL bp+4    ids;
  5610 00002B39 C8000000                 ENTER  0,0
  5611 00002B3D FF7604                   push word [bp+4]
  5612 00002B40 E852FE                   call doidiv
  5613 00002B43 83C402                   add  sp, 2
  5614 00002B46 68[512B]                 push domod_0
  5615 00002B49 E84623                   call prs
  5616 00002B4C 83C402                   add  sp, 2
  5617                                  
  5618                                  ;-571 
  5619                                  
  5620                                  
  5621                                  ;-572 int docalltype[10]; int docallvalue[10];
  5622 00002B4F C9                       LEAVE
  5623 00002B50 C3                       ret
  5624                                  ; ENDP
  5625 00002B51 5C6E206D6F76206178-     domod_0 db "\n mov ax, dx",0
  5626 00002B5A 2C20647800         
  5627                                  section .bss
  5628                                  absolute 46864
  5629 0000B710 <res 00000014>          docalltype resw 10
  5630                                  section .text
  5631                                  section .bss
  5632                                  absolute 46884
  5633 0000B724 <res 00000014>          docallvalue resw 10
  5634                                  section .text
  5635                                  
  5636                                  ;-573 char procname[IDLENMAX]; // 1=CONST, 2=String, 3=&, 4=Name 5=register
  5637                                  
  5638                                  section .bss
  5639                                  absolute 46904
  5640 0000B738 <res 00000010>          procname resb 16
  5641                                  section .text
  5642                                  
  5643                                  ;-574 
  5644                                  
  5645                                  
  5646                                  ;-575 void docall1() {int i; int narg; int t0; int n0;  int sz32;
  5647                                  
  5648                                  
  5649                                  docall1:  ; *** PROC ***
  5650                                  
  5651                                  ;-576   narg=0;  sz32=0;
  5652                                  
  5653                                  ;Function : docall1, Number of local variables: 5
  5654                                  ;   # type sign width line used address name   list of local variables
  5655                                  ;  200 var sign word   575 NULL bp-2    i
  5656                                  ;  201 var sign word   575 NULL bp-4    narg
  5657                                  ;  202 var sign word   575 NULL bp-6    t0
  5658                                  ;  203 var sign word   575 NULL bp-8    n0
  5659                                  ;  204 var sign word   575 NULL bp-10    sz32;
  5660 00002B5F C80A0000                 ENTER  10,0
  5661 00002B63 66B800000000             mov eax, 0
  5662 00002B69 8946FC                   mov word [bp-4], ax
  5663 00002B6C 66B800000000             mov eax, 0
  5664 00002B72 8946F6                   mov word [bp-10], ax
  5665                                  
  5666                                  ;-577   checknamelen();
  5667                                  
  5668 00002B75 E8C3D5                   call checknamelen
  5669                                  
  5670                                  ;-578   strcpy(&procname, symbol);
  5671                                  
  5672 00002B78 8D066E81                 lea  ax, [symbol]
  5673 00002B7C 50                       push ax
  5674 00002B7D 8D0638B7                 lea  ax, [procname]
  5675 00002B81 50                       push ax
  5676 00002B82 E80736                   call strcpy
  5677 00002B85 83C404                   add  sp, 4
  5678                                  
  5679                                  ;-579   storecall();
  5680                                  
  5681 00002B88 E82FE1                   call storecall
  5682                                  
  5683                                  ;-580   expect('(');
  5684                                  
  5685 00002B8B 6A28                     push 40
  5686 00002B8D E8E421                   call expect
  5687 00002B90 83C402                   add  sp, 2
  5688                                  
  5689                                  ;-581 	if (istoken(')') ==0 ) {
  5690                                  
  5691 00002B93 6A29                     push 41
  5692 00002B95 E8B421                   call istoken
  5693 00002B98 83C402                   add  sp, 2
  5694 00002B9B 83F800                   cmp ax, 0
  5695 00002B9E 0F85FB02                 jne @@docall1285
  5696                                  
  5697                                  ;-582 	  do { narg++;
  5698                                  
  5699                                  @@docall1286:
  5700 00002BA2 FF46FC                   inc  word [bp-4]
  5701                                  
  5702                                  ;-583 	    if (narg >9 ) error1("Max. 9 parameters");  t0=0;
  5703                                  
  5704 00002BA5 8B46FC                   mov ax, [bp-4]
  5705 00002BA8 83F809                   cmp ax, 9
  5706 00002BAB 0F8E0900                 jle @@docall1287
  5707 00002BAF 68[E12E]                 push docall1_0
  5708 00002BB2 E8DE25                   call error1
  5709 00002BB5 83C402                   add  sp, 2
  5710                                  @@docall1287:
  5711 00002BB8 66B800000000             mov eax, 0
  5712 00002BBE 8946FA                   mov word [bp-6], ax
  5713                                  
  5714                                  ;-584       if(istoken(T_CONST)) {t0=1; n0=lexvalL; }
  5715                                  
  5716 00002BC1 680101                   push 257
  5717 00002BC4 E88521                   call istoken
  5718 00002BC7 83C402                   add  sp, 2
  5719 00002BCA 08C0                     or  al, al
  5720 00002BCC 0F841000                 je @@docall1288
  5721 00002BD0 66B801000000             mov eax, 1
  5722 00002BD6 8946FA                   mov word [bp-6], ax
  5723 00002BD9 66A1[4D00]               mov eax, [lexvalL]
  5724 00002BDD 8946F8                   mov word [bp-8], ax
  5725                                  
  5726                                  ;-585       if(istoken(T_STRING)){t0=2; n0=nconst;
  5727                                  
  5728                                  @@docall1288:
  5729 00002BE0 680201                   push 258
  5730 00002BE3 E86621                   call istoken
  5731 00002BE6 83C402                   add  sp, 2
  5732 00002BE9 08C0                     or  al, al
  5733 00002BEB 0F846600                 je @@docall1289
  5734 00002BEF 66B802000000             mov eax, 2
  5735 00002BF5 8946FA                   mov word [bp-6], ax
  5736 00002BF8 A1[4500]                 mov ax, [nconst]
  5737 00002BFB 8946F8                   mov word [bp-8], ax
  5738                                  
  5739                                  ;-586         eprs("\n"); eprs(fname); eprc(95);eprnum(nconst);eprs(" db ");
  5740                                  
  5741 00002BFE 68[F32E]                 push docall1_1
  5742 00002C01 E8F121                   call eprs
  5743 00002C04 83C402                   add  sp, 2
  5744 00002C07 8D06EE81                 lea  ax, [fname]
  5745 00002C0B 50                       push ax
  5746 00002C0C E8E621                   call eprs
  5747 00002C0F 83C402                   add  sp, 2
  5748 00002C12 6A5F                     push 95
  5749 00002C14 E8CB21                   call eprc
  5750 00002C17 83C402                   add  sp, 2
  5751 00002C1A FF36[4500]               push word [nconst]
  5752 00002C1E E81423                   call eprnum
  5753 00002C21 83C402                   add  sp, 2
  5754 00002C24 68[F62E]                 push docall1_2
  5755 00002C27 E8CB21                   call eprs
  5756 00002C2A 83C402                   add  sp, 2
  5757                                  
  5758                                  ;-587         eprc(34);eprs(symbol);eprc(34);eprs(",0"); nconst++; }
  5759                                  
  5760 00002C2D 6A22                     push 34
  5761 00002C2F E8B021                   call eprc
  5762 00002C32 83C402                   add  sp, 2
  5763 00002C35 8D066E81                 lea  ax, [symbol]
  5764 00002C39 50                       push ax
  5765 00002C3A E8B821                   call eprs
  5766 00002C3D 83C402                   add  sp, 2
  5767 00002C40 6A22                     push 34
  5768 00002C42 E89D21                   call eprc
  5769 00002C45 83C402                   add  sp, 2
  5770 00002C48 68[FB2E]                 push docall1_3
  5771 00002C4B E8A721                   call eprs
  5772 00002C4E 83C402                   add  sp, 2
  5773 00002C51 FF06[4500]               inc  word [nconst]
  5774                                  
  5775                                  ;-588       if(istoken('&'))     {t0=3; name1(); n0=searchname();}
  5776                                  
  5777                                  @@docall1289:
  5778 00002C55 6A26                     push 38
  5779 00002C57 E8F220                   call istoken
  5780 00002C5A 83C402                   add  sp, 2
  5781 00002C5D 08C0                     or  al, al
  5782 00002C5F 0F841200                 je @@docall1290
  5783 00002C63 66B803000000             mov eax, 3
  5784 00002C69 8946FA                   mov word [bp-6], ax
  5785 00002C6C E823E0                   call name1
  5786 00002C6F E804DE                   call searchname
  5787 00002C72 8946F8                   mov word [bp-8], ax
  5788                                  
  5789                                  ;-589       if(istoken(T_NAME))  { n0=checkreg();
  5790                                  
  5791                                  @@docall1290:
  5792 00002C75 680001                   push 256
  5793 00002C78 E8D120                   call istoken
  5794 00002C7B 83C402                   add  sp, 2
  5795 00002C7E 08C0                     or  al, al
  5796 00002C80 0F845100                 je @@docall1291
  5797 00002C84 E84AEE                   call checkreg
  5798 00002C87 8946F8                   mov word [bp-8], ax
  5799                                  
  5800                                  ;-590         if (n0) t0=5;
  5801                                  
  5802 00002C8A 8B46F8                   mov ax, [bp-8]
  5803 00002C8D 08C0                     or  al, al
  5804 00002C8F 0F840C00                 je @@docall1292
  5805 00002C93 66B805000000             mov eax, 5
  5806 00002C99 8946FA                   mov word [bp-6], ax
  5807                                  
  5808                                  ;-591         else {t0=4; n0=searchname();
  5809                                  
  5810 00002C9C E93600                   jmp @@docall1293
  5811                                  @@docall1292:
  5812 00002C9F 66B804000000             mov eax, 4
  5813 00002CA5 8946FA                   mov word [bp-6], ax
  5814 00002CA8 E8CBDD                   call searchname
  5815 00002CAB 8946F8                   mov word [bp-8], ax
  5816                                  
  5817                                  ;-592           p1=&GType; p1=p1+n0; if (*p1=='&') t0=3; }  }
  5818                                  
  5819 00002CAE B8B782                   mov ax, GType
  5820 00002CB1 A3[C000]                 mov word [p1], ax
  5821 00002CB4 A1[C000]                 mov ax, [p1]
  5822 00002CB7 0346F8                   add ax, [bp-8]
  5823 00002CBA A3[C000]                 mov word [p1], ax
  5824 00002CBD 8B1E[C000]               mov bx, [p1]
  5825 00002CC1 8A07                     mov al, [bx]
  5826 00002CC3 B400                     mov ah, 0
  5827 00002CC5 83F826                   cmp ax, 38
  5828 00002CC8 0F850900                 jne @@docall1294
  5829 00002CCC 66B803000000             mov eax, 3
  5830 00002CD2 8946FA                   mov word [bp-6], ax
  5831                                  @@docall1294:
  5832                                  @@docall1293:
  5833                                  
  5834                                  ;-593       if (t0==0) error1("parameter not recognized (no * allowed)");
  5835                                  
  5836                                  @@docall1291:
  5837 00002CD5 8B46FA                   mov ax, [bp-6]
  5838 00002CD8 83F800                   cmp ax, 0
  5839 00002CDB 0F850900                 jne @@docall1295
  5840 00002CDF 68[FE2E]                 push docall1_4
  5841 00002CE2 E8AE24                   call error1
  5842 00002CE5 83C402                   add  sp, 2
  5843                                  
  5844                                  ;-594       docalltype [narg] = t0;
  5845                                  
  5846                                  @@docall1295:
  5847 00002CE8 8B46FA                   mov ax, [bp-6]
  5848 00002CEB 8B5EFC                   mov bx, [bp-4]
  5849 00002CEE D1E3                     shl bx, 1
  5850 00002CF0 898710B7                 mov [docalltype+bx], ax
  5851                                  
  5852                                  ;-595       docallvalue[narg] = n0;
  5853                                  
  5854 00002CF4 8B46F8                   mov ax, [bp-8]
  5855 00002CF7 8B5EFC                   mov bx, [bp-4]
  5856 00002CFA D1E3                     shl bx, 1
  5857 00002CFC 898724B7                 mov [docallvalue+bx], ax
  5858                                  
  5859                                  ;-596     } while (istoken(','));
  5860                                  
  5861 00002D00 6A2C                     push 44
  5862 00002D02 E84720                   call istoken
  5863 00002D05 83C402                   add  sp, 2
  5864 00002D08 08C0                     or  al, al
  5865 00002D0A 0F840300                 je @@docall1296
  5866 00002D0E E991FE                   jmp @@docall1286
  5867                                  @@docall1296:
  5868                                  
  5869                                  ;-597     
  5870                                  
  5871                                  
  5872                                  ;-598   	expect(')');  i=narg;
  5873                                  
  5874 00002D11 6A29                     push 41
  5875 00002D13 E85E20                   call expect
  5876 00002D16 83C402                   add  sp, 2
  5877 00002D19 8B46FC                   mov ax, [bp-4]
  5878 00002D1C 8946FE                   mov word [bp-2], ax
  5879                                  
  5880                                  ;-599     do {
  5881                                  
  5882                                  @@docall1297:
  5883                                  
  5884                                  ;-600       t0 = docalltype [i];
  5885                                  
  5886 00002D1F 8B5EFE                   mov bx, [bp-2]
  5887 00002D22 D1E3                     shl bx, 1
  5888 00002D24 8B8710B7                 mov ax, [docalltype + bx]
  5889 00002D28 8946FA                   mov word [bp-6], ax
  5890                                  
  5891                                  ;-601       n0 = docallvalue[i];     
  5892                                  
  5893 00002D2B 8B5EFE                   mov bx, [bp-2]
  5894 00002D2E D1E3                     shl bx, 1
  5895 00002D30 8B8724B7                 mov ax, [docallvalue + bx]
  5896 00002D34 8946F8                   mov word [bp-8], ax
  5897                                  
  5898                                  ;-602       if(t0==1){ prs("\n push "); pint1(n0);}
  5899                                  
  5900 00002D37 8B46FA                   mov ax, [bp-6]
  5901 00002D3A 83F801                   cmp ax, 1
  5902 00002D3D 0F851200                 jne @@docall1298
  5903 00002D41 68[262F]                 push docall1_5
  5904 00002D44 E84B21                   call prs
  5905 00002D47 83C402                   add  sp, 2
  5906 00002D4A FF76F8                   push word [bp-8]
  5907 00002D4D E84822                   call pint1
  5908 00002D50 83C402                   add  sp, 2
  5909                                  
  5910                                  ;-603       if(t0==2){ prs("\n push "); if(NASM==0) prs("offset ");
  5911                                  
  5912                                  @@docall1298:
  5913 00002D53 8B46FA                   mov ax, [bp-6]
  5914 00002D56 83F802                   cmp ax, 2
  5915 00002D59 0F853700                 jne @@docall1299
  5916 00002D5D 68[2F2F]                 push docall1_6
  5917 00002D60 E82F21                   call prs
  5918 00002D63 83C402                   add  sp, 2
  5919 00002D66 A0[1500]                 mov al, [NASM]
  5920 00002D69 3C00                     cmp al, 0
  5921 00002D6B 0F850900                 jne @@docall1300
  5922 00002D6F 68[382F]                 push docall1_7
  5923 00002D72 E81D21                   call prs
  5924 00002D75 83C402                   add  sp, 2
  5925                                  
  5926                                  ;-604         prs(fname);prc(95);pint1(n0);}
  5927                                  
  5928                                  @@docall1300:
  5929 00002D78 8D06EE81                 lea  ax, [fname]
  5930 00002D7C 50                       push ax
  5931 00002D7D E81221                   call prs
  5932 00002D80 83C402                   add  sp, 2
  5933 00002D83 6A5F                     push 95
  5934 00002D85 E89B20                   call prc
  5935 00002D88 83C402                   add  sp, 2
  5936 00002D8B FF76F8                   push word [bp-8]
  5937 00002D8E E80722                   call pint1
  5938 00002D91 83C402                   add  sp, 2
  5939                                  
  5940                                  ;-605       if(t0==3){ prs("\n lea  ax, "); if(NASM==0)prs("word ptr ");  v(n0);
  5941                                  
  5942                                  @@docall1299:
  5943 00002D94 8B46FA                   mov ax, [bp-6]
  5944 00002D97 83F803                   cmp ax, 3
  5945 00002D9A 0F852D00                 jne @@docall1301
  5946 00002D9E 68[402F]                 push docall1_8
  5947 00002DA1 E8EE20                   call prs
  5948 00002DA4 83C402                   add  sp, 2
  5949 00002DA7 A0[1500]                 mov al, [NASM]
  5950 00002DAA 3C00                     cmp al, 0
  5951 00002DAC 0F850900                 jne @@docall1302
  5952 00002DB0 68[4D2F]                 push docall1_9
  5953 00002DB3 E8DC20                   call prs
  5954 00002DB6 83C402                   add  sp, 2
  5955                                  @@docall1302:
  5956 00002DB9 FF76F8                   push word [bp-8]
  5957 00002DBC E837D3                   call v
  5958 00002DBF 83C402                   add  sp, 2
  5959                                  
  5960                                  ;-606         prs("\n push ax");}
  5961                                  
  5962 00002DC2 68[572F]                 push docall1_10
  5963 00002DC5 E8CA20                   call prs
  5964 00002DC8 83C402                   add  sp, 2
  5965                                  
  5966                                  ;-607       if(t0==4){ gettypes(n0);
  5967                                  
  5968                                  @@docall1301:
  5969 00002DCB 8B46FA                   mov ax, [bp-6]
  5970 00002DCE 83F804                   cmp ax, 4
  5971 00002DD1 0F858C00                 jne @@docall1303
  5972 00002DD5 FF76F8                   push word [bp-8]
  5973 00002DD8 E850DB                   call gettypes
  5974 00002DDB 83C402                   add  sp, 2
  5975                                  
  5976                                  ;-608         if(wi==4)      { prs("\n push "); pwi(wi); v(n0); sz32+=2; }
  5977                                  
  5978 00002DDE A1[5B00]                 mov ax, [wi]
  5979 00002DE1 83F804                   cmp ax, 4
  5980 00002DE4 0F852300                 jne @@docall1304
  5981 00002DE8 68[622F]                 push docall1_11
  5982 00002DEB E8A420                   call prs
  5983 00002DEE 83C402                   add  sp, 2
  5984 00002DF1 FF36[5B00]               push word [wi]
  5985 00002DF5 E88FD3                   call pwi
  5986 00002DF8 83C402                   add  sp, 2
  5987 00002DFB FF76F8                   push word [bp-8]
  5988 00002DFE E8F5D2                   call v
  5989 00002E01 83C402                   add  sp, 2
  5990 00002E04 8346F602                 add  word [bp-10], 2
  5991                                  
  5992                                  ;-609         else if(wi==2) { prs("\n push "); pwi(wi); v(n0);}
  5993                                  
  5994 00002E08 E95600                   jmp @@docall1305
  5995                                  @@docall1304:
  5996 00002E0B A1[5B00]                 mov ax, [wi]
  5997 00002E0E 83F802                   cmp ax, 2
  5998 00002E11 0F851F00                 jne @@docall1306
  5999 00002E15 68[6B2F]                 push docall1_12
  6000 00002E18 E87720                   call prs
  6001 00002E1B 83C402                   add  sp, 2
  6002 00002E1E FF36[5B00]               push word [wi]
  6003 00002E22 E862D3                   call pwi
  6004 00002E25 83C402                   add  sp, 2
  6005 00002E28 FF76F8                   push word [bp-8]
  6006 00002E2B E8C8D2                   call v
  6007 00002E2E 83C402                   add  sp, 2
  6008                                  
  6009                                  ;-610         else { prs("\n mov al, byte ");  if(NASM==0)prs("ptr "); v(n0);
  6010                                  
  6011 00002E31 E92D00                   jmp @@docall1307
  6012                                  @@docall1306:
  6013 00002E34 68[742F]                 push docall1_13
  6014 00002E37 E85820                   call prs
  6015 00002E3A 83C402                   add  sp, 2
  6016 00002E3D A0[1500]                 mov al, [NASM]
  6017 00002E40 3C00                     cmp al, 0
  6018 00002E42 0F850900                 jne @@docall1308
  6019 00002E46 68[852F]                 push docall1_14
  6020 00002E49 E84620                   call prs
  6021 00002E4C 83C402                   add  sp, 2
  6022                                  @@docall1308:
  6023 00002E4F FF76F8                   push word [bp-8]
  6024 00002E52 E8A1D2                   call v
  6025 00002E55 83C402                   add  sp, 2
  6026                                  
  6027                                  ;-611         prs("\n mov ah, 0\n push ax"); } }
  6028                                  
  6029 00002E58 68[8A2F]                 push docall1_15
  6030 00002E5B E83420                   call prs
  6031 00002E5E 83C402                   add  sp, 2
  6032                                  @@docall1307:
  6033                                  @@docall1305:
  6034                                  
  6035                                  ;-612       if(t0==5){ prs("\n push "); printreg(n0, 0); if (n0 >= 47) sz32+=2;  }
  6036                                  
  6037                                  @@docall1303:
  6038 00002E61 8B46FA                   mov ax, [bp-6]
  6039 00002E64 83F805                   cmp ax, 5
  6040 00002E67 0F852200                 jne @@docall1309
  6041 00002E6B 68[A12F]                 push docall1_16
  6042 00002E6E E82120                   call prs
  6043 00002E71 83C402                   add  sp, 2
  6044 00002E74 6A00                     push 0
  6045 00002E76 FF76F8                   push word [bp-8]
  6046 00002E79 E8C9F0                   call printreg
  6047 00002E7C 83C404                   add  sp, 4
  6048 00002E7F 8B46F8                   mov ax, [bp-8]
  6049 00002E82 83F82F                   cmp ax, 47 ;unsigned : 1
  6050 00002E85 0F8C0400                 jl  @@docall1310
  6051 00002E89 8346F602                 add  word [bp-10], 2
  6052                                  @@docall1310:
  6053                                  
  6054                                  ;-613    i--; } while (i > 0);  }
  6055                                  
  6056                                  @@docall1309:
  6057 00002E8D FF4EFE                   dec  word [bp-2]
  6058 00002E90 8B46FE                   mov ax, [bp-2]
  6059 00002E93 83F800                   cmp ax, 0
  6060 00002E96 0F8E0300                 jle @@docall1311
  6061 00002E9A E982FE                   jmp @@docall1297
  6062                                  @@docall1311:
  6063                                  
  6064                                  ;-614 	 prs("\n call "); prs(&procname);
  6065                                  
  6066                                  @@docall1285:
  6067 00002E9D 68[AA2F]                 push docall1_17
  6068 00002EA0 E8EF1F                   call prs
  6069 00002EA3 83C402                   add  sp, 2
  6070 00002EA6 8D0638B7                 lea  ax, [procname]
  6071 00002EAA 50                       push ax
  6072 00002EAB E8E41F                   call prs
  6073 00002EAE 83C402                   add  sp, 2
  6074                                  
  6075                                  ;-615 	 if (narg>0) {prs("\n add  sp, ");
  6076                                  
  6077 00002EB1 8B46FC                   mov ax, [bp-4]
  6078 00002EB4 83F800                   cmp ax, 0
  6079 00002EB7 0F8E2400                 jle @@docall1312
  6080 00002EBB 68[B32F]                 push docall1_18
  6081 00002EBE E8D11F                   call prs
  6082 00002EC1 83C402                   add  sp, 2
  6083                                  
  6084                                  ;-616      narg=narg+narg; narg=narg+sz32; pint1(narg); } }
  6085                                  
  6086 00002EC4 8B46FC                   mov ax, [bp-4]
  6087 00002EC7 0346FC                   add ax, [bp-4]
  6088 00002ECA 8946FC                   mov word [bp-4], ax
  6089 00002ECD 8B46FC                   mov ax, [bp-4]
  6090 00002ED0 0346F6                   add ax, [bp-10]
  6091 00002ED3 8946FC                   mov word [bp-4], ax
  6092 00002ED6 FF76FC                   push word [bp-4]
  6093 00002ED9 E8BC20                   call pint1
  6094 00002EDC 83C402                   add  sp, 2
  6095                                  @@docall1312:
  6096                                  
  6097                                  ;-617 //////////////////////////////////////////////////////////////////////////////////
  6098                                  
  6099                                  
  6100                                  ;-618 int main() { getarg();
  6101 00002EDF C9                       LEAVE
  6102 00002EE0 C3                       ret
  6103                                  ; ENDP
  6104 00002EE1 4D61782E2039207061-     docall1_0 db "Max. 9 parameters",0
  6105 00002EEA 72616D657465727300 
  6106 00002EF3 5C6E00                  docall1_1 db "\n",0
  6107 00002EF6 2064622000              docall1_2 db " db ",0
  6108 00002EFB 2C3000                  docall1_3 db ",0",0
  6109 00002EFE 706172616D65746572-     docall1_4 db "parameter not recognized (no * allowed)",0
  6110 00002F07 206E6F74207265636F-
  6111 00002F10 676E697A656420286E-
  6112 00002F19 6F202A20616C6C6F77-
  6113 00002F22 65642900           
  6114 00002F26 5C6E20707573682000      docall1_5 db "\n push ",0
  6115 00002F2F 5C6E20707573682000      docall1_6 db "\n push ",0
  6116 00002F38 6F66667365742000        docall1_7 db "offset ",0
  6117 00002F40 5C6E206C6561202061-     docall1_8 db "\n lea  ax, ",0
  6118 00002F49 782C2000           
  6119 00002F4D 776F72642070747220-     docall1_9 db "word ptr ",0
  6120 00002F56 00                 
  6121 00002F57 5C6E20707573682061-     docall1_10 db "\n push ax",0
  6122 00002F60 7800               
  6123 00002F62 5C6E20707573682000      docall1_11 db "\n push ",0
  6124 00002F6B 5C6E20707573682000      docall1_12 db "\n push ",0
  6125 00002F74 5C6E206D6F7620616C-     docall1_13 db "\n mov al, byte ",0
  6126 00002F7D 2C20627974652000   
  6127 00002F85 7074722000              docall1_14 db "ptr ",0
  6128 00002F8A 5C6E206D6F76206168-     docall1_15 db "\n mov ah, 0\n push ax",0
  6129 00002F93 2C20305C6E20707573-
  6130 00002F9C 6820617800         
  6131 00002FA1 5C6E20707573682000      docall1_16 db "\n push ",0
  6132 00002FAA 5C6E2063616C6C2000      docall1_17 db "\n call ",0
  6133 00002FB3 5C6E20616464202073-     docall1_18 db "\n add  sp, ",0
  6134 00002FBC 702C2000           
  6135                                  
  6136                                  main:  ; *** PROC ***
  6137 00002FC0 E88C00                   call getarg
  6138                                  
  6139                                  ;-619   memresize(4096);       if (DOS_ERR) error1("memresize");
  6140                                  
  6141 00002FC3 680010                   push 4096
  6142 00002FC6 E8142E                   call memresize
  6143 00002FC9 83C402                   add  sp, 2
  6144 00002FCC A1[C200]                 mov ax, [DOS_ERR]
  6145 00002FCF 08C0                     or  al, al
  6146 00002FD1 0F840900                 je @@main313
  6147 00002FD5 68[1B30]                 push main_0
  6148 00002FD8 E8B821                   call error1
  6149 00002FDB 83C402                   add  sp, 2
  6150                                  
  6151                                  ;-620   segE=memalloc(4096);   if (DOS_ERR) error1("alloc memory");
  6152                                  
  6153                                  @@main313:
  6154 00002FDE 680010                   push 4096
  6155 00002FE1 E8122E                   call memalloc
  6156 00002FE4 83C402                   add  sp, 2
  6157 00002FE7 A3[B800]                 mov word [segE], ax
  6158 00002FEA A1[C200]                 mov ax, [DOS_ERR]
  6159 00002FED 08C0                     or  al, al
  6160 00002FEF 0F840900                 je @@main314
  6161 00002FF3 68[2530]                 push main_1
  6162 00002FF6 E89A21                   call error1
  6163 00002FF9 83C402                   add  sp, 2
  6164                                  
  6165                                  ;-621   CNameTop=0;            getfirstchar();
  6166                                  
  6167                                  @@main314:
  6168 00002FFC 66B800000000             mov eax, 0
  6169 00003002 A3[B400]                 mov word [CNameTop], ax
  6170 00003005 E82A00                   call getfirstchar
  6171                                  
  6172                                  ;-622   parse();
  6173                                  
  6174 00003008 E8F505                   call parse
  6175                                  
  6176                                  ;-623   callrecursive=0; checkcalls(); epilog();
  6177                                  
  6178 0000300B 66B800000000             mov eax, 0
  6179 00003011 A3[4B00]                 mov word [callrecursive], ax
  6180 00003014 E89B06                   call checkcalls
  6181 00003017 E8CE28                   call epilog
  6182                                  
  6183                                  ;-624 }
  6184                                  
  6185                                  
  6186                                  ;-625 int getfirstchar() { fgetsp=&fgetsdest; *fgetsp=0; thechar=fgets1(); }
  6187                                  
  6188 0000301A C3                       ret
  6189                                  ; ENDP
  6190 0000301B 6D656D726573697A65-     main_0 db "memresize",0
  6191 00003024 00                 
  6192 00003025 616C6C6F63206D656D-     main_1 db "alloc memory",0
  6193 0000302E 6F727900           
  6194                                  
  6195                                  getfirstchar:  ; *** PROC ***
  6196 00003032 B847B6                   mov ax, fgetsdest
  6197 00003035 A3[B600]                 mov word [fgetsp], ax
  6198 00003038 66B800000000             mov eax, 0
  6199 0000303E 8B1E[B600]               mov  bx, [fgetsp]
  6200 00003042 8807                     mov  [bx], al
  6201 00003044 E84D20                   call fgets1
  6202 00003047 A2[4200]                 mov byte [thechar], al
  6203                                  
  6204                                  ;-626 char *arglen=0x80; char *argv=0x82;
  6205                                  
  6206 0000304A C3                       ret
  6207                                  ; ENDP
  6208 0000304B 8000                    arglen dw 128
  6209 0000304D 8200                    argv dw 130
  6210                                  
  6211                                  ;-627 
  6212                                  
  6213                                  
  6214                                  ;-628 int getarg() { int arglen1; int i; char *c;
  6215                                  
  6216                                  
  6217                                  getarg:  ; *** PROC ***
  6218                                  
  6219                                  ;-629   arglen1=*arglen;                                     NASM=1; //default
  6220                                  
  6221                                  ;Function : getarg, Number of local variables: 3
  6222                                  ;   # type sign width line used address name   list of local variables
  6223                                  ;  200 var sign word   628 NULL bp-2    arglen1
  6224                                  ;  201 var sign word   628 NULL bp-4    i
  6225                                  ;  202 ptr sign byte   628 NULL bp-6    c;
  6226 0000304F C8060000                 ENTER  6,0
  6227 00003053 8B1E[4B30]               mov bx, [arglen]
  6228 00003057 8A07                     mov al, [bx]
  6229 00003059 B400                     mov ah, 0
  6230 0000305B 8946FE                   mov word [bp-2], ax
  6231 0000305E 66B801000000             mov eax, 1
  6232 00003064 A2[1500]                 mov byte [NASM], al
  6233                                  
  6234                                  ;-630   if (arglen1) { i=arglen1+129; *i=0; }
  6235                                  
  6236 00003067 8B46FE                   mov ax, [bp-2]
  6237 0000306A 08C0                     or  al, al
  6238 0000306C 0F841700                 je @@getarg315
  6239 00003070 8B46FE                   mov ax, [bp-2]
  6240 00003073 058100                   add ax, 129
  6241 00003076 8946FC                   mov word [bp-4], ax
  6242 00003079 66B800000000             mov eax, 0
  6243 0000307F 8B5EFC                   mov  bx, [bp-4]
  6244 00003082 8907                     mov  [bx], ax
  6245                                  
  6246                                  ;-631   else { cputs(Version1);
  6247                                  
  6248 00003084 E93C00                   jmp @@getarg316
  6249                                  @@getarg315:
  6250 00003087 8D06[0300]               lea  ax, [Version1]
  6251 0000308B 50                       push ax
  6252 0000308C E8F32F                   call cputs
  6253 0000308F 83C402                   add  sp, 2
  6254                                  
  6255                                  ;-632     cputs(" Usage: F.COM [/N/P] in_file[.C] (/N=NASM, /P=ProtMode): ");
  6256                                  
  6257 00003092 68[CC33]                 push getarg_0
  6258 00003095 E8EA2F                   call cputs
  6259 00003098 83C402                   add  sp, 2
  6260                                  
  6261                                  ;-633     DOS_NoBytes=readRL(argv, 0, CMDLENMAX); c=DOS_NoBytes+128; *c=0; prnl(); }
  6262                                  
  6263 0000309B 6A43                     push 67
  6264 0000309D 6A00                     push 0
  6265 0000309F FF36[4D30]               push word [argv]
  6266 000030A3 E8F332                   call readRL
  6267 000030A6 83C406                   add  sp, 6
  6268 000030A9 A3[C400]                 mov word [DOS_NoBytes], ax
  6269 000030AC A1[C400]                 mov ax, [DOS_NoBytes]
  6270 000030AF 058000                   add ax, 128
  6271 000030B2 8946FA                   mov word [bp-6], ax
  6272 000030B5 66B800000000             mov eax, 0
  6273 000030BB 8B5EFA                   mov  bx, [bp-6]
  6274 000030BE 8807                     mov  [bx], al
  6275 000030C0 E8C11D                   call prnl
  6276                                  
  6277                                  ;-634   strcpy(namein, argv);
  6278                                  
  6279                                  @@getarg316:
  6280 000030C3 FF36[4D30]               push word [argv]
  6281 000030C7 8D063182                 lea  ax, [namein]
  6282 000030CB 50                       push ax
  6283 000030CC E8BD30                   call strcpy
  6284 000030CF 83C404                   add  sp, 4
  6285                                  
  6286                                  ;-635   if (instr2(namein, '.') == 0) strcat1(namein, ".C");
  6287                                  
  6288 000030D2 6A2E                     push 46
  6289 000030D4 8D063182                 lea  ax, [namein]
  6290 000030D8 50                       push ax
  6291 000030D9 E82131                   call instr2
  6292 000030DC 83C404                   add  sp, 4
  6293 000030DF 83F800                   cmp ax, 0
  6294 000030E2 0F850E00                 jne @@getarg317
  6295 000030E6 68[0634]                 push getarg_1
  6296 000030E9 8D063182                 lea  ax, [namein]
  6297 000030ED 50                       push ax
  6298 000030EE E84432                   call strcat1
  6299 000030F1 83C404                   add  sp, 4
  6300                                  
  6301                                  ;-636   toupper(namein);
  6302                                  
  6303                                  @@getarg317:
  6304 000030F4 8D063182                 lea  ax, [namein]
  6305 000030F8 50                       push ax
  6306 000030F9 E82230                   call toupper
  6307 000030FC 83C402                   add  sp, 2
  6308                                  
  6309                                  ;-637   c=instr2(namein, '/');
  6310                                  
  6311 000030FF 6A2F                     push 47
  6312 00003101 8D063182                 lea  ax, [namein]
  6313 00003105 50                       push ax
  6314 00003106 E8F430                   call instr2
  6315 00003109 83C404                   add  sp, 4
  6316 0000310C 8946FA                   mov word [bp-6], ax
  6317                                  
  6318                                  ;-638   while (c != 0)  { c++;   //TODO only one parameter possible
  6319                                  
  6320                                  @@getarg318:
  6321 0000310F 8B46FA                   mov ax, [bp-6]
  6322 00003112 83F800                   cmp ax, 0
  6323 00003115 0F845500                 je  @@getarg319
  6324 00003119 FF46FA                   inc  word [bp-6]
  6325                                  
  6326                                  ;-639          if (*c == 'N') NASM=1;
  6327                                  
  6328 0000311C 8B5EFA                   mov bx, [bp-6]
  6329 0000311F 8A07                     mov al, [bx]
  6330 00003121 B400                     mov ah, 0
  6331 00003123 83F84E                   cmp ax, 78
  6332 00003126 0F850900                 jne @@getarg320
  6333 0000312A 66B801000000             mov eax, 1
  6334 00003130 A2[1500]                 mov byte [NASM], al
  6335                                  
  6336                                  ;-640          if (*c == 'P') PROTECTED=1;
  6337                                  
  6338                                  @@getarg320:
  6339 00003133 8B5EFA                   mov bx, [bp-6]
  6340 00003136 8A07                     mov al, [bx]
  6341 00003138 B400                     mov ah, 0
  6342 0000313A 83F850                   cmp ax, 80
  6343 0000313D 0F850900                 jne @@getarg321
  6344 00003141 66B801000000             mov eax, 1
  6345 00003147 A2[1600]                 mov byte [PROTECTED], al
  6346                                  
  6347                                  ;-641 //    else {cputs("Parameter unknown "); exitR(3);  }
  6348                                  
  6349                                  
  6350                                  ;-642     c+=2; strcpy(namein, c);
  6351                                  
  6352                                  @@getarg321:
  6353 0000314A 8346FA02                 add  word [bp-6], 2
  6354 0000314E FF76FA                   push word [bp-6]
  6355 00003151 8D063182                 lea  ax, [namein]
  6356 00003155 50                       push ax
  6357 00003156 E83330                   call strcpy
  6358 00003159 83C404                   add  sp, 4
  6359                                  
  6360                                  ;-643     c=instr2(namein, '/'); }
  6361                                  
  6362 0000315C 6A2F                     push 47
  6363 0000315E 8D063182                 lea  ax, [namein]
  6364 00003162 50                       push ax
  6365 00003163 E89730                   call instr2
  6366 00003166 83C404                   add  sp, 4
  6367 00003169 8946FA                   mov word [bp-6], ax
  6368                                  
  6369                                  ;-644   ltrim(namein);
  6370                                  
  6371 0000316C EBA1                     jmp @@getarg318
  6372                                  @@getarg319:
  6373 0000316E 8D063182                 lea  ax, [namein]
  6374 00003172 50                       push ax
  6375 00003173 E88404                   call ltrim
  6376 00003176 83C402                   add  sp, 2
  6377                                  
  6378                                  ;-645   strcpy(namelst, namein); i=strlen(namelst); i--; c=&namelst+i; *c='S';
  6379                                  
  6380 00003179 8D063182                 lea  ax, [namein]
  6381 0000317D 50                       push ax
  6382 0000317E 8D067482                 lea  ax, [namelst]
  6383 00003182 50                       push ax
  6384 00003183 E80630                   call strcpy
  6385 00003186 83C404                   add  sp, 4
  6386 00003189 8D067482                 lea  ax, [namelst]
  6387 0000318D 50                       push ax
  6388 0000318E E8D02F                   call strlen
  6389 00003191 83C402                   add  sp, 2
  6390 00003194 8946FC                   mov word [bp-4], ax
  6391 00003197 FF4EFC                   dec  word [bp-4]
  6392 0000319A B87482                   mov ax, namelst
  6393 0000319D 0346FC                   add ax, [bp-4]
  6394 000031A0 8946FA                   mov word [bp-6], ax
  6395 000031A3 66B853000000             mov eax, 83
  6396 000031A9 8B5EFA                   mov  bx, [bp-6]
  6397 000031AC 8807                     mov  [bx], al
  6398                                  
  6399                                  ;-646  
  6400                                  
  6401                                  
  6402                                  ;-647   fdin=openR (namein);
  6403                                  
  6404 000031AE 8D063182                 lea  ax, [namein]
  6405 000031B2 50                       push ax
  6406 000031B3 E8AD31                   call openR
  6407 000031B6 83C402                   add  sp, 2
  6408 000031B9 A3[3400]                 mov word [fdin], ax
  6409                                  
  6410                                  ;-648   if(DOS_ERR){cputs("Source file missing (.C): "); cputs(namein); exitR(1); }
  6411                                  
  6412 000031BC A1[C200]                 mov ax, [DOS_ERR]
  6413 000031BF 08C0                     or  al, al
  6414 000031C1 0F841C00                 je @@getarg322
  6415 000031C5 68[0934]                 push getarg_2
  6416 000031C8 E8B72E                   call cputs
  6417 000031CB 83C402                   add  sp, 2
  6418 000031CE 8D063182                 lea  ax, [namein]
  6419 000031D2 50                       push ax
  6420 000031D3 E8AC2E                   call cputs
  6421 000031D6 83C402                   add  sp, 2
  6422 000031D9 6A01                     push 1
  6423 000031DB E8F331                   call exitR
  6424 000031DE 83C402                   add  sp, 2
  6425                                  
  6426                                  ;-649   fdout=creatR(namelst);
  6427                                  
  6428                                  @@getarg322:
  6429 000031E1 8D067482                 lea  ax, [namelst]
  6430 000031E5 50                       push ax
  6431 000031E6 E88931                   call creatR
  6432 000031E9 83C402                   add  sp, 2
  6433 000031EC A3[3600]                 mov word [fdout], ax
  6434                                  
  6435                                  ;-650   if(DOS_ERR){cputs("list file not creatable: ");cputs(namelst);exitR(2);}
  6436                                  
  6437 000031EF A1[C200]                 mov ax, [DOS_ERR]
  6438 000031F2 08C0                     or  al, al
  6439 000031F4 0F841C00                 je @@getarg323
  6440 000031F8 68[2434]                 push getarg_3
  6441 000031FB E8842E                   call cputs
  6442 000031FE 83C402                   add  sp, 2
  6443 00003201 8D067482                 lea  ax, [namelst]
  6444 00003205 50                       push ax
  6445 00003206 E8792E                   call cputs
  6446 00003209 83C402                   add  sp, 2
  6447 0000320C 6A02                     push 2
  6448 0000320E E8C031                   call exitR
  6449 00003211 83C402                   add  sp, 2
  6450                                  
  6451                                  ;-651   prs("\n; ");prs(Version1);
  6452                                  
  6453                                  @@getarg323:
  6454 00003214 68[3E34]                 push getarg_4
  6455 00003217 E8781C                   call prs
  6456 0000321A 83C402                   add  sp, 2
  6457 0000321D 8D06[0300]               lea  ax, [Version1]
  6458 00003221 50                       push ax
  6459 00003222 E86D1C                   call prs
  6460 00003225 83C402                   add  sp, 2
  6461                                  
  6462                                  ;-652   prs(", Arglen: "); pint1(arglen1); if(arglen1){prs(", Argv: "); prs(argv);}
  6463                                  
  6464 00003228 68[4334]                 push getarg_5
  6465 0000322B E8641C                   call prs
  6466 0000322E 83C402                   add  sp, 2
  6467 00003231 FF76FE                   push word [bp-2]
  6468 00003234 E8611D                   call pint1
  6469 00003237 83C402                   add  sp, 2
  6470 0000323A 8B46FE                   mov ax, [bp-2]
  6471 0000323D 08C0                     or  al, al
  6472 0000323F 0F841300                 je @@getarg324
  6473 00003243 68[4E34]                 push getarg_6
  6474 00003246 E8491C                   call prs
  6475 00003249 83C402                   add  sp, 2
  6476 0000324C FF36[4D30]               push word [argv]
  6477 00003250 E83F1C                   call prs
  6478 00003253 83C402                   add  sp, 2
  6479                                  
  6480                                  ;-653   prs(", Source: "); prs(namein);  prs(", Output asm: "); prs(namelst);
  6481                                  
  6482                                  @@getarg324:
  6483 00003256 68[5734]                 push getarg_7
  6484 00003259 E8361C                   call prs
  6485 0000325C 83C402                   add  sp, 2
  6486 0000325F 8D063182                 lea  ax, [namein]
  6487 00003263 50                       push ax
  6488 00003264 E82B1C                   call prs
  6489 00003267 83C402                   add  sp, 2
  6490 0000326A 68[6234]                 push getarg_8
  6491 0000326D E8221C                   call prs
  6492 00003270 83C402                   add  sp, 2
  6493 00003273 8D067482                 lea  ax, [namelst]
  6494 00003277 50                       push ax
  6495 00003278 E8171C                   call prs
  6496 0000327B 83C402                   add  sp, 2
  6497                                  
  6498                                  ;-654   prs("\n;PROTECTED: "); pint1(PROTECTED);
  6499                                  
  6500 0000327E 68[7134]                 push getarg_9
  6501 00003281 E80E1C                   call prs
  6502 00003284 83C402                   add  sp, 2
  6503 00003287 A0[1600]                 mov al, byte [PROTECTED]
  6504 0000328A B400                     mov ah, 0
  6505 0000328C 50                       push ax
  6506 0000328D E8081D                   call pint1
  6507 00003290 83C402                   add  sp, 2
  6508                                  
  6509                                  ;-655   if (isvirtual86()) prs(" V86 ON. ");else prs(" V86 OFF. ");
  6510                                  
  6511 00003293 E8232C                   call isvirtual86
  6512 00003296 08C0                     or  al, al
  6513 00003298 0F840C00                 je @@getarg325
  6514 0000329C 68[8034]                 push getarg_10
  6515 0000329F E8F01B                   call prs
  6516 000032A2 83C402                   add  sp, 2
  6517 000032A5 E90900                   jmp @@getarg326
  6518                                  @@getarg325:
  6519 000032A8 68[8A34]                 push getarg_11
  6520 000032AB E8E41B                   call prs
  6521 000032AE 83C402                   add  sp, 2
  6522                                  
  6523                                  ;-656   if (is32bit()) prs(" 32bit: ON. "); else prs("  32bit: OFF. ");
  6524                                  
  6525                                  @@getarg326:
  6526 000032B1 E80C2C                   call is32bit
  6527 000032B4 08C0                     or  al, al
  6528 000032B6 0F840C00                 je @@getarg327
  6529 000032BA 68[9534]                 push getarg_12
  6530 000032BD E8D21B                   call prs
  6531 000032C0 83C402                   add  sp, 2
  6532 000032C3 E90900                   jmp @@getarg328
  6533                                  @@getarg327:
  6534 000032C6 68[A234]                 push getarg_13
  6535 000032C9 E8C61B                   call prs
  6536 000032CC 83C402                   add  sp, 2
  6537                                  
  6538                                  ;-657   if (NASM) prs("\norg  256 ; NASM ON\njmp main"); else
  6539                                  
  6540                                  @@getarg328:
  6541 000032CF A0[1500]                 mov al, [NASM]
  6542 000032D2 08C0                     or  al, al
  6543 000032D4 0F840C00                 je @@getarg329
  6544 000032D8 68[B134]                 push getarg_14
  6545 000032DB E8B41B                   call prs
  6546 000032DE 83C402                   add  sp, 2
  6547                                  
  6548                                  ;-658   prs("\n.MODEL TINY,C\n.386P\n.CODE\nJUMPS\nLOCALS\nSTARTUPCODE\njmp main");
  6549                                  
  6550 000032E1 E90900                   jmp @@getarg330
  6551                                  @@getarg329:
  6552 000032E4 68[D034]                 push getarg_15
  6553 000032E7 E8A81B                   call prs
  6554 000032EA 83C402                   add  sp, 2
  6555                                  
  6556                                  ;-659   if (PROTECTED) {prs("\n; Protected (unreal) mode ON. Ldata: "); prL(ldata);
  6557                                  
  6558                                  @@getarg330:
  6559 000032ED A0[1600]                 mov al, [PROTECTED]
  6560 000032F0 08C0                     or  al, al
  6561 000032F2 0F84D400                 je @@getarg331
  6562 000032F6 68[1435]                 push getarg_16
  6563 000032F9 E8961B                   call prs
  6564 000032FC 83C402                   add  sp, 2
  6565 000032FF 66FF36[1C00]             push dword [ldata]
  6566 00003304 E83E2C                   call prL
  6567 00003307 83C404                   add  sp, 4
  6568                                  
  6569                                  ;-660   if (isvirtual86()) cputs(" V86 ON. ");else cputs(" V86 OFF. ");
  6570                                  
  6571 0000330A E8AC2B                   call isvirtual86
  6572 0000330D 08C0                     or  al, al
  6573 0000330F 0F840C00                 je @@getarg332
  6574 00003313 68[3C35]                 push getarg_17
  6575 00003316 E8692D                   call cputs
  6576 00003319 83C402                   add  sp, 2
  6577 0000331C E90900                   jmp @@getarg333
  6578                                  @@getarg332:
  6579 0000331F 68[4635]                 push getarg_18
  6580 00003322 E85D2D                   call cputs
  6581 00003325 83C402                   add  sp, 2
  6582                                  
  6583                                  ;-661   if (is32bit()) cputs(" 32bit: ON. "); else cputs("  32bit: OFF. ");
  6584                                  
  6585                                  @@getarg333:
  6586 00003328 E8952B                   call is32bit
  6587 0000332B 08C0                     or  al, al
  6588 0000332D 0F840C00                 je @@getarg334
  6589 00003331 68[5135]                 push getarg_19
  6590 00003334 E84B2D                   call cputs
  6591 00003337 83C402                   add  sp, 2
  6592 0000333A E90900                   jmp @@getarg335
  6593                                  @@getarg334:
  6594 0000333D 68[5E35]                 push getarg_20
  6595 00003340 E83F2D                   call cputs
  6596 00003343 83C402                   add  sp, 2
  6597                                  
  6598                                  ;-662   INITCPU32();
  6599                                  
  6600                                  @@getarg335:
  6601 00003346 E8962B                   call INITCPU32
  6602                                  
  6603                                  ;-663   prs("\n;After INITCPU32: ");
  6604                                  
  6605 00003349 68[6D35]                 push getarg_21
  6606 0000334C E8431B                   call prs
  6607 0000334F 83C402                   add  sp, 2
  6608                                  
  6609                                  ;-664   if (isvirtual86()) cputs(" after INIT V86 ON. ");
  6610                                  
  6611 00003352 E8642B                   call isvirtual86
  6612 00003355 08C0                     or  al, al
  6613 00003357 0F840C00                 je @@getarg336
  6614 0000335B 68[8235]                 push getarg_22
  6615 0000335E E8212D                   call cputs
  6616 00003361 83C402                   add  sp, 2
  6617                                  
  6618                                  ;-665     else cputs(" after INIT V86 OFF. ");
  6619                                  
  6620 00003364 E90900                   jmp @@getarg337
  6621                                  @@getarg336:
  6622 00003367 68[9735]                 push getarg_23
  6623 0000336A E8152D                   call cputs
  6624 0000336D 83C402                   add  sp, 2
  6625                                  
  6626                                  ;-666   if (is32bit()) cputs(" 32bit: ON. "); else cputs("  32bit: OFF. ");
  6627                                  
  6628                                  @@getarg337:
  6629 00003370 E84D2B                   call is32bit
  6630 00003373 08C0                     or  al, al
  6631 00003375 0F840C00                 je @@getarg338
  6632 00003379 68[AD35]                 push getarg_24
  6633 0000337C E8032D                   call cputs
  6634 0000337F 83C402                   add  sp, 2
  6635 00003382 E90900                   jmp @@getarg339
  6636                                  @@getarg338:
  6637 00003385 68[BA35]                 push getarg_25
  6638 00003388 E8F72C                   call cputs
  6639 0000338B 83C402                   add  sp, 2
  6640                                  
  6641                                  ;-667    if (isvirtual86()) prs(" V86 ON. ");else prs(" V86 OFF. ");
  6642                                  
  6643                                  @@getarg339:
  6644 0000338E E8282B                   call isvirtual86
  6645 00003391 08C0                     or  al, al
  6646 00003393 0F840C00                 je @@getarg340
  6647 00003397 68[C935]                 push getarg_26
  6648 0000339A E8F51A                   call prs
  6649 0000339D 83C402                   add  sp, 2
  6650 000033A0 E90900                   jmp @@getarg341
  6651                                  @@getarg340:
  6652 000033A3 68[D335]                 push getarg_27
  6653 000033A6 E8E91A                   call prs
  6654 000033A9 83C402                   add  sp, 2
  6655                                  
  6656                                  ;-668   if (is32bit()) prs(" 32bit: ON. "); else prs("  32bit: OFF. ");   }
  6657                                  
  6658                                  @@getarg341:
  6659 000033AC E8112B                   call is32bit
  6660 000033AF 08C0                     or  al, al
  6661 000033B1 0F840C00                 je @@getarg342
  6662 000033B5 68[DE35]                 push getarg_28
  6663 000033B8 E8D71A                   call prs
  6664 000033BB 83C402                   add  sp, 2
  6665 000033BE E90900                   jmp @@getarg343
  6666                                  @@getarg342:
  6667 000033C1 68[EB35]                 push getarg_29
  6668 000033C4 E8CB1A                   call prs
  6669 000033C7 83C402                   add  sp, 2
  6670                                  @@getarg343:
  6671                                  
  6672                                  ;-669 }
  6673                                  
  6674                                  @@getarg331:
  6675                                  
  6676                                  ;-670 ltrim(char *s) {
  6677 000033CA C9                       LEAVE
  6678 000033CB C3                       ret
  6679                                  ; ENDP
  6680 000033CC 2055736167653A2046-     getarg_0 db " Usage: F.COM [/N/P] in_file[.C] (/N=NASM, /P=ProtMode): ",0
  6681 000033D5 2E434F4D205B2F4E2F-
  6682 000033DE 505D20696E5F66696C-
  6683 000033E7 655B2E435D20282F4E-
  6684 000033F0 3D4E41534D2C202F50-
  6685 000033F9 3D50726F744D6F6465-
  6686 00003402 293A2000           
  6687 00003406 2E4300                  getarg_1 db ".C",0
  6688 00003409 536F75726365206669-     getarg_2 db "Source file missing (.C): ",0
  6689 00003412 6C65206D697373696E-
  6690 0000341B 6720282E43293A2000 
  6691 00003424 6C6973742066696C65-     getarg_3 db "list file not creatable: ",0
  6692 0000342D 206E6F742063726561-
  6693 00003436 7461626C653A2000   
  6694 0000343E 5C6E3B2000              getarg_4 db "\n; ",0
  6695 00003443 2C204172676C656E3A-     getarg_5 db ", Arglen: ",0
  6696 0000344C 2000               
  6697 0000344E 2C20417267763A2000      getarg_6 db ", Argv: ",0
  6698 00003457 2C20536F757263653A-     getarg_7 db ", Source: ",0
  6699 00003460 2000               
  6700 00003462 2C204F757470757420-     getarg_8 db ", Output asm: ",0
  6701 0000346B 61736D3A2000       
  6702 00003471 5C6E3B50524F544543-     getarg_9 db "\n;PROTECTED: ",0
  6703 0000347A 5445443A2000       
  6704 00003480 20563836204F4E2E20-     getarg_10 db " V86 ON. ",0
  6705 00003489 00                 
  6706 0000348A 20563836204F46462E-     getarg_11 db " V86 OFF. ",0
  6707 00003493 2000               
  6708 00003495 2033326269743A204F-     getarg_12 db " 32bit: ON. ",0
  6709 0000349E 4E2E2000           
  6710 000034A2 202033326269743A20-     getarg_13 db "  32bit: OFF. ",0
  6711 000034AB 4F46462E2000       
  6712 000034B1 5C6E6F726720203235-     getarg_14 db "\norg  256 ; NASM ON\njmp main",0
  6713 000034BA 36203B204E41534D20-
  6714 000034C3 4F4E5C6E6A6D70206D-
  6715 000034CC 61696E00           
  6716 000034D0 5C6E2E4D4F44454C20-     getarg_15 db "\n.MODEL TINY,C\n.386P\n.CODE\nJUMPS\nLOCALS\nSTARTUPCODE\njmp main",0
  6717 000034D9 54494E592C435C6E2E-
  6718 000034E2 333836505C6E2E434F-
  6719 000034EB 44455C6E4A554D5053-
  6720 000034F4 5C6E4C4F43414C535C-
  6721 000034FD 6E5354415254555043-
  6722 00003506 4F44455C6E6A6D7020-
  6723 0000350F 6D61696E00         
  6724 00003514 5C6E3B2050726F7465-     getarg_16 db "\n; Protected (unreal) mode ON. Ldata: ",0
  6725 0000351D 637465642028756E72-
  6726 00003526 65616C29206D6F6465-
  6727 0000352F 204F4E2E204C646174-
  6728 00003538 613A2000           
  6729 0000353C 20563836204F4E2E20-     getarg_17 db " V86 ON. ",0
  6730 00003545 00                 
  6731 00003546 20563836204F46462E-     getarg_18 db " V86 OFF. ",0
  6732 0000354F 2000               
  6733 00003551 2033326269743A204F-     getarg_19 db " 32bit: ON. ",0
  6734 0000355A 4E2E2000           
  6735 0000355E 202033326269743A20-     getarg_20 db "  32bit: OFF. ",0
  6736 00003567 4F46462E2000       
  6737 0000356D 5C6E3B416674657220-     getarg_21 db "\n;After INITCPU32: ",0
  6738 00003576 494E49544350553332-
  6739 0000357F 3A2000             
  6740 00003582 20616674657220494E-     getarg_22 db " after INIT V86 ON. ",0
  6741 0000358B 495420563836204F4E-
  6742 00003594 2E2000             
  6743 00003597 20616674657220494E-     getarg_23 db " after INIT V86 OFF. ",0
  6744 000035A0 495420563836204F46-
  6745 000035A9 462E2000           
  6746 000035AD 2033326269743A204F-     getarg_24 db " 32bit: ON. ",0
  6747 000035B6 4E2E2000           
  6748 000035BA 202033326269743A20-     getarg_25 db "  32bit: OFF. ",0
  6749 000035C3 4F46462E2000       
  6750 000035C9 20563836204F4E2E20-     getarg_26 db " V86 ON. ",0
  6751 000035D2 00                 
  6752 000035D3 20563836204F46462E-     getarg_27 db " V86 OFF. ",0
  6753 000035DC 2000               
  6754 000035DE 2033326269743A204F-     getarg_28 db " 32bit: ON. ",0
  6755 000035E7 4E2E2000           
  6756 000035EB 202033326269743A20-     getarg_29 db "  32bit: OFF. ",0
  6757 000035F4 4F46462E2000       
  6758                                  
  6759                                  ltrim:  ; *** PROC ***
  6760                                  
  6761                                  ;-671     }
  6762                                  
  6763                                  ;Function : ltrim, Number of local variables: 1
  6764                                  ;   # type sign width line used address name   list of local variables
  6765                                  ;  200 ptr sign byte   670 NULL bp+4    s;
  6766 000035FA C8000000                 ENTER  0,0
  6767                                  
  6768                                  ;-672 int parse() { token=getlex(); do {
  6769 000035FE C9                       LEAVE
  6770 000035FF C3                       ret
  6771                                  ; ENDP
  6772                                  
  6773                                  parse:  ; *** PROC ***
  6774 00003600 E8A90E                   call getlex
  6775 00003603 A3[3D00]                 mov word [token], ax
  6776                                  @@parse344:
  6777                                  
  6778                                  ;-673     if (token <= 0) return 1;
  6779                                  
  6780 00003606 A1[3D00]                 mov ax, [token]
  6781 00003609 83F800                   cmp ax, 0
  6782 0000360C 0F8F0900                 jg  @@parse345
  6783 00003610 66B801000000             mov eax, 1
  6784 00003616 E97D00                   jmp @@retn
  6785                                  
  6786                                  ;-674     if (istoken('#')) {
  6787                                  
  6788                                  @@parse345:
  6789 00003619 6A23                     push 35
  6790 0000361B E82E17                   call istoken
  6791 0000361E 83C402                   add  sp, 2
  6792 00003621 08C0                     or  al, al
  6793 00003623 0F843600                 je @@parse346
  6794                                  
  6795                                  ;-675       if (istoken(T_DEFINE)) dodefine();
  6796                                  
  6797 00003627 68FF01                   push 511
  6798 0000362A E81F17                   call istoken
  6799 0000362D 83C402                   add  sp, 2
  6800 00003630 08C0                     or  al, al
  6801 00003632 0F840600                 je @@parse347
  6802 00003636 E8B105                   call dodefine
  6803                                  
  6804                                  ;-676       else if (istoken(T_INCLUDE)) doinclude();
  6805                                  
  6806 00003639 E91E00                   jmp @@parse348
  6807                                  @@parse347:
  6808 0000363C 68FE01                   push 510
  6809 0000363F E80A17                   call istoken
  6810 00003642 83C402                   add  sp, 2
  6811 00003645 08C0                     or  al, al
  6812 00003647 0F840600                 je @@parse349
  6813 0000364B E89204                   call doinclude
  6814                                  
  6815                                  ;-677       else error1("define or include expected");  }
  6816                                  
  6817 0000364E E90900                   jmp @@parse350
  6818                                  @@parse349:
  6819 00003651 68[9736]                 push parse_0
  6820 00003654 E83C1B                   call error1
  6821 00003657 83C402                   add  sp, 2
  6822                                  @@parse350:
  6823                                  @@parse348:
  6824                                  
  6825                                  ;-678     else{ typeName();
  6826                                  
  6827 0000365A E92A00                   jmp @@parse351
  6828                                  @@parse346:
  6829 0000365D E8F6D4                   call typeName
  6830                                  
  6831                                  ;-679     if (token=='(') dofunc(); else if (istoken('!')) doLdata(); else doglob(); }
  6832                                  
  6833 00003660 A1[3D00]                 mov ax, [token]
  6834 00003663 83F828                   cmp ax, 40
  6835 00003666 0F850600                 jne @@parse352
  6836 0000366A E83CD8                   call dofunc
  6837 0000366D E91700                   jmp @@parse353
  6838                                  @@parse352:
  6839 00003670 6A21                     push 33
  6840 00003672 E8D716                   call istoken
  6841 00003675 83C402                   add  sp, 2
  6842 00003678 08C0                     or  al, al
  6843 0000367A 0F840600                 je @@parse354
  6844 0000367E E89FCB                   call doLdata
  6845 00003681 E90300                   jmp @@parse355
  6846                                  @@parse354:
  6847 00003684 E85ACD                   call doglob
  6848                                  @@parse355:
  6849                                  @@parse353:
  6850                                  
  6851                                  ;-680   } while(1);
  6852                                  
  6853                                  @@parse351:
  6854 00003687 66B801000000             mov eax, 1
  6855 0000368D 08C0                     or  al, al
  6856 0000368F 0F840300                 je @@parse356
  6857 00003693 E970FF                   jmp @@parse344
  6858                                  @@parse356:
  6859                                  
  6860                                  ;-681 }
  6861                                  
  6862                                  
  6863                                  ;-682 int checkcalls() { int i; int j; int k;
  6864                                  
  6865                                  @@retn:
  6866 00003696 C3                       ret
  6867                                  ; ENDP
  6868 00003697 646566696E65206F72-     parse_0 db "define or include expected",0
  6869 000036A0 20696E636C75646520-
  6870 000036A9 657870656374656400 
  6871                                  
  6872                                  checkcalls:  ; *** PROC ***
  6873                                  
  6874                                  ;-683   prs("\n \n; missing functions: ");
  6875                                  
  6876                                  ;Function : checkcalls, Number of local variables: 3
  6877                                  ;   # type sign width line used address name   list of local variables
  6878                                  ;  200 var sign word   682 NULL bp-2    i
  6879                                  ;  201 var sign word   682 NULL bp-4    j
  6880                                  ;  202 var sign word   682 NULL bp-6    k;
  6881 000036B2 C8060000                 ENTER  6,0
  6882 000036B6 68[A637]                 push checkcalls_0
  6883 000036B9 E8D617                   call prs
  6884 000036BC 83C402                   add  sp, 2
  6885                                  
  6886                                  ;-684   i=0;  k=0;
  6887                                  
  6888 000036BF 66B800000000             mov eax, 0
  6889 000036C5 8946FE                   mov word [bp-2], ax
  6890 000036C8 66B800000000             mov eax, 0
  6891 000036CE 8946FA                   mov word [bp-6], ax
  6892                                  
  6893                                  ;-685   while (i < CTop) {  pt=CName[i]; from_far(NA, pt);
  6894                                  
  6895                                  @@checkcalls357:
  6896 000036D1 8B46FE                   mov ax, [bp-2]
  6897 000036D4 3B06[6400]               cmp ax, [CTop]
  6898 000036D8 0F8DA000                 jge @@checkcalls358
  6899 000036DC 8B5EFE                   mov bx, [bp-2]
  6900 000036DF D1E3                     shl bx, 1
  6901 000036E1 8B87D79E                 mov ax, [CName + bx]
  6902 000036E5 A3[BE00]                 mov word [pt], ax
  6903 000036E8 FF36[BE00]               push word [pt]
  6904 000036EC 8D06[7200]               lea  ax, [NA]
  6905 000036F0 50                       push ax
  6906 000036F1 E89527                   call from_far
  6907 000036F4 83C404                   add  sp, 4
  6908                                  
  6909                                  ;-686     j=0;
  6910                                  
  6911 000036F7 66B800000000             mov eax, 0
  6912 000036FD 8946FC                   mov word [bp-4], ax
  6913                                  
  6914                                  ;-687     do {  p1=FName[j]; from_far(NB, p1);
  6915                                  
  6916                                  @@checkcalls359:
  6917 00003700 8B5EFC                   mov bx, [bp-4]
  6918 00003703 D1E3                     shl bx, 1
  6919 00003705 8B877F9C                 mov ax, [FName + bx]
  6920 00003709 A3[C000]                 mov word [p1], ax
  6921 0000370C FF36[C000]               push word [p1]
  6922 00003710 8D06[9300]               lea  ax, [NB]
  6923 00003714 50                       push ax
  6924 00003715 E87127                   call from_far
  6925 00003718 83C404                   add  sp, 4
  6926                                  
  6927                                  ;-688       if (eqstr(NA, NB)){ CUnres[i]=1; j=FTop; }
  6928                                  
  6929 0000371B 8D06[9300]               lea  ax, [NB]
  6930 0000371F 50                       push ax
  6931 00003720 8D06[7200]               lea  ax, [NA]
  6932 00003724 50                       push ax
  6933 00003725 E8B82B                   call eqstr
  6934 00003728 83C404                   add  sp, 4
  6935 0000372B 08C0                     or  al, al
  6936 0000372D 0F841300                 je @@checkcalls360
  6937 00003731 66B801000000             mov eax, 1
  6938 00003737 8B5EFE                   mov bx, [bp-2]
  6939 0000373A 888777AE                 mov [CUnres+bx], al
  6940 0000373E A1[6200]                 mov ax, [FTop]
  6941 00003741 8946FC                   mov word [bp-4], ax
  6942                                  
  6943                                  ;-689       j++;
  6944                                  
  6945                                  @@checkcalls360:
  6946 00003744 FF46FC                   inc  word [bp-4]
  6947                                  
  6948                                  ;-690       } while (j < FTop);
  6949                                  
  6950 00003747 8B46FC                   mov ax, [bp-4]
  6951 0000374A 3B06[6200]               cmp ax, [FTop]
  6952 0000374E 0F8D0200                 jge @@checkcalls361
  6953 00003752 EBAC                     jmp @@checkcalls359
  6954                                  @@checkcalls361:
  6955                                  
  6956                                  ;-691     if (j == FTop) { k++; prs("\n; "); prs(NA); }
  6957                                  
  6958 00003754 8B46FC                   mov ax, [bp-4]
  6959 00003757 3B06[6200]               cmp ax, [FTop]
  6960 0000375B 0F851700                 jne @@checkcalls362
  6961 0000375F FF46FA                   inc  word [bp-6]
  6962 00003762 68[C137]                 push checkcalls_1
  6963 00003765 E82A17                   call prs
  6964 00003768 83C402                   add  sp, 2
  6965 0000376B 8D06[7200]               lea  ax, [NA]
  6966 0000376F 50                       push ax
  6967 00003770 E81F17                   call prs
  6968 00003773 83C402                   add  sp, 2
  6969                                  
  6970                                  ;-692     i++; }
  6971                                  
  6972                                  @@checkcalls362:
  6973 00003776 FF46FE                   inc  word [bp-2]
  6974                                  
  6975                                  ;-693   if (k!=0) doar(k); else {prs(" All FUNCTIONs in place");
  6976                                  
  6977 00003779 E955FF                   jmp @@checkcalls357
  6978                                  @@checkcalls358:
  6979 0000377C 8B46FA                   mov ax, [bp-6]
  6980 0000377F 83F800                   cmp ax, 0
  6981 00003782 0F840C00                 je  @@checkcalls363
  6982 00003786 FF76FA                   push word [bp-6]
  6983 00003789 E86400                   call doar
  6984 0000378C 83C402                   add  sp, 2
  6985 0000378F E91200                   jmp @@checkcalls364
  6986                                  @@checkcalls363:
  6987 00003792 68[C637]                 push checkcalls_2
  6988 00003795 E8FA16                   call prs
  6989 00003798 83C402                   add  sp, 2
  6990                                  
  6991                                  ;-694     cputs("  All CALLs OK! "); }
  6992                                  
  6993 0000379B 68[DE37]                 push checkcalls_3
  6994 0000379E E8E128                   call cputs
  6995 000037A1 83C402                   add  sp, 2
  6996                                  
  6997                                  ;-695 }
  6998                                  
  6999                                  @@checkcalls364:
  7000                                  
  7001                                  ;-696 char wasfunction;
  7002 000037A4 C9                       LEAVE
  7003 000037A5 C3                       ret
  7004                                  ; ENDP
  7005 000037A6 5C6E205C6E3B206D69-     checkcalls_0 db "\n \n; missing functions: ",0
  7006 000037AF 7373696E672066756E-
  7007 000037B8 6374696F6E733A2000 
  7008 000037C1 5C6E3B2000              checkcalls_1 db "\n; ",0
  7009 000037C6 20416C6C2046554E43-     checkcalls_2 db " All FUNCTIONs in place",0
  7010 000037CF 54494F4E7320696E20-
  7011 000037D8 706C61636500       
  7012 000037DE 2020416C6C2043414C-     checkcalls_3 db "  All CALLs OK! ",0
  7013 000037E7 4C73204F4B212000   
  7014 000037EF 00                      wasfunction db 0
  7015                                  
  7016                                  ;-697 int doar(int k) { int i; int fdtemp; int fdout1; int used; int found;
  7017                                  
  7018                                  
  7019                                  doar:  ; *** PROC ***
  7020                                  
  7021                                  ;-698   cputs("  Open CALLs :"); pint(k);
  7022                                  
  7023                                  ;Function : doar, Number of local variables: 6
  7024                                  ;   # type sign width line used address name   list of local variables
  7025                                  ;  200 var sign word   697 NULL bp+4    k
  7026                                  ;  201 var sign word   697 NULL bp-2    i
  7027                                  ;  202 var sign word   697 NULL bp-4    fdtemp
  7028                                  ;  203 var sign word   697 NULL bp-6    fdout1
  7029                                  ;  204 var sign word   697 NULL bp-8    used
  7030                                  ;  205 var sign word   697 NULL bp-10    found;
  7031 000037F0 C80A0000                 ENTER  10,0
  7032 000037F4 68[8C39]                 push doar_0
  7033 000037F7 E88828                   call cputs
  7034 000037FA 83C402                   add  sp, 2
  7035 000037FD FF7604                   push word [bp+4]
  7036 00003800 E8B828                   call pint
  7037 00003803 83C402                   add  sp, 2
  7038                                  
  7039                                  ;-699   prs("\n; Number of unresolved CALLs :"); printint51(k);
  7040                                  
  7041 00003806 68[9B39]                 push doar_1
  7042 00003809 E88616                   call prs
  7043 0000380C 83C402                   add  sp, 2
  7044 0000380F FF7604                   push word [bp+4]
  7045 00003812 E82818                   call printint51
  7046 00003815 83C402                   add  sp, 2
  7047                                  
  7048                                  ;-700   fdin=openR (archivename);
  7049                                  
  7050 00003818 8D06[3800]               lea  ax, [archivename]
  7051 0000381C 50                       push ax
  7052 0000381D E8432B                   call openR
  7053 00003820 83C402                   add  sp, 2
  7054 00003823 A3[3400]                 mov word [fdin], ax
  7055                                  
  7056                                  ;-701   if(DOS_ERR){cputs("Archive file missing: "); cputs(archivename); exitR(3); }
  7057                                  
  7058 00003826 A1[C200]                 mov ax, [DOS_ERR]
  7059 00003829 08C0                     or  al, al
  7060 0000382B 0F841C00                 je @@doar365
  7061 0000382F 68[BC39]                 push doar_2
  7062 00003832 E84D28                   call cputs
  7063 00003835 83C402                   add  sp, 2
  7064 00003838 8D06[3800]               lea  ax, [archivename]
  7065 0000383C 50                       push ax
  7066 0000383D E84228                   call cputs
  7067 00003840 83C402                   add  sp, 2
  7068 00003843 6A03                     push 3
  7069 00003845 E8892B                   call exitR
  7070 00003848 83C402                   add  sp, 2
  7071                                  
  7072                                  ;-702   prs("\n;use archive file: "); prs(archivename);
  7073                                  
  7074                                  @@doar365:
  7075 0000384B 68[D339]                 push doar_3
  7076 0000384E E84116                   call prs
  7077 00003851 83C402                   add  sp, 2
  7078 00003854 8D06[3800]               lea  ax, [archivename]
  7079 00003858 50                       push ax
  7080 00003859 E83616                   call prs
  7081 0000385C 83C402                   add  sp, 2
  7082                                  
  7083                                  ;-703   fdtemp=fdout; wasfunction=0; getfirstchar();
  7084                                  
  7085 0000385F A1[3600]                 mov ax, [fdout]
  7086 00003862 8946FC                   mov word [bp-4], ax
  7087 00003865 66B800000000             mov eax, 0
  7088 0000386B A2[EF37]                 mov byte [wasfunction], al
  7089 0000386E E8C1F7                   call getfirstchar
  7090                                  
  7091                                  ;-704   do { fdout=0;do {found=getfunctionhead();}while (found==0); fdout=fdtemp;
  7092                                  
  7093                                  @@doar366:
  7094 00003871 66B800000000             mov eax, 0
  7095 00003877 A3[3600]                 mov word [fdout], ax
  7096                                  @@doar367:
  7097 0000387A E8F001                   call getfunctionhead
  7098 0000387D 8946F6                   mov word [bp-10], ax
  7099 00003880 8B46F6                   mov ax, [bp-10]
  7100 00003883 83F800                   cmp ax, 0
  7101 00003886 0F850200                 jne @@doar368
  7102 0000388A EBEE                     jmp @@doar367
  7103                                  @@doar368:
  7104 0000388C 8B46FC                   mov ax, [bp-4]
  7105 0000388F A3[3600]                 mov word [fdout], ax
  7106                                  
  7107                                  ;-705        if (found > 0) { used=0; i=0;
  7108                                  
  7109 00003892 8B46F6                   mov ax, [bp-10]
  7110 00003895 83F800                   cmp ax, 0
  7111 00003898 0F8EAB00                 jle @@doar369
  7112 0000389C 66B800000000             mov eax, 0
  7113 000038A2 8946F8                   mov word [bp-8], ax
  7114 000038A5 66B800000000             mov eax, 0
  7115 000038AB 8946FE                   mov word [bp-2], ax
  7116                                  
  7117                                  ;-706          while (i < CTop) { pt=CName[i]; from_far(NA, pt);
  7118                                  
  7119                                  @@doar370:
  7120 000038AE 8B46FE                   mov ax, [bp-2]
  7121 000038B1 3B06[6400]               cmp ax, [CTop]
  7122 000038B5 0F8D5300                 jge @@doar371
  7123 000038B9 8B5EFE                   mov bx, [bp-2]
  7124 000038BC D1E3                     shl bx, 1
  7125 000038BE 8B87D79E                 mov ax, [CName + bx]
  7126 000038C2 A3[BE00]                 mov word [pt], ax
  7127 000038C5 FF36[BE00]               push word [pt]
  7128 000038C9 8D06[7200]               lea  ax, [NA]
  7129 000038CD 50                       push ax
  7130 000038CE E8B825                   call from_far
  7131 000038D1 83C404                   add  sp, 4
  7132                                  
  7133                                  ;-707            if (eqstr(symbol, NA)) {
  7134                                  
  7135 000038D4 8D06[7200]               lea  ax, [NA]
  7136 000038D8 50                       push ax
  7137 000038D9 8D066E81                 lea  ax, [symbol]
  7138 000038DD 50                       push ax
  7139 000038DE E8FF29                   call eqstr
  7140 000038E1 83C404                   add  sp, 4
  7141 000038E4 08C0                     or  al, al
  7142 000038E6 0F841D00                 je @@doar372
  7143                                  
  7144                                  ;-708              if (CUnres[i] == 0) {CUnres[i]=1; used++; } }       i++; }
  7145                                  
  7146 000038EA 8B5EFE                   mov bx, [bp-2]
  7147 000038ED 8A8777AE                 mov al, [CUnres + bx]
  7148 000038F1 3C00                     cmp al, 0
  7149 000038F3 0F851000                 jne @@doar373
  7150 000038F7 66B801000000             mov eax, 1
  7151 000038FD 8B5EFE                   mov bx, [bp-2]
  7152 00003900 888777AE                 mov [CUnres+bx], al
  7153 00003904 FF46F8                   inc  word [bp-8]
  7154                                  @@doar373:
  7155                                  @@doar372:
  7156 00003907 FF46FE                   inc  word [bp-2]
  7157                                  
  7158                                  ;-709            if (used) { prs("\n;Number of CALLs:"); printint51(used);
  7159                                  
  7160 0000390A EBA2                     jmp @@doar370
  7161                                  @@doar371:
  7162 0000390C 8B46F8                   mov ax, [bp-8]
  7163 0000390F 08C0                     or  al, al
  7164 00003911 0F843200                 je @@doar374
  7165 00003915 68[E939]                 push doar_4
  7166 00003918 E87715                   call prs
  7167 0000391B 83C402                   add  sp, 2
  7168 0000391E FF76F8                   push word [bp-8]
  7169 00003921 E81917                   call printint51
  7170 00003924 83C402                   add  sp, 2
  7171                                  
  7172                                  ;-710               prs(" : "); prs(symbol); dofunc(); wasfunction=1; }  }
  7173                                  
  7174 00003927 68[FD39]                 push doar_5
  7175 0000392A E86515                   call prs
  7176 0000392D 83C402                   add  sp, 2
  7177 00003930 8D066E81                 lea  ax, [symbol]
  7178 00003934 50                       push ax
  7179 00003935 E85A15                   call prs
  7180 00003938 83C402                   add  sp, 2
  7181 0000393B E86BD5                   call dofunc
  7182 0000393E 66B801000000             mov eax, 1
  7183 00003944 A2[EF37]                 mov byte [wasfunction], al
  7184                                  @@doar374:
  7185                                  
  7186                                  ;-711      } while (token);
  7187                                  
  7188                                  @@doar369:
  7189 00003947 A1[3D00]                 mov ax, [token]
  7190 0000394A 08C0                     or  al, al
  7191 0000394C 0F840300                 je @@doar375
  7192 00003950 E91EFF                   jmp @@doar366
  7193                                  @@doar375:
  7194                                  
  7195                                  ;-712   prs("\n;End of archive file. ");
  7196                                  
  7197 00003953 68[013A]                 push doar_6
  7198 00003956 E83915                   call prs
  7199 00003959 83C402                   add  sp, 2
  7200                                  
  7201                                  ;-713   callrecursive++; if (callrecursive < 5) checkcalls();
  7202                                  
  7203 0000395C FF06[4B00]               inc  word [callrecursive]
  7204 00003960 A1[4B00]                 mov ax, [callrecursive]
  7205 00003963 83F805                   cmp ax, 5
  7206 00003966 0F8D0600                 jge @@doar376
  7207 0000396A E845FD                   call checkcalls
  7208                                  
  7209                                  ;-714     else {putch(10); cputs("***ERROR*** function(s) missing! ");
  7210                                  
  7211 0000396D E91A00                   jmp @@doar377
  7212                                  @@doar376:
  7213 00003970 6A0A                     push 10
  7214 00003972 E8F426                   call putch
  7215 00003975 83C402                   add  sp, 2
  7216 00003978 68[1A3A]                 push doar_7
  7217 0000397B E80427                   call cputs
  7218 0000397E 83C402                   add  sp, 2
  7219                                  
  7220                                  ;-715     error1("At least 1 function is missing in archive file! "); }
  7221                                  
  7222 00003981 68[3C3A]                 push doar_8
  7223 00003984 E80C18                   call error1
  7224 00003987 83C402                   add  sp, 2
  7225                                  
  7226                                  ;-716 }
  7227                                  
  7228                                  @@doar377:
  7229                                  
  7230                                  ;-717 int getfunctionhead() {
  7231 0000398A C9                       LEAVE
  7232 0000398B C3                       ret
  7233                                  ; ENDP
  7234 0000398C 20204F70656E204341-     doar_0 db "  Open CALLs :",0
  7235 00003995 4C4C73203A00       
  7236 0000399B 5C6E3B204E756D6265-     doar_1 db "\n; Number of unresolved CALLs :",0
  7237 000039A4 72206F6620756E7265-
  7238 000039AD 736F6C766564204341-
  7239 000039B6 4C4C73203A00       
  7240 000039BC 417263686976652066-     doar_2 db "Archive file missing: ",0
  7241 000039C5 696C65206D69737369-
  7242 000039CE 6E673A2000         
  7243 000039D3 5C6E3B757365206172-     doar_3 db "\n;use archive file: ",0
  7244 000039DC 63686976652066696C-
  7245 000039E5 653A2000           
  7246 000039E9 5C6E3B4E756D626572-     doar_4 db "\n;Number of CALLs:",0
  7247 000039F2 206F662043414C4C73-
  7248 000039FB 3A00               
  7249 000039FD 203A2000                doar_5 db " : ",0
  7250 00003A01 5C6E3B456E64206F66-     doar_6 db "\n;End of archive file. ",0
  7251 00003A0A 206172636869766520-
  7252 00003A13 66696C652E2000     
  7253 00003A1A 2A2A2A4552524F522A-     doar_7 db "***ERROR*** function(s) missing! ",0
  7254 00003A23 2A2A2066756E637469-
  7255 00003A2C 6F6E287329206D6973-
  7256 00003A35 73696E67212000     
  7257 00003A3C 4174206C6561737420-     doar_8 db "At least 1 function is missing in archive file! ",0
  7258 00003A45 312066756E6374696F-
  7259 00003A4E 6E206973206D697373-
  7260 00003A57 696E6720696E206172-
  7261 00003A60 63686976652066696C-
  7262 00003A69 65212000           
  7263                                  
  7264                                  getfunctionhead:  ; *** PROC ***
  7265                                  
  7266                                  ;-718   if (wasfunction) wasfunction=0; else token=getlex();
  7267                                  
  7268 00003A6D A0[EF37]                 mov al, [wasfunction]
  7269 00003A70 08C0                     or  al, al
  7270 00003A72 0F840C00                 je @@getfunctionhead378
  7271 00003A76 66B800000000             mov eax, 0
  7272 00003A7C A2[EF37]                 mov byte [wasfunction], al
  7273 00003A7F E90600                   jmp @@getfunctionhead379
  7274                                  @@getfunctionhead378:
  7275 00003A82 E8270A                   call getlex
  7276 00003A85 A3[3D00]                 mov word [token], ax
  7277                                  
  7278                                  ;-719   if (token == 0) return 0xFFFF;
  7279                                  
  7280                                  @@getfunctionhead379:
  7281 00003A88 A1[3D00]                 mov ax, [token]
  7282 00003A8B 83F800                   cmp ax, 0
  7283 00003A8E 0F850900                 jne @@getfunctionhead380
  7284 00003A92 66B8FFFF0000             mov eax, 65535
  7285 00003A98 E94400                   jmp @@retn
  7286                                  
  7287                                  ;-720   if(istoken(T_INT))  { if (token != T_NAME) return 0;
  7288                                  
  7289                                  @@getfunctionhead380:
  7290 00003A9B 680502                   push 517
  7291 00003A9E E8AB12                   call istoken
  7292 00003AA1 83C402                   add  sp, 2
  7293 00003AA4 08C0                     or  al, al
  7294 00003AA6 0F842C00                 je @@getfunctionhead381
  7295 00003AAA A1[3D00]                 mov ax, [token]
  7296 00003AAD 3D0001                   cmp ax, 256
  7297 00003AB0 0F840900                 je  @@getfunctionhead382
  7298 00003AB4 66B800000000             mov eax, 0
  7299 00003ABA E92200                   jmp @@retn
  7300                                  
  7301                                  ;-721   token=getlex(); if (token == '(') return 1; }
  7302                                  
  7303                                  @@getfunctionhead382:
  7304 00003ABD E8EC09                   call getlex
  7305 00003AC0 A3[3D00]                 mov word [token], ax
  7306 00003AC3 A1[3D00]                 mov ax, [token]
  7307 00003AC6 83F828                   cmp ax, 40
  7308 00003AC9 0F850900                 jne @@getfunctionhead383
  7309 00003ACD 66B801000000             mov eax, 1
  7310 00003AD3 E90900                   jmp @@retn
  7311                                  @@getfunctionhead383:
  7312                                  
  7313                                  ;-722   return 0;
  7314                                  
  7315                                  @@getfunctionhead381:
  7316 00003AD6 66B800000000             mov eax, 0
  7317 00003ADC E90000                   jmp @@retn
  7318                                  
  7319                                  ;-723 }
  7320                                  
  7321                                  
  7322                                  ;-724 void doinclude() { int fdtemp;
  7323                                  
  7324                                  @@retn:
  7325 00003ADF C3                       ret
  7326                                  ; ENDP
  7327                                  
  7328                                  doinclude:  ; *** PROC ***
  7329                                  
  7330                                  ;-725   if (token==T_STRING) {  fdtemp=fdin;
  7331                                  
  7332                                  ;Function : doinclude, Number of local variables: 1
  7333                                  ;   # type sign width line used address name   list of local variables
  7334                                  ;  200 var sign word   724 NULL bp-2    fdtemp;
  7335 00003AE0 C8020000                 ENTER  2,0
  7336 00003AE4 A1[3D00]                 mov ax, [token]
  7337 00003AE7 3D0201                   cmp ax, 258
  7338 00003AEA 0F859C00                 jne @@doinclude384
  7339 00003AEE A1[3400]                 mov ax, [fdin]
  7340 00003AF1 8946FE                   mov word [bp-2], ax
  7341                                  
  7342                                  ;-726   prs("\n;Use include file: "); prs(symbol);
  7343                                  
  7344 00003AF4 68[8C3B]                 push doinclude_0
  7345 00003AF7 E89813                   call prs
  7346 00003AFA 83C402                   add  sp, 2
  7347 00003AFD 8D066E81                 lea  ax, [symbol]
  7348 00003B01 50                       push ax
  7349 00003B02 E88D13                   call prs
  7350 00003B05 83C402                   add  sp, 2
  7351                                  
  7352                                  ;-727   fdin=openR(symbol);
  7353                                  
  7354 00003B08 8D066E81                 lea  ax, [symbol]
  7355 00003B0C 50                       push ax
  7356 00003B0D E85328                   call openR
  7357 00003B10 83C402                   add  sp, 2
  7358 00003B13 A3[3400]                 mov word [fdin], ax
  7359                                  
  7360                                  ;-728   if (DOS_ERR !=0) {LIST=1;
  7361                                  
  7362 00003B16 A1[C200]                 mov ax, [DOS_ERR]
  7363 00003B19 83F800                   cmp ax, 0
  7364 00003B1C 0F842F00                 je  @@doinclude385
  7365 00003B20 66B801000000             mov eax, 1
  7366 00003B26 A2[1700]                 mov byte [LIST], al
  7367                                  
  7368                                  ;-729     prs("\n;Include file missing: "); prs(symbol);
  7369                                  
  7370 00003B29 68[A23B]                 push doinclude_1
  7371 00003B2C E86313                   call prs
  7372 00003B2F 83C402                   add  sp, 2
  7373 00003B32 8D066E81                 lea  ax, [symbol]
  7374 00003B36 50                       push ax
  7375 00003B37 E85813                   call prs
  7376 00003B3A 83C402                   add  sp, 2
  7377                                  
  7378                                  ;-730     cputs(" STOP!!!");
  7379                                  
  7380 00003B3D 68[BC3B]                 push doinclude_2
  7381 00003B40 E83F25                   call cputs
  7382 00003B43 83C402                   add  sp, 2
  7383                                  
  7384                                  ;-731     error1("\n; Stop!!");
  7385                                  
  7386 00003B46 68[C53B]                 push doinclude_3
  7387 00003B49 E84716                   call error1
  7388 00003B4C 83C402                   add  sp, 2
  7389                                  
  7390                                  ;-732     }
  7391                                  
  7392                                  
  7393                                  ;-733   linenoinclude=lineno; lineno=1;
  7394                                  
  7395                                  @@doinclude385:
  7396 00003B4F A1[BA00]                 mov ax, [lineno]
  7397 00003B52 A3[BC00]                 mov word [linenoinclude], ax
  7398 00003B55 66B801000000             mov eax, 1
  7399 00003B5B A3[BA00]                 mov word [lineno], ax
  7400                                  
  7401                                  ;-734   parse(); lineno=linenoinclude;
  7402                                  
  7403 00003B5E E89FFA                   call parse
  7404 00003B61 A1[BC00]                 mov ax, [linenoinclude]
  7405 00003B64 A3[BA00]                 mov word [lineno], ax
  7406                                  
  7407                                  ;-735   fdin=fdtemp; prs("\n;Back to main program: "); prs(namein);
  7408                                  
  7409 00003B67 8B46FE                   mov ax, [bp-2]
  7410 00003B6A A3[3400]                 mov word [fdin], ax
  7411 00003B6D 68[D03B]                 push doinclude_4
  7412 00003B70 E81F13                   call prs
  7413 00003B73 83C402                   add  sp, 2
  7414 00003B76 8D063182                 lea  ax, [namein]
  7415 00003B7A 50                       push ax
  7416 00003B7B E81413                   call prs
  7417 00003B7E 83C402                   add  sp, 2
  7418                                  
  7419                                  ;-736   getfirstchar(); token=getlex(); }
  7420                                  
  7421 00003B81 E8AEF4                   call getfirstchar
  7422 00003B84 E82509                   call getlex
  7423 00003B87 A3[3D00]                 mov word [token], ax
  7424                                  
  7425                                  ;-737 }
  7426                                  
  7427                                  @@doinclude384:
  7428                                  
  7429                                  ;-738 int dodefine() { int i; int j; int fdtemp;
  7430 00003B8A C9                       LEAVE
  7431 00003B8B C3                       ret
  7432                                  ; ENDP
  7433 00003B8C 5C6E3B55736520696E-     doinclude_0 db "\n;Use include file: ",0
  7434 00003B95 636C7564652066696C-
  7435 00003B9E 653A2000           
  7436 00003BA2 5C6E3B496E636C7564-     doinclude_1 db "\n;Include file missing: ",0
  7437 00003BAB 652066696C65206D69-
  7438 00003BB4 7373696E673A2000   
  7439 00003BBC 2053544F5021212100      doinclude_2 db " STOP!!!",0
  7440 00003BC5 5C6E3B2053746F7021-     doinclude_3 db "\n; Stop!!",0
  7441 00003BCE 2100               
  7442 00003BD0 5C6E3B4261636B2074-     doinclude_4 db "\n;Back to main program: ",0
  7443 00003BD9 6F206D61696E207072-
  7444 00003BE2 6F6772616D3A2000   
  7445                                  
  7446                                  dodefine:  ; *** PROC ***
  7447                                  
  7448                                  ;-739   if (eqstr(symbol, "BSS"   )) {BSS =1; token=getlex(); return; }
  7449                                  
  7450                                  ;Function : dodefine, Number of local variables: 3
  7451                                  ;   # type sign width line used address name   list of local variables
  7452                                  ;  200 var sign word   738 NULL bp-2    i
  7453                                  ;  201 var sign word   738 NULL bp-4    j
  7454                                  ;  202 var sign word   738 NULL bp-6    fdtemp;
  7455 00003BEA C8060000                 ENTER  6,0
  7456 00003BEE 68[B83D]                 push dodefine_0
  7457 00003BF1 8D066E81                 lea  ax, [symbol]
  7458 00003BF5 50                       push ax
  7459 00003BF6 E8E726                   call eqstr
  7460 00003BF9 83C404                   add  sp, 4
  7461 00003BFC 08C0                     or  al, al
  7462 00003BFE 0F841200                 je @@dodefine386
  7463 00003C02 66B801000000             mov eax, 1
  7464 00003C08 A2[1400]                 mov byte [BSS], al
  7465 00003C0B E89E08                   call getlex
  7466 00003C0E A3[3D00]                 mov word [token], ax
  7467 00003C11 E9A201                   jmp @@retn
  7468                                  
  7469                                  ;-740   if (eqstr(symbol, "LIST"  )) {LIST=1; token=getlex(); return; }
  7470                                  
  7471                                  @@dodefine386:
  7472 00003C14 68[BC3D]                 push dodefine_1
  7473 00003C17 8D066E81                 lea  ax, [symbol]
  7474 00003C1B 50                       push ax
  7475 00003C1C E8C126                   call eqstr
  7476 00003C1F 83C404                   add  sp, 4
  7477 00003C22 08C0                     or  al, al
  7478 00003C24 0F841200                 je @@dodefine387
  7479 00003C28 66B801000000             mov eax, 1
  7480 00003C2E A2[1700]                 mov byte [LIST], al
  7481 00003C31 E87808                   call getlex
  7482 00003C34 A3[3D00]                 mov word [token], ax
  7483 00003C37 E97C01                   jmp @@retn
  7484                                  
  7485                                  ;-741   if (eqstr(symbol, "NOLIST")) {LIST=0; token=getlex(); return; }
  7486                                  
  7487                                  @@dodefine387:
  7488 00003C3A 68[C13D]                 push dodefine_2
  7489 00003C3D 8D066E81                 lea  ax, [symbol]
  7490 00003C41 50                       push ax
  7491 00003C42 E89B26                   call eqstr
  7492 00003C45 83C404                   add  sp, 4
  7493 00003C48 08C0                     or  al, al
  7494 00003C4A 0F841200                 je @@dodefine388
  7495 00003C4E 66B800000000             mov eax, 0
  7496 00003C54 A2[1700]                 mov byte [LIST], al
  7497 00003C57 E85208                   call getlex
  7498 00003C5A A3[3D00]                 mov word [token], ax
  7499 00003C5D E95601                   jmp @@retn
  7500                                  
  7501                                  ;-742   if (eqstr(symbol, "ORGDATA")) {token=getlex();
  7502                                  
  7503                                  @@dodefine388:
  7504 00003C60 68[C83D]                 push dodefine_3
  7505 00003C63 8D066E81                 lea  ax, [symbol]
  7506 00003C67 50                       push ax
  7507 00003C68 E87526                   call eqstr
  7508 00003C6B 83C404                   add  sp, 4
  7509 00003C6E 08C0                     or  al, al
  7510 00003C70 0F843500                 je @@dodefine389
  7511 00003C74 E83508                   call getlex
  7512 00003C77 A3[3D00]                 mov word [token], ax
  7513                                  
  7514                                  ;-743     if (token==T_CONST)  { ORGDATAORIG=lexvalL; orgData=lexvalL; }
  7515                                  
  7516 00003C7A A1[3D00]                 mov ax, [token]
  7517 00003C7D 3D0101                   cmp ax, 257
  7518 00003C80 0F851300                 jne @@dodefine390
  7519 00003C84 66A1[4D00]               mov eax, [lexvalL]
  7520 00003C88 66A3[2400]               mov dword [ORGDATAORIG], eax
  7521 00003C8C 66A1[4D00]               mov eax, [lexvalL]
  7522 00003C90 66A3[2800]               mov dword [orgData], eax
  7523                                  
  7524                                  ;-744     else  error1("start address missing"); token=getlex(); return; }
  7525                                  
  7526 00003C94 E90900                   jmp @@dodefine391
  7527                                  @@dodefine390:
  7528 00003C97 68[D03D]                 push dodefine_4
  7529 00003C9A E8F614                   call error1
  7530 00003C9D 83C402                   add  sp, 2
  7531                                  @@dodefine391:
  7532 00003CA0 E80908                   call getlex
  7533 00003CA3 A3[3D00]                 mov word [token], ax
  7534 00003CA6 E90D01                   jmp @@retn
  7535                                  
  7536                                  ;-745   if (eqstr(symbol, "ARCHIVE")) {token=getlex();
  7537                                  
  7538                                  @@dodefine389:
  7539 00003CA9 68[E63D]                 push dodefine_5
  7540 00003CAC 8D066E81                 lea  ax, [symbol]
  7541 00003CB0 50                       push ax
  7542 00003CB1 E82C26                   call eqstr
  7543 00003CB4 83C404                   add  sp, 4
  7544 00003CB7 08C0                     or  al, al
  7545 00003CB9 0F844900                 je @@dodefine392
  7546 00003CBD E8EC07                   call getlex
  7547 00003CC0 A3[3D00]                 mov word [token], ax
  7548                                  
  7549                                  ;-746     if (token==T_STRING) { prs("\n;Use archive file: ");
  7550                                  
  7551 00003CC3 A1[3D00]                 mov ax, [token]
  7552 00003CC6 3D0201                   cmp ax, 258
  7553 00003CC9 0F852700                 jne @@dodefine393
  7554 00003CCD 68[EE3D]                 push dodefine_6
  7555 00003CD0 E8BF11                   call prs
  7556 00003CD3 83C402                   add  sp, 2
  7557                                  
  7558                                  ;-747     strcpy(archivename, symbol); prs(archivename);
  7559                                  
  7560 00003CD6 8D066E81                 lea  ax, [symbol]
  7561 00003CDA 50                       push ax
  7562 00003CDB 8D06[3800]               lea  ax, [archivename]
  7563 00003CDF 50                       push ax
  7564 00003CE0 E8A924                   call strcpy
  7565 00003CE3 83C404                   add  sp, 4
  7566 00003CE6 8D06[3800]               lea  ax, [archivename]
  7567 00003CEA 50                       push ax
  7568 00003CEB E8A411                   call prs
  7569 00003CEE 83C402                   add  sp, 2
  7570                                  
  7571                                  ;-748     } else error1("Name of archive file missing"); token=getlex(); return;}
  7572                                  
  7573 00003CF1 E90900                   jmp @@dodefine394
  7574                                  @@dodefine393:
  7575 00003CF4 68[043E]                 push dodefine_7
  7576 00003CF7 E89914                   call error1
  7577 00003CFA 83C402                   add  sp, 2
  7578                                  @@dodefine394:
  7579 00003CFD E8AC07                   call getlex
  7580 00003D00 A3[3D00]                 mov word [token], ax
  7581 00003D03 E9B000                   jmp @@retn
  7582                                  
  7583                                  ;-749    expect(T_NAME);
  7584                                  
  7585                                  @@dodefine392:
  7586 00003D06 680001                   push 256
  7587 00003D09 E86810                   call expect
  7588 00003D0C 83C402                   add  sp, 2
  7589                                  
  7590                                  ;-750   if (token==T_CONST) { 
  7591                                  
  7592 00003D0F A1[3D00]                 mov ax, [token]
  7593 00003D12 3D0101                   cmp ax, 257
  7594 00003D15 0F859D00                 jne @@dodefine395
  7595                                  
  7596                                  ;-751     if (GTop >= LSTART) error1("global table (define) full");
  7597                                  
  7598 00003D19 A1[5E00]                 mov ax, [GTop]
  7599 00003D1C 3DC800                   cmp ax, 200 ;unsigned : 1
  7600 00003D1F 0F8C0900                 jl  @@dodefine396
  7601 00003D23 68[213E]                 push dodefine_8
  7602 00003D26 E86A14                   call error1
  7603 00003D29 83C402                   add  sp, 2
  7604                                  
  7605                                  ;-752     checknamelen();
  7606                                  
  7607                                  @@dodefine396:
  7608 00003D2C E80CC4                   call checknamelen
  7609                                  
  7610                                  ;-753     GSign [GTop]='U'; GWidth[GTop]=1; GType [GTop]='#';
  7611                                  
  7612 00003D2F 66B855000000             mov eax, 85
  7613 00003D35 8B1E[5E00]               mov bx, [GTop]
  7614 00003D39 8887E383                 mov [GSign+bx], al
  7615 00003D3D 66B801000000             mov eax, 1
  7616 00003D43 8B1E[5E00]               mov bx, [GTop]
  7617 00003D47 88870F85                 mov [GWidth+bx], al
  7618 00003D4B 66B823000000             mov eax, 35
  7619 00003D51 8B1E[5E00]               mov bx, [GTop]
  7620 00003D55 8887B782                 mov [GType+bx], al
  7621                                  
  7622                                  ;-754     GLino [GTop]=lineno-1; GUsed [GTop]=0;
  7623                                  
  7624 00003D59 A1[BA00]                 mov ax, [lineno]
  7625 00003D5C 83E801                   sub ax, 1
  7626 00003D5F 8B1E[5E00]               mov bx, [GTop]
  7627 00003D63 D1E3                     shl bx, 1
  7628 00003D65 89873B86                 mov [GLino+bx], ax
  7629 00003D69 66B800000000             mov eax, 0
  7630 00003D6F 8B1E[5E00]               mov bx, [GTop]
  7631 00003D73 D1E3                     shl bx, 1
  7632 00003D75 89879388                 mov [GUsed+bx], ax
  7633                                  
  7634                                  ;-755     GAdr  [GTop]=LONG0;
  7635                                  
  7636 00003D79 66A1[2000]               mov eax, [LONG0]
  7637 00003D7D 8B1E[5E00]               mov bx, [GTop]
  7638 00003D81 C1E302                   shl bx, 2
  7639 00003D84 6689879B8F               mov [GAdr+bx], eax
  7640                                  
  7641                                  ;-756     GName [GTop] = CNameTop; storeName();
  7642                                  
  7643 00003D89 A1[B400]                 mov ax, [CNameTop]
  7644 00003D8C 8B1E[5E00]               mov bx, [GTop]
  7645 00003D90 D1E3                     shl bx, 1
  7646 00003D92 89874B94                 mov [GName+bx], ax
  7647 00003D96 E857C4                   call storeName
  7648                                  
  7649                                  ;-757     GData[GTop]=lexvalL;
  7650                                  
  7651 00003D99 66A1[4D00]               mov eax, [lexvalL]
  7652 00003D9D 8B1E[5E00]               mov bx, [GTop]
  7653 00003DA1 C1E302                   shl bx, 2
  7654 00003DA4 668987EB8A               mov [GData+bx], eax
  7655                                  
  7656                                  ;-758     expect(T_CONST); GTop++;  } 
  7657                                  
  7658 00003DA9 680101                   push 257
  7659 00003DAC E8C50F                   call expect
  7660 00003DAF 83C402                   add  sp, 2
  7661 00003DB2 FF06[5E00]               inc  word [GTop]
  7662                                  
  7663                                  ;-759 }
  7664                                  
  7665                                  @@dodefine395:
  7666                                  
  7667                                  ;-760 int stmt() { int c; char cha;
  7668                                  
  7669 00003DB6 C9                      @@retn: LEAVE
  7670 00003DB7 C3                       ret
  7671                                  ; ENDP
  7672 00003DB8 42535300                dodefine_0 db "BSS",0
  7673 00003DBC 4C49535400              dodefine_1 db "LIST",0
  7674 00003DC1 4E4F4C49535400          dodefine_2 db "NOLIST",0
  7675 00003DC8 4F52474441544100        dodefine_3 db "ORGDATA",0
  7676 00003DD0 737461727420616464-     dodefine_4 db "start address missing",0
  7677 00003DD9 72657373206D697373-
  7678 00003DE2 696E6700           
  7679 00003DE6 4152434849564500        dodefine_5 db "ARCHIVE",0
  7680 00003DEE 5C6E3B557365206172-     dodefine_6 db "\n;Use archive file: ",0
  7681 00003DF7 63686976652066696C-
  7682 00003E00 653A2000           
  7683 00003E04 4E616D65206F662061-     dodefine_7 db "Name of archive file missing",0
  7684 00003E0D 726368697665206669-
  7685 00003E16 6C65206D697373696E-
  7686 00003E1F 6700               
  7687 00003E21 676C6F62616C207461-     dodefine_8 db "global table (define) full",0
  7688 00003E2A 626C65202864656669-
  7689 00003E33 6E65292066756C6C00 
  7690                                  
  7691                                  stmt:  ; *** PROC ***
  7692                                  
  7693                                  ;-761        if(istoken('{'))     {while(istoken('}')==0) stmt();}
  7694                                  
  7695                                  ;Function : stmt, Number of local variables: 2
  7696                                  ;   # type sign width line used address name   list of local variables
  7697                                  ;  200 var sign word   760 NULL bp-2    c
  7698                                  ;  201 var sign byte   760 NULL bp-4    cha;
  7699 00003E3C C8040000                 ENTER  4,0
  7700 00003E40 6A7B                     push 123
  7701 00003E42 E8070F                   call istoken
  7702 00003E45 83C402                   add  sp, 2
  7703 00003E48 08C0                     or  al, al
  7704 00003E4A 0F841700                 je @@stmt397
  7705                                  @@stmt398:
  7706 00003E4E 6A7D                     push 125
  7707 00003E50 E8F90E                   call istoken
  7708 00003E53 83C402                   add  sp, 2
  7709 00003E56 83F800                   cmp ax, 0
  7710 00003E59 0F850500                 jne @@stmt399
  7711 00003E5D E8DCFF                   call stmt
  7712 00003E60 EBEC                     jmp @@stmt398
  7713                                  @@stmt399:
  7714                                  
  7715                                  ;-762   else if(istoken(T_IF))    doif();
  7716                                  
  7717 00003E62 E90502                   jmp @@stmt400
  7718                                  @@stmt397:
  7719 00003E65 680102                   push 513
  7720 00003E68 E8E10E                   call istoken
  7721 00003E6B 83C402                   add  sp, 2
  7722 00003E6E 08C0                     or  al, al
  7723 00003E70 0F840600                 je @@stmt401
  7724 00003E74 E84304                   call doif
  7725                                  
  7726                                  ;-763   else if(istoken(T_DO))    dodo();
  7727                                  
  7728 00003E77 E9F001                   jmp @@stmt402
  7729                                  @@stmt401:
  7730 00003E7A 680402                   push 516
  7731 00003E7D E8CC0E                   call istoken
  7732 00003E80 83C402                   add  sp, 2
  7733 00003E83 08C0                     or  al, al
  7734 00003E85 0F840600                 je @@stmt403
  7735 00003E89 E82205                   call dodo
  7736                                  
  7737                                  ;-764   else if(istoken(T_WHILE)) dowhile();
  7738                                  
  7739 00003E8C E9DB01                   jmp @@stmt404
  7740                                  @@stmt403:
  7741 00003E8F 680302                   push 515
  7742 00003E92 E8B70E                   call istoken
  7743 00003E95 83C402                   add  sp, 2
  7744 00003E98 08C0                     or  al, al
  7745 00003E9A 0F840600                 je @@stmt405
  7746 00003E9E E85A05                   call dowhile
  7747                                  
  7748                                  ;-765   else if(istoken(T_GOTO))  {prs("\n jmp @@");name1();prs(symbol);expect(';');}
  7749                                  
  7750 00003EA1 E9C601                   jmp @@stmt406
  7751                                  @@stmt405:
  7752 00003EA4 680902                   push 521
  7753 00003EA7 E8A20E                   call istoken
  7754 00003EAA 83C402                   add  sp, 2
  7755 00003EAD 08C0                     or  al, al
  7756 00003EAF 0F842200                 je @@stmt407
  7757 00003EB3 68[6C40]                 push stmt_0
  7758 00003EB6 E8D90F                   call prs
  7759 00003EB9 83C402                   add  sp, 2
  7760 00003EBC E8D3CD                   call name1
  7761 00003EBF 8D066E81                 lea  ax, [symbol]
  7762 00003EC3 50                       push ax
  7763 00003EC4 E8CB0F                   call prs
  7764 00003EC7 83C402                   add  sp, 2
  7765 00003ECA 6A3B                     push 59
  7766 00003ECC E8A50E                   call expect
  7767 00003ECF 83C402                   add  sp, 2
  7768                                  
  7769                                  ;-766   else if(token==T_ASM)     {prs("\n"); c=next();
  7770                                  
  7771 00003ED2 E99501                   jmp @@stmt408
  7772                                  @@stmt407:
  7773 00003ED5 A1[3D00]                 mov ax, [token]
  7774 00003ED8 3D0602                   cmp ax, 518
  7775 00003EDB 0F853300                 jne @@stmt409
  7776 00003EDF 68[7640]                 push stmt_1
  7777 00003EE2 E8AD0F                   call prs
  7778 00003EE5 83C402                   add  sp, 2
  7779 00003EE8 E8430E                   call next
  7780 00003EEB 8946FE                   mov word [bp-2], ax
  7781                                  
  7782                                  ;-767     while(c != '\n') { prc(c);	c=next(); }; token=getlex(); }
  7783                                  
  7784                                  @@stmt410:
  7785 00003EEE 8B46FE                   mov ax, [bp-2]
  7786 00003EF1 83F80A                   cmp ax, 10
  7787 00003EF4 0F841100                 je  @@stmt411
  7788 00003EF8 FF76FE                   push word [bp-2]
  7789 00003EFB E8250F                   call prc
  7790 00003EFE 83C402                   add  sp, 2
  7791 00003F01 E82A0E                   call next
  7792 00003F04 8946FE                   mov word [bp-2], ax
  7793 00003F07 EBE5                     jmp @@stmt410
  7794                                  @@stmt411:
  7795 00003F09 E8A005                   call getlex
  7796 00003F0C A3[3D00]                 mov word [token], ax
  7797                                  
  7798                                  ;-768   else if(istoken(T_ASMBLOCK)) { if (token== '{' )  { prs("\n"); cha=next();  
  7799                                  
  7800 00003F0F E95801                   jmp @@stmt412
  7801                                  @@stmt409:
  7802 00003F12 680702                   push 519
  7803 00003F15 E8340E                   call istoken
  7804 00003F18 83C402                   add  sp, 2
  7805 00003F1B 08C0                     or  al, al
  7806 00003F1D 0F844A00                 je @@stmt413
  7807 00003F21 A1[3D00]                 mov ax, [token]
  7808 00003F24 83F87B                   cmp ax, 123
  7809 00003F27 0F853400                 jne @@stmt414
  7810 00003F2B 68[7940]                 push stmt_2
  7811 00003F2E E8610F                   call prs
  7812 00003F31 83C402                   add  sp, 2
  7813 00003F34 E8F70D                   call next
  7814 00003F37 8846FC                   mov byte [bp-4], al
  7815                                  
  7816                                  ;-769     while(_ cha!= '}') { prc(cha); cha=next(); }
  7817                                  
  7818                                  @@stmt415:
  7819 00003F3A 807EFC7D                cmp byte [bp-4], 125
  7820 00003F3E 0F841400                 je  @@stmt416
  7821 00003F42 8A46FC                   mov al, byte [bp-4]
  7822 00003F45 B400                     mov ah, 0
  7823 00003F47 50                       push ax
  7824 00003F48 E8D80E                   call prc
  7825 00003F4B 83C402                   add  sp, 2
  7826 00003F4E E8DD0D                   call next
  7827 00003F51 8846FC                   mov byte [bp-4], al
  7828                                  
  7829                                  ;-770     token=getlex(); }
  7830                                  
  7831 00003F54 EBE4                     jmp @@stmt415
  7832                                  @@stmt416:
  7833 00003F56 E85305                   call getlex
  7834 00003F59 A3[3D00]                 mov word [token], ax
  7835                                  
  7836                                  ;-771     else error1("Curly open expected"); }
  7837                                  
  7838 00003F5C E90900                   jmp @@stmt417
  7839                                  @@stmt414:
  7840 00003F5F 68[7C40]                 push stmt_3
  7841 00003F62 E82E12                   call error1
  7842 00003F65 83C402                   add  sp, 2
  7843                                  @@stmt417:
  7844                                  
  7845                                  ;-772   else if(istoken(T_INTH))  {prs("\n int  "); expect(T_CONST);
  7846                                  
  7847 00003F68 E9FF00                   jmp @@stmt418
  7848                                  @@stmt413:
  7849 00003F6B 685802                   push 600
  7850 00003F6E E8DB0D                   call istoken
  7851 00003F71 83C402                   add  sp, 2
  7852 00003F74 08C0                     or  al, al
  7853 00003F76 0F842800                 je @@stmt419
  7854 00003F7A 68[9040]                 push stmt_4
  7855 00003F7D E8120F                   call prs
  7856 00003F80 83C402                   add  sp, 2
  7857 00003F83 680101                   push 257
  7858 00003F86 E8EB0D                   call expect
  7859 00003F89 83C402                   add  sp, 2
  7860                                  
  7861                                  ;-773     prL(lexvalL); expect(';');    }
  7862                                  
  7863 00003F8C 66FF36[4D00]             push dword [lexvalL]
  7864 00003F91 E8B11F                   call prL
  7865 00003F94 83C404                   add  sp, 4
  7866 00003F97 6A3B                     push 59
  7867 00003F99 E8D80D                   call expect
  7868 00003F9C 83C402                   add  sp, 2
  7869                                  
  7870                                  ;-774   else if(istoken(T_IFCARRY))doifcarry();
  7871                                  
  7872 00003F9F E9C800                   jmp @@stmt420
  7873                                  @@stmt419:
  7874 00003FA2 685902                   push 601
  7875 00003FA5 E8A40D                   call istoken
  7876 00003FA8 83C402                   add  sp, 2
  7877 00003FAB 08C0                     or  al, al
  7878 00003FAD 0F840600                 je @@stmt421
  7879 00003FB1 E86803                   call doifcarry
  7880                                  
  7881                                  ;-775   else if(istoken(T_IFZERO))doifzero();
  7882                                  
  7883 00003FB4 E9B300                   jmp @@stmt422
  7884                                  @@stmt421:
  7885 00003FB7 685A02                   push 602
  7886 00003FBA E88F0D                   call istoken
  7887 00003FBD 83C402                   add  sp, 2
  7888 00003FC0 08C0                     or  al, al
  7889 00003FC2 0F840600                 je @@stmt423
  7890 00003FC6 E89C03                   call doifzero
  7891                                  
  7892                                  ;-776   else if(istoken(T_EMIT))   doemit();
  7893                                  
  7894 00003FC9 E99E00                   jmp @@stmt424
  7895                                  @@stmt423:
  7896 00003FCC 680802                   push 520
  7897 00003FCF E87A0D                   call istoken
  7898 00003FD2 83C402                   add  sp, 2
  7899 00003FD5 08C0                     or  al, al
  7900 00003FD7 0F840600                 je @@stmt425
  7901 00003FDB E8CE00                   call doemit
  7902                                  
  7903                                  ;-777   else if(istoken(';'))      { }
  7904                                  
  7905 00003FDE E98900                   jmp @@stmt426
  7906                                  @@stmt425:
  7907 00003FE1 6A3B                     push 59
  7908 00003FE3 E8660D                   call istoken
  7909 00003FE6 83C402                   add  sp, 2
  7910 00003FE9 08C0                     or  al, al
  7911 00003FEB 0F840300                 je @@stmt427
  7912                                  
  7913                                  ;-778   else if(istoken(T_RETURN)) {if (token!=';')  exprstart();
  7914                                  
  7915 00003FEF E97800                   jmp @@stmt428
  7916                                  @@stmt427:
  7917 00003FF2 680002                   push 512
  7918 00003FF5 E8540D                   call istoken
  7919 00003FF8 83C402                   add  sp, 2
  7920 00003FFB 08C0                     or  al, al
  7921 00003FFD 0F842500                 je @@stmt429
  7922 00004001 A1[3D00]                 mov ax, [token]
  7923 00004004 83F83B                   cmp ax, 59
  7924 00004007 0F840300                 je  @@stmt430
  7925 0000400B E852D3                   call exprstart
  7926                                  
  7927                                  ;-779     prs("\n jmp @@retn"); nreturn++; expect(';');}
  7928                                  
  7929                                  @@stmt430:
  7930 0000400E 68[9940]                 push stmt_5
  7931 00004011 E87E0E                   call prs
  7932 00004014 83C402                   add  sp, 2
  7933 00004017 FF06[4700]               inc  word [nreturn]
  7934 0000401B 6A3B                     push 59
  7935 0000401D E8540D                   call expect
  7936 00004020 83C402                   add  sp, 2
  7937                                  
  7938                                  ;-780   else if(_ thechar==':')      {prs("\n@@"); // Label
  7939                                  
  7940 00004023 E94400                   jmp @@stmt431
  7941                                  @@stmt429:
  7942 00004026 803E[4200]3A            cmp byte [thechar], 58
  7943 0000402B 0F853000                 jne @@stmt432
  7944 0000402F 68[A740]                 push stmt_6
  7945 00004032 E85D0E                   call prs
  7946 00004035 83C402                   add  sp, 2
  7947                                  
  7948                                  ;-781      prs(symbol); prc(':');  expect(T_NAME); expect(':'); }
  7949                                  
  7950 00004038 8D066E81                 lea  ax, [symbol]
  7951 0000403C 50                       push ax
  7952 0000403D E8520E                   call prs
  7953 00004040 83C402                   add  sp, 2
  7954 00004043 6A3A                     push 58
  7955 00004045 E8DB0D                   call prc
  7956 00004048 83C402                   add  sp, 2
  7957 0000404B 680001                   push 256
  7958 0000404E E8230D                   call expect
  7959 00004051 83C402                   add  sp, 2
  7960 00004054 6A3A                     push 58
  7961 00004056 E81B0D                   call expect
  7962 00004059 83C402                   add  sp, 2
  7963                                  
  7964                                  ;-782   else                       {exprstart(); expect(';'); } }
  7965                                  
  7966 0000405C E90B00                   jmp @@stmt433
  7967                                  @@stmt432:
  7968 0000405F E8FED2                   call exprstart
  7969 00004062 6A3B                     push 59
  7970 00004064 E80D0D                   call expect
  7971 00004067 83C402                   add  sp, 2
  7972                                  @@stmt433:
  7973                                  @@stmt431:
  7974                                  @@stmt428:
  7975                                  @@stmt426:
  7976                                  @@stmt424:
  7977                                  @@stmt422:
  7978                                  @@stmt420:
  7979                                  @@stmt418:
  7980                                  @@stmt412:
  7981                                  @@stmt408:
  7982                                  @@stmt406:
  7983                                  @@stmt404:
  7984                                  @@stmt402:
  7985                                  @@stmt400:
  7986                                  
  7987                                  ;-783 
  7988                                  
  7989                                  
  7990                                  ;-784 int doemit() {prs("\n db ");
  7991 0000406A C9                       LEAVE
  7992 0000406B C3                       ret
  7993                                  ; ENDP
  7994 0000406C 5C6E206A6D70204040-     stmt_0 db "\n jmp @@",0
  7995 00004075 00                 
  7996 00004076 5C6E00                  stmt_1 db "\n",0
  7997 00004079 5C6E00                  stmt_2 db "\n",0
  7998 0000407C 4375726C79206F7065-     stmt_3 db "Curly open expected",0
  7999 00004085 6E2065787065637465-
  8000 0000408E 6400               
  8001 00004090 5C6E20696E74202000      stmt_4 db "\n int  ",0
  8002 00004099 5C6E206A6D70204040-     stmt_5 db "\n jmp @@retn",0
  8003 000040A2 7265746E00         
  8004 000040A7 5C6E404000              stmt_6 db "\n@@",0
  8005                                  
  8006                                  doemit:  ; *** PROC ***
  8007 000040AC 68[E940]                 push doemit_0
  8008 000040AF E8E00D                   call prs
  8009 000040B2 83C402                   add  sp, 2
  8010                                  
  8011                                  ;-785   L1: token=getlex(); prL(lexvalL); token=getlex();
  8012                                  
  8013                                  @@L1:
  8014 000040B5 E8F403                   call getlex
  8015 000040B8 A3[3D00]                 mov word [token], ax
  8016 000040BB 66FF36[4D00]             push dword [lexvalL]
  8017 000040C0 E8821E                   call prL
  8018 000040C3 83C404                   add  sp, 4
  8019 000040C6 E8E303                   call getlex
  8020 000040C9 A3[3D00]                 mov word [token], ax
  8021                                  
  8022                                  ;-786     if (token== ',') {prc(','); goto L1;} expect(')'); }
  8023                                  
  8024 000040CC A1[3D00]                 mov ax, [token]
  8025 000040CF 83F82C                   cmp ax, 44
  8026 000040D2 0F850A00                 jne @@doemit434
  8027 000040D6 6A2C                     push 44
  8028 000040D8 E8480D                   call prc
  8029 000040DB 83C402                   add  sp, 2
  8030 000040DE EBD5                     jmp @@L1
  8031                                  @@doemit434:
  8032 000040E0 6A29                     push 41
  8033 000040E2 E88F0C                   call expect
  8034 000040E5 83C402                   add  sp, 2
  8035                                  
  8036                                  ;-787 
  8037                                  
  8038                                  
  8039                                  ;-788 int cmpneg(int ids) {
  8040                                  
  8041 000040E8 C3                       ret
  8042                                  ; ENDP
  8043 000040E9 5C6E2064622000          doemit_0 db "\n db ",0
  8044                                  
  8045                                  cmpneg:  ; *** PROC ***
  8046                                  
  8047                                  ;-789        if(iscmp==T_EQ) prs("\n jne @@");         //ZF=0
  8048                                  
  8049                                  ;Function : cmpneg, Number of local variables: 1
  8050                                  ;   # type sign width line used address name   list of local variables
  8051                                  ;  200 var sign word   788 NULL bp+4    ids;
  8052 000040F0 C8000000                 ENTER  0,0
  8053 000040F4 A1[4300]                 mov ax, [iscmp]
  8054 000040F7 3D2603                   cmp ax, 806
  8055 000040FA 0F850C00                 jne @@cmpneg435
  8056 000040FE 68[D141]                 push cmpneg_0
  8057 00004101 E88E0D                   call prs
  8058 00004104 83C402                   add  sp, 2
  8059                                  
  8060                                  ;-790   else if(iscmp==T_NE) prs("\n je  @@");         //ZF=1
  8061                                  
  8062 00004107 E9C500                   jmp @@cmpneg436
  8063                                  @@cmpneg435:
  8064 0000410A A1[4300]                 mov ax, [iscmp]
  8065 0000410D 3D2703                   cmp ax, 807
  8066 00004110 0F850C00                 jne @@cmpneg437
  8067 00004114 68[DB41]                 push cmpneg_1
  8068 00004117 E8780D                   call prs
  8069 0000411A 83C402                   add  sp, 2
  8070                                  
  8071                                  ;-791   else if(iscmp==T_LE) if (ids) prs("\n jg  @@");//ZF=0      SF =OF
  8072                                  
  8073 0000411D E9AF00                   jmp @@cmpneg438
  8074                                  @@cmpneg437:
  8075 00004120 A1[4300]                 mov ax, [iscmp]
  8076 00004123 3D3803                   cmp ax, 824
  8077 00004126 0F852100                 jne @@cmpneg439
  8078 0000412A 8B4604                   mov ax, [bp+4]
  8079 0000412D 08C0                     or  al, al
  8080 0000412F 0F840C00                 je @@cmpneg440
  8081 00004133 68[E541]                 push cmpneg_2
  8082 00004136 E8590D                   call prs
  8083 00004139 83C402                   add  sp, 2
  8084                                  
  8085                                  ;-792                            else prs("\n ja  @@");//ZF=0 CF=0
  8086                                  
  8087 0000413C E90900                   jmp @@cmpneg441
  8088                                  @@cmpneg440:
  8089 0000413F 68[EF41]                 push cmpneg_3
  8090 00004142 E84D0D                   call prs
  8091 00004145 83C402                   add  sp, 2
  8092                                  
  8093                                  ;-793   else if(iscmp==T_GE) if (ids){prs(" ;unsigned : "); prunsign1(ids);
  8094                                  
  8095                                  @@cmpneg441:
  8096 00004148 E98400                   jmp @@cmpneg442
  8097                                  @@cmpneg439:
  8098 0000414B A1[4300]                 mov ax, [iscmp]
  8099 0000414E 3D2B03                   cmp ax, 811
  8100 00004151 0F854500                 jne @@cmpneg443
  8101 00004155 8B4604                   mov ax, [bp+4]
  8102 00004158 08C0                     or  al, al
  8103 0000415A 0F841E00                 je @@cmpneg444
  8104 0000415E 68[F941]                 push cmpneg_4
  8105 00004161 E82E0D                   call prs
  8106 00004164 83C402                   add  sp, 2
  8107 00004167 FF7604                   push word [bp+4]
  8108 0000416A E88E0E                   call prunsign1
  8109 0000416D 83C402                   add  sp, 2
  8110                                  
  8111                                  ;-794                                prs("\n jl  @@");}//          SF!=OF
  8112                                  
  8113 00004170 68[0742]                 push cmpneg_5
  8114 00004173 E81C0D                   call prs
  8115 00004176 83C402                   add  sp, 2
  8116                                  
  8117                                  ;-795                            else{prs(" ;unsigned : "); prunsign1(ids);
  8118                                  
  8119 00004179 E91B00                   jmp @@cmpneg445
  8120                                  @@cmpneg444:
  8121 0000417C 68[1142]                 push cmpneg_6
  8122 0000417F E8100D                   call prs
  8123 00004182 83C402                   add  sp, 2
  8124 00004185 FF7604                   push word [bp+4]
  8125 00004188 E8700E                   call prunsign1
  8126 0000418B 83C402                   add  sp, 2
  8127                                  
  8128                                  ;-796                                prs("\n jb  @@");}//jb=jc=CF=1
  8129                                  
  8130 0000418E 68[1F42]                 push cmpneg_7
  8131 00004191 E8FE0C                   call prs
  8132 00004194 83C402                   add  sp, 2
  8133                                  
  8134                                  ;-797   else if(iscmp=='<' ) prs("\n jge @@");         //          SF =OF
  8135                                  
  8136                                  @@cmpneg445:
  8137 00004197 E93500                   jmp @@cmpneg446
  8138                                  @@cmpneg443:
  8139 0000419A A1[4300]                 mov ax, [iscmp]
  8140 0000419D 83F83C                   cmp ax, 60
  8141 000041A0 0F850C00                 jne @@cmpneg447
  8142 000041A4 68[2942]                 push cmpneg_8
  8143 000041A7 E8E80C                   call prs
  8144 000041AA 83C402                   add  sp, 2
  8145                                  
  8146                                  ;-798   else if(iscmp=='>' ) prs("\n jle @@");         //ZF=1 oder SF!=OF
  8147                                  
  8148 000041AD E91F00                   jmp @@cmpneg448
  8149                                  @@cmpneg447:
  8150 000041B0 A1[4300]                 mov ax, [iscmp]
  8151 000041B3 83F83E                   cmp ax, 62
  8152 000041B6 0F850C00                 jne @@cmpneg449
  8153 000041BA 68[3342]                 push cmpneg_9
  8154 000041BD E8D20C                   call prs
  8155 000041C0 83C402                   add  sp, 2
  8156                                  
  8157                                  ;-799   else error1("Vergleich unbekannt in CMPNEG()");  }
  8158                                  
  8159 000041C3 E90900                   jmp @@cmpneg450
  8160                                  @@cmpneg449:
  8161 000041C6 68[3D42]                 push cmpneg_10
  8162 000041C9 E8C70F                   call error1
  8163 000041CC 83C402                   add  sp, 2
  8164                                  @@cmpneg450:
  8165                                  @@cmpneg448:
  8166                                  @@cmpneg446:
  8167                                  @@cmpneg442:
  8168                                  @@cmpneg438:
  8169                                  @@cmpneg436:
  8170                                  
  8171                                  ;-800 
  8172                                  
  8173                                  
  8174                                  ;-801 int prlabel(int n) {prs("\n@@"); prs(fname); pint1(n); prc(':'); }
  8175 000041CF C9                       LEAVE
  8176 000041D0 C3                       ret
  8177                                  ; ENDP
  8178 000041D1 5C6E206A6E65204040-     cmpneg_0 db "\n jne @@",0
  8179 000041DA 00                 
  8180 000041DB 5C6E206A6520204040-     cmpneg_1 db "\n je  @@",0
  8181 000041E4 00                 
  8182 000041E5 5C6E206A6720204040-     cmpneg_2 db "\n jg  @@",0
  8183 000041EE 00                 
  8184 000041EF 5C6E206A6120204040-     cmpneg_3 db "\n ja  @@",0
  8185 000041F8 00                 
  8186 000041F9 203B756E7369676E65-     cmpneg_4 db " ;unsigned : ",0
  8187 00004202 64203A2000         
  8188 00004207 5C6E206A6C20204040-     cmpneg_5 db "\n jl  @@",0
  8189 00004210 00                 
  8190 00004211 203B756E7369676E65-     cmpneg_6 db " ;unsigned : ",0
  8191 0000421A 64203A2000         
  8192 0000421F 5C6E206A6220204040-     cmpneg_7 db "\n jb  @@",0
  8193 00004228 00                 
  8194 00004229 5C6E206A6765204040-     cmpneg_8 db "\n jge @@",0
  8195 00004232 00                 
  8196 00004233 5C6E206A6C65204040-     cmpneg_9 db "\n jle @@",0
  8197 0000423C 00                 
  8198 0000423D 566572676C65696368-     cmpneg_10 db "Vergleich unbekannt in CMPNEG()",0
  8199 00004246 20756E62656B616E6E-
  8200 0000424F 7420696E20434D504E-
  8201 00004258 4547282900         
  8202                                  
  8203                                  prlabel:  ; *** PROC ***
  8204                                  ;Function : prlabel, Number of local variables: 1
  8205                                  ;   # type sign width line used address name   list of local variables
  8206                                  ;  200 var sign word   801 NULL bp+4    n;
  8207 0000425D C8000000                 ENTER  0,0
  8208 00004261 68[8842]                 push prlabel_0
  8209 00004264 E82B0C                   call prs
  8210 00004267 83C402                   add  sp, 2
  8211 0000426A 8D06EE81                 lea  ax, [fname]
  8212 0000426E 50                       push ax
  8213 0000426F E8200C                   call prs
  8214 00004272 83C402                   add  sp, 2
  8215 00004275 FF7604                   push word [bp+4]
  8216 00004278 E81D0D                   call pint1
  8217 0000427B 83C402                   add  sp, 2
  8218 0000427E 6A3A                     push 58
  8219 00004280 E8A00B                   call prc
  8220 00004283 83C402                   add  sp, 2
  8221                                  
  8222                                  ;-802 int prjump (int n) {prs("\n jmp @@"); prs(fname); pint1(n); }
  8223 00004286 C9                       LEAVE
  8224 00004287 C3                       ret
  8225                                  ; ENDP
  8226 00004288 5C6E404000              prlabel_0 db "\n@@",0
  8227                                  
  8228                                  prjump:  ; *** PROC ***
  8229                                  ;Function : prjump, Number of local variables: 1
  8230                                  ;   # type sign width line used address name   list of local variables
  8231                                  ;  200 var sign word   802 NULL bp+4    n;
  8232 0000428D C8000000                 ENTER  0,0
  8233 00004291 68[B042]                 push prjump_0
  8234 00004294 E8FB0B                   call prs
  8235 00004297 83C402                   add  sp, 2
  8236 0000429A 8D06EE81                 lea  ax, [fname]
  8237 0000429E 50                       push ax
  8238 0000429F E8F00B                   call prs
  8239 000042A2 83C402                   add  sp, 2
  8240 000042A5 FF7604                   push word [bp+4]
  8241 000042A8 E8ED0C                   call pint1
  8242 000042AB 83C402                   add  sp, 2
  8243                                  
  8244                                  ;-803 int doif() {int jdest; int tst; pexpr(); nlabel++; jdest=nlabel;
  8245 000042AE C9                       LEAVE
  8246 000042AF C3                       ret
  8247                                  ; ENDP
  8248 000042B0 5C6E206A6D70204040-     prjump_0 db "\n jmp @@",0
  8249 000042B9 00                 
  8250                                  
  8251                                  doif:  ; *** PROC ***
  8252                                  ;Function : doif, Number of local variables: 2
  8253                                  ;   # type sign width line used address name   list of local variables
  8254                                  ;  200 var sign word   803 NULL bp-2    jdest
  8255                                  ;  201 var sign word   803 NULL bp-4    tst;
  8256 000042BA C8040000                 ENTER  4,0
  8257 000042BE E84DCF                   call pexpr
  8258 000042C1 FF06[4900]               inc  word [nlabel]
  8259 000042C5 A1[4900]                 mov ax, [nlabel]
  8260 000042C8 8946FE                   mov word [bp-2], ax
  8261                                  
  8262                                  ;-804   pint1(jdest); stmt();
  8263                                  
  8264 000042CB FF76FE                   push word [bp-2]
  8265 000042CE E8C70C                   call pint1
  8266 000042D1 83C402                   add  sp, 2
  8267 000042D4 E865FB                   call stmt
  8268                                  
  8269                                  ;-805   if (istoken(T_ELSE)) { nlabel++; tst=nlabel;
  8270                                  
  8271 000042D7 680202                   push 514
  8272 000042DA E86F0A                   call istoken
  8273 000042DD 83C402                   add  sp, 2
  8274 000042E0 08C0                     or  al, al
  8275 000042E2 0F842B00                 je @@doif451
  8276 000042E6 FF06[4900]               inc  word [nlabel]
  8277 000042EA A1[4900]                 mov ax, [nlabel]
  8278 000042ED 8946FC                   mov word [bp-4], ax
  8279                                  
  8280                                  ;-806     prjump(tst); prlabel(jdest); stmt(); prlabel(tst); }
  8281                                  
  8282 000042F0 FF76FC                   push word [bp-4]
  8283 000042F3 E897FF                   call prjump
  8284 000042F6 83C402                   add  sp, 2
  8285 000042F9 FF76FE                   push word [bp-2]
  8286 000042FC E85EFF                   call prlabel
  8287 000042FF 83C402                   add  sp, 2
  8288 00004302 E837FB                   call stmt
  8289 00004305 FF76FC                   push word [bp-4]
  8290 00004308 E852FF                   call prlabel
  8291 0000430B 83C402                   add  sp, 2
  8292                                  
  8293                                  ;-807   else prlabel(jdest); }
  8294                                  
  8295 0000430E E90900                   jmp @@doif452
  8296                                  @@doif451:
  8297 00004311 FF76FE                   push word [bp-2]
  8298 00004314 E846FF                   call prlabel
  8299 00004317 83C402                   add  sp, 2
  8300                                  @@doif452:
  8301                                  
  8302                                  ;-808 int doifcarry() {int jdest;  nlabel++; jdest=nlabel;
  8303 0000431A C9                       LEAVE
  8304 0000431B C3                       ret
  8305                                  ; ENDP
  8306                                  
  8307                                  doifcarry:  ; *** PROC ***
  8308                                  ;Function : doifcarry, Number of local variables: 1
  8309                                  ;   # type sign width line used address name   list of local variables
  8310                                  ;  200 var sign word   808 NULL bp-2    jdest;
  8311 0000431C C8020000                 ENTER  2,0
  8312 00004320 FF06[4900]               inc  word [nlabel]
  8313 00004324 A1[4900]                 mov ax, [nlabel]
  8314 00004327 8946FE                   mov word [bp-2], ax
  8315                                  
  8316                                  ;-809   prs("\n jae short @@");/*jnc*/ prs(fname);  pint1(jdest);
  8317                                  
  8318 0000432A 68[5543]                 push doifcarry_0
  8319 0000432D E8620B                   call prs
  8320 00004330 83C402                   add  sp, 2
  8321 00004333 8D06EE81                 lea  ax, [fname]
  8322 00004337 50                       push ax
  8323 00004338 E8570B                   call prs
  8324 0000433B 83C402                   add  sp, 2
  8325 0000433E FF76FE                   push word [bp-2]
  8326 00004341 E8540C                   call pint1
  8327 00004344 83C402                   add  sp, 2
  8328                                  
  8329                                  ;-810   stmt(); prlabel(jdest); }
  8330                                  
  8331 00004347 E8F2FA                   call stmt
  8332 0000434A FF76FE                   push word [bp-2]
  8333 0000434D E80DFF                   call prlabel
  8334 00004350 83C402                   add  sp, 2
  8335                                  
  8336                                  ;-811 int doifzero() {int jdest;  nlabel++; jdest=nlabel;
  8337 00004353 C9                       LEAVE
  8338 00004354 C3                       ret
  8339                                  ; ENDP
  8340 00004355 5C6E206A6165207368-     doifcarry_0 db "\n jae short @@",0
  8341 0000435E 6F727420404000     
  8342                                  
  8343                                  doifzero:  ; *** PROC ***
  8344                                  ;Function : doifzero, Number of local variables: 1
  8345                                  ;   # type sign width line used address name   list of local variables
  8346                                  ;  200 var sign word   811 NULL bp-2    jdest;
  8347 00004365 C8020000                 ENTER  2,0
  8348 00004369 FF06[4900]               inc  word [nlabel]
  8349 0000436D A1[4900]                 mov ax, [nlabel]
  8350 00004370 8946FE                   mov word [bp-2], ax
  8351                                  
  8352                                  ;-812   prs("\n jne short @@");        prs(fname);  pint1(jdest);
  8353                                  
  8354 00004373 68[9E43]                 push doifzero_0
  8355 00004376 E8190B                   call prs
  8356 00004379 83C402                   add  sp, 2
  8357 0000437C 8D06EE81                 lea  ax, [fname]
  8358 00004380 50                       push ax
  8359 00004381 E80E0B                   call prs
  8360 00004384 83C402                   add  sp, 2
  8361 00004387 FF76FE                   push word [bp-2]
  8362 0000438A E80B0C                   call pint1
  8363 0000438D 83C402                   add  sp, 2
  8364                                  
  8365                                  ;-813   stmt(); prlabel(jdest); }
  8366                                  
  8367 00004390 E8A9FA                   call stmt
  8368 00004393 FF76FE                   push word [bp-2]
  8369 00004396 E8C4FE                   call prlabel
  8370 00004399 83C402                   add  sp, 2
  8371                                  
  8372                                  ;-814 int dodo() {int jdest; int jtemp;
  8373 0000439C C9                       LEAVE
  8374 0000439D C3                       ret
  8375                                  ; ENDP
  8376 0000439E 5C6E206A6E65207368-     doifzero_0 db "\n jne short @@",0
  8377 000043A7 6F727420404000     
  8378                                  
  8379                                  dodo:  ; *** PROC ***
  8380                                  
  8381                                  ;-815   nlabel++; jdest=nlabel; prlabel(jdest); stmt();
  8382                                  
  8383                                  ;Function : dodo, Number of local variables: 2
  8384                                  ;   # type sign width line used address name   list of local variables
  8385                                  ;  200 var sign word   814 NULL bp-2    jdest
  8386                                  ;  201 var sign word   814 NULL bp-4    jtemp;
  8387 000043AE C8040000                 ENTER  4,0
  8388 000043B2 FF06[4900]               inc  word [nlabel]
  8389 000043B6 A1[4900]                 mov ax, [nlabel]
  8390 000043B9 8946FE                   mov word [bp-2], ax
  8391 000043BC FF76FE                   push word [bp-2]
  8392 000043BF E89BFE                   call prlabel
  8393 000043C2 83C402                   add  sp, 2
  8394 000043C5 E874FA                   call stmt
  8395                                  
  8396                                  ;-816   expect(T_WHILE); pexpr(); nlabel++; jtemp=nlabel; pint1(jtemp);
  8397                                  
  8398 000043C8 680302                   push 515
  8399 000043CB E8A609                   call expect
  8400 000043CE 83C402                   add  sp, 2
  8401 000043D1 E83ACE                   call pexpr
  8402 000043D4 FF06[4900]               inc  word [nlabel]
  8403 000043D8 A1[4900]                 mov ax, [nlabel]
  8404 000043DB 8946FC                   mov word [bp-4], ax
  8405 000043DE FF76FC                   push word [bp-4]
  8406 000043E1 E8B40B                   call pint1
  8407 000043E4 83C402                   add  sp, 2
  8408                                  
  8409                                  ;-817   prjump(jdest); prlabel(jtemp); }
  8410                                  
  8411 000043E7 FF76FE                   push word [bp-2]
  8412 000043EA E8A0FE                   call prjump
  8413 000043ED 83C402                   add  sp, 2
  8414 000043F0 FF76FC                   push word [bp-4]
  8415 000043F3 E867FE                   call prlabel
  8416 000043F6 83C402                   add  sp, 2
  8417                                  
  8418                                  ;-818 int dowhile() {int jdest; int tst; nlabel++; jdest=nlabel;
  8419 000043F9 C9                       LEAVE
  8420 000043FA C3                       ret
  8421                                  ; ENDP
  8422                                  
  8423                                  dowhile:  ; *** PROC ***
  8424                                  ;Function : dowhile, Number of local variables: 2
  8425                                  ;   # type sign width line used address name   list of local variables
  8426                                  ;  200 var sign word   818 NULL bp-2    jdest
  8427                                  ;  201 var sign word   818 NULL bp-4    tst;
  8428 000043FB C8040000                 ENTER  4,0
  8429 000043FF FF06[4900]               inc  word [nlabel]
  8430 00004403 A1[4900]                 mov ax, [nlabel]
  8431 00004406 8946FE                   mov word [bp-2], ax
  8432                                  
  8433                                  ;-819   prlabel(jdest); pexpr(); nlabel++; tst=nlabel; pint1(tst);
  8434                                  
  8435 00004409 FF76FE                   push word [bp-2]
  8436 0000440C E84EFE                   call prlabel
  8437 0000440F 83C402                   add  sp, 2
  8438 00004412 E8F9CD                   call pexpr
  8439 00004415 FF06[4900]               inc  word [nlabel]
  8440 00004419 A1[4900]                 mov ax, [nlabel]
  8441 0000441C 8946FC                   mov word [bp-4], ax
  8442 0000441F FF76FC                   push word [bp-4]
  8443 00004422 E8730B                   call pint1
  8444 00004425 83C402                   add  sp, 2
  8445                                  
  8446                                  ;-820   stmt(); prjump(jdest); prlabel(tst); }
  8447                                  
  8448 00004428 E811FA                   call stmt
  8449 0000442B FF76FE                   push word [bp-2]
  8450 0000442E E85CFE                   call prjump
  8451 00004431 83C402                   add  sp, 2
  8452 00004434 FF76FC                   push word [bp-4]
  8453 00004437 E823FE                   call prlabel
  8454 0000443A 83C402                   add  sp, 2
  8455                                  
  8456                                  ;-821 int isrelational() {
  8457 0000443D C9                       LEAVE
  8458 0000443E C3                       ret
  8459                                  ; ENDP
  8460                                  
  8461                                  isrelational:  ; *** PROC ***
  8462                                  
  8463                                  ;-822   if (token==T_EQ) goto w; if (token==T_NE) goto w;
  8464                                  
  8465 0000443F A1[3D00]                 mov ax, [token]
  8466 00004442 3D2603                   cmp ax, 806
  8467 00004445 0F850300                 jne @@isrelational453
  8468 00004449 E94A00                   jmp @@w
  8469                                  @@isrelational453:
  8470 0000444C A1[3D00]                 mov ax, [token]
  8471 0000444F 3D2703                   cmp ax, 807
  8472 00004452 0F850300                 jne @@isrelational454
  8473 00004456 E93D00                   jmp @@w
  8474                                  
  8475                                  ;-823   if (token==T_LE) goto w; if (token==T_GE) goto w;
  8476                                  
  8477                                  @@isrelational454:
  8478 00004459 A1[3D00]                 mov ax, [token]
  8479 0000445C 3D3803                   cmp ax, 824
  8480 0000445F 0F850300                 jne @@isrelational455
  8481 00004463 E93000                   jmp @@w
  8482                                  @@isrelational455:
  8483 00004466 A1[3D00]                 mov ax, [token]
  8484 00004469 3D2B03                   cmp ax, 811
  8485 0000446C 0F850300                 jne @@isrelational456
  8486 00004470 E92300                   jmp @@w
  8487                                  
  8488                                  ;-824   if (token=='<' ) goto w; if (token=='>' ) goto w;
  8489                                  
  8490                                  @@isrelational456:
  8491 00004473 A1[3D00]                 mov ax, [token]
  8492 00004476 83F83C                   cmp ax, 60
  8493 00004479 0F850300                 jne @@isrelational457
  8494 0000447D E91600                   jmp @@w
  8495                                  @@isrelational457:
  8496 00004480 A1[3D00]                 mov ax, [token]
  8497 00004483 83F83E                   cmp ax, 62
  8498 00004486 0F850300                 jne @@isrelational458
  8499 0000448A E90900                   jmp @@w
  8500                                  
  8501                                  ;-825   return 0;  w: iscmp=token; token=getlex(); return 1;}
  8502                                  
  8503                                  @@isrelational458:
  8504 0000448D 66B800000000             mov eax, 0
  8505 00004493 E91500                   jmp @@retn
  8506                                  @@w:
  8507 00004496 A1[3D00]                 mov ax, [token]
  8508 00004499 A3[4300]                 mov word [iscmp], ax
  8509 0000449C E80D00                   call getlex
  8510 0000449F A3[3D00]                 mov word [token], ax
  8511 000044A2 66B801000000             mov eax, 1
  8512 000044A8 E90000                   jmp @@retn
  8513                                  
  8514                                  ;-826  
  8515                                  
  8516                                  
  8517                                  ;-827 char symboltemp[80];    
  8518                                  
  8519                                  @@retn:
  8520 000044AB C3                       ret
  8521                                  ; ENDP
  8522                                  section .bss
  8523                                  absolute 46920
  8524 0000B748 <res 00000050>          symboltemp resb 80
  8525                                  section .text
  8526                                  
  8527                                  ;-828 int getlex() { char c; char *p; 
  8528                                  
  8529                                  
  8530                                  getlex:  ; *** PROC ***
  8531                                  
  8532                                  ;-829 g1: c=next(); if (c == 0) return 0; if (c <= ' ') goto g1;
  8533                                  
  8534                                  ;Function : getlex, Number of local variables: 2
  8535                                  ;   # type sign width line used address name   list of local variables
  8536                                  ;  200 var sign byte   828 NULL bp-2    c
  8537                                  ;  201 ptr sign byte   828 NULL bp-4    p;
  8538 000044AC C8040000                 ENTER  4,0
  8539                                  @@g1:
  8540 000044B0 E87B08                   call next
  8541 000044B3 8846FE                   mov byte [bp-2], al
  8542 000044B6 8A46FE                   mov al, [bp-2]
  8543 000044B9 3C00                     cmp al, 0
  8544 000044BB 0F850900                 jne @@getlex459
  8545 000044BF 66B800000000             mov eax, 0
  8546 000044C5 E90406                   jmp @@retn
  8547                                  @@getlex459:
  8548 000044C8 8A46FE                   mov al, [bp-2]
  8549 000044CB 3C20                     cmp al, 32
  8550 000044CD 0F8F0200                 jg  @@getlex460
  8551 000044D1 EBDD                     jmp @@g1
  8552                                  
  8553                                  ;-830   if (c=='=') {if(thechar=='=') {next(); return T_EQ; }}
  8554                                  
  8555                                  @@getlex460:
  8556 000044D3 8A46FE                   mov al, [bp-2]
  8557 000044D6 3C3D                     cmp al, 61
  8558 000044D8 0F851500                 jne @@getlex461
  8559 000044DC A0[4200]                 mov al, [thechar]
  8560 000044DF 3C3D                     cmp al, 61
  8561 000044E1 0F850C00                 jne @@getlex462
  8562 000044E5 E84608                   call next
  8563 000044E8 66B826030000             mov eax, 806
  8564 000044EE E9DB05                   jmp @@retn
  8565                                  @@getlex462:
  8566                                  
  8567                                  ;-831   if (c=='!') {if(thechar=='=') {next(); return T_NE; }}
  8568                                  
  8569                                  @@getlex461:
  8570 000044F1 8A46FE                   mov al, [bp-2]
  8571 000044F4 3C21                     cmp al, 33
  8572 000044F6 0F851500                 jne @@getlex463
  8573 000044FA A0[4200]                 mov al, [thechar]
  8574 000044FD 3C3D                     cmp al, 61
  8575 000044FF 0F850C00                 jne @@getlex464
  8576 00004503 E82808                   call next
  8577 00004506 66B827030000             mov eax, 807
  8578 0000450C E9BD05                   jmp @@retn
  8579                                  @@getlex464:
  8580                                  
  8581                                  ;-832   if (c=='<') {if(thechar=='=') {next(); return T_LE; }}
  8582                                  
  8583                                  @@getlex463:
  8584 0000450F 8A46FE                   mov al, [bp-2]
  8585 00004512 3C3C                     cmp al, 60
  8586 00004514 0F851500                 jne @@getlex465
  8587 00004518 A0[4200]                 mov al, [thechar]
  8588 0000451B 3C3D                     cmp al, 61
  8589 0000451D 0F850C00                 jne @@getlex466
  8590 00004521 E80A08                   call next
  8591 00004524 66B838030000             mov eax, 824
  8592 0000452A E99F05                   jmp @@retn
  8593                                  @@getlex466:
  8594                                  
  8595                                  ;-833   if (c=='>') {if(thechar=='=') {next(); return T_GE; }}
  8596                                  
  8597                                  @@getlex465:
  8598 0000452D 8A46FE                   mov al, [bp-2]
  8599 00004530 3C3E                     cmp al, 62
  8600 00004532 0F851500                 jne @@getlex467
  8601 00004536 A0[4200]                 mov al, [thechar]
  8602 00004539 3C3D                     cmp al, 61
  8603 0000453B 0F850C00                 jne @@getlex468
  8604 0000453F E8EC07                   call next
  8605 00004542 66B82B030000             mov eax, 811
  8606 00004548 E98105                   jmp @@retn
  8607                                  @@getlex468:
  8608                                  
  8609                                  ;-834   if (c=='<') {if(thechar=='<') {next(); return T_LESSLESS;  }}
  8610                                  
  8611                                  @@getlex467:
  8612 0000454B 8A46FE                   mov al, [bp-2]
  8613 0000454E 3C3C                     cmp al, 60
  8614 00004550 0F851500                 jne @@getlex469
  8615 00004554 A0[4200]                 mov al, [thechar]
  8616 00004557 3C3C                     cmp al, 60
  8617 00004559 0F850C00                 jne @@getlex470
  8618 0000455D E8CE07                   call next
  8619 00004560 66B8D8040000             mov eax, 1240
  8620 00004566 E96305                   jmp @@retn
  8621                                  @@getlex470:
  8622                                  
  8623                                  ;-835   if (c=='>') {if(thechar=='>') {next(); return T_GREATGREAT;}}
  8624                                  
  8625                                  @@getlex469:
  8626 00004569 8A46FE                   mov al, [bp-2]
  8627 0000456C 3C3E                     cmp al, 62
  8628 0000456E 0F851500                 jne @@getlex471
  8629 00004572 A0[4200]                 mov al, [thechar]
  8630 00004575 3C3E                     cmp al, 62
  8631 00004577 0F850C00                 jne @@getlex472
  8632 0000457B E8B007                   call next
  8633 0000457E 66B8D9040000             mov eax, 1241
  8634 00004584 E94505                   jmp @@retn
  8635                                  @@getlex472:
  8636                                  
  8637                                  ;-836   if (c=='+') {if(thechar=='+') {next(); return T_PLUSPLUS;  }}
  8638                                  
  8639                                  @@getlex471:
  8640 00004587 8A46FE                   mov al, [bp-2]
  8641 0000458A 3C2B                     cmp al, 43
  8642 0000458C 0F851500                 jne @@getlex473
  8643 00004590 A0[4200]                 mov al, [thechar]
  8644 00004593 3C2B                     cmp al, 43
  8645 00004595 0F850C00                 jne @@getlex474
  8646 00004599 E89207                   call next
  8647 0000459C 66B8C3040000             mov eax, 1219
  8648 000045A2 E92705                   jmp @@retn
  8649                                  @@getlex474:
  8650                                  
  8651                                  ;-837   if (c=='-') {if(thechar=='-') {next(); return T_MINUSMINUS;}}
  8652                                  
  8653                                  @@getlex473:
  8654 000045A5 8A46FE                   mov al, [bp-2]
  8655 000045A8 3C2D                     cmp al, 45
  8656 000045AA 0F851500                 jne @@getlex475
  8657 000045AE A0[4200]                 mov al, [thechar]
  8658 000045B1 3C2D                     cmp al, 45
  8659 000045B3 0F850C00                 jne @@getlex476
  8660 000045B7 E87407                   call next
  8661 000045BA 66B8C9040000             mov eax, 1225
  8662 000045C0 E90905                   jmp @@retn
  8663                                  @@getlex476:
  8664                                  
  8665                                  ;-838   if (c=='+') {if(thechar=='=') {next(); return T_PLUSASS;   }}
  8666                                  
  8667                                  @@getlex475:
  8668 000045C3 8A46FE                   mov al, [bp-2]
  8669 000045C6 3C2B                     cmp al, 43
  8670 000045C8 0F851500                 jne @@getlex477
  8671 000045CC A0[4200]                 mov al, [thechar]
  8672 000045CF 3C3D                     cmp al, 61
  8673 000045D1 0F850C00                 jne @@getlex478
  8674 000045D5 E85607                   call next
  8675 000045D8 66B8CE040000             mov eax, 1230
  8676 000045DE E9EB04                   jmp @@retn
  8677                                  @@getlex478:
  8678                                  
  8679                                  ;-839   if (c=='-') {if(thechar=='=') {next(); return T_MINUSASS;  }}
  8680                                  
  8681                                  @@getlex477:
  8682 000045E1 8A46FE                   mov al, [bp-2]
  8683 000045E4 3C2D                     cmp al, 45
  8684 000045E6 0F851500                 jne @@getlex479
  8685 000045EA A0[4200]                 mov al, [thechar]
  8686 000045ED 3C3D                     cmp al, 61
  8687 000045EF 0F850C00                 jne @@getlex480
  8688 000045F3 E83807                   call next
  8689 000045F6 66B8CF040000             mov eax, 1231
  8690 000045FC E9CD04                   jmp @@retn
  8691                                  @@getlex480:
  8692                                  
  8693                                  ;-840   if (c=='&') {if(thechar=='=') {next(); return T_ANDASS;    }}
  8694                                  
  8695                                  @@getlex479:
  8696 000045FF 8A46FE                   mov al, [bp-2]
  8697 00004602 3C26                     cmp al, 38
  8698 00004604 0F851500                 jne @@getlex481
  8699 00004608 A0[4200]                 mov al, [thechar]
  8700 0000460B 3C3D                     cmp al, 61
  8701 0000460D 0F850C00                 jne @@getlex482
  8702 00004611 E81A07                   call next
  8703 00004614 66B8D2040000             mov eax, 1234
  8704 0000461A E9AF04                   jmp @@retn
  8705                                  @@getlex482:
  8706                                  
  8707                                  ;-841   if (c=='|') {if(thechar=='=') {next(); return T_ORASS;     }}    
  8708                                  
  8709                                  @@getlex481:
  8710 0000461D 8A46FE                   mov al, [bp-2]
  8711 00004620 3C7C                     cmp al, 124
  8712 00004622 0F851500                 jne @@getlex483
  8713 00004626 A0[4200]                 mov al, [thechar]
  8714 00004629 3C3D                     cmp al, 61
  8715 0000462B 0F850C00                 jne @@getlex484
  8716 0000462F E8FC06                   call next
  8717 00004632 66B8D3040000             mov eax, 1235
  8718 00004638 E99104                   jmp @@retn
  8719                                  @@getlex484:
  8720                                  
  8721                                  ;-842   if (c=='*') {if(thechar=='=') {next(); return T_MULASS;    }}
  8722                                  
  8723                                  @@getlex483:
  8724 0000463B 8A46FE                   mov al, [bp-2]
  8725 0000463E 3C2A                     cmp al, 42
  8726 00004640 0F851500                 jne @@getlex485
  8727 00004644 A0[4200]                 mov al, [thechar]
  8728 00004647 3C3D                     cmp al, 61
  8729 00004649 0F850C00                 jne @@getlex486
  8730 0000464D E8DE06                   call next
  8731 00004650 66B8D0040000             mov eax, 1232
  8732 00004656 E97304                   jmp @@retn
  8733                                  @@getlex486:
  8734                                  
  8735                                  ;-843   if (c=='/') {if(thechar=='=') {next(); return T_DIVASS;    }}        
  8736                                  
  8737                                  @@getlex485:
  8738 00004659 8A46FE                   mov al, [bp-2]
  8739 0000465C 3C2F                     cmp al, 47
  8740 0000465E 0F851500                 jne @@getlex487
  8741 00004662 A0[4200]                 mov al, [thechar]
  8742 00004665 3C3D                     cmp al, 61
  8743 00004667 0F850C00                 jne @@getlex488
  8744 0000466B E8C006                   call next
  8745 0000466E 66B8D1040000             mov eax, 1233
  8746 00004674 E95504                   jmp @@retn
  8747                                  @@getlex488:
  8748                                  
  8749                                  ;-844   if (instr1("()[]{},;*:%-><=+!&|#?", c)) return c ;
  8750                                  
  8751                                  @@getlex487:
  8752 00004677 8A46FE                   mov al, byte [bp-2]
  8753 0000467A B400                     mov ah, 0
  8754 0000467C 50                       push ax
  8755 0000467D 68[CE4A]                 push getlex_0
  8756 00004680 E8421B                   call instr1
  8757 00004683 83C404                   add  sp, 4
  8758 00004686 08C0                     or  al, al
  8759 00004688 0F840600                 je @@getlex489
  8760 0000468C 8A46FE                   mov al, [bp-2]
  8761 0000468F E93A04                   jmp @@retn
  8762                                  
  8763                                  ;-845   if (c == '/') { if (thechar == '/') {
  8764                                  
  8765                                  @@getlex489:
  8766 00004692 8A46FE                   mov al, [bp-2]
  8767 00004695 3C2F                     cmp al, 47
  8768 00004697 0F852000                 jne @@getlex490
  8769 0000469B A0[4200]                 mov al, [thechar]
  8770 0000469E 3C2F                     cmp al, 47
  8771 000046A0 0F851700                 jne @@getlex491
  8772                                  
  8773                                  ;-846       do c=next(); while(c != 13); /* c=next(); */ return getlex(); } }
  8774                                  
  8775                                  @@getlex492:
  8776 000046A4 E88706                   call next
  8777 000046A7 8846FE                   mov byte [bp-2], al
  8778 000046AA 8A46FE                   mov al, [bp-2]
  8779 000046AD 3C0D                     cmp al, 13
  8780 000046AF 0F840200                 je  @@getlex493
  8781 000046B3 EBEF                     jmp @@getlex492
  8782                                  @@getlex493:
  8783 000046B5 E8F4FD                   call getlex
  8784 000046B8 E91104                   jmp @@retn
  8785                                  @@getlex491:
  8786                                  
  8787                                  ;-847   if (c == '/') { if (thechar == '*') {
  8788                                  
  8789                                  @@getlex490:
  8790 000046BB 8A46FE                   mov al, [bp-2]
  8791 000046BE 3C2F                     cmp al, 47
  8792 000046C0 0F853D00                 jne @@getlex494
  8793 000046C4 A0[4200]                 mov al, [thechar]
  8794 000046C7 3C2A                     cmp al, 42
  8795 000046C9 0F852B00                 jne @@getlex495
  8796                                  
  8797                                  ;-848       g2: c=next(); if (c != '*') goto g2; if (thechar != '/') goto g2;
  8798                                  
  8799                                  @@g2:
  8800 000046CD E85E06                   call next
  8801 000046D0 8846FE                   mov byte [bp-2], al
  8802 000046D3 8A46FE                   mov al, [bp-2]
  8803 000046D6 3C2A                     cmp al, 42
  8804 000046D8 0F840200                 je  @@getlex496
  8805 000046DC EBEF                     jmp @@g2
  8806                                  @@getlex496:
  8807 000046DE A0[4200]                 mov al, [thechar]
  8808 000046E1 3C2F                     cmp al, 47
  8809 000046E3 0F840200                 je  @@getlex497
  8810 000046E7 EBE4                     jmp @@g2
  8811                                  
  8812                                  ;-849       c=next(); return getlex(); } else  return '/'; }
  8813                                  
  8814                                  @@getlex497:
  8815 000046E9 E84206                   call next
  8816 000046EC 8846FE                   mov byte [bp-2], al
  8817 000046EF E8BAFD                   call getlex
  8818 000046F2 E9D703                   jmp @@retn
  8819 000046F5 E90900                   jmp @@getlex498
  8820                                  @@getlex495:
  8821 000046F8 66B82F000000             mov eax, 47
  8822 000046FE E9CB03                   jmp @@retn
  8823                                  @@getlex498:
  8824                                  
  8825                                  ;-850   if (c == '"') {getstring(c); return T_STRING;}
  8826                                  
  8827                                  @@getlex494:
  8828 00004701 8A46FE                   mov al, [bp-2]
  8829 00004704 3C22                     cmp al, 34
  8830 00004706 0F851500                 jne @@getlex499
  8831 0000470A 8A46FE                   mov al, byte [bp-2]
  8832 0000470D B400                     mov ah, 0
  8833 0000470F 50                       push ax
  8834 00004710 E8E105                   call getstring
  8835 00004713 83C402                   add  sp, 2
  8836 00004716 66B802010000             mov eax, 258
  8837 0000471C E9AD03                   jmp @@retn
  8838                                  
  8839                                  ;-851   if (digit(c)) { getdigit(c); return T_CONST; }
  8840                                  
  8841                                  @@getlex499:
  8842 0000471F 8A46FE                   mov al, byte [bp-2]
  8843 00004722 B400                     mov ah, 0
  8844 00004724 50                       push ax
  8845 00004725 E80A1B                   call digit
  8846 00004728 83C402                   add  sp, 2
  8847 0000472B 08C0                     or  al, al
  8848 0000472D 0F841500                 je @@getlex500
  8849 00004731 8A46FE                   mov al, byte [bp-2]
  8850 00004734 B400                     mov ah, 0
  8851 00004736 50                       push ax
  8852 00004737 E8CC04                   call getdigit
  8853 0000473A 83C402                   add  sp, 2
  8854 0000473D 66B801010000             mov eax, 257
  8855 00004743 E98603                   jmp @@retn
  8856                                  
  8857                                  ;-852   if (c==39) { lexvalL=next();
  8858                                  
  8859                                  @@getlex500:
  8860 00004746 8A46FE                   mov al, [bp-2]
  8861 00004749 3C27                     cmp al, 39
  8862 0000474B 0F856800                 jne @@getlex501
  8863 0000474F E8DC05                   call next
  8864 00004752 66A3[4D00]               mov dword [lexvalL], eax
  8865                                  
  8866                                  ;-853     if (lexvalL==92) {lexvalL=next();
  8867                                  
  8868 00004756 66A1[4D00]               mov eax, [lexvalL]
  8869 0000475A 6683F85C                 cmp eax, 92
  8870 0000475E 0F854900                 jne @@getlex502
  8871 00004762 E8C905                   call next
  8872 00004765 66A3[4D00]               mov dword [lexvalL], eax
  8873                                  
  8874                                  ;-854       if (lexvalL=='n') lexvalL=10; if (lexvalL=='t') lexvalL= 9;
  8875                                  
  8876 00004769 66A1[4D00]               mov eax, [lexvalL]
  8877 0000476D 6683F86E                 cmp eax, 110
  8878 00004771 0F850A00                 jne @@getlex503
  8879 00004775 66B80A000000             mov eax, 10
  8880 0000477B 66A3[4D00]               mov dword [lexvalL], eax
  8881                                  @@getlex503:
  8882 0000477F 66A1[4D00]               mov eax, [lexvalL]
  8883 00004783 6683F874                 cmp eax, 116
  8884 00004787 0F850A00                 jne @@getlex504
  8885 0000478B 66B809000000             mov eax, 9
  8886 00004791 66A3[4D00]               mov dword [lexvalL], eax
  8887                                  
  8888                                  ;-855       if (lexvalL=='0') lexvalL= 0; } next(); return T_CONST; }
  8889                                  
  8890                                  @@getlex504:
  8891 00004795 66A1[4D00]               mov eax, [lexvalL]
  8892 00004799 6683F830                 cmp eax, 48
  8893 0000479D 0F850A00                 jne @@getlex505
  8894 000047A1 66B800000000             mov eax, 0
  8895 000047A7 66A3[4D00]               mov dword [lexvalL], eax
  8896                                  @@getlex505:
  8897                                  @@getlex502:
  8898 000047AB E88005                   call next
  8899 000047AE 66B801010000             mov eax, 257
  8900 000047B4 E91503                   jmp @@retn
  8901                                  
  8902                                  ;-856   if (letter(c)) { 
  8903                                  
  8904                                  @@getlex501:
  8905 000047B7 8A46FE                   mov al, byte [bp-2]
  8906 000047BA B400                     mov ah, 0
  8907 000047BC 50                       push ax
  8908 000047BD E8A51A                   call letter
  8909 000047C0 83C402                   add  sp, 2
  8910 000047C3 08C0                     or  al, al
  8911 000047C5 0F84FA02                 je @@getlex506
  8912                                  
  8913                                  ;-857     strcpy(symboltemp, symbol); p=&symbol;  *p=c;  p++;
  8914                                  
  8915 000047C9 8D066E81                 lea  ax, [symbol]
  8916 000047CD 50                       push ax
  8917 000047CE 8D0648B7                 lea  ax, [symboltemp]
  8918 000047D2 50                       push ax
  8919 000047D3 E8B619                   call strcpy
  8920 000047D6 83C404                   add  sp, 4
  8921 000047D9 B86E81                   mov ax, symbol
  8922 000047DC 8946FC                   mov word [bp-4], ax
  8923 000047DF 8A46FE                   mov al, [bp-2]
  8924 000047E2 8B5EFC                   mov  bx, [bp-4]
  8925 000047E5 8807                     mov  [bx], al
  8926 000047E7 FF46FC                   inc  word [bp-4]
  8927                                  
  8928                                  ;-858     while(letter(thechar)) {c=next(); *p=c;  p++; } 
  8929                                  
  8930                                  @@getlex507:
  8931 000047EA A0[4200]                 mov al, byte [thechar]
  8932 000047ED B400                     mov ah, 0
  8933 000047EF 50                       push ax
  8934 000047F0 E8721A                   call letter
  8935 000047F3 83C402                   add  sp, 2
  8936 000047F6 08C0                     or  al, al
  8937 000047F8 0F841300                 je @@getlex508
  8938 000047FC E82F05                   call next
  8939 000047FF 8846FE                   mov byte [bp-2], al
  8940 00004802 8A46FE                   mov al, [bp-2]
  8941 00004805 8B5EFC                   mov  bx, [bp-4]
  8942 00004808 8807                     mov  [bx], al
  8943 0000480A FF46FC                   inc  word [bp-4]
  8944                                  
  8945                                  ;-859       *p=0;
  8946                                  
  8947 0000480D EBDB                     jmp @@getlex507
  8948                                  @@getlex508:
  8949 0000480F 66B800000000             mov eax, 0
  8950 00004815 8B5EFC                   mov  bx, [bp-4]
  8951 00004818 8807                     mov  [bx], al
  8952                                  
  8953                                  ;-860     if (eqstr(symbol,"signed"  )) return T_SIGNED;
  8954                                  
  8955 0000481A 68[E44A]                 push getlex_1
  8956 0000481D 8D066E81                 lea  ax, [symbol]
  8957 00004821 50                       push ax
  8958 00004822 E8BB1A                   call eqstr
  8959 00004825 83C404                   add  sp, 4
  8960 00004828 08C0                     or  al, al
  8961 0000482A 0F840900                 je @@getlex509
  8962 0000482E 66B813020000             mov eax, 531
  8963 00004834 E99502                   jmp @@retn
  8964                                  
  8965                                  ;-861     if (eqstr(symbol,"unsigned")) return T_UNSIGNED;
  8966                                  
  8967                                  @@getlex509:
  8968 00004837 68[EB4A]                 push getlex_2
  8969 0000483A 8D066E81                 lea  ax, [symbol]
  8970 0000483E 50                       push ax
  8971 0000483F E89E1A                   call eqstr
  8972 00004842 83C404                   add  sp, 4
  8973 00004845 08C0                     or  al, al
  8974 00004847 0F840900                 je @@getlex510
  8975 0000484B 66B814020000             mov eax, 532
  8976 00004851 E97802                   jmp @@retn
  8977                                  
  8978                                  ;-862     if (eqstr(symbol,"void"    )) return T_VOID;
  8979                                  
  8980                                  @@getlex510:
  8981 00004854 68[F44A]                 push getlex_3
  8982 00004857 8D066E81                 lea  ax, [symbol]
  8983 0000485B 50                       push ax
  8984 0000485C E8811A                   call eqstr
  8985 0000485F 83C404                   add  sp, 4
  8986 00004862 08C0                     or  al, al
  8987 00004864 0F840900                 je @@getlex511
  8988 00004868 66B811020000             mov eax, 529
  8989 0000486E E95B02                   jmp @@retn
  8990                                  
  8991                                  ;-863     if (eqstr(symbol,"int"     )) return T_INT;
  8992                                  
  8993                                  @@getlex511:
  8994 00004871 68[F94A]                 push getlex_4
  8995 00004874 8D066E81                 lea  ax, [symbol]
  8996 00004878 50                       push ax
  8997 00004879 E8641A                   call eqstr
  8998 0000487C 83C404                   add  sp, 4
  8999 0000487F 08C0                     or  al, al
  9000 00004881 0F840900                 je @@getlex512
  9001 00004885 66B805020000             mov eax, 517
  9002 0000488B E93E02                   jmp @@retn
  9003                                  
  9004                                  ;-864     if (eqstr(symbol,"short"   )) return T_SHORT;
  9005                                  
  9006                                  @@getlex512:
  9007 0000488E 68[FD4A]                 push getlex_5
  9008 00004891 8D066E81                 lea  ax, [symbol]
  9009 00004895 50                       push ax
  9010 00004896 E8471A                   call eqstr
  9011 00004899 83C404                   add  sp, 4
  9012 0000489C 08C0                     or  al, al
  9013 0000489E 0F840900                 je @@getlex513
  9014 000048A2 66B816020000             mov eax, 534
  9015 000048A8 E92102                   jmp @@retn
  9016                                  
  9017                                  ;-865     if (eqstr(symbol,"long"    )) return T_LONG;
  9018                                  
  9019                                  @@getlex513:
  9020 000048AB 68[034B]                 push getlex_6
  9021 000048AE 8D066E81                 lea  ax, [symbol]
  9022 000048B2 50                       push ax
  9023 000048B3 E82A1A                   call eqstr
  9024 000048B6 83C404                   add  sp, 4
  9025 000048B9 08C0                     or  al, al
  9026 000048BB 0F840900                 je @@getlex514
  9027 000048BF 66B815020000             mov eax, 533
  9028 000048C5 E90402                   jmp @@retn
  9029                                  
  9030                                  ;-866     if (eqstr(symbol,"uint32"  )) return T_UINT32;
  9031                                  
  9032                                  @@getlex514:
  9033 000048C8 68[084B]                 push getlex_7
  9034 000048CB 8D066E81                 lea  ax, [symbol]
  9035 000048CF 50                       push ax
  9036 000048D0 E80D1A                   call eqstr
  9037 000048D3 83C404                   add  sp, 4
  9038 000048D6 08C0                     or  al, al
  9039 000048D8 0F840900                 je @@getlex515
  9040 000048DC 66B821020000             mov eax, 545
  9041 000048E2 E9E701                   jmp @@retn
  9042                                  
  9043                                  ;-867     if (eqstr(symbol,"inth"    )) return T_INTH;
  9044                                  
  9045                                  @@getlex515:
  9046 000048E5 68[0F4B]                 push getlex_8
  9047 000048E8 8D066E81                 lea  ax, [symbol]
  9048 000048EC 50                       push ax
  9049 000048ED E8F019                   call eqstr
  9050 000048F0 83C404                   add  sp, 4
  9051 000048F3 08C0                     or  al, al
  9052 000048F5 0F840900                 je @@getlex516
  9053 000048F9 66B858020000             mov eax, 600
  9054 000048FF E9CA01                   jmp @@retn
  9055                                  
  9056                                  ;-868     if (eqstr(symbol,"char"    )) return T_CHAR;
  9057                                  
  9058                                  @@getlex516:
  9059 00004902 68[144B]                 push getlex_9
  9060 00004905 8D066E81                 lea  ax, [symbol]
  9061 00004909 50                       push ax
  9062 0000490A E8D319                   call eqstr
  9063 0000490D 83C404                   add  sp, 4
  9064 00004910 08C0                     or  al, al
  9065 00004912 0F840900                 je @@getlex517
  9066 00004916 66B812020000             mov eax, 530
  9067 0000491C E9AD01                   jmp @@retn
  9068                                  
  9069                                  ;-869     if (eqstr(symbol,"asm"     )) return T_ASM;
  9070                                  
  9071                                  @@getlex517:
  9072 0000491F 68[194B]                 push getlex_10
  9073 00004922 8D066E81                 lea  ax, [symbol]
  9074 00004926 50                       push ax
  9075 00004927 E8B619                   call eqstr
  9076 0000492A 83C404                   add  sp, 4
  9077 0000492D 08C0                     or  al, al
  9078 0000492F 0F840900                 je @@getlex518
  9079 00004933 66B806020000             mov eax, 518
  9080 00004939 E99001                   jmp @@retn
  9081                                  
  9082                                  ;-870     if (eqstr(symbol,"__asm"   )) return T_ASMBLOCK;
  9083                                  
  9084                                  @@getlex518:
  9085 0000493C 68[1D4B]                 push getlex_11
  9086 0000493F 8D066E81                 lea  ax, [symbol]
  9087 00004943 50                       push ax
  9088 00004944 E89919                   call eqstr
  9089 00004947 83C404                   add  sp, 4
  9090 0000494A 08C0                     or  al, al
  9091 0000494C 0F840900                 je @@getlex519
  9092 00004950 66B807020000             mov eax, 519
  9093 00004956 E97301                   jmp @@retn
  9094                                  
  9095                                  ;-871     if (eqstr(symbol,"__emit__")) return T_EMIT;
  9096                                  
  9097                                  @@getlex519:
  9098 00004959 68[234B]                 push getlex_12
  9099 0000495C 8D066E81                 lea  ax, [symbol]
  9100 00004960 50                       push ax
  9101 00004961 E87C19                   call eqstr
  9102 00004964 83C404                   add  sp, 4
  9103 00004967 08C0                     or  al, al
  9104 00004969 0F840900                 je @@getlex520
  9105 0000496D 66B808020000             mov eax, 520
  9106 00004973 E95601                   jmp @@retn
  9107                                  
  9108                                  ;-872     if (eqstr(symbol,"return"  )) return T_RETURN;
  9109                                  
  9110                                  @@getlex520:
  9111 00004976 68[2C4B]                 push getlex_13
  9112 00004979 8D066E81                 lea  ax, [symbol]
  9113 0000497D 50                       push ax
  9114 0000497E E85F19                   call eqstr
  9115 00004981 83C404                   add  sp, 4
  9116 00004984 08C0                     or  al, al
  9117 00004986 0F840900                 je @@getlex521
  9118 0000498A 66B800020000             mov eax, 512
  9119 00004990 E93901                   jmp @@retn
  9120                                  
  9121                                  ;-873     if (eqstr(symbol,"if"      )) return T_IF;
  9122                                  
  9123                                  @@getlex521:
  9124 00004993 68[334B]                 push getlex_14
  9125 00004996 8D066E81                 lea  ax, [symbol]
  9126 0000499A 50                       push ax
  9127 0000499B E84219                   call eqstr
  9128 0000499E 83C404                   add  sp, 4
  9129 000049A1 08C0                     or  al, al
  9130 000049A3 0F840900                 je @@getlex522
  9131 000049A7 66B801020000             mov eax, 513
  9132 000049AD E91C01                   jmp @@retn
  9133                                  
  9134                                  ;-874     if (eqstr(symbol,"ifcarry" )) return T_IFCARRY;
  9135                                  
  9136                                  @@getlex522:
  9137 000049B0 68[364B]                 push getlex_15
  9138 000049B3 8D066E81                 lea  ax, [symbol]
  9139 000049B7 50                       push ax
  9140 000049B8 E82519                   call eqstr
  9141 000049BB 83C404                   add  sp, 4
  9142 000049BE 08C0                     or  al, al
  9143 000049C0 0F840900                 je @@getlex523
  9144 000049C4 66B859020000             mov eax, 601
  9145 000049CA E9FF00                   jmp @@retn
  9146                                  
  9147                                  ;-875     if (eqstr(symbol,"ifzero"  )) return T_IFZERO;
  9148                                  
  9149                                  @@getlex523:
  9150 000049CD 68[3E4B]                 push getlex_16
  9151 000049D0 8D066E81                 lea  ax, [symbol]
  9152 000049D4 50                       push ax
  9153 000049D5 E80819                   call eqstr
  9154 000049D8 83C404                   add  sp, 4
  9155 000049DB 08C0                     or  al, al
  9156 000049DD 0F840900                 je @@getlex524
  9157 000049E1 66B85A020000             mov eax, 602
  9158 000049E7 E9E200                   jmp @@retn
  9159                                  
  9160                                  ;-876     if (eqstr(symbol,"else"    )) return T_ELSE;
  9161                                  
  9162                                  @@getlex524:
  9163 000049EA 68[454B]                 push getlex_17
  9164 000049ED 8D066E81                 lea  ax, [symbol]
  9165 000049F1 50                       push ax
  9166 000049F2 E8EB18                   call eqstr
  9167 000049F5 83C404                   add  sp, 4
  9168 000049F8 08C0                     or  al, al
  9169 000049FA 0F840900                 je @@getlex525
  9170 000049FE 66B802020000             mov eax, 514
  9171 00004A04 E9C500                   jmp @@retn
  9172                                  
  9173                                  ;-877     if (eqstr(symbol,"while"   )) return T_WHILE;
  9174                                  
  9175                                  @@getlex525:
  9176 00004A07 68[4A4B]                 push getlex_18
  9177 00004A0A 8D066E81                 lea  ax, [symbol]
  9178 00004A0E 50                       push ax
  9179 00004A0F E8CE18                   call eqstr
  9180 00004A12 83C404                   add  sp, 4
  9181 00004A15 08C0                     or  al, al
  9182 00004A17 0F840900                 je @@getlex526
  9183 00004A1B 66B803020000             mov eax, 515
  9184 00004A21 E9A800                   jmp @@retn
  9185                                  
  9186                                  ;-878     if (eqstr(symbol,"do"      )) return T_DO;
  9187                                  
  9188                                  @@getlex526:
  9189 00004A24 68[504B]                 push getlex_19
  9190 00004A27 8D066E81                 lea  ax, [symbol]
  9191 00004A2B 50                       push ax
  9192 00004A2C E8B118                   call eqstr
  9193 00004A2F 83C404                   add  sp, 4
  9194 00004A32 08C0                     or  al, al
  9195 00004A34 0F840900                 je @@getlex527
  9196 00004A38 66B804020000             mov eax, 516
  9197 00004A3E E98B00                   jmp @@retn
  9198                                  
  9199                                  ;-879     if (eqstr(symbol,"goto"    )) return T_GOTO;
  9200                                  
  9201                                  @@getlex527:
  9202 00004A41 68[534B]                 push getlex_20
  9203 00004A44 8D066E81                 lea  ax, [symbol]
  9204 00004A48 50                       push ax
  9205 00004A49 E89418                   call eqstr
  9206 00004A4C 83C404                   add  sp, 4
  9207 00004A4F 08C0                     or  al, al
  9208 00004A51 0F840900                 je @@getlex528
  9209 00004A55 66B809020000             mov eax, 521
  9210 00004A5B E96E00                   jmp @@retn
  9211                                  
  9212                                  ;-880     if (eqstr(symbol,"define"  )) return T_DEFINE;   
  9213                                  
  9214                                  @@getlex528:
  9215 00004A5E 68[584B]                 push getlex_21
  9216 00004A61 8D066E81                 lea  ax, [symbol]
  9217 00004A65 50                       push ax
  9218 00004A66 E87718                   call eqstr
  9219 00004A69 83C404                   add  sp, 4
  9220 00004A6C 08C0                     or  al, al
  9221 00004A6E 0F840900                 je @@getlex529
  9222 00004A72 66B8FF010000             mov eax, 511
  9223 00004A78 E95100                   jmp @@retn
  9224                                  
  9225                                  ;-881     if (eqstr(symbol,"include" )) return T_INCLUDE;   
  9226                                  
  9227                                  @@getlex529:
  9228 00004A7B 68[5F4B]                 push getlex_22
  9229 00004A7E 8D066E81                 lea  ax, [symbol]
  9230 00004A82 50                       push ax
  9231 00004A83 E85A18                   call eqstr
  9232 00004A86 83C404                   add  sp, 4
  9233 00004A89 08C0                     or  al, al
  9234 00004A8B 0F840900                 je @@getlex530
  9235 00004A8F 66B8FE010000             mov eax, 510
  9236 00004A95 E93400                   jmp @@retn
  9237                                  
  9238                                  ;-882     if (convertdefine() ) {strcpy(symbol, symboltemp); return T_CONST;}
  9239                                  
  9240                                  @@getlex530:
  9241 00004A98 E8E600                   call convertdefine
  9242 00004A9B 08C0                     or  al, al
  9243 00004A9D 0F841900                 je @@getlex531
  9244 00004AA1 8D0648B7                 lea  ax, [symboltemp]
  9245 00004AA5 50                       push ax
  9246 00004AA6 8D066E81                 lea  ax, [symbol]
  9247 00004AAA 50                       push ax
  9248 00004AAB E8DE16                   call strcpy
  9249 00004AAE 83C404                   add  sp, 4
  9250 00004AB1 66B801010000             mov eax, 257
  9251 00004AB7 E91200                   jmp @@retn
  9252                                  
  9253                                  ;-883     return T_NAME; } error1("Input item not recognized"); }
  9254                                  
  9255                                  @@getlex531:
  9256 00004ABA 66B800010000             mov eax, 256
  9257 00004AC0 E90900                   jmp @@retn
  9258                                  @@getlex506:
  9259 00004AC3 68[674B]                 push getlex_23
  9260 00004AC6 E8CA06                   call error1
  9261 00004AC9 83C402                   add  sp, 2
  9262                                  
  9263                                  ;-884 
  9264                                  
  9265                                  
  9266                                  ;-885 int convertdefine() { int i; int j;   i=0;
  9267                                  
  9268 00004ACC C9                      @@retn: LEAVE
  9269 00004ACD C3                       ret
  9270                                  ; ENDP
  9271 00004ACE 28295B5D7B7D2C3B2A-     getlex_0 db "()[]{},;*:%-><=+!&|#?",0
  9272 00004AD7 3A252D3E3C3D2B2126-
  9273 00004AE0 7C233F00           
  9274 00004AE4 7369676E656400          getlex_1 db "signed",0
  9275 00004AEB 756E7369676E656400      getlex_2 db "unsigned",0
  9276 00004AF4 766F696400              getlex_3 db "void",0
  9277 00004AF9 696E7400                getlex_4 db "int",0
  9278 00004AFD 73686F727400            getlex_5 db "short",0
  9279 00004B03 6C6F6E6700              getlex_6 db "long",0
  9280 00004B08 75696E74333200          getlex_7 db "uint32",0
  9281 00004B0F 696E746800              getlex_8 db "inth",0
  9282 00004B14 6368617200              getlex_9 db "char",0
  9283 00004B19 61736D00                getlex_10 db "asm",0
  9284 00004B1D 5F5F61736D00            getlex_11 db "__asm",0
  9285 00004B23 5F5F656D69745F5F00      getlex_12 db "__emit__",0
  9286 00004B2C 72657475726E00          getlex_13 db "return",0
  9287 00004B33 696600                  getlex_14 db "if",0
  9288 00004B36 6966636172727900        getlex_15 db "ifcarry",0
  9289 00004B3E 69667A65726F00          getlex_16 db "ifzero",0
  9290 00004B45 656C736500              getlex_17 db "else",0
  9291 00004B4A 7768696C6500            getlex_18 db "while",0
  9292 00004B50 646F00                  getlex_19 db "do",0
  9293 00004B53 676F746F00              getlex_20 db "goto",0
  9294 00004B58 646566696E6500          getlex_21 db "define",0
  9295 00004B5F 696E636C75646500        getlex_22 db "include",0
  9296 00004B67 496E70757420697465-     getlex_23 db "Input item not recognized",0
  9297 00004B70 6D206E6F7420726563-
  9298 00004B79 6F676E697A656400   
  9299                                  
  9300                                  convertdefine:  ; *** PROC ***
  9301                                  ;Function : convertdefine, Number of local variables: 2
  9302                                  ;   # type sign width line used address name   list of local variables
  9303                                  ;  200 var sign word   885 NULL bp-2    i
  9304                                  ;  201 var sign word   885 NULL bp-4    j;
  9305 00004B81 C8040000                 ENTER  4,0
  9306 00004B85 66B800000000             mov eax, 0
  9307 00004B8B 8946FE                   mov word [bp-2], ax
  9308                                  
  9309                                  ;-886   while (i < GTop) {  j=GName[i]; from_far(NA, j);
  9310                                  
  9311                                  @@convertdefine532:
  9312 00004B8E 8B46FE                   mov ax, [bp-2]
  9313 00004B91 3B06[5E00]               cmp ax, [GTop]
  9314 00004B95 0F8D6200                 jge @@convertdefine533
  9315 00004B99 8B5EFE                   mov bx, [bp-2]
  9316 00004B9C D1E3                     shl bx, 1
  9317 00004B9E 8B874B94                 mov ax, [GName + bx]
  9318 00004BA2 8946FC                   mov word [bp-4], ax
  9319 00004BA5 FF76FC                   push word [bp-4]
  9320 00004BA8 8D06[7200]               lea  ax, [NA]
  9321 00004BAC 50                       push ax
  9322 00004BAD E8D912                   call from_far
  9323 00004BB0 83C404                   add  sp, 4
  9324                                  
  9325                                  ;-887    if (eqstr(symbol, NA)) { if (GType[i]=='#') { lexvalL=GData[i];
  9326                                  
  9327 00004BB3 8D06[7200]               lea  ax, [NA]
  9328 00004BB7 50                       push ax
  9329 00004BB8 8D066E81                 lea  ax, [symbol]
  9330 00004BBC 50                       push ax
  9331 00004BBD E82017                   call eqstr
  9332 00004BC0 83C404                   add  sp, 4
  9333 00004BC3 08C0                     or  al, al
  9334 00004BC5 0F842D00                 je @@convertdefine534
  9335 00004BC9 8B5EFE                   mov bx, [bp-2]
  9336 00004BCC 8A87B782                 mov al, [GType + bx]
  9337 00004BD0 3C23                     cmp al, 35
  9338 00004BD2 0F852000                 jne @@convertdefine535
  9339 00004BD6 8B5EFE                   mov bx, [bp-2]
  9340 00004BD9 C1E302                   shl bx, 2
  9341 00004BDC 668B87EB8A               mov eax, [GData + bx]
  9342 00004BE1 66A3[4D00]               mov dword [lexvalL], eax
  9343                                  
  9344                                  ;-888      eax=lexvalL; _ lexvalL=eax;   return T_CONST; } }      i++; }
  9345                                  
  9346 00004BE5 66A1[4D00]               mov  eax, [lexvalL]
  9347 00004BE9 66A3[4D00]              mov [lexvalL], eax
  9348 00004BED 66B801010000             mov eax, 257
  9349 00004BF3 E90E00                   jmp @@retn
  9350                                  @@convertdefine535:
  9351                                  @@convertdefine534:
  9352 00004BF6 FF46FE                   inc  word [bp-2]
  9353                                  
  9354                                  ;-889    return 0;
  9355                                  
  9356 00004BF9 EB93                     jmp @@convertdefine532
  9357                                  @@convertdefine533:
  9358 00004BFB 66B800000000             mov eax, 0
  9359 00004C01 E90000                   jmp @@retn
  9360                                  
  9361                                  ;-890 }
  9362                                  
  9363                                  
  9364                                  ;-891 long getdigit(unsigned char c) { unsigned long L;
  9365                                  
  9366 00004C04 C9                      @@retn: LEAVE
  9367 00004C05 C3                       ret
  9368                                  ; ENDP
  9369                                  
  9370                                  getdigit:  ; *** PROC ***
  9371                                  
  9372                                  ;-892     _ lexvalL=0;    c-='0';
  9373                                  
  9374                                  ;Function : getdigit, Number of local variables: 2
  9375                                  ;   # type sign width line used address name   list of local variables
  9376                                  ;  200 var unsg byte   891 NULL bp+4    c
  9377                                  ;  201 var unsg dwrd   891 NULL bp-4    L;
  9378 00004C06 C8040000                 ENTER  4,0
  9379 00004C0A 66C706[4D00]000000-     mov dword [lexvalL], 0
  9380 00004C12 00                 
  9381 00004C13 806E0430                 sub  byte [bp+4], 48
  9382                                  
  9383                                  ;-893     eax=c;/* cast B2L */  _ lexvalL=eax;
  9384                                  
  9385 00004C17 660FB64604               movzx  eax, byte [bp+4]
  9386 00004C1C 66A3[4D00]              mov [lexvalL], eax
  9387                                  
  9388                                  ;-894     if (thechar=='x') thechar='X';
  9389                                  
  9390 00004C20 A0[4200]                 mov al, [thechar]
  9391 00004C23 3C78                     cmp al, 120
  9392 00004C25 0F850900                 jne @@getdigit536
  9393 00004C29 66B858000000             mov eax, 88
  9394 00004C2F A2[4200]                 mov byte [thechar], al
  9395                                  
  9396                                  ;-895     if (thechar=='X') { next();
  9397                                  
  9398                                  @@getdigit536:
  9399 00004C32 A0[4200]                 mov al, [thechar]
  9400 00004C35 3C58                     cmp al, 88
  9401 00004C37 0F855D00                 jne @@getdigit537
  9402 00004C3B E8F000                   call next
  9403                                  
  9404                                  ;-896       while(letter(thechar)) { c=next(); if(_ c>96) c-=39;
  9405                                  
  9406                                  @@getdigit538:
  9407 00004C3E A0[4200]                 mov al, byte [thechar]
  9408 00004C41 B400                     mov ah, 0
  9409 00004C43 50                       push ax
  9410 00004C44 E81E16                   call letter
  9411 00004C47 83C402                   add  sp, 2
  9412 00004C4A 08C0                     or  al, al
  9413 00004C4C 0F844500                 je @@getdigit539
  9414 00004C50 E8DB00                   call next
  9415 00004C53 884604                   mov byte [bp+4], al
  9416 00004C56 807E0460                cmp byte [bp+4], 96
  9417 00004C5A 0F8E0400                 jle @@getdigit540
  9418 00004C5E 806E0427                 sub  byte [bp+4], 39
  9419                                  
  9420                                  ;-897 	      if (_ c>64) c-=7; c-=48; lexvalL=lexvalL << 4; // * 16
  9421                                  
  9422                                  @@getdigit540:
  9423 00004C62 807E0440                cmp byte [bp+4], 64
  9424 00004C66 0F8E0400                 jle @@getdigit541
  9425 00004C6A 806E0407                 sub  byte [bp+4], 7
  9426                                  @@getdigit541:
  9427 00004C6E 806E0430                 sub  byte [bp+4], 48
  9428 00004C72 66A1[4D00]               mov eax, [lexvalL]
  9429 00004C76 66C1E004                 shl eax, 4
  9430 00004C7A 66A3[4D00]               mov dword [lexvalL], eax
  9431                                  
  9432                                  ;-898     eax=c;/* cast B2L */  _ L=eax;  lexvalL=lexvalL+L; } }
  9433                                  
  9434 00004C7E 660FB64604               movzx  eax, byte [bp+4]
  9435 00004C83 668946FC                mov [bp-4], eax
  9436 00004C87 66A1[4D00]               mov eax, [lexvalL]
  9437 00004C8B 660346FC                 add eax, [bp-4]
  9438 00004C8F 66A3[4D00]               mov dword [lexvalL], eax
  9439 00004C93 EBA9                     jmp @@getdigit538
  9440                                  @@getdigit539:
  9441                                  
  9442                                  ;-899     else
  9443                                  
  9444                                  
  9445                                  ;-900     { while(digit(thechar )) { c=next(); c-='0'; lexvalL=lexvalL*10;
  9446                                  
  9447 00004C95 E94400                   jmp @@getdigit542
  9448                                  @@getdigit537:
  9449                                  @@getdigit543:
  9450 00004C98 A0[4200]                 mov al, byte [thechar]
  9451 00004C9B B400                     mov ah, 0
  9452 00004C9D 50                       push ax
  9453 00004C9E E89115                   call digit
  9454 00004CA1 83C402                   add  sp, 2
  9455 00004CA4 08C0                     or  al, al
  9456 00004CA6 0F843200                 je @@getdigit544
  9457 00004CAA E88100                   call next
  9458 00004CAD 884604                   mov byte [bp+4], al
  9459 00004CB0 806E0430                 sub  byte [bp+4], 48
  9460 00004CB4 66A1[4D00]               mov eax, [lexvalL]
  9461 00004CB8 66BB0A000000             mov ebx, 10
  9462 00004CBE 66F7E3                   mul ebx
  9463 00004CC1 66A3[4D00]               mov dword [lexvalL], eax
  9464                                  
  9465                                  ;-901     eax=c;/* cast B2L */  _ L=eax;  lexvalL=lexvalL+L; } }
  9466                                  
  9467 00004CC5 660FB64604               movzx  eax, byte [bp+4]
  9468 00004CCA 668946FC                mov [bp-4], eax
  9469 00004CCE 66A1[4D00]               mov eax, [lexvalL]
  9470 00004CD2 660346FC                 add eax, [bp-4]
  9471 00004CD6 66A3[4D00]               mov dword [lexvalL], eax
  9472 00004CDA EBBC                     jmp @@getdigit543
  9473                                  @@getdigit544:
  9474                                  
  9475                                  ;-902     _ islong=0; if (_ lexvalL > 0xFFFF) islong++;
  9476                                  
  9477                                  @@getdigit542:
  9478 00004CDC C606[5100]00            mov byte [islong], 0
  9479 00004CE1 66813E[4D00]FFFF00-     cmp dword [lexvalL], 65535
  9480 00004CE9 00                 
  9481 00004CEA 0F8E0400                 jle @@getdigit545
  9482 00004CEE FE06[5100]               inc  byte [islong]
  9483                                  
  9484                                  ;-903 }
  9485                                  
  9486                                  @@getdigit545:
  9487                                  
  9488                                  ;-904 int getstring(int delim) {int c; char *p;  p=&symbol; c=next();
  9489 00004CF2 C9                       LEAVE
  9490 00004CF3 C3                       ret
  9491                                  ; ENDP
  9492                                  
  9493                                  getstring:  ; *** PROC ***
  9494                                  ;Function : getstring, Number of local variables: 3
  9495                                  ;   # type sign width line used address name   list of local variables
  9496                                  ;  200 var sign word   904 NULL bp+4    delim
  9497                                  ;  201 var sign word   904 NULL bp-2    c
  9498                                  ;  202 ptr sign byte   904 NULL bp-4    p;
  9499 00004CF4 C8040000                 ENTER  4,0
  9500 00004CF8 B86E81                   mov ax, symbol
  9501 00004CFB 8946FC                   mov word [bp-4], ax
  9502 00004CFE E82D00                   call next
  9503 00004D01 8946FE                   mov word [bp-2], ax
  9504                                  
  9505                                  ;-905   while (c != delim) {*p=c; p++; c=next(); } *p=0; }
  9506                                  
  9507                                  @@getstring546:
  9508 00004D04 8B46FE                   mov ax, [bp-2]
  9509 00004D07 3B4604                   cmp ax, [bp+4]
  9510 00004D0A 0F841300                 je  @@getstring547
  9511 00004D0E 8B46FE                   mov ax, [bp-2]
  9512 00004D11 8B5EFC                   mov  bx, [bp-4]
  9513 00004D14 8807                     mov  [bx], al
  9514 00004D16 FF46FC                   inc  word [bp-4]
  9515 00004D19 E81200                   call next
  9516 00004D1C 8946FE                   mov word [bp-2], ax
  9517 00004D1F EBE3                     jmp @@getstring546
  9518                                  @@getstring547:
  9519 00004D21 66B800000000             mov eax, 0
  9520 00004D27 8B5EFC                   mov  bx, [bp-4]
  9521 00004D2A 8807                     mov  [bx], al
  9522                                  
  9523                                  ;-906 
  9524                                  
  9525                                  
  9526                                  ;-907 int next() {char r; r = thechar; globC=r; thechar = fgets1(); return r; }
  9527 00004D2C C9                       LEAVE
  9528 00004D2D C3                       ret
  9529                                  ; ENDP
  9530                                  
  9531                                  next:  ; *** PROC ***
  9532                                  ;Function : next, Number of local variables: 1
  9533                                  ;   # type sign width line used address name   list of local variables
  9534                                  ;  200 var sign byte   907 NULL bp-2    r;
  9535 00004D2E C8020000                 ENTER  2,0
  9536 00004D32 A0[4200]                 mov al, [thechar]
  9537 00004D35 8846FE                   mov byte [bp-2], al
  9538 00004D38 8A46FE                   mov al, [bp-2]
  9539 00004D3B A2[3F00]                 mov byte [globC], al
  9540 00004D3E E85303                   call fgets1
  9541 00004D41 A2[4200]                 mov byte [thechar], al
  9542 00004D44 8A46FE                   mov al, [bp-2]
  9543 00004D47 E90000                   jmp @@retn
  9544                                  
  9545                                  ;-908 int istoken(int t) {if (token == t) { token=getlex(); return 1; } return 0;}
  9546                                  
  9547 00004D4A C9                      @@retn: LEAVE
  9548 00004D4B C3                       ret
  9549                                  ; ENDP
  9550                                  
  9551                                  istoken:  ; *** PROC ***
  9552                                  ;Function : istoken, Number of local variables: 1
  9553                                  ;   # type sign width line used address name   list of local variables
  9554                                  ;  200 var sign word   908 NULL bp+4    t;
  9555 00004D4C C8000000                 ENTER  0,0
  9556 00004D50 A1[3D00]                 mov ax, [token]
  9557 00004D53 3B4604                   cmp ax, [bp+4]
  9558 00004D56 0F850F00                 jne @@istoken548
  9559 00004D5A E84FF7                   call getlex
  9560 00004D5D A3[3D00]                 mov word [token], ax
  9561 00004D60 66B801000000             mov eax, 1
  9562 00004D66 E90900                   jmp @@retn
  9563                                  @@istoken548:
  9564 00004D69 66B800000000             mov eax, 0
  9565 00004D6F E90000                   jmp @@retn
  9566                                  
  9567                                  ;-909 int expect(int t) {if (istoken(t)==0) { *cloc=0; prs(co); listproc();
  9568                                  
  9569 00004D72 C9                      @@retn: LEAVE
  9570 00004D73 C3                       ret
  9571                                  ; ENDP
  9572                                  
  9573                                  expect:  ; *** PROC ***
  9574                                  ;Function : expect, Number of local variables: 1
  9575                                  ;   # type sign width line used address name   list of local variables
  9576                                  ;  200 var sign word   909 NULL bp+4    t;
  9577 00004D74 C8000000                 ENTER  0,0
  9578 00004D78 FF7604                   push word [bp+4]
  9579 00004D7B E8CEFF                   call istoken
  9580 00004D7E 83C402                   add  sp, 2
  9581 00004D81 83F800                   cmp ax, 0
  9582 00004D84 0F853500                 jne @@expect549
  9583 00004D88 66B800000000             mov eax, 0
  9584 00004D8E 8B1E[3000]               mov  bx, [cloc]
  9585 00004D92 8807                     mov  [bx], al
  9586 00004D94 8D063075                 lea  ax, [co]
  9587 00004D98 50                       push ax
  9588 00004D99 E8F600                   call prs
  9589 00004D9C 83C402                   add  sp, 2
  9590 00004D9F E8FC04                   call listproc
  9591                                  
  9592                                  ;-910   prs("\nExpected ASCII(dez): "); pint1(t); error1(" not found"); } }
  9593                                  
  9594 00004DA2 68[BF4D]                 push expect_0
  9595 00004DA5 E8EA00                   call prs
  9596 00004DA8 83C402                   add  sp, 2
  9597 00004DAB FF7604                   push word [bp+4]
  9598 00004DAE E8E701                   call pint1
  9599 00004DB1 83C402                   add  sp, 2
  9600 00004DB4 68[D74D]                 push expect_1
  9601 00004DB7 E8D903                   call error1
  9602 00004DBA 83C402                   add  sp, 2
  9603                                  @@expect549:
  9604                                  
  9605                                  ;-911 
  9606                                  
  9607                                  
  9608                                  ;-912 int eprc(char c)  {*cloc=c; cloc++; }
  9609 00004DBD C9                       LEAVE
  9610 00004DBE C3                       ret
  9611                                  ; ENDP
  9612 00004DBF 5C6E45787065637465-     expect_0 db "\nExpected ASCII(dez): ",0
  9613 00004DC8 642041534349492864-
  9614 00004DD1 657A293A2000       
  9615 00004DD7 206E6F7420666F756E-     expect_1 db " not found",0
  9616 00004DE0 6400               
  9617                                  
  9618                                  eprc:  ; *** PROC ***
  9619                                  ;Function : eprc, Number of local variables: 1
  9620                                  ;   # type sign width line used address name   list of local variables
  9621                                  ;  200 var sign byte   912 NULL bp+4    c;
  9622 00004DE2 C8000000                 ENTER  0,0
  9623 00004DE6 8A4604                   mov al, [bp+4]
  9624 00004DE9 8B1E[3000]               mov  bx, [cloc]
  9625 00004DED 8807                     mov  [bx], al
  9626 00004DEF FF06[3000]               inc  word [cloc]
  9627                                  
  9628                                  ;-913 int eprs(char *s) {char c;  while(*s) { c=*s; eprc(c); s++; } }
  9629 00004DF3 C9                       LEAVE
  9630 00004DF4 C3                       ret
  9631                                  ; ENDP
  9632                                  
  9633                                  eprs:  ; *** PROC ***
  9634                                  ;Function : eprs, Number of local variables: 2
  9635                                  ;   # type sign width line used address name   list of local variables
  9636                                  ;  200 ptr sign byte   913 NULL bp+4    s
  9637                                  ;  201 var sign byte   913 NULL bp-2    c;
  9638 00004DF5 C8020000                 ENTER  2,0
  9639                                  @@eprs550:
  9640 00004DF9 8B5E04                   mov bx, [bp+4]
  9641 00004DFC 8A07                     mov al, [bx]
  9642 00004DFE B400                     mov ah, 0
  9643 00004E00 08C0                     or  al, al
  9644 00004E02 0F841B00                 je @@eprs551
  9645 00004E06 8B5E04                   mov bx, [bp+4]
  9646 00004E09 8A07                     mov al, [bx]
  9647 00004E0B B400                     mov ah, 0
  9648 00004E0D 8846FE                   mov byte [bp-2], al
  9649 00004E10 8A46FE                   mov al, byte [bp-2]
  9650 00004E13 B400                     mov ah, 0
  9651 00004E15 50                       push ax
  9652 00004E16 E8C9FF                   call eprc
  9653 00004E19 83C402                   add  sp, 2
  9654 00004E1C FF4604                   inc  word [bp+4]
  9655 00004E1F EBD8                     jmp @@eprs550
  9656                                  @@eprs551:
  9657                                  
  9658                                  ;-914 int prc(unsigned char c) { if (LIST) { if (_ c==10) {ax=13; writetty(); }
  9659 00004E21 C9                       LEAVE
  9660 00004E22 C3                       ret
  9661                                  ; ENDP
  9662                                  
  9663                                  prc:  ; *** PROC ***
  9664                                  ;Function : prc, Number of local variables: 1
  9665                                  ;   # type sign width line used address name   list of local variables
  9666                                  ;  200 var unsg byte   914 NULL bp+4    c;
  9667 00004E23 C8000000                 ENTER  0,0
  9668 00004E27 A0[1700]                 mov al, [LIST]
  9669 00004E2A 08C0                     or  al, al
  9670 00004E2C 0F841400                 je @@prc552
  9671 00004E30 807E040A                cmp byte [bp+4], 10
  9672 00004E34 0F850600                 jne @@prc553
  9673 00004E38 B80D00                   mov  ax, 13
  9674 00004E3B E82312                   call writetty
  9675                                  
  9676                                  ;-915    al=c; writetty(); } fputcR(c, fdout); }
  9677                                  
  9678                                  @@prc553:
  9679 00004E3E 8A4604                   mov  al, [bp+4]
  9680 00004E41 E81D12                   call writetty
  9681                                  @@prc552:
  9682 00004E44 FF36[3600]               push word [fdout]
  9683 00004E48 8A4604                   mov al, byte [bp+4]
  9684 00004E4B B400                     mov ah, 0
  9685 00004E4D 50                       push ax
  9686 00004E4E E85D15                   call fputcR
  9687 00004E51 83C404                   add  sp, 4
  9688                                  
  9689                                  ;-916 int prscomment(unsigned char *s) {unsigned char c;
  9690 00004E54 C9                       LEAVE
  9691 00004E55 C3                       ret
  9692                                  ; ENDP
  9693                                  
  9694                                  prscomment:  ; *** PROC ***
  9695                                  
  9696                                  ;-917   while(*s){c=*s;prc(c);s++;} }
  9697                                  
  9698                                  ;Function : prscomment, Number of local variables: 2
  9699                                  ;   # type sign width line used address name   list of local variables
  9700                                  ;  200 ptr unsg byte   916 NULL bp+4    s
  9701                                  ;  201 var unsg byte   916 NULL bp-2    c;
  9702 00004E56 C8020000                 ENTER  2,0
  9703                                  @@prscomment554:
  9704 00004E5A 8B5E04                   mov bx, [bp+4]
  9705 00004E5D 8A07                     mov al, [bx]
  9706 00004E5F B400                     mov ah, 0
  9707 00004E61 08C0                     or  al, al
  9708 00004E63 0F841B00                 je @@prscomment555
  9709 00004E67 8B5E04                   mov bx, [bp+4]
  9710 00004E6A 8A07                     mov al, [bx]
  9711 00004E6C B400                     mov ah, 0
  9712 00004E6E 8846FE                   mov byte [bp-2], al
  9713 00004E71 8A46FE                   mov al, byte [bp-2]
  9714 00004E74 B400                     mov ah, 0
  9715 00004E76 50                       push ax
  9716 00004E77 E8A9FF                   call prc
  9717 00004E7A 83C402                   add  sp, 2
  9718 00004E7D FF4604                   inc  word [bp+4]
  9719 00004E80 EBD8                     jmp @@prscomment554
  9720                                  @@prscomment555:
  9721                                  
  9722                                  ;-918 int prnl() { prs("\n ");}
  9723 00004E82 C9                       LEAVE
  9724 00004E83 C3                       ret
  9725                                  ; ENDP
  9726                                  
  9727                                  prnl:  ; *** PROC ***
  9728 00004E84 68[8E4E]                 push prnl_0
  9729 00004E87 E80800                   call prs
  9730 00004E8A 83C402                   add  sp, 2
  9731                                  
  9732                                  ;-919 
  9733                                  
  9734                                  
  9735                                  ;-920 int prs(unsigned char *s) {unsigned char c; int com; com=0;
  9736                                  
  9737 00004E8D C3                       ret
  9738                                  ; ENDP
  9739 00004E8E 5C6E2000                prnl_0 db "\n ",0
  9740                                  
  9741                                  prs:  ; *** PROC ***
  9742                                  ;Function : prs, Number of local variables: 3
  9743                                  ;   # type sign width line used address name   list of local variables
  9744                                  ;  200 ptr unsg byte   920 NULL bp+4    s
  9745                                  ;  201 var unsg byte   920 NULL bp-2    c
  9746                                  ;  202 var sign word   920 NULL bp-4    com;
  9747 00004E92 C8040000                 ENTER  4,0
  9748 00004E96 66B800000000             mov eax, 0
  9749 00004E9C 8946FC                   mov word [bp-4], ax
  9750                                  
  9751                                  ;-921   while(*s) { c=*s; if (c==34) if (com) com=0; else com=1;
  9752                                  
  9753                                  @@prs556:
  9754 00004E9F 8B5E04                   mov bx, [bp+4]
  9755 00004EA2 8A07                     mov al, [bx]
  9756 00004EA4 B400                     mov ah, 0
  9757 00004EA6 08C0                     or  al, al
  9758 00004EA8 0F848700                 je @@prs557
  9759 00004EAC 8B5E04                   mov bx, [bp+4]
  9760 00004EAF 8A07                     mov al, [bx]
  9761 00004EB1 B400                     mov ah, 0
  9762 00004EB3 8846FE                   mov byte [bp-2], al
  9763 00004EB6 8A46FE                   mov al, [bp-2]
  9764 00004EB9 3C22                     cmp al, 34
  9765 00004EBB 0F851E00                 jne @@prs558
  9766 00004EBF 8B46FC                   mov ax, [bp-4]
  9767 00004EC2 08C0                     or  al, al
  9768 00004EC4 0F840C00                 je @@prs559
  9769 00004EC8 66B800000000             mov eax, 0
  9770 00004ECE 8946FC                   mov word [bp-4], ax
  9771 00004ED1 E90900                   jmp @@prs560
  9772                                  @@prs559:
  9773 00004ED4 66B801000000             mov eax, 1
  9774 00004EDA 8946FC                   mov word [bp-4], ax
  9775                                  
  9776                                  ;-922     if (c==92) { if (com==0) { s++; c=*s;
  9777                                  
  9778                                  @@prs560:
  9779                                  @@prs558:
  9780 00004EDD 8A46FE                   mov al, [bp-2]
  9781 00004EE0 3C5C                     cmp al, 92
  9782 00004EE2 0F853B00                 jne @@prs561
  9783 00004EE6 8B46FC                   mov ax, [bp-4]
  9784 00004EE9 83F800                   cmp ax, 0
  9785 00004EEC 0F853100                 jne @@prs562
  9786 00004EF0 FF4604                   inc  word [bp+4]
  9787 00004EF3 8B5E04                   mov bx, [bp+4]
  9788 00004EF6 8A07                     mov al, [bx]
  9789 00004EF8 B400                     mov ah, 0
  9790 00004EFA 8846FE                   mov byte [bp-2], al
  9791                                  
  9792                                  ;-923           if (c=='n') c=10; if (c=='t') c= 9; } } prc(c); s++;  } }
  9793                                  
  9794 00004EFD 8A46FE                   mov al, [bp-2]
  9795 00004F00 3C6E                     cmp al, 110
  9796 00004F02 0F850900                 jne @@prs563
  9797 00004F06 66B80A000000             mov eax, 10
  9798 00004F0C 8846FE                   mov byte [bp-2], al
  9799                                  @@prs563:
  9800 00004F0F 8A46FE                   mov al, [bp-2]
  9801 00004F12 3C74                     cmp al, 116
  9802 00004F14 0F850900                 jne @@prs564
  9803 00004F18 66B809000000             mov eax, 9
  9804 00004F1E 8846FE                   mov byte [bp-2], al
  9805                                  @@prs564:
  9806                                  @@prs562:
  9807                                  @@prs561:
  9808 00004F21 8A46FE                   mov al, byte [bp-2]
  9809 00004F24 B400                     mov ah, 0
  9810 00004F26 50                       push ax
  9811 00004F27 E8F9FE                   call prc
  9812 00004F2A 83C402                   add  sp, 2
  9813 00004F2D FF4604                   inc  word [bp+4]
  9814 00004F30 E96CFF                   jmp @@prs556
  9815                                  @@prs557:
  9816                                  
  9817                                  ;-924 int eprnum(int n){int e; if(n<0) { eprc('-'); n=mkneg(n); }
  9818 00004F33 C9                       LEAVE
  9819 00004F34 C3                       ret
  9820                                  ; ENDP
  9821                                  
  9822                                  eprnum:  ; *** PROC ***
  9823                                  ;Function : eprnum, Number of local variables: 2
  9824                                  ;   # type sign width line used address name   list of local variables
  9825                                  ;  200 var sign word   924 NULL bp+4    n
  9826                                  ;  201 var sign word   924 NULL bp-2    e;
  9827 00004F35 C8020000                 ENTER  2,0
  9828 00004F39 8B4604                   mov ax, [bp+4]
  9829 00004F3C 83F800                   cmp ax, 0
  9830 00004F3F 0F8D1400                 jge @@eprnum565
  9831 00004F43 6A2D                     push 45
  9832 00004F45 E89AFE                   call eprc
  9833 00004F48 83C402                   add  sp, 2
  9834 00004F4B FF7604                   push word [bp+4]
  9835 00004F4E E85F11                   call mkneg
  9836 00004F51 83C402                   add  sp, 2
  9837 00004F54 894604                   mov word [bp+4], ax
  9838                                  
  9839                                  ;-925   if (n >= 10) {e=n/10; eprnum(e);}  n=n%10; n=n+'0'; eprc(n); }
  9840                                  
  9841                                  @@eprnum565:
  9842 00004F57 8B4604                   mov ax, [bp+4]
  9843 00004F5A 83F80A                   cmp ax, 10 ;unsigned : 1
  9844 00004F5D 0F8C1500                 jl  @@eprnum566
  9845 00004F61 8B4604                   mov ax, [bp+4]
  9846 00004F64 BB0A00                   mov bx, 10
  9847 00004F67 99                       cwd
  9848 00004F68 F7FB                     idiv bx
  9849 00004F6A 8946FE                   mov word [bp-2], ax
  9850 00004F6D FF76FE                   push word [bp-2]
  9851 00004F70 E8C2FF                   call eprnum
  9852 00004F73 83C402                   add  sp, 2
  9853                                  @@eprnum566:
  9854 00004F76 8B4604                   mov ax, [bp+4]
  9855 00004F79 BB0A00                   mov bx, 10
  9856 00004F7C 99                       cwd
  9857 00004F7D F7FB                     idiv bx
  9858 00004F7F 89D0                     mov ax, dx
  9859 00004F81 894604                   mov word [bp+4], ax
  9860 00004F84 8B4604                   mov ax, [bp+4]
  9861 00004F87 83C030                   add ax, 48
  9862 00004F8A 894604                   mov word [bp+4], ax
  9863 00004F8D FF7604                   push word [bp+4]
  9864 00004F90 E84FFE                   call eprc
  9865 00004F93 83C402                   add  sp, 2
  9866                                  
  9867                                  ;-926 int pint1 (int n){int e; if(n<0) {  prc('-');  n=mkneg(n); }
  9868 00004F96 C9                       LEAVE
  9869 00004F97 C3                       ret
  9870                                  ; ENDP
  9871                                  
  9872                                  pint1:  ; *** PROC ***
  9873                                  ;Function : pint1, Number of local variables: 2
  9874                                  ;   # type sign width line used address name   list of local variables
  9875                                  ;  200 var sign word   926 NULL bp+4    n
  9876                                  ;  201 var sign word   926 NULL bp-2    e;
  9877 00004F98 C8020000                 ENTER  2,0
  9878 00004F9C 8B4604                   mov ax, [bp+4]
  9879 00004F9F 83F800                   cmp ax, 0
  9880 00004FA2 0F8D1400                 jge @@pint1567
  9881 00004FA6 6A2D                     push 45
  9882 00004FA8 E878FE                   call prc
  9883 00004FAB 83C402                   add  sp, 2
  9884 00004FAE FF7604                   push word [bp+4]
  9885 00004FB1 E8FC10                   call mkneg
  9886 00004FB4 83C402                   add  sp, 2
  9887 00004FB7 894604                   mov word [bp+4], ax
  9888                                  
  9889                                  ;-927   if (n >= 10) {e=n/10;  pint1(e);}  n=n%10; n=n+'0'; prc(n); }
  9890                                  
  9891                                  @@pint1567:
  9892 00004FBA 8B4604                   mov ax, [bp+4]
  9893 00004FBD 83F80A                   cmp ax, 10 ;unsigned : 1
  9894 00004FC0 0F8C1500                 jl  @@pint1568
  9895 00004FC4 8B4604                   mov ax, [bp+4]
  9896 00004FC7 BB0A00                   mov bx, 10
  9897 00004FCA 99                       cwd
  9898 00004FCB F7FB                     idiv bx
  9899 00004FCD 8946FE                   mov word [bp-2], ax
  9900 00004FD0 FF76FE                   push word [bp-2]
  9901 00004FD3 E8C2FF                   call pint1
  9902 00004FD6 83C402                   add  sp, 2
  9903                                  @@pint1568:
  9904 00004FD9 8B4604                   mov ax, [bp+4]
  9905 00004FDC BB0A00                   mov bx, 10
  9906 00004FDF 99                       cwd
  9907 00004FE0 F7FB                     idiv bx
  9908 00004FE2 89D0                     mov ax, dx
  9909 00004FE4 894604                   mov word [bp+4], ax
  9910 00004FE7 8B4604                   mov ax, [bp+4]
  9911 00004FEA 83C030                   add ax, 48
  9912 00004FED 894604                   mov word [bp+4], ax
  9913 00004FF0 FF7604                   push word [bp+4]
  9914 00004FF3 E82DFE                   call prc
  9915 00004FF6 83C402                   add  sp, 2
  9916                                  
  9917                                  ;-928 int prunsign1(unsigned int n) { unsigned int e;
  9918 00004FF9 C9                       LEAVE
  9919 00004FFA C3                       ret
  9920                                  ; ENDP
  9921                                  
  9922                                  prunsign1:  ; *** PROC ***
  9923                                  
  9924                                  ;-929   if ( _ n >= 10) {  e=n/10; /*DIV*/ prunsign1(e); }
  9925                                  
  9926                                  ;Function : prunsign1, Number of local variables: 2
  9927                                  ;   # type sign width line used address name   list of local variables
  9928                                  ;  200 var unsg word   928 NULL bp+4    n
  9929                                  ;  201 var unsg word   928 NULL bp-2    e;
  9930 00004FFB C8020000                 ENTER  2,0
  9931 00004FFF 837E040A                cmp word [bp+4], 10 ;unsigned : 0
  9932 00005003 0F821700                 jb  @@prunsign1569
  9933 00005007 8B4604                   mov ax, [bp+4]
  9934 0000500A BB0A00                   mov bx, 10
  9935 0000500D BA0000                   mov dx, 0
  9936 00005010 F7F3                     div bx
  9937 00005012 8946FE                   mov word [bp-2], ax
  9938 00005015 FF76FE                   push word [bp-2]
  9939 00005018 E8E0FF                   call prunsign1
  9940 0000501B 83C402                   add  sp, 2
  9941                                  
  9942                                  ;-930     n = n % 10; /*unsigned mod*/   n += '0'; prc(n); }
  9943                                  
  9944                                  @@prunsign1569:
  9945 0000501E 8B4604                   mov ax, [bp+4]
  9946 00005021 BB0A00                   mov bx, 10
  9947 00005024 BA0000                   mov dx, 0
  9948 00005027 F7F3                     div bx
  9949 00005029 89D0                     mov ax, dx
  9950 0000502B 894604                   mov word [bp+4], ax
  9951 0000502E 83460430                 add  word [bp+4], 48
  9952 00005032 FF7604                   push word [bp+4]
  9953 00005035 E8EBFD                   call prc
  9954 00005038 83C402                   add  sp, 2
  9955                                  
  9956                                  ;-931 int printint51(unsigned int j)  {
  9957 0000503B C9                       LEAVE
  9958 0000503C C3                       ret
  9959                                  ; ENDP
  9960                                  
  9961                                  printint51:  ; *** PROC ***
  9962                                  
  9963                                  ;-932   if (j<10000) prc(32); if (j<1000) prc(32);  if (j<100) prc(32);
  9964                                  
  9965                                  ;Function : printint51, Number of local variables: 1
  9966                                  ;   # type sign width line used address name   list of local variables
  9967                                  ;  200 var unsg word   931 NULL bp+4    j;
  9968 0000503D C8000000                 ENTER  0,0
  9969 00005041 8B4604                   mov ax, [bp+4]
  9970 00005044 3D1027                   cmp ax, 10000
  9971 00005047 0F8D0800                 jge @@printint51570
  9972 0000504B 6A20                     push 32
  9973 0000504D E8D3FD                   call prc
  9974 00005050 83C402                   add  sp, 2
  9975                                  @@printint51570:
  9976 00005053 8B4604                   mov ax, [bp+4]
  9977 00005056 3DE803                   cmp ax, 1000
  9978 00005059 0F8D0800                 jge @@printint51571
  9979 0000505D 6A20                     push 32
  9980 0000505F E8C1FD                   call prc
  9981 00005062 83C402                   add  sp, 2
  9982                                  @@printint51571:
  9983 00005065 8B4604                   mov ax, [bp+4]
  9984 00005068 83F864                   cmp ax, 100
  9985 0000506B 0F8D0800                 jge @@printint51572
  9986 0000506F 6A20                     push 32
  9987 00005071 E8AFFD                   call prc
  9988 00005074 83C402                   add  sp, 2
  9989                                  
  9990                                  ;-933    if (j<10) prc(32);  prunsign1(j); }
  9991                                  
  9992                                  @@printint51572:
  9993 00005077 8B4604                   mov ax, [bp+4]
  9994 0000507A 83F80A                   cmp ax, 10
  9995 0000507D 0F8D0800                 jge @@printint51573
  9996 00005081 6A20                     push 32
  9997 00005083 E89DFD                   call prc
  9998 00005086 83C402                   add  sp, 2
  9999                                  @@printint51573:
 10000 00005089 FF7604                   push word [bp+4]
 10001 0000508C E86CFF                   call prunsign1
 10002 0000508F 83C402                   add  sp, 2
 10003                                  
 10004                                  ;-934 
 10005                                  
 10006                                  
 10007                                  ;-935 int fgets1() { char c; c=*fgetsp;
 10008 00005092 C9                       LEAVE
 10009 00005093 C3                       ret
 10010                                  ; ENDP
 10011                                  
 10012                                  fgets1:  ; *** PROC ***
 10013                                  ;Function : fgets1, Number of local variables: 1
 10014                                  ;   # type sign width line used address name   list of local variables
 10015                                  ;  200 var sign byte   935 NULL bp-2    c;
 10016 00005094 C8020000                 ENTER  2,0
 10017 00005098 8B1E[B600]               mov bx, [fgetsp]
 10018 0000509C 8A07                     mov al, [bx]
 10019 0000509E B400                     mov ah, 0
 10020 000050A0 8846FE                   mov byte [bp-2], al
 10021                                  
 10022                                  ;-936   if (c==0) { printinputline(); if (DOS_NoBytes == 0) return 0;
 10023                                  
 10024 000050A3 8A46FE                   mov al, [bp-2]
 10025 000050A6 3C00                     cmp al, 0
 10026 000050A8 0F853000                 jne @@fgets1574
 10027 000050AC E83D00                   call printinputline
 10028 000050AF A1[C400]                 mov ax, [DOS_NoBytes]
 10029 000050B2 83F800                   cmp ax, 0
 10030 000050B5 0F850900                 jne @@fgets1575
 10031 000050B9 66B800000000             mov eax, 0
 10032 000050BF E92800                   jmp @@retn
 10033                                  
 10034                                  ;-937     fgetsp=&fgetsdest; c=*fgetsp; spalte=0; }
 10035                                  
 10036                                  @@fgets1575:
 10037 000050C2 B847B6                   mov ax, fgetsdest
 10038 000050C5 A3[B600]                 mov word [fgetsp], ax
 10039 000050C8 8B1E[B600]               mov bx, [fgetsp]
 10040 000050CC 8A07                     mov al, [bx]
 10041 000050CE B400                     mov ah, 0
 10042 000050D0 8846FE                   mov byte [bp-2], al
 10043 000050D3 66B800000000             mov eax, 0
 10044 000050D9 A3[4000]                 mov word [spalte], ax
 10045                                  
 10046                                  ;-938   fgetsp++; spalte++;  return c; }
 10047                                  
 10048                                  @@fgets1574:
 10049 000050DC FF06[B600]               inc  word [fgetsp]
 10050 000050E0 FF06[4000]               inc  word [spalte]
 10051 000050E4 8A46FE                   mov al, [bp-2]
 10052 000050E7 E90000                   jmp @@retn
 10053                                  
 10054                                  ;-939 int printinputline() { fgetsp=&fgetsdest;
 10055                                  
 10056 000050EA C9                      @@retn: LEAVE
 10057 000050EB C3                       ret
 10058                                  ; ENDP
 10059                                  
 10060                                  printinputline:  ; *** PROC ***
 10061 000050EC B847B6                   mov ax, fgetsdest
 10062 000050EF A3[B600]                 mov word [fgetsp], ax
 10063                                  
 10064                                  ;-940   do {DOS_NoBytes=readR(&DOS_ByteRead, fdin);
 10065                                  
 10066                                  @@printinputline576:
 10067 000050F2 FF36[3400]               push word [fdin]
 10068 000050F6 8D06[C600]               lea  ax, [DOS_ByteRead]
 10069 000050FA 50                       push ax
 10070 000050FB E88612                   call readR
 10071 000050FE 83C404                   add  sp, 4
 10072 00005101 A3[C400]                 mov word [DOS_NoBytes], ax
 10073                                  
 10074                                  ;-941   if (DOS_NoBytes == 0) return; 
 10075                                  
 10076 00005104 A1[C400]                 mov ax, [DOS_NoBytes]
 10077 00005107 83F800                   cmp ax, 0
 10078 0000510A 0F850300                 jne @@printinputline577
 10079 0000510E E95700                   jmp @@retn
 10080                                  
 10081                                  ;-942     *fgetsp=DOS_ByteRead; fgetsp++;} 
 10082                                  
 10083                                  @@printinputline577:
 10084 00005111 A0[C600]                 mov al, [DOS_ByteRead]
 10085 00005114 8B1E[B600]               mov  bx, [fgetsp]
 10086 00005118 8807                     mov  [bx], al
 10087 0000511A FF06[B600]               inc  word [fgetsp]
 10088                                  
 10089                                  ;-943   while (DOS_ByteRead != 10); *fgetsp=0;
 10090                                  
 10091 0000511E A0[C600]                 mov al, [DOS_ByteRead]
 10092 00005121 3C0A                     cmp al, 10
 10093 00005123 0F840200                 je  @@printinputline578
 10094 00005127 EBC9                     jmp @@printinputline576
 10095                                  @@printinputline578:
 10096 00005129 66B800000000             mov eax, 0
 10097 0000512F 8B1E[B600]               mov  bx, [fgetsp]
 10098 00005133 8807                     mov  [bx], al
 10099                                  
 10100                                  ;-944     if (fdout) { prs("\n\n;-"); prunsign1(lineno); prc(' '); lineno++;
 10101                                  
 10102 00005135 A1[3600]                 mov ax, [fdout]
 10103 00005138 08C0                     or  al, al
 10104 0000513A 0F842A00                 je @@printinputline579
 10105 0000513E 68[6951]                 push printinputline_0
 10106 00005141 E84EFD                   call prs
 10107 00005144 83C402                   add  sp, 2
 10108 00005147 FF36[BA00]               push word [lineno]
 10109 0000514B E8ADFE                   call prunsign1
 10110 0000514E 83C402                   add  sp, 2
 10111 00005151 6A20                     push 32
 10112 00005153 E8CDFC                   call prc
 10113 00005156 83C402                   add  sp, 2
 10114 00005159 FF06[BA00]               inc  word [lineno]
 10115                                  
 10116                                  ;-945       prscomment(&fgetsdest);}
 10117                                  
 10118 0000515D 8D0647B6                 lea  ax, [fgetsdest]
 10119 00005161 50                       push ax
 10120 00005162 E8F1FC                   call prscomment
 10121 00005165 83C402                   add  sp, 2
 10122                                  
 10123                                  ;-946 }
 10124                                  
 10125                                  @@printinputline579:
 10126                                  
 10127                                  ;-947 int end1(int n) {fcloseR(fdin); fcloseR(fdout); exitR(n); }
 10128                                  
 10129                                  @@retn:
 10130 00005168 C3                       ret
 10131                                  ; ENDP
 10132 00005169 5C6E5C6E3B2D00          printinputline_0 db "\n\n;-",0
 10133                                  
 10134                                  end1:  ; *** PROC ***
 10135                                  ;Function : end1, Number of local variables: 1
 10136                                  ;   # type sign width line used address name   list of local variables
 10137                                  ;  200 var sign word   947 NULL bp+4    n;
 10138 00005170 C8000000                 ENTER  0,0
 10139 00005174 FF36[3400]               push word [fdin]
 10140 00005178 E84812                   call fcloseR
 10141 0000517B 83C402                   add  sp, 2
 10142 0000517E FF36[3600]               push word [fdout]
 10143 00005182 E83E12                   call fcloseR
 10144 00005185 83C402                   add  sp, 2
 10145 00005188 FF7604                   push word [bp+4]
 10146 0000518B E84312                   call exitR
 10147 0000518E 83C402                   add  sp, 2
 10148                                  
 10149                                  ;-948 int error1(char *s) { LIST=1;
 10150 00005191 C9                       LEAVE
 10151 00005192 C3                       ret
 10152                                  ; ENDP
 10153                                  
 10154                                  error1:  ; *** PROC ***
 10155                                  ;Function : error1, Number of local variables: 1
 10156                                  ;   # type sign width line used address name   list of local variables
 10157                                  ;  200 ptr sign byte   948 NULL bp+4    s;
 10158 00005193 C8000000                 ENTER  0,0
 10159 00005197 66B801000000             mov eax, 1
 10160 0000519D A2[1700]                 mov byte [LIST], al
 10161                                  
 10162                                  ;-949   lineno--;
 10163                                  
 10164 000051A0 FF0E[BA00]               dec  word [lineno]
 10165                                  
 10166                                  ;-950   prnl(); prscomment(&fgetsdest);
 10167                                  
 10168 000051A4 E8DDFC                   call prnl
 10169 000051A7 8D0647B6                 lea  ax, [fgetsdest]
 10170 000051AB 50                       push ax
 10171 000051AC E8A7FC                   call prscomment
 10172 000051AF 83C402                   add  sp, 2
 10173                                  
 10174                                  ;-951   prs(";Line: "); prunsign1(lineno);
 10175                                  
 10176 000051B2 68[4552]                 push error1_0
 10177 000051B5 E8DAFC                   call prs
 10178 000051B8 83C402                   add  sp, 2
 10179 000051BB FF36[BA00]               push word [lineno]
 10180 000051BF E839FE                   call prunsign1
 10181 000051C2 83C402                   add  sp, 2
 10182                                  
 10183                                  ;-952   prs(" ************** ERROR: "); prs(s);
 10184                                  
 10185 000051C5 68[4D52]                 push error1_1
 10186 000051C8 E8C7FC                   call prs
 10187 000051CB 83C402                   add  sp, 2
 10188 000051CE FF7604                   push word [bp+4]
 10189 000051D1 E8BEFC                   call prs
 10190 000051D4 83C402                   add  sp, 2
 10191                                  
 10192                                  ;-953   prs("  in column: "); pint1(spalte);
 10193                                  
 10194 000051D7 68[6552]                 push error1_2
 10195 000051DA E8B5FC                   call prs
 10196 000051DD 83C402                   add  sp, 2
 10197 000051E0 FF36[4000]               push word [spalte]
 10198 000051E4 E8B1FD                   call pint1
 10199 000051E7 83C402                   add  sp, 2
 10200                                  
 10201                                  ;-954   prs("\nToken: "); prunsign1(token); prs(", globC: "); prc(globC);
 10202                                  
 10203 000051EA 68[7352]                 push error1_3
 10204 000051ED E8A2FC                   call prs
 10205 000051F0 83C402                   add  sp, 2
 10206 000051F3 FF36[3D00]               push word [token]
 10207 000051F7 E801FE                   call prunsign1
 10208 000051FA 83C402                   add  sp, 2
 10209 000051FD 68[7D52]                 push error1_4
 10210 00005200 E88FFC                   call prs
 10211 00005203 83C402                   add  sp, 2
 10212 00005206 A0[3F00]                 mov al, byte [globC]
 10213 00005209 B400                     mov ah, 0
 10214 0000520B 50                       push ax
 10215 0000520C E814FC                   call prc
 10216 0000520F 83C402                   add  sp, 2
 10217                                  
 10218                                  ;-955   prs(", thechar: "); pint1(thechar); prs(", symbol: "); prs(symbol);
 10219                                  
 10220 00005212 68[8752]                 push error1_5
 10221 00005215 E87AFC                   call prs
 10222 00005218 83C402                   add  sp, 2
 10223 0000521B A0[4200]                 mov al, byte [thechar]
 10224 0000521E B400                     mov ah, 0
 10225 00005220 50                       push ax
 10226 00005221 E874FD                   call pint1
 10227 00005224 83C402                   add  sp, 2
 10228 00005227 68[9352]                 push error1_6
 10229 0000522A E865FC                   call prs
 10230 0000522D 83C402                   add  sp, 2
 10231 00005230 8D066E81                 lea  ax, [symbol]
 10232 00005234 50                       push ax
 10233 00005235 E85AFC                   call prs
 10234 00005238 83C402                   add  sp, 2
 10235                                  
 10236                                  ;-956   end1(1); }
 10237                                  
 10238 0000523B 6A01                     push 1
 10239 0000523D E830FF                   call end1
 10240 00005240 83C402                   add  sp, 2
 10241                                  
 10242                                  ;-957 int listproc() {int i; 
 10243 00005243 C9                       LEAVE
 10244 00005244 C3                       ret
 10245                                  ; ENDP
 10246 00005245 3B4C696E653A2000        error1_0 db ";Line: ",0
 10247 0000524D 202A2A2A2A2A2A2A2A-     error1_1 db " ************** ERROR: ",0
 10248 00005256 2A2A2A2A2A2A204552-
 10249 0000525F 524F523A2000       
 10250 00005265 2020696E20636F6C75-     error1_2 db "  in column: ",0
 10251 0000526E 6D6E3A2000         
 10252 00005273 5C6E546F6B656E3A20-     error1_3 db "\nToken: ",0
 10253 0000527C 00                 
 10254 0000527D 2C20676C6F62433A20-     error1_4 db ", globC: ",0
 10255 00005286 00                 
 10256 00005287 2C2074686563686172-     error1_5 db ", thechar: ",0
 10257 00005290 3A2000             
 10258 00005293 2C2073796D626F6C3A-     error1_6 db ", symbol: ",0
 10259 0000529C 2000               
 10260                                  
 10261                                  listproc:  ; *** PROC ***
 10262                                  
 10263                                  ;-958   if (LTop > LSTART) {
 10264                                  
 10265                                  ;Function : listproc, Number of local variables: 1
 10266                                  ;   # type sign width line used address name   list of local variables
 10267                                  ;  200 var sign word   957 NULL bp-2    i;
 10268 0000529E C8020000                 ENTER  2,0
 10269 000052A2 A1[6000]                 mov ax, [LTop]
 10270 000052A5 3DC800                   cmp ax, 200
 10271 000052A8 0F8E5A00                 jle @@listproc580
 10272                                  
 10273                                  ;-959   prs("\n;Function : "); prs(fname);
 10274                                  
 10275 000052AC 68[0853]                 push listproc_0
 10276 000052AF E8E0FB                   call prs
 10277 000052B2 83C402                   add  sp, 2
 10278 000052B5 8D06EE81                 lea  ax, [fname]
 10279 000052B9 50                       push ax
 10280 000052BA E8D5FB                   call prs
 10281 000052BD 83C402                   add  sp, 2
 10282                                  
 10283                                  ;-960   prs(", Number of local variables: "); i=LTop - LSTART; prunsign1(i);
 10284                                  
 10285 000052C0 68[1753]                 push listproc_1
 10286 000052C3 E8CCFB                   call prs
 10287 000052C6 83C402                   add  sp, 2
 10288 000052C9 A1[6000]                 mov ax, [LTop]
 10289 000052CC 2DC800                   sub ax, 200
 10290 000052CF 8946FE                   mov word [bp-2], ax
 10291 000052D2 FF76FE                   push word [bp-2]
 10292 000052D5 E823FD                   call prunsign1
 10293 000052D8 83C402                   add  sp, 2
 10294                                  
 10295                                  ;-961   prs("\n;   # type sign width line used address name   list of local variables");
 10296                                  
 10297 000052DB 68[3553]                 push listproc_2
 10298 000052DE E8B1FB                   call prs
 10299 000052E1 83C402                   add  sp, 2
 10300                                  
 10301                                  ;-962     i=LSTART; 
 10302                                  
 10303 000052E4 66B8C8000000             mov eax, 200
 10304 000052EA 8946FE                   mov word [bp-2], ax
 10305                                  
 10306                                  ;-963     while (i < LTop) { listvar(i); i++; } } 
 10307                                  
 10308                                  @@listproc581:
 10309 000052ED 8B46FE                   mov ax, [bp-2]
 10310 000052F0 3B06[6000]               cmp ax, [LTop]
 10311 000052F4 0F8D0E00                 jge @@listproc582
 10312 000052F8 FF76FE                   push word [bp-2]
 10313 000052FB E88000                   call listvar
 10314 000052FE 83C402                   add  sp, 2
 10315 00005301 FF46FE                   inc  word [bp-2]
 10316 00005304 EBE7                     jmp @@listproc581
 10317                                  @@listproc582:
 10318                                  
 10319                                  ;-964 }
 10320                                  
 10321                                  @@listproc580:
 10322                                  
 10323                                  ;-965 int listvar(unsigned int i) {unsigned int j; char c;  long L;
 10324 00005306 C9                       LEAVE
 10325 00005307 C3                       ret
 10326                                  ; ENDP
 10327 00005308 5C6E3B46756E637469-     listproc_0 db "\n;Function : ",0
 10328 00005311 6F6E203A2000       
 10329 00005317 2C204E756D62657220-     listproc_1 db ", Number of local variables: ",0
 10330 00005320 6F66206C6F63616C20-
 10331 00005329 7661726961626C6573-
 10332 00005332 3A2000             
 10333 00005335 5C6E3B202020232074-     listproc_2 db "\n;   # type sign width line used address name   list of local variables",0
 10334 0000533E 797065207369676E20-
 10335 00005347 7769647468206C696E-
 10336 00005350 652075736564206164-
 10337 00005359 6472657373206E616D-
 10338 00005362 652020206C69737420-
 10339 0000536B 6F66206C6F63616C20-
 10340 00005374 7661726961626C6573-
 10341 0000537D 00                 
 10342                                  
 10343                                  listvar:  ; *** PROC ***
 10344                                  
 10345                                  ;-966   prs("\n;"); printint51(i); prc(32);
 10346                                  
 10347                                  ;Function : listvar, Number of local variables: 4
 10348                                  ;   # type sign width line used address name   list of local variables
 10349                                  ;  200 var unsg word   965 NULL bp+4    i
 10350                                  ;  201 var unsg word   965 NULL bp-2    j
 10351                                  ;  202 var sign byte   965 NULL bp-4    c
 10352                                  ;  203 var sign dwrd   965 NULL bp-8    L;
 10353 0000537E C8080000                 ENTER  8,0
 10354 00005382 68[B255]                 push listvar_0
 10355 00005385 E80AFB                   call prs
 10356 00005388 83C402                   add  sp, 2
 10357 0000538B FF7604                   push word [bp+4]
 10358 0000538E E8ACFC                   call printint51
 10359 00005391 83C402                   add  sp, 2
 10360 00005394 6A20                     push 32
 10361 00005396 E88AFA                   call prc
 10362 00005399 83C402                   add  sp, 2
 10363                                  
 10364                                  ;-967   c=GType [i]; if(c=='V')prs("var ");   if(c=='*')prs("ptr ");
 10365                                  
 10366 0000539C 8B5E04                   mov bx, [bp+4]
 10367 0000539F 8A87B782                 mov al, [GType + bx]
 10368 000053A3 8846FC                   mov byte [bp-4], al
 10369 000053A6 8A46FC                   mov al, [bp-4]
 10370 000053A9 3C56                     cmp al, 86
 10371 000053AB 0F850900                 jne @@listvar583
 10372 000053AF 68[B655]                 push listvar_1
 10373 000053B2 E8DDFA                   call prs
 10374 000053B5 83C402                   add  sp, 2
 10375                                  @@listvar583:
 10376 000053B8 8A46FC                   mov al, [bp-4]
 10377 000053BB 3C2A                     cmp al, 42
 10378 000053BD 0F850900                 jne @@listvar584
 10379 000053C1 68[BB55]                 push listvar_2
 10380 000053C4 E8CBFA                   call prs
 10381 000053C7 83C402                   add  sp, 2
 10382                                  
 10383                                  ;-968                if(c=='&')prs("arr ");   if(c=='#')prs("def ");
 10384                                  
 10385                                  @@listvar584:
 10386 000053CA 8A46FC                   mov al, [bp-4]
 10387 000053CD 3C26                     cmp al, 38
 10388 000053CF 0F850900                 jne @@listvar585
 10389 000053D3 68[C055]                 push listvar_3
 10390 000053D6 E8B9FA                   call prs
 10391 000053D9 83C402                   add  sp, 2
 10392                                  @@listvar585:
 10393 000053DC 8A46FC                   mov al, [bp-4]
 10394 000053DF 3C23                     cmp al, 35
 10395 000053E1 0F850900                 jne @@listvar586
 10396 000053E5 68[C555]                 push listvar_4
 10397 000053E8 E8A7FA                   call prs
 10398 000053EB 83C402                   add  sp, 2
 10399                                  
 10400                                  ;-969   c=GSign [i]; if(c=='S')prs("sign ");  if(c=='U')prs("unsg ");
 10401                                  
 10402                                  @@listvar586:
 10403 000053EE 8B5E04                   mov bx, [bp+4]
 10404 000053F1 8A87E383                 mov al, [GSign + bx]
 10405 000053F5 8846FC                   mov byte [bp-4], al
 10406 000053F8 8A46FC                   mov al, [bp-4]
 10407 000053FB 3C53                     cmp al, 83
 10408 000053FD 0F850900                 jne @@listvar587
 10409 00005401 68[CA55]                 push listvar_5
 10410 00005404 E88BFA                   call prs
 10411 00005407 83C402                   add  sp, 2
 10412                                  @@listvar587:
 10413 0000540A 8A46FC                   mov al, [bp-4]
 10414 0000540D 3C55                     cmp al, 85
 10415 0000540F 0F850900                 jne @@listvar588
 10416 00005413 68[D055]                 push listvar_6
 10417 00005416 E879FA                   call prs
 10418 00005419 83C402                   add  sp, 2
 10419                                  
 10420                                  ;-970   c=GWidth[i]; if(c==  1)prs("byte " ); if(c==  2)prs("word " );
 10421                                  
 10422                                  @@listvar588:
 10423 0000541C 8B5E04                   mov bx, [bp+4]
 10424 0000541F 8A870F85                 mov al, [GWidth + bx]
 10425 00005423 8846FC                   mov byte [bp-4], al
 10426 00005426 8A46FC                   mov al, [bp-4]
 10427 00005429 3C01                     cmp al, 1
 10428 0000542B 0F850900                 jne @@listvar589
 10429 0000542F 68[D655]                 push listvar_7
 10430 00005432 E85DFA                   call prs
 10431 00005435 83C402                   add  sp, 2
 10432                                  @@listvar589:
 10433 00005438 8A46FC                   mov al, [bp-4]
 10434 0000543B 3C02                     cmp al, 2
 10435 0000543D 0F850900                 jne @@listvar590
 10436 00005441 68[DC55]                 push listvar_8
 10437 00005444 E84BFA                   call prs
 10438 00005447 83C402                   add  sp, 2
 10439                                  
 10440                                  ;-971                if(c==  4)prs("dwrd " );
 10441                                  
 10442                                  @@listvar590:
 10443 0000544A 8A46FC                   mov al, [bp-4]
 10444 0000544D 3C04                     cmp al, 4
 10445 0000544F 0F850900                 jne @@listvar591
 10446 00005453 68[E255]                 push listvar_9
 10447 00005456 E839FA                   call prs
 10448 00005459 83C402                   add  sp, 2
 10449                                  
 10450                                  ;-972   j=GLino[i]; printint51(j);
 10451                                  
 10452                                  @@listvar591:
 10453 0000545C 8B5E04                   mov bx, [bp+4]
 10454 0000545F D1E3                     shl bx, 1
 10455 00005461 8B873B86                 mov ax, [GLino + bx]
 10456 00005465 8946FE                   mov word [bp-2], ax
 10457 00005468 FF76FE                   push word [bp-2]
 10458 0000546B E8CFFB                   call printint51
 10459 0000546E 83C402                   add  sp, 2
 10460                                  
 10461                                  ;-973   j=GUsed[i]; if (j) printint51(j);
 10462                                  
 10463 00005471 8B5E04                   mov bx, [bp+4]
 10464 00005474 D1E3                     shl bx, 1
 10465 00005476 8B879388                 mov ax, [GUsed + bx]
 10466 0000547A 8946FE                   mov word [bp-2], ax
 10467 0000547D 8B46FE                   mov ax, [bp-2]
 10468 00005480 08C0                     or  al, al
 10469 00005482 0F840C00                 je @@listvar592
 10470 00005486 FF76FE                   push word [bp-2]
 10471 00005489 E8B1FB                   call printint51
 10472 0000548C 83C402                   add  sp, 2
 10473                                  
 10474                                  ;-974   else {if(GType[i]=='#') prs("    -"); else prs(" NULL");}
 10475                                  
 10476 0000548F E92200                   jmp @@listvar593
 10477                                  @@listvar592:
 10478 00005492 8B5E04                   mov bx, [bp+4]
 10479 00005495 8A87B782                 mov al, [GType + bx]
 10480 00005499 3C23                     cmp al, 35
 10481 0000549B 0F850C00                 jne @@listvar594
 10482 0000549F 68[E855]                 push listvar_10
 10483 000054A2 E8EDF9                   call prs
 10484 000054A5 83C402                   add  sp, 2
 10485 000054A8 E90900                   jmp @@listvar595
 10486                                  @@listvar594:
 10487 000054AB 68[EE55]                 push listvar_11
 10488 000054AE E8E1F9                   call prs
 10489 000054B1 83C402                   add  sp, 2
 10490                                  @@listvar595:
 10491                                  
 10492                                  ;-975   prc(32);  L=GAdr[i];
 10493                                  
 10494                                  @@listvar593:
 10495 000054B4 6A20                     push 32
 10496 000054B6 E86AF9                   call prc
 10497 000054B9 83C402                   add  sp, 2
 10498 000054BC 8B5E04                   mov bx, [bp+4]
 10499 000054BF C1E302                   shl bx, 2
 10500 000054C2 668B879B8F               mov eax, [GAdr + bx]
 10501 000054C7 668946F8                 mov dword [bp-8], eax
 10502                                  
 10503                                  ;-976   if (i < LSTART) { /*if (L == 0) prs("       0"); else */ prf(L, 8); }
 10504                                  
 10505 000054CB 8B4604                   mov ax, [bp+4]
 10506 000054CE 3DC800                   cmp ax, 200
 10507 000054D1 0F8D0F00                 jge @@listvar596
 10508 000054D5 6A08                     push 8
 10509 000054D7 66FF76F8                 push dword [bp-8]
 10510 000054DB E8880A                   call prf
 10511 000054DE 83C406                   add  sp, 6
 10512                                  
 10513                                  ;-977   else  { prs("bp"); j=GData[i];
 10514                                  
 10515 000054E1 E93B00                   jmp @@listvar597
 10516                                  @@listvar596:
 10517 000054E4 68[F455]                 push listvar_12
 10518 000054E7 E8A8F9                   call prs
 10519 000054EA 83C402                   add  sp, 2
 10520 000054ED 8B5E04                   mov bx, [bp+4]
 10521 000054F0 C1E302                   shl bx, 2
 10522 000054F3 668B87EB8A               mov eax, [GData + bx]
 10523 000054F8 8946FE                   mov word [bp-2], ax
 10524                                  
 10525                                  ;-978     if (j > 0) prc('+'); pint1(j); prs("   "); }
 10526                                  
 10527 000054FB 8B46FE                   mov ax, [bp-2]
 10528 000054FE 83F800                   cmp ax, 0
 10529 00005501 0F8E0800                 jle @@listvar598
 10530 00005505 6A2B                     push 43
 10531 00005507 E819F9                   call prc
 10532 0000550A 83C402                   add  sp, 2
 10533                                  @@listvar598:
 10534 0000550D FF76FE                   push word [bp-2]
 10535 00005510 E885FA                   call pint1
 10536 00005513 83C402                   add  sp, 2
 10537 00005516 68[F755]                 push listvar_13
 10538 00005519 E876F9                   call prs
 10539 0000551C 83C402                   add  sp, 2
 10540                                  
 10541                                  ;-979   prc(32);  j=GName[i]; from_far(NA, j); prs(NA);
 10542                                  
 10543                                  @@listvar597:
 10544 0000551F 6A20                     push 32
 10545 00005521 E8FFF8                   call prc
 10546 00005524 83C402                   add  sp, 2
 10547 00005527 8B5E04                   mov bx, [bp+4]
 10548 0000552A D1E3                     shl bx, 1
 10549 0000552C 8B874B94                 mov ax, [GName + bx]
 10550 00005530 8946FE                   mov word [bp-2], ax
 10551 00005533 FF76FE                   push word [bp-2]
 10552 00005536 8D06[7200]               lea  ax, [NA]
 10553 0000553A 50                       push ax
 10554 0000553B E84B09                   call from_far
 10555 0000553E 83C404                   add  sp, 4
 10556 00005541 8D06[7200]               lea  ax, [NA]
 10557 00005545 50                       push ax
 10558 00005546 E849F9                   call prs
 10559 00005549 83C402                   add  sp, 2
 10560                                  
 10561                                  ;-980   if(GType[i]=='#') { prc('='); L=GData[i]; prL(L); }
 10562                                  
 10563 0000554C 8B5E04                   mov bx, [bp+4]
 10564 0000554F 8A87B782                 mov al, [GType + bx]
 10565 00005553 3C23                     cmp al, 35
 10566 00005555 0F852100                 jne @@listvar599
 10567 00005559 6A3D                     push 61
 10568 0000555B E8C5F8                   call prc
 10569 0000555E 83C402                   add  sp, 2
 10570 00005561 8B5E04                   mov bx, [bp+4]
 10571 00005564 C1E302                   shl bx, 2
 10572 00005567 668B87EB8A               mov eax, [GData + bx]
 10573 0000556C 668946F8                 mov dword [bp-8], eax
 10574 00005570 66FF76F8                 push dword [bp-8]
 10575 00005574 E8CE09                   call prL
 10576 00005577 83C404                   add  sp, 4
 10577                                  
 10578                                  ;-981   if(GType[i]=='&') { prc('['); L=GData[i]; prL(L); prc(']');}
 10579                                  
 10580                                  @@listvar599:
 10581 0000557A 8B5E04                   mov bx, [bp+4]
 10582 0000557D 8A87B782                 mov al, [GType + bx]
 10583 00005581 3C26                     cmp al, 38
 10584 00005583 0F852900                 jne @@listvar600
 10585 00005587 6A5B                     push 91
 10586 00005589 E897F8                   call prc
 10587 0000558C 83C402                   add  sp, 2
 10588 0000558F 8B5E04                   mov bx, [bp+4]
 10589 00005592 C1E302                   shl bx, 2
 10590 00005595 668B87EB8A               mov eax, [GData + bx]
 10591 0000559A 668946F8                 mov dword [bp-8], eax
 10592 0000559E 66FF76F8                 push dword [bp-8]
 10593 000055A2 E8A009                   call prL
 10594 000055A5 83C404                   add  sp, 4
 10595 000055A8 6A5D                     push 93
 10596 000055AA E876F8                   call prc
 10597 000055AD 83C402                   add  sp, 2
 10598                                  
 10599                                  ;-982 }
 10600                                  
 10601                                  @@listvar600:
 10602                                  
 10603                                  ;-983 int listcall() { int i;  prs("\n\n;    #  addr name   list of CALLs\n");
 10604 000055B0 C9                       LEAVE
 10605 000055B1 C3                       ret
 10606                                  ; ENDP
 10607 000055B2 5C6E3B00                listvar_0 db "\n;",0
 10608 000055B6 7661722000              listvar_1 db "var ",0
 10609 000055BB 7074722000              listvar_2 db "ptr ",0
 10610 000055C0 6172722000              listvar_3 db "arr ",0
 10611 000055C5 6465662000              listvar_4 db "def ",0
 10612 000055CA 7369676E2000            listvar_5 db "sign ",0
 10613 000055D0 756E73672000            listvar_6 db "unsg ",0
 10614 000055D6 627974652000            listvar_7 db "byte ",0
 10615 000055DC 776F72642000            listvar_8 db "word ",0
 10616 000055E2 647772642000            listvar_9 db "dwrd ",0
 10617 000055E8 202020202D00            listvar_10 db "    -",0
 10618 000055EE 204E554C4C00            listvar_11 db " NULL",0
 10619 000055F4 627000                  listvar_12 db "bp",0
 10620 000055F7 20202000                listvar_13 db "   ",0
 10621                                  
 10622                                  listcall:  ; *** PROC ***
 10623                                  ;Function : listcall, Number of local variables: 1
 10624                                  ;   # type sign width line used address name   list of local variables
 10625                                  ;  200 var sign word   983 NULL bp-2    i;
 10626 000055FB C8020000                 ENTER  2,0
 10627 000055FF 68[2C56]                 push listcall_0
 10628 00005602 E88DF8                   call prs
 10629 00005605 83C402                   add  sp, 2
 10630                                  
 10631                                  ;-984   i=0;  while (i< CTop) { calllisting(i); i++; } }
 10632                                  
 10633 00005608 66B800000000             mov eax, 0
 10634 0000560E 8946FE                   mov word [bp-2], ax
 10635                                  @@listcall601:
 10636 00005611 8B46FE                   mov ax, [bp-2]
 10637 00005614 3B06[6400]               cmp ax, [CTop]
 10638 00005618 0F8D0E00                 jge @@listcall602
 10639 0000561C FF76FE                   push word [bp-2]
 10640 0000561F E83200                   call calllisting
 10641 00005622 83C402                   add  sp, 2
 10642 00005625 FF46FE                   inc  word [bp-2]
 10643 00005628 EBE7                     jmp @@listcall601
 10644                                  @@listcall602:
 10645                                  
 10646                                  ;-985 int calllisting(int i) {char c; int j;
 10647 0000562A C9                       LEAVE
 10648 0000562B C3                       ret
 10649                                  ; ENDP
 10650 0000562C 5C6E5C6E3B20202020-     listcall_0 db "\n\n;    #  addr name   list of CALLs\n",0
 10651 00005635 23202061646472206E-
 10652 0000563E 616D652020206C6973-
 10653 00005647 74206F662043414C4C-
 10654 00005650 735C6E00           
 10655                                  
 10656                                  calllisting:  ; *** PROC ***
 10657                                  
 10658                                  ;-986   prs("\n;"); printint51(i); prc(32);
 10659                                  
 10660                                  ;Function : calllisting, Number of local variables: 3
 10661                                  ;   # type sign width line used address name   list of local variables
 10662                                  ;  200 var sign word   985 NULL bp+4    i
 10663                                  ;  201 var sign byte   985 NULL bp-2    c
 10664                                  ;  202 var sign word   985 NULL bp-4    j;
 10665 00005654 C8040000                 ENTER  4,0
 10666 00005658 68[C656]                 push calllisting_0
 10667 0000565B E834F8                   call prs
 10668 0000565E 83C402                   add  sp, 2
 10669 00005661 FF7604                   push word [bp+4]
 10670 00005664 E8D6F9                   call printint51
 10671 00005667 83C402                   add  sp, 2
 10672 0000566A 6A20                     push 32
 10673 0000566C E8B4F7                   call prc
 10674 0000566F 83C402                   add  sp, 2
 10675                                  
 10676                                  ;-987   c=CUnres [i]; if(c==0)prs("unresolved ");
 10677                                  
 10678 00005672 8B5E04                   mov bx, [bp+4]
 10679 00005675 8A8777AE                 mov al, [CUnres + bx]
 10680 00005679 8846FE                   mov byte [bp-2], al
 10681 0000567C 8A46FE                   mov al, [bp-2]
 10682 0000567F 3C00                     cmp al, 0
 10683 00005681 0F850900                 jne @@calllisting603
 10684 00005685 68[CA56]                 push calllisting_1
 10685 00005688 E807F8                   call prs
 10686 0000568B 83C402                   add  sp, 2
 10687                                  
 10688                                  ;-988   j=CName[i];            printint51(j); prc(32);
 10689                                  
 10690                                  @@calllisting603:
 10691 0000568E 8B5E04                   mov bx, [bp+4]
 10692 00005691 D1E3                     shl bx, 1
 10693 00005693 8B87D79E                 mov ax, [CName + bx]
 10694 00005697 8946FC                   mov word [bp-4], ax
 10695 0000569A FF76FC                   push word [bp-4]
 10696 0000569D E89DF9                   call printint51
 10697 000056A0 83C402                   add  sp, 2
 10698 000056A3 6A20                     push 32
 10699 000056A5 E87BF7                   call prc
 10700 000056A8 83C402                   add  sp, 2
 10701                                  
 10702                                  ;-989   from_far(NA, j);   prs(NA);
 10703                                  
 10704 000056AB FF76FC                   push word [bp-4]
 10705 000056AE 8D06[7200]               lea  ax, [NA]
 10706 000056B2 50                       push ax
 10707 000056B3 E8D307                   call from_far
 10708 000056B6 83C404                   add  sp, 4
 10709 000056B9 8D06[7200]               lea  ax, [NA]
 10710 000056BD 50                       push ax
 10711 000056BE E8D1F7                   call prs
 10712 000056C1 83C402                   add  sp, 2
 10713                                  
 10714                                  ;-990 }
 10715                                  
 10716                                  
 10717                                  ;-991 int countcalls(int f) { unsigned int i;
 10718 000056C4 C9                       LEAVE
 10719 000056C5 C3                       ret
 10720                                  ; ENDP
 10721 000056C6 5C6E3B00                calllisting_0 db "\n;",0
 10722 000056CA 756E7265736F6C7665-     calllisting_1 db "unresolved ",0
 10723 000056D3 642000             
 10724                                  
 10725                                  countcalls:  ; *** PROC ***
 10726                                  
 10727                                  ;-992   pt=FName[f]; from_far(NA, pt);
 10728                                  
 10729                                  ;Function : countcalls, Number of local variables: 2
 10730                                  ;   # type sign width line used address name   list of local variables
 10731                                  ;  200 var sign word   991 NULL bp+4    f
 10732                                  ;  201 var unsg word   991 NULL bp-2    i;
 10733 000056D6 C8020000                 ENTER  2,0
 10734 000056DA 8B5E04                   mov bx, [bp+4]
 10735 000056DD D1E3                     shl bx, 1
 10736 000056DF 8B877F9C                 mov ax, [FName + bx]
 10737 000056E3 A3[BE00]                 mov word [pt], ax
 10738 000056E6 FF36[BE00]               push word [pt]
 10739 000056EA 8D06[7200]               lea  ax, [NA]
 10740 000056EE 50                       push ax
 10741 000056EF E89707                   call from_far
 10742 000056F2 83C404                   add  sp, 4
 10743                                  
 10744                                  ;-993   i=0;  while (i < CTop) {  p1=CName[i]; from_far(NB, p1);
 10745                                  
 10746 000056F5 66B800000000             mov eax, 0
 10747 000056FB 8946FE                   mov word [bp-2], ax
 10748                                  @@countcalls604:
 10749 000056FE 8B46FE                   mov ax, [bp-2]
 10750 00005701 3B06[6400]               cmp ax, [CTop]
 10751 00005705 0F8D4B00                 jge @@countcalls605
 10752 00005709 8B5EFE                   mov bx, [bp-2]
 10753 0000570C D1E3                     shl bx, 1
 10754 0000570E 8B87D79E                 mov ax, [CName + bx]
 10755 00005712 A3[C000]                 mov word [p1], ax
 10756 00005715 FF36[C000]               push word [p1]
 10757 00005719 8D06[9300]               lea  ax, [NB]
 10758 0000571D 50                       push ax
 10759 0000571E E86807                   call from_far
 10760 00005721 83C404                   add  sp, 4
 10761                                  
 10762                                  ;-994     if (eqstr(NA, NB))  FCalls[f] = FCalls[f] + 1;         i++; }
 10763                                  
 10764 00005724 8D06[9300]               lea  ax, [NB]
 10765 00005728 50                       push ax
 10766 00005729 8D06[7200]               lea  ax, [NA]
 10767 0000572D 50                       push ax
 10768 0000572E E8AF0B                   call eqstr
 10769 00005731 83C404                   add  sp, 4
 10770 00005734 08C0                     or  al, al
 10771 00005736 0F841500                 je @@countcalls606
 10772 0000573A 8B5E04                   mov bx, [bp+4]
 10773 0000573D D1E3                     shl bx, 1
 10774 0000573F 8B87279A                 mov ax, [FCalls + bx]
 10775 00005743 83C001                   add ax, 1
 10776 00005746 8B5E04                   mov bx, [bp+4]
 10777 00005749 D1E3                     shl bx, 1
 10778 0000574B 8987279A                 mov [FCalls+bx], ax
 10779                                  @@countcalls606:
 10780 0000574F FF46FE                   inc  word [bp-2]
 10781                                  
 10782                                  ;-995 }
 10783                                  
 10784 00005752 EBAA                     jmp @@countcalls604
 10785                                  @@countcalls605:
 10786                                  
 10787                                  ;-996 int listfunc() { int i;
 10788 00005754 C9                       LEAVE
 10789 00005755 C3                       ret
 10790                                  ; ENDP
 10791                                  
 10792                                  listfunc:  ; *** PROC ***
 10793                                  
 10794                                  ;-997   prs("\n\n\n;   # Calls Line Width  Name   list of functions\n");
 10795                                  
 10796                                  ;Function : listfunc, Number of local variables: 1
 10797                                  ;   # type sign width line used address name   list of local variables
 10798                                  ;  200 var sign word   996 NULL bp-2    i;
 10799 00005756 C8020000                 ENTER  2,0
 10800 0000575A 68[A957]                 push listfunc_0
 10801 0000575D E832F7                   call prs
 10802 00005760 83C402                   add  sp, 2
 10803                                  
 10804                                  ;-998   i=0;  while (i < FTop) { countcalls (i); i++; } 
 10805                                  
 10806 00005763 66B800000000             mov eax, 0
 10807 00005769 8946FE                   mov word [bp-2], ax
 10808                                  @@listfunc607:
 10809 0000576C 8B46FE                   mov ax, [bp-2]
 10810 0000576F 3B06[6200]               cmp ax, [FTop]
 10811 00005773 0F8D0E00                 jge @@listfunc608
 10812 00005777 FF76FE                   push word [bp-2]
 10813 0000577A E859FF                   call countcalls
 10814 0000577D 83C402                   add  sp, 2
 10815 00005780 FF46FE                   inc  word [bp-2]
 10816                                  
 10817                                  ;-999   i=0;  while (i < FTop) { funclisting(i); i++; } }
 10818                                  
 10819 00005783 EBE7                     jmp @@listfunc607
 10820                                  @@listfunc608:
 10821 00005785 66B800000000             mov eax, 0
 10822 0000578B 8946FE                   mov word [bp-2], ax
 10823                                  @@listfunc609:
 10824 0000578E 8B46FE                   mov ax, [bp-2]
 10825 00005791 3B06[6200]               cmp ax, [FTop]
 10826 00005795 0F8D0E00                 jge @@listfunc610
 10827 00005799 FF76FE                   push word [bp-2]
 10828 0000579C E84300                   call funclisting
 10829 0000579F 83C402                   add  sp, 2
 10830 000057A2 FF46FE                   inc  word [bp-2]
 10831 000057A5 EBE7                     jmp @@listfunc609
 10832                                  @@listfunc610:
 10833                                  
 10834                                  ;-1000 int funclisting(int i) {int j;  char c;
 10835 000057A7 C9                       LEAVE
 10836 000057A8 C3                       ret
 10837                                  ; ENDP
 10838 000057A9 5C6E5C6E5C6E3B2020-     listfunc_0 db "\n\n\n;   # Calls Line Width  Name   list of functions\n",0
 10839 000057B2 20232043616C6C7320-
 10840 000057BB 4C696E652057696474-
 10841 000057C4 6820204E616D652020-
 10842 000057CD 206C697374206F6620-
 10843 000057D6 66756E6374696F6E73-
 10844 000057DF 5C6E00             
 10845                                  
 10846                                  funclisting:  ; *** PROC ***
 10847                                  
 10848                                  ;-1001   prs("\n;");    printint51(i);
 10849                                  
 10850                                  ;Function : funclisting, Number of local variables: 3
 10851                                  ;   # type sign width line used address name   list of local variables
 10852                                  ;  200 var sign word  1000 NULL bp+4    i
 10853                                  ;  201 var sign word  1000 NULL bp-2    j
 10854                                  ;  202 var sign byte  1000 NULL bp-4    c;
 10855 000057E2 C8040000                 ENTER  4,0
 10856 000057E6 68[C458]                 push funclisting_0
 10857 000057E9 E8A6F6                   call prs
 10858 000057EC 83C402                   add  sp, 2
 10859 000057EF FF7604                   push word [bp+4]
 10860 000057F2 E848F8                   call printint51
 10861 000057F5 83C402                   add  sp, 2
 10862                                  
 10863                                  ;-1002   j = FCalls[i]; if (j) printint51(j); else prs(" NULL");
 10864                                  
 10865 000057F8 8B5E04                   mov bx, [bp+4]
 10866 000057FB D1E3                     shl bx, 1
 10867 000057FD 8B87279A                 mov ax, [FCalls + bx]
 10868 00005801 8946FE                   mov word [bp-2], ax
 10869 00005804 8B46FE                   mov ax, [bp-2]
 10870 00005807 08C0                     or  al, al
 10871 00005809 0F840C00                 je @@funclisting611
 10872 0000580D FF76FE                   push word [bp-2]
 10873 00005810 E82AF8                   call printint51
 10874 00005813 83C402                   add  sp, 2
 10875 00005816 E90900                   jmp @@funclisting612
 10876                                  @@funclisting611:
 10877 00005819 68[C858]                 push funclisting_1
 10878 0000581C E873F6                   call prs
 10879 0000581F 83C402                   add  sp, 2
 10880                                  
 10881                                  ;-1003   j = FLino[i];   printint51(j); prc(32);
 10882                                  
 10883                                  @@funclisting612:
 10884 00005822 8B5E04                   mov bx, [bp+4]
 10885 00005825 D1E3                     shl bx, 1
 10886 00005827 8B87CF97                 mov ax, [FLino + bx]
 10887 0000582B 8946FE                   mov word [bp-2], ax
 10888 0000582E FF76FE                   push word [bp-2]
 10889 00005831 E809F8                   call printint51
 10890 00005834 83C402                   add  sp, 2
 10891 00005837 6A20                     push 32
 10892 00005839 E8E7F5                   call prc
 10893 0000583C 83C402                   add  sp, 2
 10894                                  
 10895                                  ;-1004   c=FWidth[i];
 10896                                  
 10897 0000583F 8B5E04                   mov bx, [bp+4]
 10898 00005842 8A87A396                 mov al, [FWidth + bx]
 10899 00005846 8846FC                   mov byte [bp-4], al
 10900                                  
 10901                                  ;-1005   if(_ c==0)prs("void " );    if(_ c==1)prs("byte " );
 10902                                  
 10903 00005849 807EFC00                cmp byte [bp-4], 0
 10904 0000584D 0F850900                 jne @@funclisting613
 10905 00005851 68[CE58]                 push funclisting_2
 10906 00005854 E83BF6                   call prs
 10907 00005857 83C402                   add  sp, 2
 10908                                  @@funclisting613:
 10909 0000585A 807EFC01                cmp byte [bp-4], 1
 10910 0000585E 0F850900                 jne @@funclisting614
 10911 00005862 68[D458]                 push funclisting_3
 10912 00005865 E82AF6                   call prs
 10913 00005868 83C402                   add  sp, 2
 10914                                  
 10915                                  ;-1006   if(_ c==2)prs("word " );    if(_ c==4)prs("dwrd " );
 10916                                  
 10917                                  @@funclisting614:
 10918 0000586B 807EFC02                cmp byte [bp-4], 2
 10919 0000586F 0F850900                 jne @@funclisting615
 10920 00005873 68[DA58]                 push funclisting_4
 10921 00005876 E819F6                   call prs
 10922 00005879 83C402                   add  sp, 2
 10923                                  @@funclisting615:
 10924 0000587C 807EFC04                cmp byte [bp-4], 4
 10925 00005880 0F850900                 jne @@funclisting616
 10926 00005884 68[E058]                 push funclisting_5
 10927 00005887 E808F6                   call prs
 10928 0000588A 83C402                   add  sp, 2
 10929                                  
 10930                                  ;-1007   prc(32); prc(32);
 10931                                  
 10932                                  @@funclisting616:
 10933 0000588D 6A20                     push 32
 10934 0000588F E891F5                   call prc
 10935 00005892 83C402                   add  sp, 2
 10936 00005895 6A20                     push 32
 10937 00005897 E889F5                   call prc
 10938 0000589A 83C402                   add  sp, 2
 10939                                  
 10940                                  ;-1008   j=FName[i]; from_far(NA, j); prs(NA);
 10941                                  
 10942 0000589D 8B5E04                   mov bx, [bp+4]
 10943 000058A0 D1E3                     shl bx, 1
 10944 000058A2 8B877F9C                 mov ax, [FName + bx]
 10945 000058A6 8946FE                   mov word [bp-2], ax
 10946 000058A9 FF76FE                   push word [bp-2]
 10947 000058AC 8D06[7200]               lea  ax, [NA]
 10948 000058B0 50                       push ax
 10949 000058B1 E8D505                   call from_far
 10950 000058B4 83C404                   add  sp, 4
 10951 000058B7 8D06[7200]               lea  ax, [NA]
 10952 000058BB 50                       push ax
 10953 000058BC E8D3F5                   call prs
 10954 000058BF 83C402                   add  sp, 2
 10955                                  
 10956                                  ;-1009 }
 10957                                  
 10958                                  
 10959                                  ;-1010 unsigned int MAXUI=65535;
 10960 000058C2 C9                       LEAVE
 10961 000058C3 C3                       ret
 10962                                  ; ENDP
 10963 000058C4 5C6E3B00                funclisting_0 db "\n;",0
 10964 000058C8 204E554C4C00            funclisting_1 db " NULL",0
 10965 000058CE 766F69642000            funclisting_2 db "void ",0
 10966 000058D4 627974652000            funclisting_3 db "byte ",0
 10967 000058DA 776F72642000            funclisting_4 db "word ",0
 10968 000058E0 647772642000            funclisting_5 db "dwrd ",0
 10969 000058E6 FFFF                    MAXUI dw 65535
 10970                                  
 10971                                  ;-1011 int epilog() {unsigned int i; 
 10972                                  
 10973                                  
 10974                                  epilog:  ; *** PROC ***
 10975                                  
 10976                                  ;-1012   strcpy(symbol, "LastFunctionByt");  storefunc();
 10977                                  
 10978                                  ;Function : epilog, Number of local variables: 1
 10979                                  ;   # type sign width line used address name   list of local variables
 10980                                  ;  200 var unsg word  1011 NULL bp-2    i;
 10981 000058E8 C8020000                 ENTER  2,0
 10982 000058EC 68[A85B]                 push epilog_0
 10983 000058EF 8D066E81                 lea  ax, [symbol]
 10984 000058F3 50                       push ax
 10985 000058F4 E89508                   call strcpy
 10986 000058F7 83C404                   add  sp, 4
 10987 000058FA E831B4                   call storefunc
 10988                                  
 10989                                  ;-1013   prs("\nLastFunctionByt:db 0E8h, 0, 0\npop ax\nret");
 10990                                  
 10991 000058FD 68[B85B]                 push epilog_1
 10992 00005900 E88FF5                   call prs
 10993 00005903 83C402                   add  sp, 2
 10994                                  
 10995                                  ;-1014   prs("\n \n;   # type sign width line used  address name   list of global variables\n");
 10996                                  
 10997 00005906 68[E55B]                 push epilog_2
 10998 00005909 E886F5                   call prs
 10999 0000590C 83C402                   add  sp, 2
 11000                                  
 11001                                  ;-1015   i=1;      while (i< GTop) { listvar(i); i++; }
 11002                                  
 11003 0000590F 66B801000000             mov eax, 1
 11004 00005915 8946FE                   mov word [bp-2], ax
 11005                                  @@epilog617:
 11006 00005918 8B46FE                   mov ax, [bp-2]
 11007 0000591B 3B06[5E00]               cmp ax, [GTop]
 11008 0000591F 0F8D0E00                 jge @@epilog618
 11009 00005923 FF76FE                   push word [bp-2]
 11010 00005926 E855FA                   call listvar
 11011 00005929 83C402                   add  sp, 2
 11012 0000592C FF46FE                   inc  word [bp-2]
 11013                                  
 11014                                  ;-1016   listfunc();   listcall();   LIST=1;
 11015                                  
 11016 0000592F EBE7                     jmp @@epilog617
 11017                                  @@epilog618:
 11018 00005931 E822FE                   call listfunc
 11019 00005934 E8C4FC                   call listcall
 11020 00005937 66B801000000             mov eax, 1
 11021 0000593D A2[1700]                 mov byte [LIST], al
 11022                                  
 11023                                  ;-1017   prs("\n;Input: "); prs(&namein);
 11024                                  
 11025 00005940 68[355C]                 push epilog_3
 11026 00005943 E84CF5                   call prs
 11027 00005946 83C402                   add  sp, 2
 11028 00005949 8D063182                 lea  ax, [namein]
 11029 0000594D 50                       push ax
 11030 0000594E E841F5                   call prs
 11031 00005951 83C402                   add  sp, 2
 11032                                  
 11033                                  ;-1018   prs(", List: ");   prs(&namelst);
 11034                                  
 11035 00005954 68[405C]                 push epilog_4
 11036 00005957 E838F5                   call prs
 11037 0000595A 83C402                   add  sp, 2
 11038 0000595D 8D067482                 lea  ax, [namelst]
 11039 00005961 50                       push ax
 11040 00005962 E82DF5                   call prs
 11041 00005965 83C402                   add  sp, 2
 11042                                  
 11043                                  ;-1019   prs(",  Lines:"); printint51(lineno);
 11044                                  
 11045 00005968 68[495C]                 push epilog_5
 11046 0000596B E824F5                   call prs
 11047 0000596E 83C402                   add  sp, 2
 11048 00005971 FF36[BA00]               push word [lineno]
 11049 00005975 E8C5F6                   call printint51
 11050 00005978 83C402                   add  sp, 2
 11051                                  
 11052                                  ;-1020   prs("\n;Glob. variables:"); GTop--; printint51(GTop);
 11053                                  
 11054 0000597B 68[535C]                 push epilog_6
 11055 0000597E E811F5                   call prs
 11056 00005981 83C402                   add  sp, 2
 11057 00005984 FF0E[5E00]               dec  word [GTop]
 11058 00005988 FF36[5E00]               push word [GTop]
 11059 0000598C E8AEF6                   call printint51
 11060 0000598F 83C402                   add  sp, 2
 11061                                  
 11062                                  ;-1021   prs(" max.:"); printint51(LSTART);
 11063                                  
 11064 00005992 68[675C]                 push epilog_7
 11065 00005995 E8FAF4                   call prs
 11066 00005998 83C402                   add  sp, 2
 11067 0000599B 68C800                   push 200
 11068 0000599E E89CF6                   call printint51
 11069 000059A1 83C402                   add  sp, 2
 11070                                  
 11071                                  ;-1022   prs("\n;Local variables:"); i=maxloc-LSTART; printint51(i);
 11072                                  
 11073 000059A4 68[6E5C]                 push epilog_8
 11074 000059A7 E8E8F4                   call prs
 11075 000059AA 83C402                   add  sp, 2
 11076 000059AD A1[3200]                 mov ax, [maxloc]
 11077 000059B0 2DC800                   sub ax, 200
 11078 000059B3 8946FE                   mov word [bp-2], ax
 11079 000059B6 FF76FE                   push word [bp-2]
 11080 000059B9 E881F6                   call printint51
 11081 000059BC 83C402                   add  sp, 2
 11082                                  
 11083                                  ;-1023   prs(" max.:"); _ i=VARMAX; i-=LSTART; printint51(i);
 11084                                  
 11085 000059BF 68[825C]                 push epilog_9
 11086 000059C2 E8CDF4                   call prs
 11087 000059C5 83C402                   add  sp, 2
 11088 000059C8 C746FE2C01              mov word [bp-2], 300; i
 11089 000059CD 816EFEC800               sub  word [bp-2], 200
 11090 000059D2 FF76FE                   push word [bp-2]
 11091 000059D5 E865F6                   call printint51
 11092 000059D8 83C402                   add  sp, 2
 11093                                  
 11094                                  ;-1024   prs(", in '"); prs(locname); prs("'");
 11095                                  
 11096 000059DB 68[895C]                 push epilog_10
 11097 000059DE E8B1F4                   call prs
 11098 000059E1 83C402                   add  sp, 2
 11099 000059E4 8D062B81                 lea  ax, [locname]
 11100 000059E8 50                       push ax
 11101 000059E9 E8A6F4                   call prs
 11102 000059EC 83C402                   add  sp, 2
 11103 000059EF 68[905C]                 push epilog_11
 11104 000059F2 E89DF4                   call prs
 11105 000059F5 83C402                   add  sp, 2
 11106                                  
 11107                                  ;-1025   prs("\n;Functions      :"); printint51(FTop);
 11108                                  
 11109 000059F8 68[925C]                 push epilog_12
 11110 000059FB E894F4                   call prs
 11111 000059FE 83C402                   add  sp, 2
 11112 00005A01 FF36[6200]               push word [FTop]
 11113 00005A05 E835F6                   call printint51
 11114 00005A08 83C402                   add  sp, 2
 11115                                  
 11116                                  ;-1026   prs(" max.:"); printint51(FUNCMAX);
 11117                                  
 11118 00005A0B 68[A65C]                 push epilog_13
 11119 00005A0E E881F4                   call prs
 11120 00005A11 83C402                   add  sp, 2
 11121 00005A14 682C01                   push 300
 11122 00005A17 E823F6                   call printint51
 11123 00005A1A 83C402                   add  sp, 2
 11124                                  
 11125                                  ;-1027   prs("\n;Calls          :"); printint51(CTop);
 11126                                  
 11127 00005A1D 68[AD5C]                 push epilog_14
 11128 00005A20 E86FF4                   call prs
 11129 00005A23 83C402                   add  sp, 2
 11130 00005A26 FF36[6400]               push word [CTop]
 11131 00005A2A E810F6                   call printint51
 11132 00005A2D 83C402                   add  sp, 2
 11133                                  
 11134                                  ;-1028   prs(" max.:"); printint51(CALLMAX);
 11135                                  
 11136 00005A30 68[C15C]                 push epilog_15
 11137 00005A33 E85CF4                   call prs
 11138 00005A36 83C402                   add  sp, 2
 11139 00005A39 68D007                   push 2000
 11140 00005A3C E8FEF5                   call printint51
 11141 00005A3F 83C402                   add  sp, 2
 11142                                  
 11143                                  ;-1029   prs(", NameField:"); printint51(CNameTop);
 11144                                  
 11145 00005A42 68[C85C]                 push epilog_16
 11146 00005A45 E84AF4                   call prs
 11147 00005A48 83C402                   add  sp, 2
 11148 00005A4B FF36[B400]               push word [CNameTop]
 11149 00005A4F E8EBF5                   call printint51
 11150 00005A52 83C402                   add  sp, 2
 11151                                  
 11152                                  ;-1030   prs(" max.:"); printint51(MAXUI);
 11153                                  
 11154 00005A55 68[D55C]                 push epilog_17
 11155 00005A58 E837F4                   call prs
 11156 00005A5B 83C402                   add  sp, 2
 11157 00005A5E FF36[E658]               push word [MAXUI]
 11158 00005A62 E8D8F5                   call printint51
 11159 00005A65 83C402                   add  sp, 2
 11160                                  
 11161                                  ;-1031   __asm{call LastFunctionByt}  _ i=ax;
 11162                                  
 11163 00005A68 E87309                  call LastFunctionByt
 11164 00005A6B 8946FE                  mov [bp-2], ax
 11165                                  
 11166                                  ;-1032   prs("\n;Code until     :"); printint51(i);
 11167                                  
 11168 00005A6E 68[DC5C]                 push epilog_18
 11169 00005A71 E81EF4                   call prs
 11170 00005A74 83C402                   add  sp, 2
 11171 00005A77 FF76FE                   push word [bp-2]
 11172 00005A7A E8C0F5                   call printint51
 11173 00005A7D 83C402                   add  sp, 2
 11174                                  
 11175                                  ;-1033   prs(" max.: "); printint51(ORGDATAORIG); i=ORGDATAORIG-i; prs(", free:");
 11176                                  
 11177 00005A80 68[F05C]                 push epilog_19
 11178 00005A83 E80CF4                   call prs
 11179 00005A86 83C402                   add  sp, 2
 11180 00005A89 66FF36[2400]             push dword [ORGDATAORIG]
 11181 00005A8E E8ACF5                   call printint51
 11182 00005A91 83C404                   add  sp, 4
 11183 00005A94 66A1[2400]               mov eax, [ORGDATAORIG]
 11184 00005A98 2B46FE                   sub ax, [bp-2]
 11185 00005A9B 8946FE                   mov word [bp-2], ax
 11186 00005A9E 68[F85C]                 push epilog_20
 11187 00005AA1 E8EEF3                   call prs
 11188 00005AA4 83C402                   add  sp, 2
 11189                                  
 11190                                  ;-1034   printint51(i); if (i <= 1000) {prs(" *** Warning *** Code area too small");
 11191                                  
 11192 00005AA7 FF76FE                   push word [bp-2]
 11193 00005AAA E890F5                   call printint51
 11194 00005AAD 83C402                   add  sp, 2
 11195 00005AB0 8B46FE                   mov ax, [bp-2]
 11196 00005AB3 3DE803                   cmp ax, 1000
 11197 00005AB6 0F871D00                 ja  @@epilog619
 11198 00005ABA 68[005D]                 push epilog_21
 11199 00005ABD E8D2F3                   call prs
 11200 00005AC0 83C402                   add  sp, 2
 11201                                  
 11202                                  ;-1035   prs("\nuse #define ORGDATA > "); printint51(ORGDATAORIG); }
 11203                                  
 11204 00005AC3 68[255D]                 push epilog_22
 11205 00005AC6 E8C9F3                   call prs
 11206 00005AC9 83C402                   add  sp, 2
 11207 00005ACC 66FF36[2400]             push dword [ORGDATAORIG]
 11208 00005AD1 E869F5                   call printint51
 11209 00005AD4 83C404                   add  sp, 4
 11210                                  
 11211                                  ;-1036   prs("\n;Data (HeapEnd) :"); prunsign1(orgData); i=MAXUI-orgData;
 11212                                  
 11213                                  @@epilog619:
 11214 00005AD7 68[3E5D]                 push epilog_23
 11215 00005ADA E8B5F3                   call prs
 11216 00005ADD 83C402                   add  sp, 2
 11217 00005AE0 66FF36[2800]             push dword [orgData]
 11218 00005AE5 E813F5                   call prunsign1
 11219 00005AE8 83C404                   add  sp, 4
 11220 00005AEB A1[E658]                 mov ax, [MAXUI]
 11221 00005AEE 662B06[2800]             sub eax, [orgData]
 11222 00005AF3 8946FE                   mov word [bp-2], ax
 11223                                  
 11224                                  ;-1037   prs(", resting stacksize: ");printint51(i);
 11225                                  
 11226 00005AF6 68[525D]                 push epilog_24
 11227 00005AF9 E896F3                   call prs
 11228 00005AFC 83C402                   add  sp, 2
 11229 00005AFF FF76FE                   push word [bp-2]
 11230 00005B02 E838F5                   call printint51
 11231 00005B05 83C402                   add  sp, 2
 11232                                  
 11233                                  ;-1038   if (i <= 5000) prs(" *** Warning *** Stack too small");
 11234                                  
 11235 00005B08 8B46FE                   mov ax, [bp-2]
 11236 00005B0B 3D8813                   cmp ax, 5000
 11237 00005B0E 0F870900                 ja  @@epilog620
 11238 00005B12 68[685D]                 push epilog_25
 11239 00005B15 E87AF3                   call prs
 11240 00005B18 83C402                   add  sp, 2
 11241                                  
 11242                                  ;-1039   prs("\n;Max. Const in '"); prs(coname); prs("' :"); printint51(maxco);
 11243                                  
 11244                                  @@epilog620:
 11245 00005B1B 68[895D]                 push epilog_26
 11246 00005B1E E871F3                   call prs
 11247 00005B21 83C402                   add  sp, 2
 11248 00005B24 8D06E880                 lea  ax, [coname]
 11249 00005B28 50                       push ax
 11250 00005B29 E866F3                   call prs
 11251 00005B2C 83C402                   add  sp, 2
 11252 00005B2F 68[9C5D]                 push epilog_27
 11253 00005B32 E85DF3                   call prs
 11254 00005B35 83C402                   add  sp, 2
 11255 00005B38 FF36[2C00]               push word [maxco]
 11256 00005B3C E8FEF4                   call printint51
 11257 00005B3F 83C402                   add  sp, 2
 11258                                  
 11259                                  ;-1040   prs(" max."); printint51(COMAX); i=COMAX; i=i-maxco; prs(", free:");
 11260                                  
 11261 00005B42 68[A05D]                 push epilog_28
 11262 00005B45 E84AF3                   call prs
 11263 00005B48 83C402                   add  sp, 2
 11264 00005B4B 68B80B                   push 3000
 11265 00005B4E E8ECF4                   call printint51
 11266 00005B51 83C402                   add  sp, 2
 11267 00005B54 66B8B80B0000             mov eax, 3000
 11268 00005B5A 8946FE                   mov word [bp-2], ax
 11269 00005B5D 8B46FE                   mov ax, [bp-2]
 11270 00005B60 2B06[2C00]               sub ax, [maxco]
 11271 00005B64 8946FE                   mov word [bp-2], ax
 11272 00005B67 68[A65D]                 push epilog_29
 11273 00005B6A E825F3                   call prs
 11274 00005B6D 83C402                   add  sp, 2
 11275                                  
 11276                                  ;-1041   printint51(i);if (i <= 1000)prs(" *** Warning *** constant area too small");
 11277                                  
 11278 00005B70 FF76FE                   push word [bp-2]
 11279 00005B73 E8C7F4                   call printint51
 11280 00005B76 83C402                   add  sp, 2
 11281 00005B79 8B46FE                   mov ax, [bp-2]
 11282 00005B7C 3DE803                   cmp ax, 1000
 11283 00005B7F 0F870900                 ja  @@epilog621
 11284 00005B83 68[AE5D]                 push epilog_30
 11285 00005B86 E809F3                   call prs
 11286 00005B89 83C402                   add  sp, 2
 11287                                  
 11288                                  ;-1042   if(NASM==0)prs("\nEND");end1(0);}
 11289                                  
 11290                                  @@epilog621:
 11291 00005B8C A0[1500]                 mov al, [NASM]
 11292 00005B8F 3C00                     cmp al, 0
 11293 00005B91 0F850900                 jne @@epilog622
 11294 00005B95 68[D75D]                 push epilog_31
 11295 00005B98 E8F7F2                   call prs
 11296 00005B9B 83C402                   add  sp, 2
 11297                                  @@epilog622:
 11298 00005B9E 6A00                     push 0
 11299 00005BA0 E8CDF5                   call end1
 11300 00005BA3 83C402                   add  sp, 2
 11301                                  
 11302                                  ;-1043 // while(expr) stmt; do stmt while(expr); FOR: i=0; while(i<10){stmt; i++;}
 11303                                  
 11304                                  
 11305                                  ;-1044 
 11306                                  
 11307                                  
 11308                                  ;-1045 int memresize(unsigned int i) {
 11309 00005BA6 C9                       LEAVE
 11310 00005BA7 C3                       ret
 11311                                  ; ENDP
 11312 00005BA8 4C61737446756E6374-     epilog_0 db "LastFunctionByt",0
 11313 00005BB1 696F6E42797400     
 11314 00005BB8 5C6E4C61737446756E-     epilog_1 db "\nLastFunctionByt:db 0E8h, 0, 0\npop ax\nret",0
 11315 00005BC1 6374696F6E4279743A-
 11316 00005BCA 646220304538682C20-
 11317 00005BD3 302C20305C6E706F70-
 11318 00005BDC 2061785C6E72657400 
 11319 00005BE5 5C6E205C6E3B202020-     epilog_2 db "\n \n;   # type sign width line used  address name   list of global variables\n",0
 11320 00005BEE 232074797065207369-
 11321 00005BF7 676E20776964746820-
 11322 00005C00 6C696E652075736564-
 11323 00005C09 202061646472657373-
 11324 00005C12 206E616D652020206C-
 11325 00005C1B 697374206F6620676C-
 11326 00005C24 6F62616C2076617269-
 11327 00005C2D 61626C65735C6E00   
 11328 00005C35 5C6E3B496E7075743A-     epilog_3 db "\n;Input: ",0
 11329 00005C3E 2000               
 11330 00005C40 2C204C6973743A2000      epilog_4 db ", List: ",0
 11331 00005C49 2C20204C696E65733A-     epilog_5 db ",  Lines:",0
 11332 00005C52 00                 
 11333 00005C53 5C6E3B476C6F622E20-     epilog_6 db "\n;Glob. variables:",0
 11334 00005C5C 7661726961626C6573-
 11335 00005C65 3A00               
 11336 00005C67 206D61782E3A00          epilog_7 db " max.:",0
 11337 00005C6E 5C6E3B4C6F63616C20-     epilog_8 db "\n;Local variables:",0
 11338 00005C77 7661726961626C6573-
 11339 00005C80 3A00               
 11340 00005C82 206D61782E3A00          epilog_9 db " max.:",0
 11341 00005C89 2C20696E202700          epilog_10 db ", in '",0
 11342 00005C90 2700                    epilog_11 db "'",0
 11343 00005C92 5C6E3B46756E637469-     epilog_12 db "\n;Functions      :",0
 11344 00005C9B 6F6E73202020202020-
 11345 00005CA4 3A00               
 11346 00005CA6 206D61782E3A00          epilog_13 db " max.:",0
 11347 00005CAD 5C6E3B43616C6C7320-     epilog_14 db "\n;Calls          :",0
 11348 00005CB6 202020202020202020-
 11349 00005CBF 3A00               
 11350 00005CC1 206D61782E3A00          epilog_15 db " max.:",0
 11351 00005CC8 2C204E616D65466965-     epilog_16 db ", NameField:",0
 11352 00005CD1 6C643A00           
 11353 00005CD5 206D61782E3A00          epilog_17 db " max.:",0
 11354 00005CDC 5C6E3B436F64652075-     epilog_18 db "\n;Code until     :",0
 11355 00005CE5 6E74696C2020202020-
 11356 00005CEE 3A00               
 11357 00005CF0 206D61782E3A2000        epilog_19 db " max.: ",0
 11358 00005CF8 2C20667265653A00        epilog_20 db ", free:",0
 11359 00005D00 202A2A2A205761726E-     epilog_21 db " *** Warning *** Code area too small",0
 11360 00005D09 696E67202A2A2A2043-
 11361 00005D12 6F6465206172656120-
 11362 00005D1B 746F6F20736D616C6C-
 11363 00005D24 00                 
 11364 00005D25 5C6E75736520236465-     epilog_22 db "\nuse #define ORGDATA > ",0
 11365 00005D2E 66696E65204F524744-
 11366 00005D37 415441203E2000     
 11367 00005D3E 5C6E3B446174612028-     epilog_23 db "\n;Data (HeapEnd) :",0
 11368 00005D47 48656170456E642920-
 11369 00005D50 3A00               
 11370 00005D52 2C2072657374696E67-     epilog_24 db ", resting stacksize: ",0
 11371 00005D5B 20737461636B73697A-
 11372 00005D64 653A2000           
 11373 00005D68 202A2A2A205761726E-     epilog_25 db " *** Warning *** Stack too small",0
 11374 00005D71 696E67202A2A2A2053-
 11375 00005D7A 7461636B20746F6F20-
 11376 00005D83 736D616C6C00       
 11377 00005D89 5C6E3B4D61782E2043-     epilog_26 db "\n;Max. Const in '",0
 11378 00005D92 6F6E737420696E2027-
 11379 00005D9B 00                 
 11380 00005D9C 27203A00                epilog_27 db "' :",0
 11381 00005DA0 206D61782E00            epilog_28 db " max.",0
 11382 00005DA6 2C20667265653A00        epilog_29 db ", free:",0
 11383 00005DAE 202A2A2A205761726E-     epilog_30 db " *** Warning *** constant area too small",0
 11384 00005DB7 696E67202A2A2A2063-
 11385 00005DC0 6F6E7374616E742061-
 11386 00005DC9 72656120746F6F2073-
 11387 00005DD2 6D616C6C00         
 11388 00005DD7 5C6E454E4400            epilog_31 db "\nEND",0
 11389                                  
 11390                                  memresize:  ; *** PROC ***
 11391                                  
 11392                                  ;-1046   _ DOS_ERR=0; bx=i; _ ax=cs; es=ax; ax=0x4A00; DosInt(); }
 11393                                  
 11394                                  ;Function : memresize, Number of local variables: 1
 11395                                  ;   # type sign width line used address name   list of local variables
 11396                                  ;  200 var unsg word  1045 NULL bp+4    i;
 11397 00005DDD C8000000                 ENTER  0,0
 11398 00005DE1 C706[C200]0000          mov word [DOS_ERR], 0
 11399 00005DE7 8B5E04                   mov  bx, [bp+4]
 11400 00005DEA 8CC8                     mov  ax, cs
 11401 00005DEC 8EC0                     mov  es, ax
 11402 00005DEE B8004A                   mov  ax, 18944
 11403 00005DF1 E86605                   call DosInt
 11404                                  
 11405                                  ;-1047 int memalloc(unsigned int i) { unsigned int vAX; unsigned int vBX;
 11406 00005DF4 C9                       LEAVE
 11407 00005DF5 C3                       ret
 11408                                  ; ENDP
 11409                                  
 11410                                  memalloc:  ; *** PROC ***
 11411                                  
 11412                                  ;-1048   _ DOS_ERR=0; bx=i;  ax=0x4800; DosInt(); _ vAX=ax; _ vBX=bx;
 11413                                  
 11414                                  ;Function : memalloc, Number of local variables: 3
 11415                                  ;   # type sign width line used address name   list of local variables
 11416                                  ;  200 var unsg word  1047 NULL bp+4    i
 11417                                  ;  201 var unsg word  1047 NULL bp-2    vAX
 11418                                  ;  202 var unsg word  1047 NULL bp-4    vBX;
 11419 00005DF6 C8040000                 ENTER  4,0
 11420 00005DFA C706[C200]0000          mov word [DOS_ERR], 0
 11421 00005E00 8B5E04                   mov  bx, [bp+4]
 11422 00005E03 B80048                   mov  ax, 18432
 11423 00005E06 E85105                   call DosInt
 11424 00005E09 8946FE                  mov [bp-2], ax
 11425 00005E0C 895EFC                  mov [bp-4], bx
 11426                                  
 11427                                  ;-1049   if(DOS_ERR) return vBX;   return vAX;  }
 11428                                  
 11429 00005E0F A1[C200]                 mov ax, [DOS_ERR]
 11430 00005E12 08C0                     or  al, al
 11431 00005E14 0F840600                 je @@memalloc623
 11432 00005E18 8B46FC                   mov ax, [bp-4]
 11433 00005E1B E90600                   jmp @@retn
 11434                                  @@memalloc623:
 11435 00005E1E 8B46FE                   mov ax, [bp-2]
 11436 00005E21 E90000                   jmp @@retn
 11437                                  
 11438                                  ;-1050 int copyF(char *dest, char *src, unsigned int sz) {
 11439                                  
 11440 00005E24 C9                      @@retn: LEAVE
 11441 00005E25 C3                       ret
 11442                                  ; ENDP
 11443                                  
 11444                                  copyF:  ; *** PROC ***
 11445                                  
 11446                                  ;-1051   segE;  _ es=ax;  _ si=src;  _ di=dest; _ cx=sz;  // ds:si   es:di
 11447                                  
 11448                                  ;Function : copyF, Number of local variables: 3
 11449                                  ;   # type sign width line used address name   list of local variables
 11450                                  ;  200 ptr sign byte  1050 NULL bp+4    dest
 11451                                  ;  201 ptr sign byte  1050 NULL bp+6    src
 11452                                  ;  202 var unsg word  1050 NULL bp+8    sz;
 11453 00005E26 C8000000                 ENTER  0,0
 11454 00005E2A A1[B800]                 mov ax, [segE]
 11455 00005E2D 8EC0                     mov  es, ax
 11456 00005E2F 8B7606                   mov  si, [bp+6]
 11457 00005E32 8B7E04                   mov  di, [bp+4]
 11458 00005E35 8B4E08                   mov  cx, [bp+8]
 11459                                  
 11460                                  ;-1052   __asm{cld} __asm{rep movsb} __asm{mov byte [es:di], 0}   }
 11461                                  
 11462 00005E38 FC                      cld
 11463 00005E39 F3A4                    rep movsb
 11464 00005E3B 26C60500                mov byte [es:di], 0
 11465                                  
 11466                                  ;-1053 int loadName(char *dest, int i) {
 11467 00005E3F C9                       LEAVE
 11468 00005E40 C3                       ret
 11469                                  ; ENDP
 11470                                  
 11471                                  loadName:  ; *** PROC ***
 11472                                  
 11473                                  ;-1054   segE;  _ es=ax; _ di=dest;  GName[i]; _ si=ax;
 11474                                  
 11475                                  ;Function : loadName, Number of local variables: 2
 11476                                  ;   # type sign width line used address name   list of local variables
 11477                                  ;  200 ptr sign byte  1053 NULL bp+4    dest
 11478                                  ;  201 var sign word  1053 NULL bp+6    i;
 11479 00005E41 C8000000                 ENTER  0,0
 11480 00005E45 A1[B800]                 mov ax, [segE]
 11481 00005E48 8EC0                     mov  es, ax
 11482 00005E4A 8B7E04                   mov  di, [bp+4]
 11483 00005E4D 8B5E06                   mov bx, [bp+6]
 11484 00005E50 D1E3                     shl bx, 1
 11485 00005E52 8B874B94                 mov ax, [GName + bx]
 11486 00005E56 89C6                     mov  si, ax
 11487                                  
 11488                                  ;-1055   do{ __asm{mov al, [es:si]} si++; *di=al;  di++; } while (al != 0); }
 11489                                  
 11490                                  @@loadName624:
 11491 00005E58 268A04                  mov al, [es:si]
 11492 00005E5B 46                       inc  si
 11493 00005E5C 8805                     mov  [di], al
 11494 00005E5E 47                       inc  di
 11495 00005E5F 3C00                     cmp  al, 0
 11496 00005E61 0F840200                 je  @@loadName625
 11497 00005E65 EBF1                     jmp @@loadName624
 11498                                  @@loadName625:
 11499                                  
 11500                                  ;-1056 int to_far(char *dest, char *src) {
 11501 00005E67 C9                       LEAVE
 11502 00005E68 C3                       ret
 11503                                  ; ENDP
 11504                                  
 11505                                  to_far:  ; *** PROC ***
 11506                                  
 11507                                  ;-1057   segE;  _ es=ax;  _ si=src;  _ di=dest;  // ds:si   es:di
 11508                                  
 11509                                  ;Function : to_far, Number of local variables: 2
 11510                                  ;   # type sign width line used address name   list of local variables
 11511                                  ;  200 ptr sign byte  1056 NULL bp+4    dest
 11512                                  ;  201 ptr sign byte  1056 NULL bp+6    src;
 11513 00005E69 C8000000                 ENTER  0,0
 11514 00005E6D A1[B800]                 mov ax, [segE]
 11515 00005E70 8EC0                     mov  es, ax
 11516 00005E72 8B7606                   mov  si, [bp+6]
 11517 00005E75 8B7E04                   mov  di, [bp+4]
 11518                                  
 11519                                  ;-1058   do{ al= *si; si++; __asm{mov [es:di], al} di++; } while (al != 0); }
 11520                                  
 11521                                  @@to_far626:
 11522 00005E78 8A04                     mov  al, [si]
 11523 00005E7A 46                       inc  si
 11524 00005E7B 268805                  mov [es:di], al
 11525 00005E7E 47                       inc  di
 11526 00005E7F 3C00                     cmp  al, 0
 11527 00005E81 0F840200                 je  @@to_far627
 11528 00005E85 EBF1                     jmp @@to_far626
 11529                                  @@to_far627:
 11530                                  
 11531                                  ;-1059 int from_far(char *dest, char *src) {
 11532 00005E87 C9                       LEAVE
 11533 00005E88 C3                       ret
 11534                                  ; ENDP
 11535                                  
 11536                                  from_far:  ; *** PROC ***
 11537                                  
 11538                                  ;-1060   segE;  _ es=ax;  _ si=src;  _ di=dest;  // ds:si   es:di
 11539                                  
 11540                                  ;Function : from_far, Number of local variables: 2
 11541                                  ;   # type sign width line used address name   list of local variables
 11542                                  ;  200 ptr sign byte  1059 NULL bp+4    dest
 11543                                  ;  201 ptr sign byte  1059 NULL bp+6    src;
 11544 00005E89 C8000000                 ENTER  0,0
 11545 00005E8D A1[B800]                 mov ax, [segE]
 11546 00005E90 8EC0                     mov  es, ax
 11547 00005E92 8B7606                   mov  si, [bp+6]
 11548 00005E95 8B7E04                   mov  di, [bp+4]
 11549                                  
 11550                                  ;-1061   do{ __asm{mov al, [es:si]} si++; *di=al;  di++; } while (al != 0); }
 11551                                  
 11552                                  @@from_far628:
 11553 00005E98 268A04                  mov al, [es:si]
 11554 00005E9B 46                       inc  si
 11555 00005E9C 8805                     mov  [di], al
 11556 00005E9E 47                       inc  di
 11557 00005E9F 3C00                     cmp  al, 0
 11558 00005EA1 0F840200                 je  @@from_far629
 11559 00005EA5 EBF1                     jmp @@from_far628
 11560                                  @@from_far629:
 11561                                  
 11562                                  ;-1062 
 11563                                  
 11564                                  
 11565                                  ;-1063 unsigned char GDT1[]={15,0,0,0,0,0,0,0, 0xFF,0xFF,0x00,0x00,0x00,0x92,0xCF,0};
 11566 00005EA7 C9                       LEAVE
 11567 00005EA8 C3                       ret
 11568                                  ; ENDP
 11569 00005EA9 0F00000000000000FF-     GDT1 db 15,0,0,0,0,0,0,0,255,255,0,0,0,146,207,0
 11570 00005EB2 FF00000092CF00     
 11571                                  
 11572                                  ;-1064 int isvirtual86() { eax=cr0;  ax &= 1; }
 11573                                  
 11574                                  
 11575                                  isvirtual86:  ; *** PROC ***
 11576 00005EB9 0F20C0                   mov  eax, cr0
 11577 00005EBC 83E001                   and  ax, 1
 11578                                  
 11579                                  ;-1065 int is32bit() { ecx=0xFFFF; __asm{jmp $ + 2;} ecx++; ifzero return 0; return 1;}
 11580                                  
 11581 00005EBF C3                       ret
 11582                                  ; ENDP
 11583                                  
 11584                                  is32bit:  ; *** PROC ***
 11585 00005EC0 66B9FFFF0000             mov  ecx, 65535
 11586 00005EC6 EB00                    jmp $ + 2;
 11587 00005EC8 6641                     inc  ecx
 11588 00005ECA 7509                     jne short @@is32bit630
 11589 00005ECC 66B800000000             mov eax, 0
 11590 00005ED2 E90900                   jmp @@retn
 11591                                  @@is32bit630:
 11592 00005ED5 66B801000000             mov eax, 1
 11593 00005EDB E90000                   jmp @@retn
 11594                                  
 11595                                  ;-1066 int INITCPU32() { eax=0;	ax=ds; eax << 4;
 11596                                  
 11597                                  @@retn:
 11598 00005EDE C3                       ret
 11599                                  ; ENDP
 11600                                  
 11601                                  INITCPU32:  ; *** PROC ***
 11602 00005EDF 66B800000000             mov  eax, 0
 11603 00005EE5 8CD8                     mov  ax, ds
 11604 00005EE7 66C1E004                 shl  eax, 4
 11605                                  
 11606                                  ;-1067 	asm add eax, GDT1       ;//Offset GDT
 11607                                  
 11608 00005EEB 6605[A95E0000]           add eax, GDT1       ;//Offset GDT
 11609                                  
 11610                                  ;-1068 	asm mov [GDT1+2], eax
 11611                                  
 11612 00005EF1 66A3[AB5E]               mov [GDT1+2], eax
 11613                                  
 11614                                  ;-1069 	asm lgdt [GDT1]
 11615                                  
 11616 00005EF5 0F0116[A95E]             lgdt [GDT1]
 11617                                  
 11618                                  ;-1070 	bx=8;
 11619                                  
 11620 00005EFA BB0800                   mov  bx, 8
 11621                                  
 11622                                  ;-1071 	asm push ds
 11623                                  
 11624 00005EFD 1E                       push ds
 11625                                  
 11626                                  ;-1072 	asm cli
 11627                                  
 11628 00005EFE FA                       cli
 11629                                  
 11630                                  ;-1073 	eax=cr0; al++; cr0=eax;
 11631                                  
 11632 00005EFF 0F20C0                   mov  eax, cr0
 11633 00005F02 FEC0                     inc  al
 11634 00005F04 0F22C0                   mov  cr0, eax
 11635                                  
 11636                                  ;-1074 	asm use32
 11637                                  
 11638                                   use32
 11639                                  
 11640                                  ;-1075 	__asm{jmp dword PROT_EN}   __asm{PROT_EN:}
 11641                                  
 11642 00005F07 E900000000              jmp dword PROT_EN
 11643                                  PROT_EN:
 11644                                  
 11645                                  ;-1076 	fs=bx;	al--;	cr0=eax;
 11646                                  
 11647 00005F0C 8EE3                     mov  fs, bx
 11648 00005F0E FEC8                     dec  al
 11649 00005F10 0F22C0                   mov  cr0, eax
 11650                                  
 11651                                  ;-1077 	asm use16
 11652                                  
 11653                                   use16
 11654                                  
 11655                                  ;-1078 	__asm{jmp word PROT_DI} __asm{PROT_DI:}
 11656                                  
 11657 00005F13 E90000                  jmp word PROT_DI
 11658                                  PROT_DI:
 11659                                  
 11660                                  ;-1079 	asm sti
 11661                                  
 11662 00005F16 FB                       sti
 11663                                  
 11664                                  ;-1080 	asm pop ds
 11665                                  
 11666 00005F17 1F                       pop ds
 11667                                  
 11668                                  ;-1081 }
 11669                                  
 11670                                  
 11671                                  ;-1082 
 11672                                  
 11673                                  
 11674                                  ;-1083 void to32  (long L,  char *src) { }
 11675                                  
 11676 00005F18 C3                       ret
 11677                                  ; ENDP
 11678                                  
 11679                                  to32:  ; *** PROC ***
 11680                                  ;Function : to32, Number of local variables: 2
 11681                                  ;   # type sign width line used address name   list of local variables
 11682                                  ;  200 var sign dwrd  1083 NULL bp+4    L
 11683                                  ;  201 ptr sign byte  1083 NULL bp+8    src;
 11684 00005F19 C8000000                 ENTER  0,0
 11685                                  
 11686                                  ;-1084 void from32(char *dest, long L) { }
 11687 00005F1D C9                       LEAVE
 11688 00005F1E C3                       ret
 11689                                  ; ENDP
 11690                                  
 11691                                  from32:  ; *** PROC ***
 11692                                  ;Function : from32, Number of local variables: 2
 11693                                  ;   # type sign width line used address name   list of local variables
 11694                                  ;  200 ptr sign byte  1084 NULL bp+4    dest
 11695                                  ;  201 var sign dwrd  1084 NULL bp+6    L;
 11696 00005F1F C8000000                 ENTER  0,0
 11697                                  
 11698                                  ;-1085 void prLr(unsigned long L) {           ultoar(L, Ls); prs(Ls); }//right
 11699 00005F23 C9                       LEAVE
 11700 00005F24 C3                       ret
 11701                                  ; ENDP
 11702                                  
 11703                                  prLr:  ; *** PROC ***
 11704                                  ;Function : prLr, Number of local variables: 1
 11705                                  ;   # type sign width line used address name   list of local variables
 11706                                  ;  200 var unsg dwrd  1085 NULL bp+4    L;
 11707 00005F25 C8000000                 ENTER  0,0
 11708 00005F29 8D06[6600]               lea  ax, [Ls]
 11709 00005F2D 50                       push ax
 11710 00005F2E 66FF7604                 push dword [bp+4]
 11711 00005F32 E87F00                   call ultoar
 11712 00005F35 83C406                   add  sp, 6
 11713 00005F38 8D06[6600]               lea  ax, [Ls]
 11714 00005F3C 50                       push ax
 11715 00005F3D E852EF                   call prs
 11716 00005F40 83C402                   add  sp, 2
 11717                                  
 11718                                  ;-1086 void prL (unsigned long L) {char *p; p=ultoaL(L, Ls); prs(p ); }//left
 11719 00005F43 C9                       LEAVE
 11720 00005F44 C3                       ret
 11721                                  ; ENDP
 11722                                  
 11723                                  prL:  ; *** PROC ***
 11724                                  ;Function : prL, Number of local variables: 2
 11725                                  ;   # type sign width line used address name   list of local variables
 11726                                  ;  200 var unsg dwrd  1086 NULL bp+4    L
 11727                                  ;  201 ptr sign byte  1086 NULL bp-2    p;
 11728 00005F45 C8020000                 ENTER  2,0
 11729 00005F49 8D06[6600]               lea  ax, [Ls]
 11730 00005F4D 50                       push ax
 11731 00005F4E 66FF7604                 push dword [bp+4]
 11732 00005F52 E8C100                   call ultoaL
 11733 00005F55 83C406                   add  sp, 6
 11734 00005F58 8946FE                   mov word [bp-2], ax
 11735 00005F5B FF76FE                   push word [bp-2]
 11736 00005F5E E831EF                   call prs
 11737 00005F61 83C402                   add  sp, 2
 11738                                  
 11739                                  ;-1087 void prf(unsigned long L, int w) {char len; char *p;            //format width
 11740 00005F64 C9                       LEAVE
 11741 00005F65 C3                       ret
 11742                                  ; ENDP
 11743                                  
 11744                                  prf:  ; *** PROC ***
 11745                                  
 11746                                  ;-1088   p=ultoaL(L, Ls); len=strlen(p); if (w > len) do {prc(32); w--;} while (w>len);
 11747                                  
 11748                                  ;Function : prf, Number of local variables: 4
 11749                                  ;   # type sign width line used address name   list of local variables
 11750                                  ;  200 var unsg dwrd  1087 NULL bp+4    L
 11751                                  ;  201 var sign word  1087 NULL bp+8    w
 11752                                  ;  202 var sign byte  1087 NULL bp-2    len
 11753                                  ;  203 ptr sign byte  1087 NULL bp-4    p;
 11754 00005F66 C8040000                 ENTER  4,0
 11755 00005F6A 8D06[6600]               lea  ax, [Ls]
 11756 00005F6E 50                       push ax
 11757 00005F6F 66FF7604                 push dword [bp+4]
 11758 00005F73 E8A000                   call ultoaL
 11759 00005F76 83C406                   add  sp, 6
 11760 00005F79 8946FC                   mov word [bp-4], ax
 11761 00005F7C FF76FC                   push word [bp-4]
 11762 00005F7F E8DF01                   call strlen
 11763 00005F82 83C402                   add  sp, 2
 11764 00005F85 8846FE                   mov byte [bp-2], al
 11765 00005F88 8B4608                   mov ax, [bp+8]
 11766 00005F8B 3A46FE                   cmp al, [bp-2]
 11767 00005F8E 0F8E1700                 jle @@prf631
 11768                                  @@prf632:
 11769 00005F92 6A20                     push 32
 11770 00005F94 E88CEE                   call prc
 11771 00005F97 83C402                   add  sp, 2
 11772 00005F9A FF4E08                   dec  word [bp+8]
 11773 00005F9D 8B4608                   mov ax, [bp+8]
 11774 00005FA0 3A46FE                   cmp al, [bp-2]
 11775 00005FA3 0F8E0200                 jle @@prf633
 11776 00005FA7 EBE9                     jmp @@prf632
 11777                                  @@prf633:
 11778                                  @@prf631:
 11779                                  
 11780                                  ;-1089   prs(p); }
 11781                                  
 11782 00005FA9 FF76FC                   push word [bp-4]
 11783 00005FAC E8E3EE                   call prs
 11784 00005FAF 83C402                   add  sp, 2
 11785                                  
 11786                                  ;-1090 void ultoar(unsigned long L, char *s) { int i;//edx:eax DIV ebx = eax Modulo edx
 11787 00005FB2 C9                       LEAVE
 11788 00005FB3 C3                       ret
 11789                                  ; ENDP
 11790                                  
 11791                                  ultoar:  ; *** PROC ***
 11792                                  
 11793                                  ;-1091   i=0;  while(i<10) { *s = '0'; s++; i++; }   *s = 0;   s--;  eax=L;
 11794                                  
 11795                                  ;Function : ultoar, Number of local variables: 3
 11796                                  ;   # type sign width line used address name   list of local variables
 11797                                  ;  200 var unsg dwrd  1090 NULL bp+4    L
 11798                                  ;  201 ptr sign byte  1090 NULL bp+8    s
 11799                                  ;  202 var sign word  1090 NULL bp-2    i;
 11800 00005FB4 C8020000                 ENTER  2,0
 11801 00005FB8 66B800000000             mov eax, 0
 11802 00005FBE 8946FE                   mov word [bp-2], ax
 11803                                  @@ultoar634:
 11804 00005FC1 8B46FE                   mov ax, [bp-2]
 11805 00005FC4 83F80A                   cmp ax, 10
 11806 00005FC7 0F8D1300                 jge @@ultoar635
 11807 00005FCB 66B830000000             mov eax, 48
 11808 00005FD1 8B5E08                   mov  bx, [bp+8]
 11809 00005FD4 8807                     mov  [bx], al
 11810 00005FD6 FF4608                   inc  word [bp+8]
 11811 00005FD9 FF46FE                   inc  word [bp-2]
 11812 00005FDC EBE3                     jmp @@ultoar634
 11813                                  @@ultoar635:
 11814 00005FDE 66B800000000             mov eax, 0
 11815 00005FE4 8B5E08                   mov  bx, [bp+8]
 11816 00005FE7 8807                     mov  [bx], al
 11817 00005FE9 FF4E08                   dec  word [bp+8]
 11818 00005FEC 668B4604                 mov  eax, [bp+4]
 11819                                  
 11820                                  ;-1092   do { edx=0;  ebx=10; ebx /= ;
 11821                                  
 11822                                  @@ultoar636:
 11823 00005FF0 66BA00000000             mov  edx, 0
 11824 00005FF6 66BB0A000000             mov  ebx, 10
 11825 00005FFC 66F7F3                   div  ebx
 11826                                  
 11827                                  ;-1093        dl+=48; bx=s;   *bx=dl;  s--; } while (eax != 0);  }
 11828                                  
 11829 00005FFF 80C230                   add  dl, 48
 11830 00006002 8B5E08                   mov  bx, [bp+8]
 11831 00006005 8817                     mov  [bx], dl
 11832 00006007 FF4E08                   dec  word [bp+8]
 11833 0000600A 6683F800                 cmp  eax, 0
 11834 0000600E 0F840200                 je  @@ultoar637
 11835 00006012 EBDC                     jmp @@ultoar636
 11836                                  @@ultoar637:
 11837                                  
 11838                                  ;-1094 int ultoaL(unsigned long L, char *s) { // edx:eax DIV ebx = eax Modulo edx
 11839 00006014 C9                       LEAVE
 11840 00006015 C3                       ret
 11841                                  ; ENDP
 11842                                  
 11843                                  ultoaL:  ; *** PROC ***
 11844                                  
 11845                                  ;-1095   s=s+10;  *s = 0;   s--;  eax=L;
 11846                                  
 11847                                  ;Function : ultoaL, Number of local variables: 2
 11848                                  ;   # type sign width line used address name   list of local variables
 11849                                  ;  200 var unsg dwrd  1094 NULL bp+4    L
 11850                                  ;  201 ptr sign byte  1094 NULL bp+8    s;
 11851 00006016 C8000000                 ENTER  0,0
 11852 0000601A 8B4608                   mov ax, [bp+8]
 11853 0000601D 83C00A                   add ax, 10
 11854 00006020 894608                   mov word [bp+8], ax
 11855 00006023 66B800000000             mov eax, 0
 11856 00006029 8B5E08                   mov  bx, [bp+8]
 11857 0000602C 8807                     mov  [bx], al
 11858 0000602E FF4E08                   dec  word [bp+8]
 11859 00006031 668B4604                 mov  eax, [bp+4]
 11860                                  
 11861                                  ;-1096   do { edx=0;  ebx=10; ebx /= ;
 11862                                  
 11863                                  @@ultoaL638:
 11864 00006035 66BA00000000             mov  edx, 0
 11865 0000603B 66BB0A000000             mov  ebx, 10
 11866 00006041 66F7F3                   div  ebx
 11867                                  
 11868                                  ;-1097        dl+=48; bx=s;   *bx=dl;  s--; } while (eax != 0);   s++; ax=s;
 11869                                  
 11870 00006044 80C230                   add  dl, 48
 11871 00006047 8B5E08                   mov  bx, [bp+8]
 11872 0000604A 8817                     mov  [bx], dl
 11873 0000604C FF4E08                   dec  word [bp+8]
 11874 0000604F 6683F800                 cmp  eax, 0
 11875 00006053 0F840200                 je  @@ultoaL639
 11876 00006057 EBDC                     jmp @@ultoaL638
 11877                                  @@ultoaL639:
 11878 00006059 FF4608                   inc  word [bp+8]
 11879 0000605C 8B4608                   mov  ax, [bp+8]
 11880                                  
 11881                                  ;-1098 }
 11882 0000605F C9                       LEAVE
 11883 00006060 C3                       ret
 11884                                  ; ENDP
 11885                                   
 11886                                  ; missing functions: 
 11887                                  ; strlen
 11888                                  ; strlen
 11889                                  ; strcpy
 11890                                  ; strlen
 11891                                  ; strcpy
 11892                                  ; eqstr
 11893                                  ; eqstr
 11894                                  ; eqstr
 11895                                  ; strcpy
 11896                                  ; mkneg
 11897                                  ; strlen
 11898                                  ; strcpy
 11899                                  ; strcpy
 11900                                  ; eqstr
 11901                                  ; eqstr
 11902                                  ; strcpy
 11903                                  ; strcpy
 11904                                  ; strcpy
 11905                                  ; strcpy
 11906                                  ; strcpy
 11907                                  ; strcpy
 11908                                  ; strcpy
 11909                                  ; strcpy
 11910                                  ; strcpy
 11911                                  ; strcpy
 11912                                  ; strcpy
 11913                                  ; strcpy
 11914                                  ; eqstr
 11915                                  ; strcpy
 11916                                  ; strcpy
 11917                                  ; strlen
 11918                                  ; eqstr
 11919                                  ; eqstr
 11920                                  ; eqstr
 11921                                  ; eqstr
 11922                                  ; eqstr
 11923                                  ; eqstr
 11924                                  ; eqstr
 11925                                  ; eqstr
 11926                                  ; eqstr
 11927                                  ; eqstr
 11928                                  ; eqstr
 11929                                  ; eqstr
 11930                                  ; eqstr
 11931                                  ; eqstr
 11932                                  ; eqstr
 11933                                  ; eqstr
 11934                                  ; eqstr
 11935                                  ; eqstr
 11936                                  ; eqstr
 11937                                  ; eqstr
 11938                                  ; eqstr
 11939                                  ; eqstr
 11940                                  ; strlen
 11941                                  ; eqstr
 11942                                  ; eqstr
 11943                                  ; eqstr
 11944                                  ; eqstr
 11945                                  ; eqstr
 11946                                  ; eqstr
 11947                                  ; eqstr
 11948                                  ; eqstr
 11949                                  ; eqstr
 11950                                  ; eqstr
 11951                                  ; strcpy
 11952                                  ; cputs
 11953                                  ; cputs
 11954                                  ; readRL
 11955                                  ; strcpy
 11956                                  ; instr2
 11957                                  ; strcat1
 11958                                  ; toupper
 11959                                  ; instr2
 11960                                  ; strcpy
 11961                                  ; instr2
 11962                                  ; strcpy
 11963                                  ; strlen
 11964                                  ; openR
 11965                                  ; cputs
 11966                                  ; cputs
 11967                                  ; exitR
 11968                                  ; creatR
 11969                                  ; cputs
 11970                                  ; cputs
 11971                                  ; exitR
 11972                                  ; cputs
 11973                                  ; cputs
 11974                                  ; cputs
 11975                                  ; cputs
 11976                                  ; cputs
 11977                                  ; cputs
 11978                                  ; cputs
 11979                                  ; cputs
 11980                                  ; eqstr
 11981                                  ; cputs
 11982                                  ; cputs
 11983                                  ; pint
 11984                                  ; openR
 11985                                  ; cputs
 11986                                  ; cputs
 11987                                  ; exitR
 11988                                  ; eqstr
 11989                                  ; putch
 11990                                  ; cputs
 11991                                  ; openR
 11992                                  ; cputs
 11993                                  ; eqstr
 11994                                  ; eqstr
 11995                                  ; eqstr
 11996                                  ; eqstr
 11997                                  ; eqstr
 11998                                  ; strcpy
 11999                                  ; instr1
 12000                                  ; digit
 12001                                  ; letter
 12002                                  ; strcpy
 12003                                  ; letter
 12004                                  ; eqstr
 12005                                  ; eqstr
 12006                                  ; eqstr
 12007                                  ; eqstr
 12008                                  ; eqstr
 12009                                  ; eqstr
 12010                                  ; eqstr
 12011                                  ; eqstr
 12012                                  ; eqstr
 12013                                  ; eqstr
 12014                                  ; eqstr
 12015                                  ; eqstr
 12016                                  ; eqstr
 12017                                  ; eqstr
 12018                                  ; eqstr
 12019                                  ; eqstr
 12020                                  ; eqstr
 12021                                  ; eqstr
 12022                                  ; eqstr
 12023                                  ; eqstr
 12024                                  ; eqstr
 12025                                  ; eqstr
 12026                                  ; strcpy
 12027                                  ; eqstr
 12028                                  ; letter
 12029                                  ; digit
 12030                                  ; writetty
 12031                                  ; writetty
 12032                                  ; fputcR
 12033                                  ; mkneg
 12034                                  ; mkneg
 12035                                  ; readR
 12036                                  ; fcloseR
 12037                                  ; fcloseR
 12038                                  ; exitR
 12039                                  ; eqstr
 12040                                  ; strcpy
 12041                                  ; DosInt
 12042                                  ; DosInt
 12043                                  ; strlen
 12044                                  ; Number of unresolved CALLs :  157
 12045                                  ;use archive file: AR.C
 12046                                  
 12047                                  ;-1099 // AR.C Archive file for PLA.DE The MIT License (MIT) starting 1.1.2015  (c) 2015 Helmut Guenther
 12048                                  
 12049                                  ;Number of CALLs:    2 : writetty
 12050                                  
 12051                                  writetty:  ; *** PROC ***
 12052 00006061 B40E                     mov  ah, 14
 12053 00006063 BB0000                   mov  bx, 0
 12054 00006066 CD10                     int  16
 12055                                  
 12056                                  ;-1100 int putch(char c)  {if (_ c==10) {al=13; writetty();} al=c; writetty(); }
 12057                                  
 12058 00006068 C3                       ret
 12059                                  ; ENDP
 12060                                  ;Number of CALLs:    1 : putch
 12061                                  
 12062                                  putch:  ; *** PROC ***
 12063                                  ;Function : putch, Number of local variables: 1
 12064                                  ;   # type sign width line used address name   list of local variables
 12065                                  ;  200 var sign byte  1100 NULL bp+4    c;
 12066 00006069 C8000000                 ENTER  0,0
 12067 0000606D 807E040A                cmp byte [bp+4], 10
 12068 00006071 0F850500                 jne @@putch640
 12069 00006075 B00D                     mov  al, 13
 12070 00006077 E8E7FF                   call writetty
 12071                                  @@putch640:
 12072 0000607A 8A4604                   mov  al, [bp+4]
 12073 0000607D E8E1FF                   call writetty
 12074                                  
 12075                                  ;-1101 int cputs(char *s) {char c;  while(*s) { c=*s; putch(c); s++; } }
 12076 00006080 C9                       LEAVE
 12077 00006081 C3                       ret
 12078                                  ; ENDP
 12079                                  ;Number of CALLs:   20 : cputs
 12080                                  
 12081                                  cputs:  ; *** PROC ***
 12082                                  ;Function : cputs, Number of local variables: 2
 12083                                  ;   # type sign width line used address name   list of local variables
 12084                                  ;  200 ptr sign byte  1101 NULL bp+4    s
 12085                                  ;  201 var sign byte  1101 NULL bp-2    c;
 12086 00006082 C8020000                 ENTER  2,0
 12087                                  @@cputs641:
 12088 00006086 8B5E04                   mov bx, [bp+4]
 12089 00006089 8A07                     mov al, [bx]
 12090 0000608B B400                     mov ah, 0
 12091 0000608D 08C0                     or  al, al
 12092 0000608F 0F841B00                 je @@cputs642
 12093 00006093 8B5E04                   mov bx, [bp+4]
 12094 00006096 8A07                     mov al, [bx]
 12095 00006098 B400                     mov ah, 0
 12096 0000609A 8846FE                   mov byte [bp-2], al
 12097 0000609D 8A46FE                   mov al, byte [bp-2]
 12098 000060A0 B400                     mov ah, 0
 12099 000060A2 50                       push ax
 12100 000060A3 E8C3FF                   call putch
 12101 000060A6 83C402                   add  sp, 2
 12102 000060A9 FF4604                   inc  word [bp+4]
 12103 000060AC EBD8                     jmp @@cputs641
 12104                                  @@cputs642:
 12105                                  
 12106                                  ;-1102 int mkneg(int n) { n; __asm {neg ax} }
 12107 000060AE C9                       LEAVE
 12108 000060AF C3                       ret
 12109                                  ; ENDP
 12110                                  ;Number of CALLs:    3 : mkneg
 12111                                  
 12112                                  mkneg:  ; *** PROC ***
 12113                                  ;Function : mkneg, Number of local variables: 1
 12114                                  ;   # type sign width line used address name   list of local variables
 12115                                  ;  200 var sign word  1102 NULL bp+4    n;
 12116 000060B0 C8000000                 ENTER  0,0
 12117 000060B4 8B4604                   mov ax, [bp+4]
 12118 000060B7 F7D8                    neg ax
 12119                                  
 12120                                  ;-1103 int prunsign(unsigned int n) { unsigned int e;
 12121 000060B9 C9                       LEAVE
 12122 000060BA C3                       ret
 12123                                  ; ENDP
 12124                                  ;Number of CALLs:    1 : pint
 12125                                  
 12126                                  pint:  ; *** PROC ***
 12127                                  ;Function : pint, Number of local variables: 2
 12128                                  ;   # type sign width line used address name   list of local variables
 12129                                  ;  200 var sign word  1103 NULL bp+4    n
 12130                                  ;  201 var sign word  1103 NULL bp-2    e;
 12131 000060BB C8020000                 ENTER  2,0
 12132 000060BF 8B4604                   mov ax, [bp+4]
 12133 000060C2 83F800                   cmp ax, 0
 12134 000060C5 0F8D1400                 jge @@pint643
 12135 000060C9 6A2D                     push 45
 12136 000060CB E855ED                   call prc
 12137 000060CE 83C402                   add  sp, 2
 12138 000060D1 FF7604                   push word [bp+4]
 12139 000060D4 E8D9FF                   call mkneg
 12140 000060D7 83C402                   add  sp, 2
 12141 000060DA 894604                   mov word [bp+4], ax
 12142                                  
 12143                                  ;-1104   if (n >= 10) {e=n/10;  pint(e);}  n=n%10; n=n+'0'; putch(n); }
 12144                                  
 12145                                  @@pint643:
 12146 000060DD 8B4604                   mov ax, [bp+4]
 12147 000060E0 83F80A                   cmp ax, 10 ;unsigned : 1
 12148 000060E3 0F8C1500                 jl  @@pint644
 12149 000060E7 8B4604                   mov ax, [bp+4]
 12150 000060EA BB0A00                   mov bx, 10
 12151 000060ED 99                       cwd
 12152 000060EE F7FB                     idiv bx
 12153 000060F0 8946FE                   mov word [bp-2], ax
 12154 000060F3 FF76FE                   push word [bp-2]
 12155 000060F6 E8C2FF                   call pint
 12156 000060F9 83C402                   add  sp, 2
 12157                                  @@pint644:
 12158 000060FC 8B4604                   mov ax, [bp+4]
 12159 000060FF BB0A00                   mov bx, 10
 12160 00006102 99                       cwd
 12161 00006103 F7FB                     idiv bx
 12162 00006105 89D0                     mov ax, dx
 12163 00006107 894604                   mov word [bp+4], ax
 12164 0000610A 8B4604                   mov ax, [bp+4]
 12165 0000610D 83C030                   add ax, 48
 12166 00006110 894604                   mov word [bp+4], ax
 12167 00006113 FF7604                   push word [bp+4]
 12168 00006116 E850FF                   call putch
 12169 00006119 83C402                   add  sp, 2
 12170                                  
 12171                                  ;-1105 int printint5(unsigned int j)  {
 12172 0000611C C9                       LEAVE
 12173 0000611D C3                       ret
 12174                                  ; ENDP
 12175                                  ;Number of CALLs:    1 : toupper
 12176                                  
 12177                                  toupper:  ; *** PROC ***
 12178                                  ;Function : toupper, Number of local variables: 1
 12179                                  ;   # type sign width line used address name   list of local variables
 12180                                  ;  200 ptr sign byte  1105 NULL bp+4    s;
 12181 0000611E C8000000                 ENTER  0,0
 12182                                  @@toupper645:
 12183 00006122 8B5E04                   mov bx, [bp+4]
 12184 00006125 8A07                     mov al, [bx]
 12185 00006127 B400                     mov ah, 0
 12186 00006129 08C0                     or  al, al
 12187 0000612B 0F843000                 je @@toupper646
 12188 0000612F 8B5E04                   mov bx, [bp+4]
 12189 00006132 8A07                     mov al, [bx]
 12190 00006134 B400                     mov ah, 0
 12191 00006136 83F861                   cmp ax, 97 ;unsigned : 1
 12192 00006139 0F8C1D00                 jl  @@toupper647
 12193 0000613D 8B5E04                   mov bx, [bp+4]
 12194 00006140 8A07                     mov al, [bx]
 12195 00006142 B400                     mov ah, 0
 12196 00006144 83F87A                   cmp ax, 122
 12197 00006147 0F8F0F00                 jg  @@toupper648
 12198 0000614B 8B5E04                   mov bx, [bp+4]
 12199 0000614E 8A07                     mov al, [bx]
 12200 00006150 B400                     mov ah, 0
 12201 00006152 83E820                   sub ax, 32
 12202 00006155 8B5E04                   mov  bx, [bp+4]
 12203 00006158 8807                     mov  [bx], al
 12204                                  @@toupper648:
 12205                                  @@toupper647:
 12206 0000615A FF4604                   inc  word [bp+4]
 12207 0000615D EBC3                     jmp @@toupper645
 12208                                  @@toupper646:
 12209                                  
 12210                                  ;-1106 //int head1  (char *s) {char c; do{c=*s; c=letter(c); s++; }while(c) *s=0; }
 12211                                  
 12212                                  
 12213                                  ;-1107 int strlen(char *s) { int c; c=0; while (*s!=0) {s++; c++;} return c; }
 12214 0000615F C9                       LEAVE
 12215 00006160 C3                       ret
 12216                                  ; ENDP
 12217                                  ;Number of CALLs:    8 : strlen
 12218                                  
 12219                                  strlen:  ; *** PROC ***
 12220                                  ;Function : strlen, Number of local variables: 2
 12221                                  ;   # type sign width line used address name   list of local variables
 12222                                  ;  200 ptr sign byte  1107 NULL bp+4    s
 12223                                  ;  201 var sign word  1107 NULL bp-2    c;
 12224 00006161 C8020000                 ENTER  2,0
 12225 00006165 66B800000000             mov eax, 0
 12226 0000616B 8946FE                   mov word [bp-2], ax
 12227                                  @@strlen649:
 12228 0000616E 8B5E04                   mov bx, [bp+4]
 12229 00006171 8A07                     mov al, [bx]
 12230 00006173 B400                     mov ah, 0
 12231 00006175 83F800                   cmp ax, 0
 12232 00006178 0F840800                 je  @@strlen650
 12233 0000617C FF4604                   inc  word [bp+4]
 12234 0000617F FF46FE                   inc  word [bp-2]
 12235 00006182 EBEA                     jmp @@strlen649
 12236                                  @@strlen650:
 12237 00006184 8B46FE                   mov ax, [bp-2]
 12238 00006187 E90000                   jmp @@retn
 12239                                  
 12240                                  ;-1108 int strcpy1(char *s, char *t) {do { *s=*t; s++; t++; } while (*t!=0); *s=0; }
 12241                                  
 12242 0000618A C9                      @@retn: LEAVE
 12243 0000618B C3                       ret
 12244                                  ; ENDP
 12245                                  ;Number of CALLs:   27 : strcpy
 12246                                  
 12247                                  strcpy:  ; *** PROC ***
 12248                                  ;Function : strcpy, Number of local variables: 2
 12249                                  ;   # type sign width line used address name   list of local variables
 12250                                  ;  200 ptr sign byte  1108 NULL bp+4    s
 12251                                  ;  201 ptr sign byte  1108 NULL bp+6    t;
 12252 0000618C C8000000                 ENTER  0,0
 12253                                  @@strcpy651:
 12254 00006190 8B5E06                   mov bx, [bp+6]
 12255 00006193 8A07                     mov al, [bx]
 12256 00006195 B400                     mov ah, 0
 12257 00006197 8B5E04                   mov  bx, [bp+4]
 12258 0000619A 8807                     mov  [bx], al
 12259 0000619C FF4604                   inc  word [bp+4]
 12260 0000619F FF4606                   inc  word [bp+6]
 12261                                  
 12262                                  ;-1109   while (*t!=0); *s=0; return s; }
 12263                                  
 12264 000061A2 8B5E06                   mov bx, [bp+6]
 12265 000061A5 8A07                     mov al, [bx]
 12266 000061A7 B400                     mov ah, 0
 12267 000061A9 83F800                   cmp ax, 0
 12268 000061AC 0F840200                 je  @@strcpy652
 12269 000061B0 EBDE                     jmp @@strcpy651
 12270                                  @@strcpy652:
 12271 000061B2 66B800000000             mov eax, 0
 12272 000061B8 8B5E04                   mov  bx, [bp+4]
 12273 000061BB 8807                     mov  [bx], al
 12274 000061BD 8B4604                   mov ax, [bp+4]
 12275 000061C0 E90000                   jmp @@retn
 12276                                  
 12277                                  ;-1110 int strncpy(char *s, char *t, int n) { if (_ n==0) return;
 12278                                  
 12279 000061C3 C9                      @@retn: LEAVE
 12280 000061C4 C3                       ret
 12281                                  ; ENDP
 12282                                  ;Number of CALLs:    1 : instr1
 12283                                  
 12284                                  instr1:  ; *** PROC ***
 12285                                  ;Function : instr1, Number of local variables: 2
 12286                                  ;   # type sign width line used address name   list of local variables
 12287                                  ;  200 ptr sign byte  1110 NULL bp+4    s
 12288                                  ;  201 var sign byte  1110 NULL bp+6    c;
 12289 000061C5 C8000000                 ENTER  0,0
 12290                                  @@instr1653:
 12291 000061C9 8B5E04                   mov bx, [bp+4]
 12292 000061CC 8A07                     mov al, [bx]
 12293 000061CE B400                     mov ah, 0
 12294 000061D0 08C0                     or  al, al
 12295 000061D2 0F841C00                 je @@instr1654
 12296 000061D6 8B5E04                   mov bx, [bp+4]
 12297 000061D9 8A07                     mov al, [bx]
 12298 000061DB B400                     mov ah, 0
 12299 000061DD 3A4606                   cmp al, [bp+6]
 12300 000061E0 0F850900                 jne @@instr1655
 12301 000061E4 66B801000000             mov eax, 1
 12302 000061EA E90E00                   jmp @@retn
 12303                                  @@instr1655:
 12304 000061ED FF4604                   inc  word [bp+4]
 12305 000061F0 EBD7                     jmp @@instr1653
 12306                                  @@instr1654:
 12307 000061F2 66B800000000             mov eax, 0
 12308 000061F8 E90000                   jmp @@retn
 12309                                  
 12310                                  ;-1111 int instr2(char *s, char c) { while(*s) { if (*s==c) return &s; s++;} return 0;}
 12311                                  
 12312 000061FB C9                      @@retn: LEAVE
 12313 000061FC C3                       ret
 12314                                  ; ENDP
 12315                                  ;Number of CALLs:    3 : instr2
 12316                                  
 12317                                  instr2:  ; *** PROC ***
 12318                                  ;Function : instr2, Number of local variables: 2
 12319                                  ;   # type sign width line used address name   list of local variables
 12320                                  ;  200 ptr sign byte  1111 NULL bp+4    s
 12321                                  ;  201 var sign byte  1111 NULL bp+6    c;
 12322 000061FD C8000000                 ENTER  0,0
 12323                                  @@instr2656:
 12324 00006201 8B5E04                   mov bx, [bp+4]
 12325 00006204 8A07                     mov al, [bx]
 12326 00006206 B400                     mov ah, 0
 12327 00006208 08C0                     or  al, al
 12328 0000620A 0F841900                 je @@instr2657
 12329 0000620E 8B5E04                   mov bx, [bp+4]
 12330 00006211 8A07                     mov al, [bx]
 12331 00006213 B400                     mov ah, 0
 12332 00006215 3A4606                   cmp al, [bp+6]
 12333 00006218 0F850600                 jne @@instr2658
 12334 0000621C 8B4604                   mov ax, [bp+4]
 12335 0000621F E90E00                   jmp @@retn
 12336                                  @@instr2658:
 12337 00006222 FF4604                   inc  word [bp+4]
 12338 00006225 EBDA                     jmp @@instr2656
 12339                                  @@instr2657:
 12340 00006227 66B800000000             mov eax, 0
 12341 0000622D E90000                   jmp @@retn
 12342                                  
 12343                                  ;-1112 int digit(char c){ if(c<'0') return 0; if(c>'9') return 0; return 1; }
 12344                                  
 12345 00006230 C9                      @@retn: LEAVE
 12346 00006231 C3                       ret
 12347                                  ; ENDP
 12348                                  ;Number of CALLs:    2 : digit
 12349                                  
 12350                                  digit:  ; *** PROC ***
 12351                                  ;Function : digit, Number of local variables: 1
 12352                                  ;   # type sign width line used address name   list of local variables
 12353                                  ;  200 var sign byte  1112 NULL bp+4    c;
 12354 00006232 C8000000                 ENTER  0,0
 12355 00006236 8A4604                   mov al, [bp+4]
 12356 00006239 3C30                     cmp al, 48
 12357 0000623B 0F8D0900                 jge @@digit659
 12358 0000623F 66B800000000             mov eax, 0
 12359 00006245 E91B00                   jmp @@retn
 12360                                  @@digit659:
 12361 00006248 8A4604                   mov al, [bp+4]
 12362 0000624B 3C39                     cmp al, 57
 12363 0000624D 0F8E0900                 jle @@digit660
 12364 00006251 66B800000000             mov eax, 0
 12365 00006257 E90900                   jmp @@retn
 12366                                  @@digit660:
 12367 0000625A 66B801000000             mov eax, 1
 12368 00006260 E90000                   jmp @@retn
 12369                                  
 12370                                  ;-1113 int letter(char c) { if (digit(c)) return 1; if (c=='_') return 1;
 12371                                  
 12372 00006263 C9                      @@retn: LEAVE
 12373 00006264 C3                       ret
 12374                                  ; ENDP
 12375                                  ;Number of CALLs:    3 : letter
 12376                                  
 12377                                  letter:  ; *** PROC ***
 12378                                  ;Function : letter, Number of local variables: 1
 12379                                  ;   # type sign width line used address name   list of local variables
 12380                                  ;  200 var sign byte  1113 NULL bp+4    c;
 12381 00006265 C8000000                 ENTER  0,0
 12382 00006269 8A4604                   mov al, byte [bp+4]
 12383 0000626C B400                     mov ah, 0
 12384 0000626E 50                       push ax
 12385 0000626F E8C0FF                   call digit
 12386 00006272 83C402                   add  sp, 2
 12387 00006275 08C0                     or  al, al
 12388 00006277 0F840900                 je @@letter661
 12389 0000627B 66B801000000             mov eax, 1
 12390 00006281 E95A00                   jmp @@retn
 12391                                  @@letter661:
 12392 00006284 8A4604                   mov al, [bp+4]
 12393 00006287 3C5F                     cmp al, 95
 12394 00006289 0F850900                 jne @@letter662
 12395 0000628D 66B801000000             mov eax, 1
 12396 00006293 E94800                   jmp @@retn
 12397                                  
 12398                                  ;-1114   if (c> 'z') return 0; if (c< '@') return 0;
 12399                                  
 12400                                  @@letter662:
 12401 00006296 8A4604                   mov al, [bp+4]
 12402 00006299 3C7A                     cmp al, 122
 12403 0000629B 0F8E0900                 jle @@letter663
 12404 0000629F 66B800000000             mov eax, 0
 12405 000062A5 E93600                   jmp @@retn
 12406                                  @@letter663:
 12407 000062A8 8A4604                   mov al, [bp+4]
 12408 000062AB 3C40                     cmp al, 64
 12409 000062AD 0F8D0900                 jge @@letter664
 12410 000062B1 66B800000000             mov eax, 0
 12411 000062B7 E92400                   jmp @@retn
 12412                                  
 12413                                  ;-1115   if (c> 'Z') { if (c< 'a') return 0; }  return 1; }
 12414                                  
 12415                                  @@letter664:
 12416 000062BA 8A4604                   mov al, [bp+4]
 12417 000062BD 3C5A                     cmp al, 90
 12418 000062BF 0F8E1200                 jle @@letter665
 12419 000062C3 8A4604                   mov al, [bp+4]
 12420 000062C6 3C61                     cmp al, 97
 12421 000062C8 0F8D0900                 jge @@letter666
 12422 000062CC 66B800000000             mov eax, 0
 12423 000062D2 E90900                   jmp @@retn
 12424                                  @@letter666:
 12425                                  @@letter665:
 12426 000062D5 66B801000000             mov eax, 1
 12427 000062DB E90000                   jmp @@retn
 12428                                  
 12429                                  ;-1116 int alnum(char c) { if (digit(c)) return 1; if (c=='_') return 1;
 12430                                  
 12431 000062DE C9                      @@retn: LEAVE
 12432 000062DF C3                       ret
 12433                                  ; ENDP
 12434                                  ;Number of CALLs:   69 : eqstr
 12435                                  
 12436                                  eqstr:  ; *** PROC ***
 12437                                  ;Function : eqstr, Number of local variables: 2
 12438                                  ;   # type sign width line used address name   list of local variables
 12439                                  ;  200 ptr sign byte  1116 NULL bp+4    p
 12440                                  ;  201 ptr sign byte  1116 NULL bp+6    q;
 12441 000062E0 C8000000                 ENTER  0,0
 12442                                  @@eqstr667:
 12443 000062E4 8B5E04                   mov bx, [bp+4]
 12444 000062E7 8A07                     mov al, [bx]
 12445 000062E9 B400                     mov ah, 0
 12446 000062EB 08C0                     or  al, al
 12447 000062ED 0F842300                 je @@eqstr668
 12448                                  
 12449                                  ;-1117     if (*p != *q) return 0; p++; q++; }
 12450                                  
 12451 000062F1 8B5E04                   mov bx, [bp+4]
 12452 000062F4 8A07                     mov al, [bx]
 12453 000062F6 B400                     mov ah, 0
 12454 000062F8 8B5E06                   mov bx, [bp+6]
 12455 000062FB 3A07                     cmp al, [bx]
 12456 000062FD B400                     mov ah, 0
 12457 000062FF 0F840900                 je  @@eqstr669
 12458 00006303 66B800000000             mov eax, 0
 12459 00006309 E92700                   jmp @@retn
 12460                                  @@eqstr669:
 12461 0000630C FF4604                   inc  word [bp+4]
 12462 0000630F FF4606                   inc  word [bp+6]
 12463                                  
 12464                                  ;-1118     if(*q) return 0; return 1; }
 12465                                  
 12466 00006312 EBD0                     jmp @@eqstr667
 12467                                  @@eqstr668:
 12468 00006314 8B5E06                   mov bx, [bp+6]
 12469 00006317 8A07                     mov al, [bx]
 12470 00006319 B400                     mov ah, 0
 12471 0000631B 08C0                     or  al, al
 12472 0000631D 0F840900                 je @@eqstr670
 12473 00006321 66B800000000             mov eax, 0
 12474 00006327 E90900                   jmp @@retn
 12475                                  @@eqstr670:
 12476 0000632A 66B801000000             mov eax, 1
 12477 00006330 E90000                   jmp @@retn
 12478                                  
 12479                                  ;-1119 int strcat1(char *s, char *t) { while (*s != 0) s++; strcpy(s, t);  }
 12480                                  
 12481 00006333 C9                      @@retn: LEAVE
 12482 00006334 C3                       ret
 12483                                  ; ENDP
 12484                                  ;Number of CALLs:    1 : strcat1
 12485                                  
 12486                                  strcat1:  ; *** PROC ***
 12487                                  ;Function : strcat1, Number of local variables: 2
 12488                                  ;   # type sign width line used address name   list of local variables
 12489                                  ;  200 ptr sign byte  1119 NULL bp+4    s
 12490                                  ;  201 ptr sign byte  1119 NULL bp+6    t;
 12491 00006335 C8000000                 ENTER  0,0
 12492                                  @@strcat1671:
 12493 00006339 8B5E04                   mov bx, [bp+4]
 12494 0000633C 8A07                     mov al, [bx]
 12495 0000633E B400                     mov ah, 0
 12496 00006340 83F800                   cmp ax, 0
 12497 00006343 0F840500                 je  @@strcat1672
 12498 00006347 FF4604                   inc  word [bp+4]
 12499 0000634A EBED                     jmp @@strcat1671
 12500                                  @@strcat1672:
 12501 0000634C FF7606                   push word [bp+6]
 12502 0000634F FF7604                   push word [bp+4]
 12503 00006352 E837FE                   call strcpy
 12504 00006355 83C404                   add  sp, 4
 12505                                  
 12506                                  ;-1120 int basename(char *s) { char *p; p=0;
 12507 00006358 C9                       LEAVE
 12508 00006359 C3                       ret
 12509                                  ; ENDP
 12510                                  ;Number of CALLs:    2 : DosInt
 12511                                  
 12512                                  DosInt:  ; *** PROC ***
 12513 0000635A CD21                     int  33
 12514 0000635C 7304                     jae short @@DosInt673
 12515 0000635E FF06[C200]               inc  word [DOS_ERR]
 12516                                  @@DosInt673:
 12517                                  
 12518                                  ;-1121 int openR (char *s) { dx=s; ax=0x3D02;  DosInt(); }
 12519                                  
 12520 00006362 C3                       ret
 12521                                  ; ENDP
 12522                                  ;Number of CALLs:    3 : openR
 12523                                  
 12524                                  openR:  ; *** PROC ***
 12525                                  ;Function : openR, Number of local variables: 1
 12526                                  ;   # type sign width line used address name   list of local variables
 12527                                  ;  200 ptr sign byte  1121 NULL bp+4    s;
 12528 00006363 C8000000                 ENTER  0,0
 12529 00006367 8B5604                   mov  dx, [bp+4]
 12530 0000636A B8023D                   mov  ax, 15618
 12531 0000636D E8EAFF                   call DosInt
 12532                                  
 12533                                  ;-1122 int creatR(char *s) { dx=s; cx=0; ax=0x3C00; DosInt(); }
 12534 00006370 C9                       LEAVE
 12535 00006371 C3                       ret
 12536                                  ; ENDP
 12537                                  ;Number of CALLs:    1 : creatR
 12538                                  
 12539                                  creatR:  ; *** PROC ***
 12540                                  ;Function : creatR, Number of local variables: 1
 12541                                  ;   # type sign width line used address name   list of local variables
 12542                                  ;  200 ptr sign byte  1122 NULL bp+4    s;
 12543 00006372 C8000000                 ENTER  0,0
 12544 00006376 8B5604                   mov  dx, [bp+4]
 12545 00006379 B90000                   mov  cx, 0
 12546 0000637C B8003C                   mov  ax, 15360
 12547 0000637F E8D8FF                   call DosInt
 12548                                  
 12549                                  ;-1123 int readR (char *s, int fd) {dx=s; cx=1; bx=fd; ax=0x3F00; DosInt(); }
 12550 00006382 C9                       LEAVE
 12551 00006383 C3                       ret
 12552                                  ; ENDP
 12553                                  ;Number of CALLs:    1 : readR
 12554                                  
 12555                                  readR:  ; *** PROC ***
 12556                                  ;Function : readR, Number of local variables: 2
 12557                                  ;   # type sign width line used address name   list of local variables
 12558                                  ;  200 ptr sign byte  1123 NULL bp+4    s
 12559                                  ;  201 var sign word  1123 NULL bp+6    fd;
 12560 00006384 C8000000                 ENTER  0,0
 12561 00006388 8B5604                   mov  dx, [bp+4]
 12562 0000638B B90100                   mov  cx, 1
 12563 0000638E 8B5E06                   mov  bx, [bp+6]
 12564 00006391 B8003F                   mov  ax, 16128
 12565 00006394 E8C3FF                   call DosInt
 12566                                  
 12567                                  ;-1124 int readRL(char *s, int fd, int len){dx=s; cx=len; bx=fd; ax=0x3F00; DosInt();}
 12568 00006397 C9                       LEAVE
 12569 00006398 C3                       ret
 12570                                  ; ENDP
 12571                                  ;Number of CALLs:    1 : readRL
 12572                                  
 12573                                  readRL:  ; *** PROC ***
 12574                                  ;Function : readRL, Number of local variables: 3
 12575                                  ;   # type sign width line used address name   list of local variables
 12576                                  ;  200 ptr sign byte  1124 NULL bp+4    s
 12577                                  ;  201 var sign word  1124 NULL bp+6    fd
 12578                                  ;  202 var sign word  1124 NULL bp+8    len;
 12579 00006399 C8000000                 ENTER  0,0
 12580 0000639D 8B5604                   mov  dx, [bp+4]
 12581 000063A0 8B4E08                   mov  cx, [bp+8]
 12582 000063A3 8B5E06                   mov  bx, [bp+6]
 12583 000063A6 B8003F                   mov  ax, 16128
 12584 000063A9 E8AEFF                   call DosInt
 12585                                  
 12586                                  ;-1125 int fputcR(char *n, int fd) { __asm{lea dx, [bp+4]}; /* = *n */
 12587 000063AC C9                       LEAVE
 12588 000063AD C3                       ret
 12589                                  ; ENDP
 12590                                  ;Number of CALLs:    1 : fputcR
 12591                                  
 12592                                  fputcR:  ; *** PROC ***
 12593                                  ;Function : fputcR, Number of local variables: 2
 12594                                  ;   # type sign width line used address name   list of local variables
 12595                                  ;  200 ptr sign byte  1125 NULL bp+4    n
 12596                                  ;  201 var sign word  1125 NULL bp+6    fd;
 12597 000063AE C8000000                 ENTER  0,0
 12598 000063B2 8D5604                  lea dx, [bp+4]
 12599                                  
 12600                                  ;-1126   cx=1; bx=fd; ax=0x4000; DosInt(); }
 12601                                  
 12602 000063B5 B90100                   mov  cx, 1
 12603 000063B8 8B5E06                   mov  bx, [bp+6]
 12604 000063BB B80040                   mov  ax, 16384
 12605 000063BE E899FF                   call DosInt
 12606                                  
 12607                                  ;-1127 int writeRL(char *s, int fd, int len){dx=s; cx=len; bx=fd; ax=0x4000; DosInt();}
 12608 000063C1 C9                       LEAVE
 12609 000063C2 C3                       ret
 12610                                  ; ENDP
 12611                                  ;Number of CALLs:    2 : fcloseR
 12612                                  
 12613                                  fcloseR:  ; *** PROC ***
 12614                                  ;Function : fcloseR, Number of local variables: 1
 12615                                  ;   # type sign width line used address name   list of local variables
 12616                                  ;  200 var sign word  1127 NULL bp+4    fd;
 12617 000063C3 C8000000                 ENTER  0,0
 12618 000063C7 8B5E04                   mov  bx, [bp+4]
 12619 000063CA B8003E                   mov  ax, 15872
 12620 000063CD CD21                     int  33
 12621                                  
 12622                                  ;-1128 int exitR(char c) {ah=0x4C; al=c; inth 0x21; }
 12623 000063CF C9                       LEAVE
 12624 000063D0 C3                       ret
 12625                                  ; ENDP
 12626                                  ;Number of CALLs:    4 : exitR
 12627                                  
 12628                                  exitR:  ; *** PROC ***
 12629                                  ;Function : exitR, Number of local variables: 1
 12630                                  ;   # type sign width line used address name   list of local variables
 12631                                  ;  200 var sign byte  1128 NULL bp+4    c;
 12632 000063D1 C8000000                 ENTER  0,0
 12633 000063D5 B44C                     mov  ah, 76
 12634 000063D7 8A4604                   mov  al, [bp+4]
 12635 000063DA CD21                     int  33
 12636                                  
 12637                                  ;-1129 int setdta(char *s) {dx=s; ah=0x1A; inth 0x21; }
 12638 000063DC C9                       LEAVE
 12639 000063DD C3                       ret
 12640                                  ; ENDP
 12641                                  ;End of archive file. 
 12642                                   
 12643                                  ; missing functions:  All FUNCTIONs in place
 12644 000063DE E80000                  LastFunctionByt:db 0E8h, 0, 0
 12645 000063E1 58                      pop ax
 12646 000063E2 C3                      ret
 12647                                   
 12648                                  ;   # type sign width line used  address name   list of global variables
 12649                                  
 12650                                  ;    1 arr sign byte     2    2        0 Version1[16]
 12651                                  ;    2 var sign byte     6    3        0 BSS
 12652                                  ;    3 var sign byte     6   17        0 NASM
 12653                                  ;    4 var sign byte     6    3        0 PROTECTED
 12654                                  ;    5 var sign byte     6    6        0 LIST
 12655                                  ;    6 def unsg byte     7    -        0 LSTART=200
 12656                                  ;    7 def unsg byte     8    -        0 VARMAX=300
 12657                                  ;    8 def unsg byte     9    -        0 FUNCMAX=300
 12658                                  ;    9 def unsg byte    10    -        0 CALLMAX=2000
 12659                                  ;   10 def unsg byte    11    -        0 IDLENMAX=16
 12660                                  ;   11 def unsg byte    13    -        0 T_NAME=256
 12661                                  ;   12 def unsg byte    14    -        0 T_CONST=257
 12662                                  ;   13 def unsg byte    15    -        0 T_STRING=258
 12663                                  ;   14 def unsg byte    16    -        0 T_INCLUDE=510
 12664                                  ;   15 def unsg byte    17    -        0 T_DEFINE=511
 12665                                  ;   16 def unsg byte    18    -        0 T_RETURN=512
 12666                                  ;   17 def unsg byte    19    -        0 T_IF=513
 12667                                  ;   18 def unsg byte    20    -        0 T_ELSE=514
 12668                                  ;   19 def unsg byte    21    -        0 T_WHILE=515
 12669                                  ;   20 def unsg byte    22    -        0 T_DO=516
 12670                                  ;   21 def unsg byte    23    -        0 T_INT=517
 12671                                  ;   22 def unsg byte    24    -        0 T_ASM=518
 12672                                  ;   23 def unsg byte    25    -        0 T_ASMBLOCK=519
 12673                                  ;   24 def unsg byte    26    -        0 T_EMIT=520
 12674                                  ;   25 def unsg byte    27    -        0 T_GOTO=521
 12675                                  ;   26 def unsg byte    28    -        0 T_VOID=529
 12676                                  ;   27 def unsg byte    29    -        0 T_CHAR=530
 12677                                  ;   28 def unsg byte    30    -        0 T_SIGNED=531
 12678                                  ;   29 def unsg byte    31    -        0 T_UNSIGNED=532
 12679                                  ;   30 def unsg byte    32    -        0 T_LONG=533
 12680                                  ;   31 def unsg byte    33    -        0 T_SHORT=534
 12681                                  ;   32 def unsg byte    34    -        0 T_UINT32=545
 12682                                  ;   33 def unsg byte    35    -        0 T_INTH=600
 12683                                  ;   34 def unsg byte    36    -        0 T_IFCARRY=601
 12684                                  ;   35 def unsg byte    37    -        0 T_IFZERO=602
 12685                                  ;   36 def unsg byte    38    -        0 T_EQ=806
 12686                                  ;   37 def unsg byte    39    -        0 T_NE=807
 12687                                  ;   38 def unsg byte    40    -        0 T_GE=811
 12688                                  ;   39 def unsg byte    41    -        0 T_LE=824
 12689                                  ;   40 def unsg byte    42    -        0 T_PLUSPLUS=1219
 12690                                  ;   41 def unsg byte    43    -        0 T_MINUSMINUS=1225
 12691                                  ;   42 def unsg byte    44    -        0 T_PLUSASS=1230
 12692                                  ;   43 def unsg byte    45    -        0 T_MINUSASS=1231
 12693                                  ;   44 def unsg byte    46    -        0 T_MULASS=1232
 12694                                  ;   45 def unsg byte    47    -        0 T_DIVASS=1233
 12695                                  ;   46 def unsg byte    48    -        0 T_ANDASS=1234
 12696                                  ;   47 def unsg byte    49    -        0 T_ORASS=1235
 12697                                  ;   48 def unsg byte    50    -        0 T_LESSLESS=1240
 12698                                  ;   49 def unsg byte    51    -        0 T_GREATGREAT=1241
 12699                                  ;   50 def unsg byte    52    -        0 COMAX=3000
 12700                                  ;   51 def unsg byte    53    -        0 CMDLENMAX=67
 12701                                  ;   52 var unsg dwrd    54 NULL        0 LDATAORIG
 12702                                  ;   53 var unsg dwrd    54    5        0 ldata
 12703                                  ;   54 var sign dwrd    54    2        0 LONG0
 12704                                  ;   55 var unsg dwrd    55    4        0 ORGDATAORIG
 12705                                  ;   56 var unsg dwrd    55   12        0 orgData
 12706                                  ;   57 arr sign byte    56    4    30000 co[3000]
 12707                                  ;   58 var sign word    56    4        0 maxco
 12708                                  ;   59 var sign word    56    3        0 maxco1
 12709                                  ;   60 arr sign byte    57    2    33000 coname[67]
 12710                                  ;   61 ptr sign byte    57    5        0 cloc
 12711                                  ;   62 arr sign byte    58    2    33067 locname[67]
 12712                                  ;   63 var sign word    58    3        0 maxloc
 12713                                  ;   64 var sign word    59    7        0 fdin
 12714                                  ;   65 var sign word    59    7        0 fdout
 12715                                  ;   66 arr sign byte    59   96    33134 symbol[128]
 12716                                  ;   67 arr sign byte    60   13    33262 fname[67]
 12717                                  ;   68 arr sign byte    60   14    33329 namein[67]
 12718                                  ;   69 arr sign byte    61    7    33396 namelst[67]
 12719                                  ;   70 arr sign byte    61    5        0 archivename[4]
 12720                                  ;   71 var sign word    62   55        0 token
 12721                                  ;   72 var sign byte    62    2        0 globC
 12722                                  ;   73 var sign word    62    3        0 spalte
 12723                                  ;   74 var sign byte    63   28        0 thechar
 12724                                  ;   75 var sign word    63    9        0 iscmp
 12725                                  ;   76 var sign word    63    4        0 nconst
 12726                                  ;   77 var sign word    64    3        0 nreturn
 12727                                  ;   78 var sign word    64   16        0 nlabel
 12728                                  ;   79 var sign word    64    3        0 callrecursive
 12729                                  ;   80 var unsg dwrd    65   55        0 lexvalL
 12730                                  ;   81 var sign byte    65    2        0 islong
 12731                                  ;   82 var sign byte    65 NULL        0 isldata
 12732                                  ;   83 var sign word    66    7        0 typei
 12733                                  ;   84 var sign byte    66   10        0 istype
 12734                                  ;   85 arr sign byte    66   12    33463 GType[300]
 12735                                  ;   86 var sign word    67    7        0 signi
 12736                                  ;   87 var sign byte    67    8        0 issign
 12737                                  ;   88 arr sign byte    67    6    33763 GSign[300]
 12738                                  ;   89 var sign word    68   10        0 widthi
 12739                                  ;   90 var sign word    68   28        0 wi
 12740                                  ;   91 var sign byte    68   30        0 iswidth
 12741                                  ;   92 arr sign byte    68    6    34063 GWidth[300]
 12742                                  ;   93 var sign word    69   38        0 GTop
 12743                                  ;   94 var sign word    69   21        0 LTop
 12744                                  ;   95 arr sign word    70    5    34363 GLino[300]
 12745                                  ;   96 arr sign word    71    7    34963 GUsed[300]
 12746                                  ;   97 arr sign dwrd    72   12    35563 GData[300]
 12747                                  ;   98 arr sign dwrd    73    5    36763 GAdr[300]
 12748                                  ;   99 arr sign word    74    7    37963 GName[300]
 12749                                  ;  100 var sign word    75   13        0 FTop
 12750                                  ;  101 arr sign byte    76    2    38563 FWidth[300]
 12751                                  ;  102 arr sign word    77    2    38863 FLino[300]
 12752                                  ;  103 arr sign word    78    4    39463 FCalls[300]
 12753                                  ;  104 arr sign word    79    5    40063 FName[300]
 12754                                  ;  105 var sign word    80    9        0 CTop
 12755                                  ;  106 arr sign word    81    5    40663 CName[2000]
 12756                                  ;  107 arr sign byte    82    5    44663 CUnres[2000]
 12757                                  ;  108 arr sign byte    83    4        0 Ls[11]
 12758                                  ;  109 arr sign byte    84   27        0 NA[32]
 12759                                  ;  110 arr sign byte    85    4        0 NB[32]
 12760                                  ;  111 arr sign byte    86    5    46663 fgetsdest[128]
 12761                                  ;  112 ptr unsg byte    86   13        0 CNameTop
 12762                                  ;  113 ptr unsg byte    87   10        0 fgetsp
 12763                                  ;  114 var unsg word    87    5        0 segE
 12764                                  ;  115 var unsg word    88   13        0 lineno
 12765                                  ;  116 var unsg word    88    2        0 linenoinclude
 12766                                  ;  117 ptr unsg byte    89    6        0 pt
 12767                                  ;  118 ptr unsg byte    89    8        0 p1
 12768                                  ;  119 var sign word    90   10        0 DOS_ERR
 12769                                  ;  120 var sign word    90    5        0 DOS_NoBytes
 12770                                  ;  121 var sign byte    90    3        0 DOS_ByteRead
 12771                                  ;  122 var sign dwrd    92 NULL  2000000 varL1
 12772                                  ;  123 var sign word    92 NULL  2000004 vari1
 12773                                  ;  124 arr unsg word    92 NULL  2000006 vari2[6]
 12774                                  ;  125 var sign byte    93 NULL  2000012 varc1
 12775                                  ;  126 var sign dwrd    93 NULL  2000013 varL2
 12776                                  ;  127 arr sign dwrd    94 NULL  2000017 arrL1[12]
 12777                                  ;  128 var sign dwrd    94 NULL  2000029 varL4
 12778                                  ;  129 var sign byte    94 NULL  2000033 chc2
 12779                                  ;  130 arr sign byte   109    2    46791 doglobName[16]
 12780                                  ;  131 var sign word   289    1        0 sd
 12781                                  ;  132 var sign byte   290   13        0 un
 12782                                  ;  133 var sign byte   291 NULL        0 ty
 12783                                  ;  134 arr sign byte   292 NULL    46807 md[3]
 12784                                  ;  135 var sign word   292    6        0 mod1
 12785                                  ;  136 var sign word   292    7        0 mod2
 12786                                  ;  137 arr sign word   293 NULL    46810 ir[3]
 12787                                  ;  138 var sign word   293   14        0 irg1
 12788                                  ;  139 var sign word   293    6        0 irg2
 12789                                  ;  140 arr sign word   294 NULL    46816 ix[3]
 12790                                  ;  141 var sign word   294    7        0 idx1
 12791                                  ;  142 var sign word   294    6        0 idx2
 12792                                  ;  143 arr sign byte   295 NULL    46822 is[3]
 12793                                  ;  144 var sign word   295    2        0 ids1
 12794                                  ;  145 var sign word   295    3        0 ids2
 12795                                  ;  146 arr sign byte   296 NULL    46825 iw[3]
 12796                                  ;  147 var sign word   296    3        0 idw1
 12797                                  ;  148 var sign word   296    6        0 idw2
 12798                                  ;  149 arr sign byte   297 NULL    46828 it[3]
 12799                                  ;  150 var sign word   297    2        0 idt1
 12800                                  ;  151 var sign word   297    3        0 idt2
 12801                                  ;  152 arr sign dwrd   298 NULL    46831 ia[3]
 12802                                  ;  153 var sign word   298 NULL        0 ida1
 12803                                  ;  154 var sign word   298 NULL        0 ida2
 12804                                  ;  155 arr sign byte   299 NULL    46843 ic[3]
 12805                                  ;  156 var sign word   299 NULL        0 idac1
 12806                                  ;  157 var sign word   299 NULL        0 idac2
 12807                                  ;  158 arr sign dwrd   300 NULL    46846 iv[3]
 12808                                  ;  159 var sign dwrd   300 NULL        0 val1
 12809                                  ;  160 var sign dwrd   300    2        0 val2
 12810                                  ;  161 arr sign byte   367   17    46858 ops[6]
 12811                                  ;  162 arr sign byte   446    3        0 printregstr[74]
 12812                                  ;  163 arr sign word   572    2    46864 docalltype[10]
 12813                                  ;  164 arr sign word   572    2    46884 docallvalue[10]
 12814                                  ;  165 arr sign byte   573    2    46904 procname[16]
 12815                                  ;  166 ptr sign byte   626    1        0 arglen
 12816                                  ;  167 ptr sign byte   626    3        0 argv
 12817                                  ;  168 var sign byte   696    4        0 wasfunction
 12818                                  ;  169 arr sign byte   827    2    46920 symboltemp[80]
 12819                                  ;  170 var unsg word  1010    2        0 MAXUI
 12820                                  ;  171 arr unsg byte  1063 NULL        0 GDT1[16]
 12821                                  
 12822                                  
 12823                                  ;   # Calls Line Width  Name   list of functions
 12824                                  
 12825                                  ;    0 NULL   95 void   ttt
 12826                                  ;    1    4   97 void   a
 12827                                  ;    2   19   98 void   v
 12828                                  ;    3    6  100 void   checknamelen
 12829                                  ;    4   12  102 void   pwi
 12830                                  ;    5    6  106 void   storeName
 12831                                  ;    6    1  110 void   doLdata
 12832                                  ;    7    1  133 void   doglob
 12833                                  ;    8   13  185 word   gettypes
 12834                                  ;    9    4  195 void   printName
 12835                                  ;   10   11  199 word   searchname
 12836                                  ;   11    4  203 word   checkName
 12837                                  ;   12   10  208 word   typeName
 12838                                  ;   13    3  218 void   name1
 12839                                  ;   14    1  220 void   storecall
 12840                                  ;   15    2  227 void   storefunc
 12841                                  ;   16    2  234 void   addlocal
 12842                                  ;   17    1  241 word   checkFunction
 12843                                  ;   18    2  247 void   dofunc
 12844                                  ;   19    1  283 word   isvariable
 12845                                  ;   20    3  303 word   pexpr
 12846                                  ;   21    1  310 void   constantexpr
 12847                                  ;   22    3  320 void   exprstart
 12848                                  ;   23    1  322 word   simplexpr
 12849                                  ;   24    1  368 word   getop
 12850                                  ;   25    4  388 word   prleftreg
 12851                                  ;   26 NULL  389 word   prleftvar
 12852                                  ;   27 NULL  391 word   printvar
 12853                                  ;   28    3  393 word   doreg1
 12854                                  ;   29    6  424 word   checkreg
 12855                                  ;   30    9  449 void   printreg
 12856                                  ;   31    6  456 word   printpri
 12857                                  ;   32    2  462 word   expr
 12858                                  ;   33    4  507 word   compoundass
 12859                                  ;   34    2  513 word   dovar1
 12860                                  ;   35    8  530 word   rterm
 12861                                  ;   36    1  540 word   doassign
 12862                                  ;   37    1  552 word   domul
 12863                                  ;   38    2  557 word   doidiv
 12864                                  ;   39    1  570 word   domod
 12865                                  ;   40    2  575 void   docall1
 12866                                  ;   41 NULL  618 word   main
 12867                                  ;   42    3  625 word   getfirstchar
 12868                                  ;   43    1  628 word   getarg
 12869                                  ;   44    1  670 word   ltrim
 12870                                  ;   45    2  672 word   parse
 12871                                  ;   46    2  682 word   checkcalls
 12872                                  ;   47    1  697 word   doar
 12873                                  ;   48    1  717 word   getfunctionhead
 12874                                  ;   49    1  724 void   doinclude
 12875                                  ;   50    1  738 word   dodefine
 12876                                  ;   51    8  760 word   stmt
 12877                                  ;   52    1  784 word   doemit
 12878                                  ;   53    3  788 word   cmpneg
 12879                                  ;   54    9  801 word   prlabel
 12880                                  ;   55    3  802 word   prjump
 12881                                  ;   56    1  803 word   doif
 12882                                  ;   57    1  808 word   doifcarry
 12883                                  ;   58    1  811 word   doifzero
 12884                                  ;   59    1  814 word   dodo
 12885                                  ;   60    1  818 word   dowhile
 12886                                  ;   61    4  821 word   isrelational
 12887                                  ;   62   23  828 word   getlex
 12888                                  ;   63    1  885 word   convertdefine
 12889                                  ;   64    1  891 dwrd   getdigit
 12890                                  ;   65    1  904 word   getstring
 12891                                  ;   66   31  907 word   next
 12892                                  ;   67   89  908 word   istoken
 12893                                  ;   68   40  909 word   expect
 12894                                  ;   69    6  912 word   eprc
 12895                                  ;   70    5  913 word   eprs
 12896                                  ;   71   49  914 word   prc
 12897                                  ;   72    3  916 word   prscomment
 12898                                  ;   73    8  918 word   prnl
 12899                                  ;   74  276  920 word   prs
 12900                                  ;   75    2  924 word   eprnum
 12901                                  ;   76   19  926 word   pint1
 12902                                  ;   77   13  928 word   prunsign1
 12903                                  ;   78   29  931 word   printint51
 12904                                  ;   79    2  935 word   fgets1
 12905                                  ;   80    1  939 word   printinputline
 12906                                  ;   81    2  947 word   end1
 12907                                  ;   82   51  948 word   error1
 12908                                  ;   83    2  957 word   listproc
 12909                                  ;   84    2  965 word   listvar
 12910                                  ;   85    1  983 word   listcall
 12911                                  ;   86    1  985 word   calllisting
 12912                                  ;   87    1  991 word   countcalls
 12913                                  ;   88    1  996 word   listfunc
 12914                                  ;   89    1 1000 word   funclisting
 12915                                  ;   90    1 1011 word   epilog
 12916                                  ;   91    1 1045 word   memresize
 12917                                  ;   92    1 1047 word   memalloc
 12918                                  ;   93 NULL 1050 word   copyF
 12919                                  ;   94    5 1053 word   loadName
 12920                                  ;   95    1 1056 word   to_far
 12921                                  ;   96   10 1059 word   from_far
 12922                                  ;   97    4 1064 word   isvirtual86
 12923                                  ;   98    4 1065 word   is32bit
 12924                                  ;   99    1 1066 word   INITCPU32
 12925                                  ;  100 NULL 1083 void   to32
 12926                                  ;  101 NULL 1084 void   from32
 12927                                  ;  102 NULL 1085 void   prLr
 12928                                  ;  103   19 1086 void   prL
 12929                                  ;  104    1 1087 void   prf
 12930                                  ;  105    1 1090 void   ultoar
 12931                                  ;  106    2 1094 word   ultoaL
 12932                                  ;  107    4 1099 word   writetty
 12933                                  ;  108    3 1100 word   putch
 12934                                  ;  109   20 1101 word   cputs
 12935                                  ;  110    4 1102 word   mkneg
 12936                                  ;  111    2 1103 word   pint
 12937                                  ;  112    1 1105 word   toupper
 12938                                  ;  113    8 1107 word   strlen
 12939                                  ;  114   28 1108 word   strcpy
 12940                                  ;  115    1 1110 word   instr1
 12941                                  ;  116    3 1111 word   instr2
 12942                                  ;  117    3 1112 word   digit
 12943                                  ;  118    3 1113 word   letter
 12944                                  ;  119   69 1116 word   eqstr
 12945                                  ;  120    1 1119 word   strcat1
 12946                                  ;  121    7 1120 word   DosInt
 12947                                  ;  122    3 1121 word   openR
 12948                                  ;  123    1 1122 word   creatR
 12949                                  ;  124    1 1123 word   readR
 12950                                  ;  125    1 1124 word   readRL
 12951                                  ;  126    1 1125 word   fputcR
 12952                                  ;  127    2 1127 word   fcloseR
 12953                                  ;  128    4 1128 word   exitR
 12954                                  ;  129 NULL 1129 word   LastFunctionByt
 12955                                  
 12956                                  ;    #  addr name   list of CALLs
 12957                                  
 12958                                  ;    0   951 prs
 12959                                  ;    1   955 printName
 12960                                  ;    2   969 prc
 12961                                  ;    3   973 printName
 12962                                  ;    4   983 prc
 12963                                  ;    5  1002 strlen
 12964                                  ;    6  1009 error1
 12965                                  ;    7  1022 prs
 12966                                  ;    8  1026 prs
 12967                                  ;    9  1030 prs
 12968                                  ;   10  1034 prs
 12969                                  ;   11  1050 strlen
 12970                                  ;   12  1057 to_far
 12971                                  ;   13  1083 error1
 12972                                  ;   14  1090 error1
 12973                                  ;   15  1097 checknamelen
 12974                                  ;   16  1110 checkName
 12975                                  ;   17  1120 error1
 12976                                  ;   18  1127 istoken
 12977                                  ;   19  1135 expect
 12978                                  ;   20  1142 expect
 12979                                  ;   21  1149 prs
 12980                                  ;   22  1153 prL
 12981                                  ;   23  1157 prs
 12982                                  ;   24  1161 prs
 12983                                  ;   25  1165 prs
 12984                                  ;   26  1169 prL
 12985                                  ;   27  1173 prc
 12986                                  ;   28  1177 storeName
 12987                                  ;   29  1187 expect
 12988                                  ;   30  1214 error1
 12989                                  ;   31  1221 error1
 12990                                  ;   32  1228 checknamelen
 12991                                  ;   33  1241 checkName
 12992                                  ;   34  1251 error1
 12993                                  ;   35  1258 istoken
 12994                                  ;   36  1266 istoken
 12995                                  ;   37  1274 prs
 12996                                  ;   38  1278 prunsign1
 12997                                  ;   39  1288 prs
 12998                                  ;   40  1292 prunsign1
 12999                                  ;   41  1302 prs
 13000                                  ;   42  1306 prs
 13001                                  ;   43  1310 prs
 13002                                  ;   44  1314 prs
 13003                                  ;   45  1318 prs
 13004                                  ;   46  1322 prs
 13005                                  ;   47  1326 prs
 13006                                  ;   48  1330 prs
 13007                                  ;   49  1334 prL
 13008                                  ;   50  1338 prs
 13009                                  ;   51  1342 prs
 13010                                  ;   52  1346 prs
 13011                                  ;   53  1350 expect
 13012                                  ;   54  1357 expect
 13013                                  ;   55  1364 error1
 13014                                  ;   56  1371 prs
 13015                                  ;   57  1375 prs
 13016                                  ;   58  1379 prs
 13017                                  ;   59  1383 strcpy
 13018                                  ;   60  1390 expect
 13019                                  ;   61  1397 istoken
 13020                                  ;   62  1405 prc
 13021                                  ;   63  1409 prscomment
 13022                                  ;   64  1420 prc
 13023                                  ;   65  1424 prs
 13024                                  ;   66  1428 strlen
 13025                                  ;   67  1435 istoken
 13026                                  ;   68  1443 prc
 13027                                  ;   69  1447 expect
 13028                                  ;   70  1454 prL
 13029                                  ;   71  1458 istoken
 13030                                  ;   72  1466 expect
 13031                                  ;   73  1473 error1
 13032                                  ;   74  1480 prs
 13033                                  ;   75  1484 prs
 13034                                  ;   76  1488 prs
 13035                                  ;   77  1492 prs
 13036                                  ;   78  1496 prs
 13037                                  ;   79  1500 prs
 13038                                  ;   80  1504 istoken
 13039                                  ;   81  1512 prc
 13040                                  ;   82  1516 istoken
 13041                                  ;   83  1524 expect
 13042                                  ;   84  1531 prL
 13043                                  ;   85  1535 prunsign1
 13044                                  ;   86  1545 strcpy
 13045                                  ;   87  1552 storeName
 13046                                  ;   88  1562 expect
 13047                                  ;   89  1598 loadName
 13048                                  ;   90  1607 prs
 13049                                  ;   91  1611 prs
 13050                                  ;   92  1615 prc
 13051                                  ;   93  1619 pint1
 13052                                  ;   94  1625 prc
 13053                                  ;   95  1642 checkName
 13054                                  ;   96  1652 error1
 13055                                  ;   97  1673 loadName
 13056                                  ;   98  1682 eqstr
 13057                                  ;   99  1688 loadName
 13058                                  ;  100  1697 eqstr
 13059                                  ;  101  1714 istoken
 13060                                  ;  102  1722 istoken
 13061                                  ;  103  1730 istoken
 13062                                  ;  104  1738 istoken
 13063                                  ;  105  1746 istoken
 13064                                  ;  106  1754 istoken
 13065                                  ;  107  1762 istoken
 13066                                  ;  108  1770 istoken
 13067                                  ;  109  1778 istoken
 13068                                  ;  110  1786 istoken
 13069                                  ;  111  1794 name1
 13070                                  ;  112  1806 error1
 13071                                  ;  113  1813 getlex
 13072                                  ;  114  1832 error1
 13073                                  ;  115  1839 error1
 13074                                  ;  116  1846 storeName
 13075                                  ;  117  1868 error1
 13076                                  ;  118  1875 storeName
 13077                                  ;  119  1894 error1
 13078                                  ;  120  1901 checkName
 13079                                  ;  121  1911 error1
 13080                                  ;  122  1918 storeName
 13081                                  ;  123  1946 from_far
 13082                                  ;  124  1955 eqstr
 13083                                  ;  125  1980 checknamelen
 13084                                  ;  126  1993 strcpy
 13085                                  ;  127  2000 checkFunction
 13086                                  ;  128  2014 error1
 13087                                  ;  129  2021 storefunc
 13088                                  ;  130  2031 prs
 13089                                  ;  131  2035 prs
 13090                                  ;  132  2039 prs
 13091                                  ;  133  2043 expect
 13092                                  ;  134  2050 istoken
 13093                                  ;  135  2058 typeName
 13094                                  ;  136  2067 addlocal
 13095                                  ;  137  2076 istoken
 13096                                  ;  138  2084 expect
 13097                                  ;  139  2091 expect
 13098                                  ;  140  2098 isvariable
 13099                                  ;  141  2109 typeName
 13100                                  ;  142  2118 checknamelen
 13101                                  ;  143  2131 addlocal
 13102                                  ;  144  2140 istoken
 13103                                  ;  145  2148 expect
 13104                                  ;  146  2155 expect
 13105                                  ;  147  2162 istoken
 13106                                  ;  148  2170 expect
 13107                                  ;  149  2177 listproc
 13108                                  ;  150  2186 prs
 13109                                  ;  151  2190 mkneg
 13110                                  ;  152  2196 pint1
 13111                                  ;  153  2202 prs
 13112                                  ;  154  2206 istoken
 13113                                  ;  155  2214 stmt
 13114                                  ;  156  2219 prs
 13115                                  ;  157  2223 prs
 13116                                  ;  158  2227 prs
 13117                                  ;  159  2231 prs
 13118                                  ;  160  2235 prs
 13119                                  ;  161  2239 strlen
 13120                                  ;  162  2246 strcpy
 13121                                  ;  163  2253 strcpy
 13122                                  ;  164  2405 expect
 13123                                  ;  165  2412 eqstr
 13124                                  ;  166  2418 constantexpr
 13125                                  ;  167  2431 checkreg
 13126                                  ;  168  2440 doreg1
 13127                                  ;  169  2447 exprstart
 13128                                  ;  170  2457 prs
 13129                                  ;  171  2461 prs
 13130                                  ;  172  2465 expect
 13131                                  ;  173  2498 getlex
 13132                                  ;  174  2505 typeName
 13133                                  ;  175  2514 searchname
 13134                                  ;  176  2525 gettypes
 13135                                  ;  177  2534 isrelational
 13136                                  ;  178  2547 error1
 13137                                  ;  179  2554 expect
 13138                                  ;  180  2561 prs
 13139                                  ;  181  2565 gettypes
 13140                                  ;  182  2574 pwi
 13141                                  ;  183  2578 v
 13142                                  ;  184  2580 prs
 13143                                  ;  185  2584 prL
 13144                                  ;  186  2588 cmpneg
 13145                                  ;  187  2595 prs
 13146                                  ;  188  2599 expect
 13147                                  ;  189  2616 eqstr
 13148                                  ;  190  2622 simplexpr
 13149                                  ;  191  2632 expr
 13150                                  ;  192  2649 getlex
 13151                                  ;  193  2656 istoken
 13152                                  ;  194  2664 prs
 13153                                  ;  195  2668 prL
 13154                                  ;  196  2672 typeName
 13155                                  ;  197  2681 docall1
 13156                                  ;  198  2689 error1
 13157                                  ;  199  2696 checkreg
 13158                                  ;  200  2705 doreg1
 13159                                  ;  201  2712 searchname
 13160                                  ;  202  2723 gettypes
 13161                                  ;  203  2732 error1
 13162                                  ;  204  2739 isrelational
 13163                                  ;  205  2752 error1
 13164                                  ;  206  2759 istoken
 13165                                  ;  207  2767 error1
 13166                                  ;  208  2774 istoken
 13167                                  ;  209  2782 prs
 13168                                  ;  210  2786 pwi
 13169                                  ;  211  2790 v
 13170                                  ;  212  2792 prs
 13171                                  ;  213  2796 prL
 13172                                  ;  214  2800 prs
 13173                                  ;  215  2804 loadName
 13174                                  ;  216  2813 prs
 13175                                  ;  217  2817 typeName
 13176                                  ;  218  2826 checkreg
 13177                                  ;  219  2835 prs
 13178                                  ;  220  2839 printreg
 13179                                  ;  221  2848 pwi
 13180                                  ;  222  2852 v
 13181                                  ;  223  2854 prs
 13182                                  ;  224  2858 printreg
 13183                                  ;  225  2867 error1
 13184                                  ;  226  2874 searchname
 13185                                  ;  227  2885 gettypes
 13186                                  ;  228  2894 error1
 13187                                  ;  229  2901 prs
 13188                                  ;  230  2905 printreg
 13189                                  ;  231  2914 error1
 13190                                  ;  232  2921 prs
 13191                                  ;  233  2925 a
 13192                                  ;  234  2927 pwi
 13193                                  ;  235  2931 v
 13194                                  ;  236  2933 prs
 13195                                  ;  237  2937 loadName
 13196                                  ;  238  2946 prs
 13197                                  ;  239  2962 istoken
 13198                                  ;  240  2970 strcpy
 13199                                  ;  241  2977 istoken
 13200                                  ;  242  2985 strcpy
 13201                                  ;  243  2992 istoken
 13202                                  ;  244  3000 strcpy
 13203                                  ;  245  3007 istoken
 13204                                  ;  246  3015 strcpy
 13205                                  ;  247  3022 istoken
 13206                                  ;  248  3030 strcpy
 13207                                  ;  249  3037 istoken
 13208                                  ;  250  3045 strcpy
 13209                                  ;  251  3052 istoken
 13210                                  ;  252  3060 strcpy
 13211                                  ;  253  3067 istoken
 13212                                  ;  254  3075 strcpy
 13213                                  ;  255  3082 istoken
 13214                                  ;  256  3090 strcpy
 13215                                  ;  257  3097 istoken
 13216                                  ;  258  3105 strcpy
 13217                                  ;  259  3112 istoken
 13218                                  ;  260  3120 strcpy
 13219                                  ;  261  3137 prnl
 13220                                  ;  262  3142 prs
 13221                                  ;  263  3146 prs
 13222                                  ;  264  3150 printreg
 13223                                  ;  265  3204 getop
 13224                                  ;  266  3210 getlex
 13225                                  ;  267  3217 isrelational
 13226                                  ;  268  3230 error1
 13227                                  ;  269  3237 strcpy
 13228                                  ;  270  3244 prleftreg
 13229                                  ;  271  3254 istoken
 13230                                  ;  272  3262 prleftreg
 13231                                  ;  273  3272 prs
 13232                                  ;  274  3276 prL
 13233                                  ;  275  3280 typeName
 13234                                  ;  276  3289 checkreg
 13235                                  ;  277  3298 prleftreg
 13236                                  ;  278  3308 prs
 13237                                  ;  279  3312 printreg
 13238                                  ;  280  3321 searchname
 13239                                  ;  281  3332 gettypes
 13240                                  ;  282  3341 eqstr
 13241                                  ;  283  3347 strcpy
 13242                                  ;  284  3354 strcpy
 13243                                  ;  285  3361 prleftreg
 13244                                  ;  286  3371 prs
 13245                                  ;  287  3375 pwi
 13246                                  ;  288  3379 a
 13247                                  ;  289  3381 pwi
 13248                                  ;  290  3385 v
 13249                                  ;  291  3387 cmpneg
 13250                                  ;  292  3394 prs
 13251                                  ;  293  3398 expect
 13252                                  ;  294  3414 strlen
 13253                                  ;  295  3421 eqstr
 13254                                  ;  296  3427 eqstr
 13255                                  ;  297  3433 eqstr
 13256                                  ;  298  3439 eqstr
 13257                                  ;  299  3445 eqstr
 13258                                  ;  300  3451 eqstr
 13259                                  ;  301  3457 eqstr
 13260                                  ;  302  3463 eqstr
 13261                                  ;  303  3469 eqstr
 13262                                  ;  304  3475 eqstr
 13263                                  ;  305  3481 eqstr
 13264                                  ;  306  3487 eqstr
 13265                                  ;  307  3493 eqstr
 13266                                  ;  308  3499 eqstr
 13267                                  ;  309  3505 eqstr
 13268                                  ;  310  3511 eqstr
 13269                                  ;  311  3517 eqstr
 13270                                  ;  312  3523 eqstr
 13271                                  ;  313  3529 eqstr
 13272                                  ;  314  3535 eqstr
 13273                                  ;  315  3541 eqstr
 13274                                  ;  316  3547 eqstr
 13275                                  ;  317  3553 strlen
 13276                                  ;  318  3560 eqstr
 13277                                  ;  319  3566 eqstr
 13278                                  ;  320  3572 eqstr
 13279                                  ;  321  3578 eqstr
 13280                                  ;  322  3584 eqstr
 13281                                  ;  323  3590 eqstr
 13282                                  ;  324  3596 eqstr
 13283                                  ;  325  3602 eqstr
 13284                                  ;  326  3608 eqstr
 13285                                  ;  327  3646 prc
 13286                                  ;  328  3650 prc
 13287                                  ;  329  3654 prc
 13288                                  ;  330  3658 prc
 13289                                  ;  331  3662 prc
 13290                                  ;  332  3677 printreg
 13291                                  ;  333  3686 printreg
 13292                                  ;  334  3695 printreg
 13293                                  ;  335  3748 istoken
 13294                                  ;  336  3756 prs
 13295                                  ;  337  3760 prL
 13296                                  ;  338  3764 typeName
 13297                                  ;  339  3773 checkreg
 13298                                  ;  340  3782 doreg1
 13299                                  ;  341  3789 docall1
 13300                                  ;  342  3797 searchname
 13301                                  ;  343  3808 gettypes
 13302                                  ;  344  3817 istoken
 13303                                  ;  345  3825 istoken
 13304                                  ;  346  3833 expect
 13305                                  ;  347  3840 searchname
 13306                                  ;  348  3851 expect
 13307                                  ;  349  3858 expect
 13308                                  ;  350  3865 gettypes
 13309                                  ;  351  3874 error1
 13310                                  ;  352  3881 istoken
 13311                                  ;  353  3889 error1
 13312                                  ;  354  3896 prs
 13313                                  ;  355  3900 pwi
 13314                                  ;  356  3904 v
 13315                                  ;  357  3906 istoken
 13316                                  ;  358  3914 error1
 13317                                  ;  359  3921 prs
 13318                                  ;  360  3925 pwi
 13319                                  ;  361  3929 v
 13320                                  ;  362  3931 istoken
 13321                                  ;  363  3939 compoundass
 13322                                  ;  364  3951 istoken
 13323                                  ;  365  3959 compoundass
 13324                                  ;  366  3971 istoken
 13325                                  ;  367  3979 compoundass
 13326                                  ;  368  3991 istoken
 13327                                  ;  369  3999 compoundass
 13328                                  ;  370  4011 istoken
 13329                                  ;  371  4019 error1
 13330                                  ;  372  4026 istoken
 13331                                  ;  373  4034 error1
 13332                                  ;  374  4041 istoken
 13333                                  ;  375  4049 expr
 13334                                  ;  376  4054 doassign
 13335                                  ;  377  4063 dovar1
 13336                                  ;  378  4070 istoken
 13337                                  ;  379  4078 rterm
 13338                                  ;  380  4084 istoken
 13339                                  ;  381  4092 rterm
 13340                                  ;  382  4098 istoken
 13341                                  ;  383  4106 rterm
 13342                                  ;  384  4112 istoken
 13343                                  ;  385  4120 rterm
 13344                                  ;  386  4126 istoken
 13345                                  ;  387  4134 rterm
 13346                                  ;  388  4140 istoken
 13347                                  ;  389  4148 rterm
 13348                                  ;  390  4154 istoken
 13349                                  ;  391  4162 domul
 13350                                  ;  392  4168 istoken
 13351                                  ;  393  4176 doidiv
 13352                                  ;  394  4183 istoken
 13353                                  ;  395  4191 domod
 13354                                  ;  396  4197 isrelational
 13355                                  ;  397  4210 rterm
 13356                                  ;  398  4216 cmpneg
 13357                                  ;  399  4247 error1
 13358                                  ;  400  4254 prnl
 13359                                  ;  401  4259 prs
 13360                                  ;  402  4263 prs
 13361                                  ;  403  4267 gettypes
 13362                                  ;  404  4276 pwi
 13363                                  ;  405  4280 v
 13364                                  ;  406  4282 prs
 13365                                  ;  407  4286 expect
 13366                                  ;  408  4293 prL
 13367                                  ;  409  4322 gettypes
 13368                                  ;  410  4331 prs
 13369                                  ;  411  4335 v
 13370                                  ;  412  4337 prnl
 13371                                  ;  413  4342 prs
 13372                                  ;  414  4346 prs
 13373                                  ;  415  4350 prs
 13374                                  ;  416  4354 prs
 13375                                  ;  417  4358 prnl
 13376                                  ;  418  4363 prs
 13377                                  ;  419  4367 prs
 13378                                  ;  420  4371 a
 13379                                  ;  421  4373 prs
 13380                                  ;  422  4377 v
 13381                                  ;  423  4379 prs
 13382                                  ;  424  4383 prs
 13383                                  ;  425  4387 prs
 13384                                  ;  426  4391 prs
 13385                                  ;  427  4395 prc
 13386                                  ;  428  4399 printpri
 13387                                  ;  429  4408 prs
 13388                                  ;  430  4412 prc
 13389                                  ;  431  4416 printName
 13390                                  ;  432  4426 prs
 13391                                  ;  433  4430 prnl
 13392                                  ;  434  4435 prs
 13393                                  ;  435  4439 prc
 13394                                  ;  436  4443 printpri
 13395                                  ;  437  4452 prs
 13396                                  ;  438  4456 v
 13397                                  ;  439  4488 istoken
 13398                                  ;  440  4496 prnl
 13399                                  ;  441  4501 prs
 13400                                  ;  442  4505 prc
 13401                                  ;  443  4509 printpri
 13402                                  ;  444  4518 prs
 13403                                  ;  445  4522 prL
 13404                                  ;  446  4526 typeName
 13405                                  ;  447  4535 searchname
 13406                                  ;  448  4546 istoken
 13407                                  ;  449  4554 searchname
 13408                                  ;  450  4565 expect
 13409                                  ;  451  4572 expect
 13410                                  ;  452  4579 gettypes
 13411                                  ;  453  4588 error1
 13412                                  ;  454  4595 eqstr
 13413                                  ;  455  4601 dovar1
 13414                                  ;  456  4638 gettypes
 13415                                  ;  457  4647 prs
 13416                                  ;  458  4651 v
 13417                                  ;  459  4653 prs
 13418                                  ;  460  4657 printpri
 13419                                  ;  461  4666 prs
 13420                                  ;  462  4670 a
 13421                                  ;  463  4672 prs
 13422                                  ;  464  4676 prs
 13423                                  ;  465  4680 prunsign1
 13424                                  ;  466  4690 v
 13425                                  ;  467  4692 prs
 13426                                  ;  468  4696 prs
 13427                                  ;  469  4700 prs
 13428                                  ;  470  4704 printName
 13429                                  ;  471  4714 prs
 13430                                  ;  472  4718 printpri
 13431                                  ;  473  4727 prs
 13432                                  ;  474  4731 pwi
 13433                                  ;  475  4735 v
 13434                                  ;  476  4737 prs
 13435                                  ;  477  4741 printpri
 13436                                  ;  478  4760 rterm
 13437                                  ;  479  4766 istoken
 13438                                  ;  480  4774 prs
 13439                                  ;  481  4778 prL
 13440                                  ;  482  4782 prs
 13441                                  ;  483  4786 error1
 13442                                  ;  484  4813 istoken
 13443                                  ;  485  4821 prs
 13444                                  ;  486  4825 prL
 13445                                  ;  487  4829 prs
 13446                                  ;  488  4833 prs
 13447                                  ;  489  4837 typeName
 13448                                  ;  490  4846 searchname
 13449                                  ;  491  4857 error1
 13450                                  ;  492  4864 gettypes
 13451                                  ;  493  4873 error1
 13452                                  ;  494  4880 error1
 13453                                  ;  495  4887 prs
 13454                                  ;  496  4891 v
 13455                                  ;  497  4893 prs
 13456                                  ;  498  4897 prs
 13457                                  ;  499  4911 doidiv
 13458                                  ;  500  4918 prs
 13459                                  ;  501  4980 checknamelen
 13460                                  ;  502  4993 strcpy
 13461                                  ;  503  5000 storecall
 13462                                  ;  504  5010 expect
 13463                                  ;  505  5017 istoken
 13464                                  ;  506  5025 error1
 13465                                  ;  507  5032 istoken
 13466                                  ;  508  5040 istoken
 13467                                  ;  509  5048 eprs
 13468                                  ;  510  5053 eprs
 13469                                  ;  511  5058 eprc
 13470                                  ;  512  5063 eprnum
 13471                                  ;  513  5070 eprs
 13472                                  ;  514  5075 eprc
 13473                                  ;  515  5080 eprs
 13474                                  ;  516  5085 eprc
 13475                                  ;  517  5090 eprs
 13476                                  ;  518  5095 istoken
 13477                                  ;  519  5103 name1
 13478                                  ;  520  5109 searchname
 13479                                  ;  521  5120 istoken
 13480                                  ;  522  5128 checkreg
 13481                                  ;  523  5137 searchname
 13482                                  ;  524  5148 error1
 13483                                  ;  525  5155 istoken
 13484                                  ;  526  5163 expect
 13485                                  ;  527  5170 prs
 13486                                  ;  528  5174 pint1
 13487                                  ;  529  5180 prs
 13488                                  ;  530  5184 prs
 13489                                  ;  531  5188 prs
 13490                                  ;  532  5192 prc
 13491                                  ;  533  5196 pint1
 13492                                  ;  534  5202 prs
 13493                                  ;  535  5206 prs
 13494                                  ;  536  5210 v
 13495                                  ;  537  5212 prs
 13496                                  ;  538  5216 gettypes
 13497                                  ;  539  5225 prs
 13498                                  ;  540  5229 pwi
 13499                                  ;  541  5233 v
 13500                                  ;  542  5235 prs
 13501                                  ;  543  5239 pwi
 13502                                  ;  544  5243 v
 13503                                  ;  545  5245 prs
 13504                                  ;  546  5249 prs
 13505                                  ;  547  5253 v
 13506                                  ;  548  5255 prs
 13507                                  ;  549  5259 prs
 13508                                  ;  550  5263 printreg
 13509                                  ;  551  5272 prs
 13510                                  ;  552  5276 prs
 13511                                  ;  553  5280 prs
 13512                                  ;  554  5284 pint1
 13513                                  ;  555  5295 getarg
 13514                                  ;  556  5302 memresize
 13515                                  ;  557  5312 error1
 13516                                  ;  558  5319 memalloc
 13517                                  ;  559  5328 error1
 13518                                  ;  560  5335 getfirstchar
 13519                                  ;  561  5348 parse
 13520                                  ;  562  5354 checkcalls
 13521                                  ;  563  5365 epilog
 13522                                  ;  564  5385 fgets1
 13523                                  ;  565  5423 cputs
 13524                                  ;  566  5429 cputs
 13525                                  ;  567  5435 readRL
 13526                                  ;  568  5442 prnl
 13527                                  ;  569  5447 strcpy
 13528                                  ;  570  5454 instr2
 13529                                  ;  571  5461 strcat1
 13530                                  ;  572  5469 toupper
 13531                                  ;  573  5477 instr2
 13532                                  ;  574  5484 strcpy
 13533                                  ;  575  5491 instr2
 13534                                  ;  576  5498 ltrim
 13535                                  ;  577  5504 strcpy
 13536                                  ;  578  5511 strlen
 13537                                  ;  579  5518 openR
 13538                                  ;  580  5524 cputs
 13539                                  ;  581  5530 cputs
 13540                                  ;  582  5536 exitR
 13541                                  ;  583  5542 creatR
 13542                                  ;  584  5549 cputs
 13543                                  ;  585  5555 cputs
 13544                                  ;  586  5561 exitR
 13545                                  ;  587  5567 prs
 13546                                  ;  588  5571 prs
 13547                                  ;  589  5575 prs
 13548                                  ;  590  5579 pint1
 13549                                  ;  591  5585 prs
 13550                                  ;  592  5589 prs
 13551                                  ;  593  5593 prs
 13552                                  ;  594  5597 prs
 13553                                  ;  595  5601 prs
 13554                                  ;  596  5605 prs
 13555                                  ;  597  5609 prs
 13556                                  ;  598  5613 pint1
 13557                                  ;  599  5619 isvirtual86
 13558                                  ;  600  5631 prs
 13559                                  ;  601  5635 prs
 13560                                  ;  602  5639 is32bit
 13561                                  ;  603  5647 prs
 13562                                  ;  604  5651 prs
 13563                                  ;  605  5655 prs
 13564                                  ;  606  5659 prs
 13565                                  ;  607  5663 prs
 13566                                  ;  608  5667 prL
 13567                                  ;  609  5671 isvirtual86
 13568                                  ;  610  5683 cputs
 13569                                  ;  611  5689 cputs
 13570                                  ;  612  5695 is32bit
 13571                                  ;  613  5703 cputs
 13572                                  ;  614  5709 cputs
 13573                                  ;  615  5715 INITCPU32
 13574                                  ;  616  5725 prs
 13575                                  ;  617  5729 isvirtual86
 13576                                  ;  618  5741 cputs
 13577                                  ;  619  5747 cputs
 13578                                  ;  620  5753 is32bit
 13579                                  ;  621  5761 cputs
 13580                                  ;  622  5767 cputs
 13581                                  ;  623  5773 isvirtual86
 13582                                  ;  624  5785 prs
 13583                                  ;  625  5789 prs
 13584                                  ;  626  5793 is32bit
 13585                                  ;  627  5801 prs
 13586                                  ;  628  5805 prs
 13587                                  ;  629  5823 getlex
 13588                                  ;  630  5830 istoken
 13589                                  ;  631  5838 istoken
 13590                                  ;  632  5846 dodefine
 13591                                  ;  633  5855 istoken
 13592                                  ;  634  5863 doinclude
 13593                                  ;  635  5873 error1
 13594                                  ;  636  5880 typeName
 13595                                  ;  637  5889 dofunc
 13596                                  ;  638  5896 istoken
 13597                                  ;  639  5904 doLdata
 13598                                  ;  640  5912 doglob
 13599                                  ;  641  5936 prs
 13600                                  ;  642  5940 from_far
 13601                                  ;  643  5949 from_far
 13602                                  ;  644  5958 eqstr
 13603                                  ;  645  5964 prs
 13604                                  ;  646  5968 prs
 13605                                  ;  647  5972 doar
 13606                                  ;  648  5977 prs
 13607                                  ;  649  5981 cputs
 13608                                  ;  650  6033 cputs
 13609                                  ;  651  6039 pint
 13610                                  ;  652  6044 prs
 13611                                  ;  653  6048 printint51
 13612                                  ;  654  6059 openR
 13613                                  ;  655  6065 cputs
 13614                                  ;  656  6071 cputs
 13615                                  ;  657  6077 exitR
 13616                                  ;  658  6083 prs
 13617                                  ;  659  6087 prs
 13618                                  ;  660  6091 getfirstchar
 13619                                  ;  661  6104 getfunctionhead
 13620                                  ;  662  6120 from_far
 13621                                  ;  663  6129 eqstr
 13622                                  ;  664  6135 prs
 13623                                  ;  665  6139 printint51
 13624                                  ;  666  6150 prs
 13625                                  ;  667  6154 prs
 13626                                  ;  668  6158 dofunc
 13627                                  ;  669  6165 prs
 13628                                  ;  670  6169 checkcalls
 13629                                  ;  671  6180 putch
 13630                                  ;  672  6186 cputs
 13631                                  ;  673  6192 error1
 13632                                  ;  674  6215 getlex
 13633                                  ;  675  6222 istoken
 13634                                  ;  676  6230 getlex
 13635                                  ;  677  6254 prs
 13636                                  ;  678  6258 prs
 13637                                  ;  679  6262 openR
 13638                                  ;  680  6268 prs
 13639                                  ;  681  6272 prs
 13640                                  ;  682  6276 cputs
 13641                                  ;  683  6282 error1
 13642                                  ;  684  6289 parse
 13643                                  ;  685  6295 prs
 13644                                  ;  686  6299 prs
 13645                                  ;  687  6303 getfirstchar
 13646                                  ;  688  6316 getlex
 13647                                  ;  689  6343 eqstr
 13648                                  ;  690  6349 getlex
 13649                                  ;  691  6356 eqstr
 13650                                  ;  692  6362 getlex
 13651                                  ;  693  6369 eqstr
 13652                                  ;  694  6375 getlex
 13653                                  ;  695  6382 eqstr
 13654                                  ;  696  6388 getlex
 13655                                  ;  697  6395 error1
 13656                                  ;  698  6402 getlex
 13657                                  ;  699  6409 eqstr
 13658                                  ;  700  6415 getlex
 13659                                  ;  701  6422 prs
 13660                                  ;  702  6426 strcpy
 13661                                  ;  703  6433 prs
 13662                                  ;  704  6437 error1
 13663                                  ;  705  6444 getlex
 13664                                  ;  706  6451 expect
 13665                                  ;  707  6458 error1
 13666                                  ;  708  6465 checknamelen
 13667                                  ;  709  6478 storeName
 13668                                  ;  710  6488 expect
 13669                                  ;  711  6506 istoken
 13670                                  ;  712  6514 istoken
 13671                                  ;  713  6522 stmt
 13672                                  ;  714  6527 istoken
 13673                                  ;  715  6535 doif
 13674                                  ;  716  6540 istoken
 13675                                  ;  717  6548 dodo
 13676                                  ;  718  6553 istoken
 13677                                  ;  719  6561 dowhile
 13678                                  ;  720  6569 istoken
 13679                                  ;  721  6577 prs
 13680                                  ;  722  6581 name1
 13681                                  ;  723  6587 prs
 13682                                  ;  724  6591 expect
 13683                                  ;  725  6598 prs
 13684                                  ;  726  6602 next
 13685                                  ;  727  6607 prc
 13686                                  ;  728  6611 next
 13687                                  ;  729  6616 getlex
 13688                                  ;  730  6623 istoken
 13689                                  ;  731  6631 prs
 13690                                  ;  732  6635 next
 13691                                  ;  733  6640 prc
 13692                                  ;  734  6644 next
 13693                                  ;  735  6649 getlex
 13694                                  ;  736  6656 error1
 13695                                  ;  737  6663 istoken
 13696                                  ;  738  6671 prs
 13697                                  ;  739  6675 expect
 13698                                  ;  740  6682 prL
 13699                                  ;  741  6686 expect
 13700                                  ;  742  6693 istoken
 13701                                  ;  743  6701 doifcarry
 13702                                  ;  744  6711 istoken
 13703                                  ;  745  6719 doifzero
 13704                                  ;  746  6728 istoken
 13705                                  ;  747  6736 doemit
 13706                                  ;  748  6743 istoken
 13707                                  ;  749  6751 istoken
 13708                                  ;  750  6759 exprstart
 13709                                  ;  751  6769 prs
 13710                                  ;  752  6773 expect
 13711                                  ;  753  6780 prs
 13712                                  ;  754  6784 prs
 13713                                  ;  755  6788 prc
 13714                                  ;  756  6792 expect
 13715                                  ;  757  6799 expect
 13716                                  ;  758  6806 exprstart
 13717                                  ;  759  6816 expect
 13718                                  ;  760  6830 prs
 13719                                  ;  761  6834 getlex
 13720                                  ;  762  6841 prL
 13721                                  ;  763  6845 getlex
 13722                                  ;  764  6852 prc
 13723                                  ;  765  6856 expect
 13724                                  ;  766  6874 prs
 13725                                  ;  767  6878 prs
 13726                                  ;  768  6882 prs
 13727                                  ;  769  6886 prs
 13728                                  ;  770  6890 prs
 13729                                  ;  771  6894 prunsign1
 13730                                  ;  772  6904 prs
 13731                                  ;  773  6908 prs
 13732                                  ;  774  6912 prunsign1
 13733                                  ;  775  6922 prs
 13734                                  ;  776  6926 prs
 13735                                  ;  777  6930 prs
 13736                                  ;  778  6934 error1
 13737                                  ;  779  6951 prs
 13738                                  ;  780  6955 prs
 13739                                  ;  781  6959 pint1
 13740                                  ;  782  6965 prc
 13741                                  ;  783  6978 prs
 13742                                  ;  784  6982 prs
 13743                                  ;  785  6986 pint1
 13744                                  ;  786  7007 pexpr
 13745                                  ;  787  7013 pint1
 13746                                  ;  788  7019 stmt
 13747                                  ;  789  7024 istoken
 13748                                  ;  790  7032 prjump
 13749                                  ;  791  7039 prlabel
 13750                                  ;  792  7047 stmt
 13751                                  ;  793  7052 prlabel
 13752                                  ;  794  7060 prlabel
 13753                                  ;  795  7084 prs
 13754                                  ;  796  7088 prs
 13755                                  ;  797  7092 pint1
 13756                                  ;  798  7098 stmt
 13757                                  ;  799  7103 prlabel
 13758                                  ;  800  7126 prs
 13759                                  ;  801  7130 prs
 13760                                  ;  802  7134 pint1
 13761                                  ;  803  7140 stmt
 13762                                  ;  804  7145 prlabel
 13763                                  ;  805  7170 prlabel
 13764                                  ;  806  7178 stmt
 13765                                  ;  807  7183 expect
 13766                                  ;  808  7190 pexpr
 13767                                  ;  809  7196 pint1
 13768                                  ;  810  7202 prjump
 13769                                  ;  811  7209 prlabel
 13770                                  ;  812  7235 prlabel
 13771                                  ;  813  7243 pexpr
 13772                                  ;  814  7249 pint1
 13773                                  ;  815  7255 stmt
 13774                                  ;  816  7260 prjump
 13775                                  ;  817  7267 prlabel
 13776                                  ;  818  7288 getlex
 13777                                  ;  819  7317 next
 13778                                  ;  820  7322 next
 13779                                  ;  821  7327 next
 13780                                  ;  822  7332 next
 13781                                  ;  823  7337 next
 13782                                  ;  824  7342 next
 13783                                  ;  825  7347 next
 13784                                  ;  826  7352 next
 13785                                  ;  827  7357 next
 13786                                  ;  828  7362 next
 13787                                  ;  829  7367 next
 13788                                  ;  830  7372 next
 13789                                  ;  831  7377 next
 13790                                  ;  832  7382 next
 13791                                  ;  833  7387 next
 13792                                  ;  834  7392 instr1
 13793                                  ;  835  7399 next
 13794                                  ;  836  7404 getlex
 13795                                  ;  837  7411 next
 13796                                  ;  838  7416 next
 13797                                  ;  839  7421 getlex
 13798                                  ;  840  7428 getstring
 13799                                  ;  841  7438 digit
 13800                                  ;  842  7444 getdigit
 13801                                  ;  843  7453 next
 13802                                  ;  844  7458 next
 13803                                  ;  845  7463 next
 13804                                  ;  846  7468 letter
 13805                                  ;  847  7475 strcpy
 13806                                  ;  848  7482 letter
 13807                                  ;  849  7489 next
 13808                                  ;  850  7494 eqstr
 13809                                  ;  851  7500 eqstr
 13810                                  ;  852  7506 eqstr
 13811                                  ;  853  7512 eqstr
 13812                                  ;  854  7518 eqstr
 13813                                  ;  855  7524 eqstr
 13814                                  ;  856  7530 eqstr
 13815                                  ;  857  7536 eqstr
 13816                                  ;  858  7542 eqstr
 13817                                  ;  859  7548 eqstr
 13818                                  ;  860  7554 eqstr
 13819                                  ;  861  7560 eqstr
 13820                                  ;  862  7566 eqstr
 13821                                  ;  863  7572 eqstr
 13822                                  ;  864  7578 eqstr
 13823                                  ;  865  7584 eqstr
 13824                                  ;  866  7590 eqstr
 13825                                  ;  867  7596 eqstr
 13826                                  ;  868  7602 eqstr
 13827                                  ;  869  7608 eqstr
 13828                                  ;  870  7614 eqstr
 13829                                  ;  871  7620 eqstr
 13830                                  ;  872  7626 convertdefine
 13831                                  ;  873  7640 strcpy
 13832                                  ;  874  7647 error1
 13833                                  ;  875  7672 from_far
 13834                                  ;  876  7681 eqstr
 13835                                  ;  877  7700 next
 13836                                  ;  878  7705 letter
 13837                                  ;  879  7712 next
 13838                                  ;  880  7717 digit
 13839                                  ;  881  7723 next
 13840                                  ;  882  7748 next
 13841                                  ;  883  7753 next
 13842                                  ;  884  7765 fgets1
 13843                                  ;  885  7782 getlex
 13844                                  ;  886  7798 istoken
 13845                                  ;  887  7806 prs
 13846                                  ;  888  7810 listproc
 13847                                  ;  889  7819 prs
 13848                                  ;  890  7823 pint1
 13849                                  ;  891  7829 error1
 13850                                  ;  892  7852 eprc
 13851                                  ;  893  7863 writetty
 13852                                  ;  894  7872 writetty
 13853                                  ;  895  7881 fputcR
 13854                                  ;  896  7903 prc
 13855                                  ;  897  7912 prs
 13856                                  ;  898  7928 prc
 13857                                  ;  899  7943 eprc
 13858                                  ;  900  7948 mkneg
 13859                                  ;  901  7954 eprnum
 13860                                  ;  902  7961 eprc
 13861                                  ;  903  7976 prc
 13862                                  ;  904  7980 mkneg
 13863                                  ;  905  7986 pint1
 13864                                  ;  906  7992 prc
 13865                                  ;  907  8010 prunsign1
 13866                                  ;  908  8020 prc
 13867                                  ;  909  8037 prc
 13868                                  ;  910  8041 prc
 13869                                  ;  911  8045 prc
 13870                                  ;  912  8049 prc
 13871                                  ;  913  8053 prunsign1
 13872                                  ;  914  8072 printinputline
 13873                                  ;  915  8102 readR
 13874                                  ;  916  8108 prs
 13875                                  ;  917  8112 prunsign1
 13876                                  ;  918  8122 prc
 13877                                  ;  919  8126 prscomment
 13878                                  ;  920  8144 fcloseR
 13879                                  ;  921  8152 fcloseR
 13880                                  ;  922  8160 exitR
 13881                                  ;  923  8175 prnl
 13882                                  ;  924  8180 prscomment
 13883                                  ;  925  8191 prs
 13884                                  ;  926  8195 prunsign1
 13885                                  ;  927  8205 prs
 13886                                  ;  928  8209 prs
 13887                                  ;  929  8213 prs
 13888                                  ;  930  8217 pint1
 13889                                  ;  931  8223 prs
 13890                                  ;  932  8227 prunsign1
 13891                                  ;  933  8237 prs
 13892                                  ;  934  8241 prc
 13893                                  ;  935  8245 prs
 13894                                  ;  936  8249 pint1
 13895                                  ;  937  8255 prs
 13896                                  ;  938  8259 prs
 13897                                  ;  939  8263 end1
 13898                                  ;  940  8279 prs
 13899                                  ;  941  8283 prs
 13900                                  ;  942  8287 prs
 13901                                  ;  943  8291 prunsign1
 13902                                  ;  944  8301 prs
 13903                                  ;  945  8305 listvar
 13904                                  ;  946  8329 prs
 13905                                  ;  947  8333 printint51
 13906                                  ;  948  8344 prc
 13907                                  ;  949  8348 prs
 13908                                  ;  950  8352 prs
 13909                                  ;  951  8356 prs
 13910                                  ;  952  8360 prs
 13911                                  ;  953  8364 prs
 13912                                  ;  954  8368 prs
 13913                                  ;  955  8372 prs
 13914                                  ;  956  8376 prs
 13915                                  ;  957  8380 prs
 13916                                  ;  958  8384 printint51
 13917                                  ;  959  8395 printint51
 13918                                  ;  960  8406 prs
 13919                                  ;  961  8410 prs
 13920                                  ;  962  8414 prc
 13921                                  ;  963  8418 prf
 13922                                  ;  964  8422 prs
 13923                                  ;  965  8426 prc
 13924                                  ;  966  8430 pint1
 13925                                  ;  967  8436 prs
 13926                                  ;  968  8440 prc
 13927                                  ;  969  8444 from_far
 13928                                  ;  970  8453 prs
 13929                                  ;  971  8457 prc
 13930                                  ;  972  8461 prL
 13931                                  ;  973  8465 prc
 13932                                  ;  974  8469 prL
 13933                                  ;  975  8473 prc
 13934                                  ;  976  8488 prs
 13935                                  ;  977  8492 calllisting
 13936                                  ;  978  8522 prs
 13937                                  ;  979  8526 printint51
 13938                                  ;  980  8537 prc
 13939                                  ;  981  8541 prs
 13940                                  ;  982  8545 printint51
 13941                                  ;  983  8556 prc
 13942                                  ;  984  8560 from_far
 13943                                  ;  985  8569 prs
 13944                                  ;  986  8588 from_far
 13945                                  ;  987  8597 from_far
 13946                                  ;  988  8606 eqstr
 13947                                  ;  989  8623 prs
 13948                                  ;  990  8627 countcalls
 13949                                  ;  991  8638 funclisting
 13950                                  ;  992  8668 prs
 13951                                  ;  993  8672 printint51
 13952                                  ;  994  8683 printint51
 13953                                  ;  995  8694 prs
 13954                                  ;  996  8698 printint51
 13955                                  ;  997  8709 prc
 13956                                  ;  998  8713 prs
 13957                                  ;  999  8717 prs
 13958                                  ; 1000  8721 prs
 13959                                  ; 1001  8725 prs
 13960                                  ; 1002  8729 prc
 13961                                  ; 1003  8733 prc
 13962                                  ; 1004  8737 from_far
 13963                                  ; 1005  8746 prs
 13964                                  ; 1006  8765 strcpy
 13965                                  ; 1007  8772 storefunc
 13966                                  ; 1008  8782 prs
 13967                                  ; 1009  8786 prs
 13968                                  ; 1010  8790 listvar
 13969                                  ; 1011  8798 listfunc
 13970                                  ; 1012  8807 listcall
 13971                                  ; 1013  8816 prs
 13972                                  ; 1014  8820 prs
 13973                                  ; 1015  8824 prs
 13974                                  ; 1016  8828 prs
 13975                                  ; 1017  8832 prs
 13976                                  ; 1018  8836 printint51
 13977                                  ; 1019  8847 prs
 13978                                  ; 1020  8851 printint51
 13979                                  ; 1021  8862 prs
 13980                                  ; 1022  8866 printint51
 13981                                  ; 1023  8877 prs
 13982                                  ; 1024  8881 printint51
 13983                                  ; 1025  8892 prs
 13984                                  ; 1026  8896 printint51
 13985                                  ; 1027  8907 prs
 13986                                  ; 1028  8911 prs
 13987                                  ; 1029  8915 prs
 13988                                  ; 1030  8919 prs
 13989                                  ; 1031  8923 printint51
 13990                                  ; 1032  8934 prs
 13991                                  ; 1033  8938 printint51
 13992                                  ; 1034  8949 prs
 13993                                  ; 1035  8953 printint51
 13994                                  ; 1036  8964 prs
 13995                                  ; 1037  8968 printint51
 13996                                  ; 1038  8979 prs
 13997                                  ; 1039  8983 printint51
 13998                                  ; 1040  8994 prs
 13999                                  ; 1041  8998 printint51
 14000                                  ; 1042  9009 prs
 14001                                  ; 1043  9013 printint51
 14002                                  ; 1044  9024 prs
 14003                                  ; 1045  9028 printint51
 14004                                  ; 1046  9039 prs
 14005                                  ; 1047  9043 printint51
 14006                                  ; 1048  9054 prs
 14007                                  ; 1049  9058 prs
 14008                                  ; 1050  9062 printint51
 14009                                  ; 1051  9073 prs
 14010                                  ; 1052  9077 prunsign1
 14011                                  ; 1053  9087 prs
 14012                                  ; 1054  9091 printint51
 14013                                  ; 1055  9102 prs
 14014                                  ; 1056  9106 prs
 14015                                  ; 1057  9110 prs
 14016                                  ; 1058  9114 prs
 14017                                  ; 1059  9118 printint51
 14018                                  ; 1060  9129 prs
 14019                                  ; 1061  9133 printint51
 14020                                  ; 1062  9144 prs
 14021                                  ; 1063  9148 printint51
 14022                                  ; 1064  9159 prs
 14023                                  ; 1065  9163 prs
 14024                                  ; 1066  9167 end1
 14025                                  ; 1067  9184 DosInt
 14026                                  ; 1068  9210 DosInt
 14027                                  ; 1069  9352 ultoar
 14028                                  ; 1070  9359 prs
 14029                                  ; 1071  9371 ultoaL
 14030                                  ; 1072  9378 prs
 14031                                  ; 1073  9396 ultoaL
 14032                                  ; 1074  9403 strlen
 14033                                  ; 1075  9410 prc
 14034                                  ; 1076  9414 prs
 14035                                  ; 1077  9459 writetty
 14036                                  ; 1078  9468 writetty
 14037                                  ; 1079  9487 putch
 14038                                  ; 1080  9510 prc
 14039                                  ; 1081  9514 mkneg
 14040                                  ; 1082  9520 pint
 14041                                  ; 1083  9525 putch
 14042                                  ; 1084  9602 digit
 14043                                  ; 1085  9630 strcpy
 14044                                  ; 1086  9652 DosInt
 14045                                  ; 1087  9668 DosInt
 14046                                  ; 1088  9686 DosInt
 14047                                  ; 1089  9709 DosInt
 14048                                  ; 1090  9728 DosInt
 14049                                  ;Input: A.C, List: A.S,  Lines: 1130
 14050                                  ;Glob. variables:  171 max.:  200
 14051                                  ;Local variables:    8 max.:  100, in 'expr'
 14052                                  ;Functions      :  130 max.:  300
 14053                                  ;Calls          : 1091 max.: 2000, NameField: 9770 max.:    65535
 14054                                  ;Code until     :25825 max.: 30000, free: 4175
 14055                                  ;Data (HeapEnd) :47000, resting stacksize: 18535
 14056                                  ;Max. Const in 'epilog' : 1131 max. 3000, free: 1869
