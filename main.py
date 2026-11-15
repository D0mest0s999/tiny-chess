import json,os,sys,urllib.request as U
E=os.environ.get("MAKEFUN_ENV_URL")or sys.exit(1)
def post(p,b):
 with U.urlopen(U.Request(E+p,data=json.dumps(b).encode(),headers={"content-type":"application/json"}))as r:return json.load(r)
V={"p":100,"n":320,"b":330,"r":500,"q":900,"k":20000}
KN=((-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1))
KD=((-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1))
DG=((-1,-1),(-1,1),(1,-1),(1,1))
OD=((-1,0),(1,0),(0,-1),(0,1))
PR="qrbn"
def F(f):
 x=f.split();b=[]
 for row in x[0].split("/"):
  for c in row:b+=["."]*int(c)if c.isdigit()else[c]
 return b,x[1]=="w"
def P(b,w):
 ms=[];st=-8 if w else 8;sr=6 if w else 1;lr=0 if w else 7
 def pm(s,t):
  if t//8==lr:
   for i in(1,2,3,4):ms.append(s|t<<6|i<<12)
  else:ms.append(s|t<<6)
 for s in range(64):
  p=b[s]
  if p=="."or p.isupper()!=w:continue
  k=p.lower();r,f=divmod(s,8)
  if k=="p":
   o=s+st
   if b[o]==".":
    pm(s,o)
    if o//8!=lr and r==sr and b[s+2*st]==".":ms.append(s|(s+2*st)<<6)
   for d in(-1,1):
    if 0<=f+d<8:
     t=s+st+d;v=b[t]
     if v!="."and v.isupper()!=w:pm(s,t)
  else:
   for dr,df in(DG if k=="b"else OD if k=="r"else KN if k=="n"else KD):
    rr,ff=r+dr,f+df
    while 0<=rr<8 and 0<=ff<8:
     v=b[rr*8+ff]
     if v==".":ms.append(s|(rr*8+ff)<<6)
     else:
      if v.isupper()!=w:ms.append(s|(rr*8+ff)<<6)
      break
     if k in"nk":break
     rr+=dr;ff+=df
 return ms
def M(b,m,w):
 f=m&63;t=(m>>6)&63;pr=(m>>12)&7;n=b[:];p=n[f];n[f]="."
 n[t]=(PR[pr-1].upper()if w else PR[pr-1])if pr else p
 return n
PS={};KE={}
for w in(1,0):
 for k in"pnbrqk":
  t=[];g=[]
  for s in range(64):
   r,f=divmod(s,8);rw=7-r if w else r;c=abs(f-3.5)+abs(rw-3.5)
   if k=="p":a=rw-1;x=6*a+2*a*a
   elif k=="n":x=int(14-4*c)
   elif k=="k":x=10 if rw==0 else-4*rw;g.append(int(16-4*c))
   else:x=0
   t.append(x if k=="k"else V[k]+x)
  PS[k.upper()if w else k]=t
  if k=="k":KE["K"if w else"k"]=g
def W(b):
 s=0;ph=0;kg=[]
 for i in range(64):
  p=b[i]
  if p==".":continue
  k=p.lower()
  if k=="k":kg.append((p,i));continue
  s+=PS[p][i]if p.isupper()else-PS[p][i]
  if k!="p":ph+=V[k]
 for p,i in kg:
  t=(KE if ph<=2400 else PS)[p];s+=t[i]if p=="K"else-t[i]
 return s
MATE=100000;CAP=150000;N=0
def ok(b,m):
 v=b[(m>>6)&63]
 if v!=".":return 100000+10*V[v.lower()]-V[b[m&63].lower()]//32
 return 90000 if(m>>12)&7 else 0
def Q(b,w,a,be,pl):
 global N;N+=1
 st=W(b)*(1 if w else-1)
 if st>=be:return be
 if st>a:a=st
 cl=[]
 for m in P(b,w):
  t=(m>>6)&63;v=b[t]
  if v.lower()=="k":return MATE-pl
  tk=V[v.lower()]if v!="."else 800 if(m>>12)&7 else-1
  if tk>=0:cl.append((ok(b,m),M(b,m,w)))
 for _,n in sorted(cl,key=lambda z:-z[0]):
  s=-Q(n,not w,-be,-a,pl+1)
  if s>=be:return be
  if s>a:a=s
  if N>CAP:return a
 return a
def S(b,w,d,a,be,pl):
 global N
 if d<=0:return Q(b,w,a,be,pl)
 N+=1
 if N>CAP:return W(b)*(1 if w else-1)
 ic=any(b[(m>>6)&63].lower()=="k"for m in P(b,not w))
 if ic:d+=1
 ms=P(b,w)
 if not ms:return-MATE+pl
 if d>2 and not ic and be<90000:
  s=-S(b,not w,d-3,-be,-be+1,pl+1)
  if s>=be:return be
 best=-10*MATE
 for m in sorted(ms,key=lambda z:-ok(b,z)):
  if b[(m>>6)&63].lower()=="k":return MATE-pl
  s=-S(M(b,m,w),not w,d-1,-be,-a,pl+1)
  if N>CAP:return W(b)*(1 if w else-1)
  if s>best:best=s
  if s>a:a=s
  if a>=be:break
 return best
def uci(m):
 nm=lambda s:"abcdefgh"[s%8]+str(8-s//8);p=(m>>12)&7
 return nm(m&63)+nm((m>>6)&63)+(PR[p-1]if p else"")
def R(b,w):
 global N
 ms=P(b,w)
 if not ms:return None
 N=0;best=ms[0];od=list(ms)
 for d in range(1,99):
  sc=[];a=-10*MATE
  for m in od:
   s=-S(M(b,m,w),not w,d-1,-10*MATE,-a,1);sc.append((s,m))
   if s>a:a=s
  sc.sort(key=lambda z:-z[0])
  if N>CAP:break
  od=[m for _,m in sc];best=sc[0][1]
  if N>CAP//2:break
 return best
def C(f,lu):
 b,w=F(f)
 try:
  m=R(b,w)
  if m is not None:
   p=uci(m)
   if p in lu:return p
 except Exception:pass
 return lu[0]
def main():
 while 1:
  g=post("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":g=post("/move",{"game":g["game"],"move":C(g["fen"],g["legal_moves"])})
if __name__=="__main__":main()
