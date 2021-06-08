#!/usr/bin/python3
import sys,pdb,re
f=sys.argv[1]
txt=open(f).read()
f=open(f+".new",'w')
plot=re.compile(r'> plot[(](.*)[)]',re.DOTALL)
for t in txt.split("Sinput"):
    if "plot(" in t:
        t=plot.sub(r"> \1",t)
    f.write(t+"Sinput")
