#!/usr/bin/env python3
import re #Regular Expressions
import pdb #Python DeBugger
import sys #for command line parsing
TEMPLATE=r"""
\frame[containsverbatim]{\frametitle{%(title)s}
<<%(args)s>>=
%(code)s
@
}
"""
sweave_rex=re.compile(r'SWEAVE[(](?P<file>[^)]+)[)]')
slide_rex=re.compile(r'##\W*(?P<title>.*?)\n(?P<code>.*?)\n\n',re.DOTALL)
def parseR(f):
    """Look for slides in an R file.

    For each slide found, return a dictionary which can be used with
    TEMPLATE for making valid Sweave code.

    """
    text=open(f).read()
    ms=[m.groupdict() for m in slide_rex.finditer(text)]
    for d in ms:
        d['noaddplot']='show.settings' in d['code']  or 'compare' in d['code']
        d['print']='head' in d['code'] or 'print' in d['code']
        d['addplot']=not (d['print'] or d['noaddplot'])
        d['fig']=d['noaddplot'] or d['addplot']
        d['args']=re.sub('[ ."=]',"-",d['title'])
        d['args']+=",fig=T,tikz=T,width=10" if d['fig'] else ""
        if d['addplot']:
            lines=d['code'].split('\n')
            d['code']='\n'.join(lines[:-1]+['plot('+lines[-1]+')'])
    return ms

def make_sweave(f):
    """Replace references to R files in f with slides.

    Returns the modified text of f, which should work with Sweave.

    """
    latex=open(f).read()
    for m in sweave_rex.finditer(latex):
        block=m.groupdict()['file']
        chunks=[TEMPLATE%d for d in parseR(block+".R")]
        latex=sweave_rex.sub('\n'.join(chunks).replace("\\","\\\\"),latex,1)
    return latex
    
if __name__=="__main__":
    print(make_sweave(sys.argv[1]))

