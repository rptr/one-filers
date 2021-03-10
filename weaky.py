#!/usr/bin/python3
# nginx config:
# location ~* /.* {
#     proxy_pass http://localhost:8000;
# }
from wsgiref.simple_server import make_server as m;import os;
os.system('mkdir p;touch p/index')
def a(e,r):
    p='p'+e['PATH_INFO'];os.system('touch '+p);l=e['CONTENT_LENGTH'];
    l=int(l)if len(l)>0 else 0;i=e['wsgi.input'].read(l)
    if l:d=open(p,'w').write(i.decode().split('=')[1]);x=open(p).read()
    f='<form method=post><textarea name=e>{}</textarea><input type=submit></fo\
    rm>'.format(open(p).read())
    c='<html><body>'+''.join([x,f])+'</body></html>'
    r('200 OK', []);return [c.encode()]
with m('',8000,a) as h:
    h.serve_forever()
