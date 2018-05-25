#!/bin/bash
cd ..
rm -rf directlabels-release
cp -r directlabels directlabels-release
PKG_TGZ=$(R CMD build directlabels-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD check --as-cran $PKG_TGZ
