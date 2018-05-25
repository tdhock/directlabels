#!/bin/bash
cd ..
rm -rf directlabels-release
cp -r directlabels directlabels-release
grep -v NoteAbove directlabels/DESCRIPTION > directlabels-release/DESCRIPTION
PKG_TGZ=$(R CMD build directlabels-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD check --as-cran $PKG_TGZ
