#! /bin/bash

# Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
# Time-stamp: <2024-01-28 17:23:59 franc>

# Construct a REDUCE IDE package archive.
# Usage: make-package.sh version-number

if [ -z $1 ]; then
    echo 'Version number required as argument.'
    exit 1
fi

echo 'Version number is' $1

# Update reduce-ide.info if necessary:
if [ reduce-ide.info -ot reduce-ide.texinfo ]; then
    makeinfo reduce-ide.texinfo
fi

pkg1='(define-package "reduce-ide" "'
pkg2="\" \"REDUCE Integrated Development Environment\" '((emacs \"27\")))"

# Construct the reduce-ide-vv directory:
dir=reduce-ide-$1
mkdir $dir
cd $dir
echo 'Website: https://reduce-algebra.sourceforge.io/reduce-ide/
Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
' > README
cat ../packages/reduce-ide-readme.txt >> README
ln -s ../reduce-delim.el
ln -s ../reduce-font-lock.el
ln -s ../reduce-mode.el
ln -s ../reduce-run.el
ln -s ../dir dir
ln -s ../reduce-ide.info
echo $pkg1$1$pkg2 > reduce-ide-pkg.el
cd ..

# Archive the reduce-ide-vv package to the package directory:
tar --create --dereference --file=packages/$dir.tar $dir

# Tidy up:
rm -rf $dir
