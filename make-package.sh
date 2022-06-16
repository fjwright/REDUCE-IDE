#! /bin/bash

# Author: Francis J. Wright <https://sourceforge.net/u/fjwright>
# Time-stamp: <2022-06-16 17:03:41 franc>

# Construct a reduce-ide package archive.
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

pkg1='(define-package "REDUCE-IDE" "'
pkg2='" "REDUCE Integrated Development Environment")
'

# Construct the reduce-ide-vv directory:
dir=reduce-ide-$1
mkdir $dir
cd $dir
ln -s ../info-dir dir
ln -s ../README.md
ln -s ../reduce-delim.el
ln -s ../reduce-font-lock.el
ln -s ../reduce-ide.info
ln -s ../reduce-mode.el
ln -s ../reduce-run.el
echo $pkg1$1$pkg2 > reduce-ide-pkg.el
cd ..

# Archive the reduce-ide-vv package to the package directory:
tar --create --dereference --file=packages/$dir.tar $dir

# Tidy up:
rm -rf $dir
