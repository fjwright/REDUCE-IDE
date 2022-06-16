#! /bin/bash

# Construct a reduce-ide package archive.
# Usage: make-package.sh version-number

if [ -z $1 ]; then
    echo 'Version number required as argument.'
    exit 1
fi

echo 'Version number is' $1

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

# Construct the reduce-ide-vv package archive in the package directory:
tar -chf packages/$dir.tar $dir

# Tidy up:
rm -rf $dir
