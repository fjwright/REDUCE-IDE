#! /bin/bash

# Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
# Time-stamp: <2024-02-07 17:10:32 franc>

# Construct a REDUCE IDE package archive.
# Must be run from the REDUCE IDE directory.
# Usage: make-package.sh version

if [ -z $1 ]; then
    echo 'Version required as argument.'
    exit 1
fi

dir=reduce-ide-$1
file=packages/$dir.tar

if [ -e $file ]; then
    read -p "Package version $1 already exists. Overwrite it? (y/n) " -n 1 input
    echo
    [[ ${input@L} == 'y' ]] || exit
fi

echo 'Version is' $1

# Update reduce-ide.info if necessary:
if [ reduce-ide.info -ot reduce-ide.texinfo ]; then
    makeinfo reduce-ide.texinfo
fi

pkg1='(define-package "reduce-ide" "'
pkg2="\" \"REDUCE Integrated Development Environment\" '((emacs \"27\")))"

# Construct the reduce-ide-<version> directory:
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

# Archive the reduce-ide-<version> package to the package directory:
tar --create --dereference --file=$file $dir

# Tidy up:
rm -rf $dir
