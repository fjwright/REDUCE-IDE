GNU Emacs REDUCE Integrated Development Environment
===================================================
Major modes for editing and running REDUCE source code
------------------------------------------------------

**[Francis Wright](https://sites.google.com/site/fjwcentaur)**

Version 1.11.5, February 2024

REDUCE IDE is a package that provides an Integrated Development Environment for the [REDUCE computer algebra system](https://reduce-algebra.sourceforge.io/) within the GNU Emacs editor.  Its two major components are Emacs Lisp libraries that provide major modes for editing REDUCE source code and running a *command-line version* of REDUCE in an Emacs window.  It assumes that Emacs is running under a GUI such as Microsoft Windows or the X Window System under some flavour of UNIX or Linux, and displays Unicode character sets correctly.  REDUCE IDE does not include REDUCE, which is available separately from [SourceForge](https://sourceforge.net/projects/reduce-algebra/).  You don't need to install REDUCE in order to edit REDUCE source code using REDUCE IDE, but if you want to run REDUCE in REDUCE IDE then you do need to install REDUCE.

REDUCE IDE now formally requires GNU Emacs version 27 at least although I support only GNU Emacs 29 on Microsoft Windows and Linux.

Installation
------------

I recommend that you install the latest complete package, which includes the manual in Info format, by following the instructions on the [REDUCE IDE home page](https://reduce-algebra.sourceforge.io/reduce-ide/).  Alternatively, you can download a package file (see [*Repositories and Releases*](#repositories-and-releases) below) to any convenient directory and run the Emacs command `package-install-file` on it.  For further details, see [*Packages* in the Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html).

Documentation
-------------

The REDUCE IDE manual should be available as an Info document in Emacs, normally at the top of the Info directory, once you have installed the REDUCE IDE package.  It is also available as a PDF document and a multi-page HTML document via the [REDUCE IDE home page](https://reduce-algebra.sourceforge.io/reduce-ide/).

You can also generate the manual yourself in various formats, including `info`, `HTML` and `PDF`, from `reduce-ide.texinfo`, but you need the [GNU Texinfo package](https://www.gnu.org/software/texinfo/).  The commands I used to generate the formats that I provide are listed at the end of the file `reduce-ide.texinfo`.  For further details, please see sections 20 to 22 of the [Texinfo manual](https://www.gnu.org/software/texinfo/manual/texinfo/) or this [quick summary](https://en.wikipedia.org/wiki/Texinfo).

Repositories and Releases
-------------------------

The primary repository is [GitHub](https://github.com/fjwright/REDUCE-IDE).  The main source files (only, see below) for release versions are also available from [SourceForge](https://sourceforge.net/p/reduce-algebra/code/HEAD/tree/trunk/generic/emacs/).  Releases as Emacs package (`.tar`) files are available from [GitHub](https://github.com/fjwright/REDUCE-IDE/releases) and in a more convenient form that is recognised by the Emacs package manager via the [REDUCE IDE home page](https://reduce-algebra.sourceforge.io/reduce-ide/).

Main Source Files
-----------------

Required:

* `reduce-mode.el`  &ndash;  REDUCE source code editing mode

Optional:

* `reduce-font-lock.el`  &ndash;  support syntactic highlighting
* `reduce-delim.el`  &ndash;  highlight matching group or block delimiters
* `reduce-run.el`  &ndash;  run REDUCE in an Emacs buffer
* `reduce-ide.texinfo`  &ndash;  texinfo manual

To Do
-----

* Check relation between standard and REDUCE mode key bindings.
* Check switch-to-reduce and related commands.
* Resolve REDUCE Run mode fontification clash with Comint mode.
* On Microsoft Windows, make REDUCE interruptible.
* Version 2 (maybe): use treesitter for parsing.

Main Updates since REDUCE IDE 1.11
----------------------------------

* v1.11.1: The command `reduce-tagify-dir-recursively` failed on the REDUCE packages directory (at least on MS Windows) because the list of files is too long.  Add a depth arg to `reduce--directory-files-recursively` to limit the recursion depth, which works around the problem.  Handle errors better and improve tagging menu tooltips.
* Update the manual.

* v1.11.2: Update the default CSL REDUCE command on Microsoft Windows to preserve the current working directory.
* v1.11.3: Fix a bug in `reduce--current-proc-name`.
* v1.11.4: On Microsoft Windows, remove special support for PSL REDUCE, which is not needed from REDUCE revision 6726.  Note that if you have customized `reduce-run-commands` then you need to erase the customization, at least for PSL REDUCE, so that the command to run PSL REDUCE reverts to its (new) default value, which should probably be `C:/Program Files/Reduce/bin/redpsl.bat`, possibly with a different drive letter.
* v1.11.5: Change `reduce-run-commands` so that a REDUCE command is a list of strings rather than a single string, which allows spaces in both the command and its arguments.  Replace `XSL` by `NAME` in variables in `reduce-run-reduce`.
* Automatically update option `reduce-run-commands` to new structure and save it. **Not quite right yet!**
* Update REDUCE Run internal variable names to include `--` and mostly to begin with `reduce-run--`.
* On Microsoft Windows, run REDUCE directly rather than via the `.bat` files, which avoids the query "Terminate batch job (Y/N)?" when REDUCE is killed (such as by attempting to interrupt it).  But keep the `.bat` commands for now for comparison.
* **When running directly, need a way to set $REDUCE, as is done by the `.bat` files.**
