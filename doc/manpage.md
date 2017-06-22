% FEDPASTE(1)
% Ricky Elrod
% July 2017

# NAME

fedpaste – paste things to Fedora's Modern Paste instance

# SYNOPSIS

```
fedpaste [-U|--URL|--url ARG] ([COMMAND] | [FILES...] [-t|--title TITLE]
	     [-l|--language LANGUAGE] [-d|--password PASSWORD])
```

# DESCRIPTION

**fedpaste** is a simple command-line tool for pasting text to a Modern Paste
instance. It was originally written for Fedora users and developers.

# GLOBAL OPTIONS

These options can be used anywhere.

**-h**, **--help**
:   Display a help message.

**-U**, **--url**, **--URL**
:   Configure the URL with which to interact.

# PASTE OPTIONS

These options are used with pasting text or files.

**-t**, **--title**
:   Set the title of the paste.

**-l**, **--language**
:   Set the syntax-highlighting language of the paste.

**-p**, **-d**, **--password**
:   Set the password of the paste.

# COMMANDS

* sysinfo - Paste useful debugging information about your system.

## Options for sysinfo:
   * **--no-confirm** - Paste the information immediately without allowing you to review it.

# EXAMPLES

**Paste system information for debugging**:

* `fedpaste sysinfo`

**Paste output from a command**:

* `cat /proc/cpuinfo | fedpaste`

**Paste files**:

* `fedpaste -l haskell Foo.hs`
* `fedpaste -l python file1.py file2.py`

# REPORTING BUGS

Please file bugs on the issue tracker at https://github.com/relrod/fpasteng. If
for some reason you are unable to do so, please contact the maintainer at
relrod@redhat.com.

# COPYRIGHT

(c) 2017 Red Hat, Inc.
Released under the MIT license.
