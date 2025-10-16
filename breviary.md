### Conventions

`<...>` is the used to indicate custom content. E.g., `<filename>` indicates
a custom filename such as `myfile.txt`, or `myprogram.f90`, etc.

# bash

### Shortcuts in paths

`~` is your home directory (on Linux systems typically
`/home/<username>`)

`.` is current directory

`..` is parent directory

## Basic commands

### Navigation in the file system

<!-- these &nbsp;s are for a larger column -->
| Command &nbsp; &nbsp; &nbsp; &nbsp; | Description | Examples
-----------------|-------------|---------
`pwd` | print current directory (where you are) |
`ls` | list files in current directory | same as `ls .`
`ls <dir>` | list files in directory `<dir>` | does not change the current directory
`cd <dir>` | change current directory to `<dir>` | `cd test` goes into `test`, `cd ..` goes to parent directory, `cd ~` goes to home
`mkdir <dir>` | create empty directory with custom name `<dir>` | `mkdir test` creates the directory `test` in the current (`.`) directory
`rmdir <dir>` | remove directory | `<dir>` must be empty, see below for removing files

### Copying, renaming, deleting files

Command | Description
--------|-------------
`cp <file> <new_file>` | create a duplicate file named `<new_file>` of a the file `<file>`
`cp -r <dir> <new_dir>` | create a duplicate file named `<new_file>` of a the file `<file>`
`mv <old_file> <new_file>` | move `<old_file>` to `<new_file>` (equivalent to renaming the file)
`rm <file>` | removes file `<file>`, **this is definitive**
`rm -r ./'<dir>'` | remove directory `<dir>` and all files contained in it, **this is definitive**

Avoid using special characters in the names of your files
(so do not work in a complex directory structure especially on Windows where spaces are
frequent in the folder names like `My Documents` or `C:\Programmi (x86)`).
If you need to work with such files, each filename should be surrounded by quotes.
In doubt, always put quotes around filenames, they will always work:
```bash
mkdir a b c # creates 3 directories a, b, and c
mkdir 'a b c' # creates 1 directory named a b c
cp -r 'a b c' abc correctly copies the folder a b c to abc
```

# Vim

**[Vim cheat sheet](https://vim.rtorr.com/)**

## Launching the editor

To launch the Vim text editor with a file loaded: `vi <file>` or `vim <file>` opens file `<file>`.

## When inside Vim

When you launch Vim you are in command mode. This means that the
keyboard is meant to issue commands and not to type text.
All filenames are relative to where you were (`pwd`) when you
opend the editor.

## Command mode

The actual command-line is opened when pressing `:`, so we will put the colon in front of these commands.

Command | Description
--------|-------------
`:w` | save file
`:q` | try to quit Vim without saving (will ask)
`:qa!` | force quit Vim without saving (unsaved changes are lost)
`:wq` or `ZZ` | save file and quit Vim
`hjkl` | respectively move left, down, up, right
`G` | go to the end of the file
`1G` or `gg` | go to the first line of the file
`56G` or `56gg` | go to the 56th line of the file
`/<string>` | search for string, use `n` to scroll through multiple occurrences
`yy` | copy (yank) current line
`p` | paste after current line
`i` | enter INSERT MODE

## Insert mode

Use keyboard to type text. Use `ESC` to go back to COMMAND MODE above.

# Git

See https://git-scm.com/cheat-sheet for further info.

Suggested protocol: create a [GitHub](https://github.com/) account.
Create a new private repository with README through your GitHub
personal page. Clone your repository in your PC.

`$ git clone <url>` clone an existing repo. Get `<url>` from the GitHub
page of the repository that you just created.

Now you have the folder (and content) of your repository in your PC,
and you can start creating and editing the files of your project.

`$ git add <file>` start tracking file `<file>`

(if you move or delete a traked file `git mv` and `git rm` shloud be
used instead of `mv` and `rm`, respectively).

When you want to save the changes that you have made to your files,
you do a so called 'commit':

`$ git commit -a -m "<message>"` with `message` being a short text
describing the essence of the changes that you have made

To synchronize the online GitHub repository (on the GitHub server)
with your local repository (on your PC), you do a 'push':

`$ git push` (you have to issue this command from one of the locations
within the repository)

If the online repository contains updates that you do not yet have in
your local repo (beacuse, for example, you pushed the changes from
another device), in order to synchronize your local repo you do a
'pull':

`$ git pull` (you have to issue this command from one of the locations  
within the repository)

# gfortran

# gnuplot
