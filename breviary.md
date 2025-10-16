### Conventions

`<...>`is  used to indicate custom content. E.g., `<filename>` indicates
a custom filename such as `myfile.txt`, or `myprogram.f90`, etc.

In `$ ...`, the `$` symbol is the prompt symbol (you don't have to
include it when typing the commands).

# bash

### Shortcuts in paths

`~` is your home directory (on Linux systems typically
`/home/<username>`)

`.` is current directory

`..` is parent directory

## Basic commands

### Navitate the file system

`$ pwd` print current directory

`$ ls` list files in current directory

`$ mkdir <dir>` create directory with custom name `<dir>`

E.g., `$ mkdir test` creates a directory named `test`

`$ cd <dir>` change current directory to `<dir>`

E.g.:
`$ cd test` (go to directory `test`)
`$ cd ../` (go to parent directory)
`$ cd ~` (go to home directory)

`$ rmdir <dir>` remove directory (`<dir>` must be empty; see command
`rm` below)

E.g.: `$ rmdir test` removes (empty) directory `test`

### Copying, renaming, deleting files

`$ cp <file> <new file>` creates a duplicate (`<new file>`) of a file
(`<file>`)

`$ mv <file> <new file>` moves `<file>` to `<new file>` (equivalent
to renaming the file)

`$ rm <file>` removes file `<file>`
`$ rm -rf <dir>` (removes directory `<dir>` and all files contained
in it)

# vi

`$ vi <file>` or `$vim <file>` open file `<file>` with the vi text
editor

When you launch vi you are in command mode. This means that the
keyboard is meant to issue commands and not to type text.

## Command mode

`:w` save file
`:q` quit vi without saving
`:wq` save file and quit vi

`G` go to the bottom line
`1 G` (go to the first line)

`/ <string>` (search for string: use `n` to scroll through
multiple occurrences)

`i` enter INSERT MODE

## Insert mode

Use keyboard to type text. Use `ESC` to go back to command mode.

# git

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
