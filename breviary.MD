# Breviary

`<...>` used to indicate custom content.

`$ ...` the `$` symbol indicates that you are in the prompt line

## Basic bash commands

Shortcuts in paths:

`~` is your home directory (on Linux systems typically
`/home/<username>`)

`.` is current directory

`..` is parent directory

Basic commands:

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

`$ cp <file> <new file>` creates a duplicate (`<new file>`) of a file
(`<file>`)

`$ mv <file> <new file>` moves `<file>` to `<new file>` (equivalent
to renaming the file)

`$ rm <file>` removes file `<file>`
`$ rm -rf <dir>` (removes directory `<dir>` and all files contained
in it)

## Basic vi commands

How to edit text files with vi.

`$ vi <file>` or `$vim <file>` open file `<file>` with the vi text
editor

When you launch vi you are in command mode. This means that the
keyboard is meant to issue commands and not to type text.

### COMMAND MODE

`:w` save file
`:q` quit vi without saving
`:wq` save file and quit vi

`G` go to the bottom line
`1 G` (go to the first line)

`/ <string>` (search for string: use `n` to scroll through
multiple occurrences)

`i` enter INSERT MODE

### INSERT MODE

Use keyboard to type text. Use `ESC` to go back to command mode.
