### Conventions

`<...>` is the used to indicate custom content. E.g., `<filename>` indicates
a custom filename such as `myfile.txt`, or `myprogram.f95`, etc.

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
`cp <file> <new_file>` | create a duplicate file named `<new_file>` of the file `<file>`
`cp -r <dir> <new_dir>` | create a duplicate file named `<new_file>` of the file `<file>`
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

When you launch Vim you are in **Command mode**. This means that the
keyboard is meant to issue commands and not to type text.
All filenames are relative to where you were (`pwd`) when you
open the editor.

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
`i` | enter **Insert mode** (before the cursor)
`a` | enter **Insert mode** (after the cursor)
`o` | enter **Insert mode** on a new line below the current line

## Insert mode

Use keyboard to type text. Use `ESC` to go back to **Command mode** above.

# Git

See the [Git Cheat sheet](https://git-scm.com/cheat-sheet) for further info.

### Suggested protocol for working on GitHub:

1. Create a [GitHub account](https://github.com/signup).
2. Install the `gh` command line utility.
   * On Windows, in the Git bash terminal, use [WinGet](https://learn.microsoft.com/en-us/windows/package-manager/winget/):
     `winget install --id github.cli`.
     As the time of writing you will need admin rights. If you do not have them you can use
     [SSH login instead](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/about-authentication-to-github#ssh)
     for the next step.
3. Restart your terminal.
4. [Setup your Git identity](https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup)
   with the following commands:
   ```bash
   git config --global user.name "<your_first_name> <your_last_name>"
   git config --global user.email <your_email>
   ```
3. Link your local Git account to GitHub:
   ```bash
   gh auth login --git-protocol https --web --clipboard
   ```
   and press `Enter` twice. A GitHub page will open where you can paste the code already in your clipboard.
   You should now be able to publish the files from your local machine to GitHub.
5. Create a new public repository (repo) with README from within the GitHub website
   with the ["New"](https://github.com/new) button with a name of your choice.
6. On your machine, create an empty folder in a path without special characters for your future works
(e.g `C:\repositories` or `/c/repositories` in Git bash).
7. Clone your repository on your PC inside the newly created folder:
```bash
cd <repositories_folder>
gh repo clone <your_user_name>/<your_repo_name>
```
or if you do not have the `gh` utility:
```bash
git clone https://github.com/<your_user_name>/<your_repo_name>
```
You can copy the home page URL of the repository from your browser.

Now you should have the folder (and content) of your repo in your PC,
and you can start creating and editing the files of your project.

Any time you create or edit a file, you should tell git to track it with the
command `git add <file>`. If you want to rename a file, inside a git
repo use `git mv`, and to remove use `git rm`.

Once the modifications of a file done, or actually at any time you
wish to do so, you can save your progress locally with a commit:
```bash
git commit -m "<message>"
```
where `message` should be a short text describing the essence of the changes
that you have made and why.

A faster way of dealing with multiple files is to use
```bash
git commit -am "<message>"
```
Notice the `a` before the `m`, it tells Git to `git add` all the files that
have been created or edited to the commit.

To synchronize your local modifications to the online GitHub repository
(on the GitHub server), you do a 'push':
```bash
git push
```
from within the directory of your local repo.

If the online (GitHub) repo contains updates that you do not yet have in
your local repo because, for example, you pushed the changes from
another device, in order to synchronize your local repo you do a 'pull':
```bash
git pull
```
from within the directory of your local repo. This will try to download
and merge all the modifications from the server. Try not to modify your
files from your different devices without saving your progress each time
(i.e. pushing), because then Git will not know wich version you want to
keep and you'll need to solve conflicts manually.

## Exercise 1

Create a file named `helloworld.f95` with the following content:
```fortran
PROGRAM helloworld
  PRINT *, "Hello world"                                                
END PROGRAM helloworld
```
Now to compile type:
```bash
gfortran -o helloworld helloworld.f95
```
and to execute
```bash
./helloworld
```
If you encounter a 'not an executable' error, you might need to do once
```bash
chmod u+x ./helloworld
```
Note that the program name (specified via the `PROGRAM` statement)
does not necessarily have to match the name of the executable file.
In the above example it does, but does not have to.

## Exercise 2

Create a folder `test` inside your repository and move
`helloworld.f95` into that folder. Add the `helloworld.f95` to the
tracked files, commit and push.

# gfortran

# gnuplot
