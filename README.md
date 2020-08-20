# Dotconfig files
Welcome to my person dotconfig repo, a generic setup for dotconfig

In the master branch is information and other generalities, in the sub-branches these are stripped, to reduce clutter, to specific things

## Usage
1. Pick a branch
2. Git clone --bare and checkout (see guide included in credit section)
3. cp -R *.dist *
4. tweak to fit your configuration

### Notes
* Linux refers to arch linux
  * pacman derivatives for pkglist
  * yay for altpkglist
* Xanmod Kernel (compiled with _microarchitecture=14 use_numa=n use_tracers=n use_ns=y makepkg -sic)

You might need to tweak it possibly much and more for other distros

## TODO
Test step 3 for accuracy

## Credits
https://www.atlassian.com/git/tutorials/dotfiles

