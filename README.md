# My Dotfile

This might acutally be the billionth attempt at building my own rice, but this time I actually took the time to backup and save my progress. Hope they can be useful for someone else.

I use gnu stow to manage these, you can trust glob expanding to install them all with good-old `*` or you can enable specific components like so:

``` bash
stow -v vim
```

I've tried to be as modular as possible with my packages, so far they are a 1:1 with depencencies, good old unix philosophy a diffent program for each task. You can use the package names as depency list. I might create a simple makefile for stow-ing them automatically.

## Known issues
Given `--dotfiles` option of GNU stow is still kind of rough arround the edges ([see here](https://github.com/aspiers/stow/issues/33)) when navigating around this repo in the shell might just look like a buch of empty folders, don't forget `-a` when calling `ls`.

_WIP_

