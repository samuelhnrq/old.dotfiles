# My Dotfile

This might acutally be the billionth attempt at building my own rice, but this time I actually took the time to backup and save my progress. Hope they can be useful for someone else.

I use gnu stow to manage these, you can trust glob expanding to install them all with good-old `*` or you can enable specific components like so:

``` bash
stow -v --dotfiles vim
```

I've tried to be as modular as possible with my packages, so far they are a 1:1 with depencencies, good old unix philosophy a diffent program for each task. You can use the package names as depency list. I might create a simple makefile for stow-ing them automatically.

_WIP_

