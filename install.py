#!/usr/bin/env python

import os
import sys

import sh

pwd = os.getcwd()

dry = len(sys.argv) != 2
if dry:
    print 'Would perform the following actions:'

# oh-my-zsh. get fork and checkout branch.
if not os.path.exists('oh-my-zsh'):
    if dry:
        print '\tgit clone git@github.com:nathanielksmith/oh-my-zsh.git'
    else:
        sh.git('clone', 'git@github.com:nathanielksmith/oh-my-zsh.git')

if not dry:
    sh.cd('oh-my-zsh')
    sh.git('checkout', 'newtheme')
    sh.cd('..')


# setup .xmonad directory if needed
xmonad_path = '../.xmonad/'
if not os.path.exists(xmonad_path):
    if dry:
        print '\tmkdir %s' % xmonad_path
    else:
        sh.mkdir(xmonad_path)

# link dotfiles, .vim, .oh-my-zsh.
paths = [
    ['xmonad.hs', xmonad_path],
    ['.zshrc', '../'],
    ['oh-my-zsh', '../.oh-my-zsh'],
    ['.vimrc', '../'],
    ['.vim', '../'],
    ['.tmux.conf', '../'],
]

for pair in paths:
    src = os.path.join(pwd, pair[0])
    dest = pair[1]
    if dry:
        print '\tln -s %s %s' % (src, dest)
    else:
        sh.ln('-s', src, dest)

# git config
git_cfg = [
    ['user.name', 'nathaniel smith'],
    ['user.email', 'nathanielksmith@gmail.com'],
    ['color.ui', 'true'],
]
for cfg in git_cfg:
    if dry:
        print '\tgit config --global %s %s' % (cfg[0], cfg[1])
    else:
        sh.git('config', '--global', cfg[0], cfg[1])

if dry:
    print 'Run as "install.py go" to perform actions.'
