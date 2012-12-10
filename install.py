#!/usr/bin/env python

import os
import sys

import sh

pwd = os.getcwd()

dry = len(sys.argv) != 2
if dry:
    print 'Would perform the following actions:'

if not os.path.exists('oh-my-zsh'):
    if dry:
        print '\tgit clone git@github.com:nathanielksmith/oh-my-zsh.git'
    else:
        sh.git('clone', 'git@github.com:nathanielksmith/oh-my-zsh.git')

if not dry:
    sh.cd('oh-my-zsh')
    sh.git('checkout', 'newtheme')
    sh.cd('..')


xmonad_path = '../.xmonad/'
if not os.path.exists(xmonad_path):
    if dry:
        print 'mkdir %s' % xmonad_path
    else:
        sh.mkdir(xmonad_path)

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

if dry:
    print 'Run as "install.py go" to perform actions.'
