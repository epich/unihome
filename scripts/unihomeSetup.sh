#!/bin/bash
#
# Setup unihome.  Typically ran after first setting up a user account on a Unix host.
#
# Additional manual actions:
#   : In ~/.cshrc :
#       setenv unihomeDir <path>
#       source $unihomeDir/rc/unihome.cshrc
#   : In ~/.bashrc :
#       export unihomeDir <path>
#       source $unihomeDir/rc/unihome.sh
#   : If do not like dir colors (eg RHEL), 'cp /etc/DIR_COLORS ~/.dir_colors' and change 'DIR 01;34' to 'DIR 01;37'.
#   : ln -s $unihomeDir/.vimrc ~
#   : ln -s $unihomeDir/.hgrc ~

echo "No op"

# TODO: Should account for unihome being available over NFS.  Allow input param for unihome path, with default being ~/unihome .
# TODO: source rc/unihome.profile to retrieve $unihomeDir value.

# Files that don't allow includes in some form are copied.
#
# TODO: Check whether file currently exists and backup if so.
#ln -s $unihomeRootPath/rc/.vimrc ~
#ln -s $unihomeRootPath/rc/.hgrc ~

