# Should source from ~/.cshrc

# TODO: DRY principle is violated with csh and bash versions of this file.
# A good ideas for solving this are at: 
#   http://stackoverflow.com/questions/496702/can-a-shell-script-set-environment-variables-of-the-calling-shell
# To summarize it:
#   : Create unihomeEnv.perl with data structures containing env vars, aliases, etc.
#   : ln -s unihomeEnv.perl unihomeEnv.csh
#   : ln -s unihomeEnv.perl unihomeEnv.sh
#   : unihomeEnv.perl outputs shell commands based on the shell, determined from $0 .
#   : A tcsh shell that wishes to source the env would: 'eval `unihomeEnv.csh`'.  bash would: 'eval `unihomeEnv.sh`'.

if !($?unihomeDir) then
    setenv unihomeDir "~/unihome"
endif
if !($?CLEARCASE_ROOT) then
    setenv CLEARCASE_ROOT 0
endif
# Packages I want to install that aren't always a standard part of a Linux distro.
setenv unihomePackages "vim mercurial"
setenv EDITOR vim

# Default history buffer always too small
set history=10000
# Change how shell keeps command history
set histdup erase
set ignoreeof=1

#alias title 'echo -n "\033]0;\!*\007"'
alias title 'echo -n "]2;\!*"'
# Make hostname shorter
set hostString=`hostname | sed 's/\..*$//'`

# Set prompt
#
# To use cyan color prompt: [36m%}
# To use yellow color prompt: [33m%}
if ($?prompt) then
  if (-o /bin/su ) then
    set prompt = "$hostString : \!# "
  else
    if ($CLEARCASE_ROOT == 0 ) then
      set ccroot = ""
    else
      set ccroot = " `basename $CLEARCASE_ROOT`"
    endif
  endif
  set prompt = "%{\033[36m%}$hostString$ccroot %~ :%{\033[0m%} "
endif

alias cwdcmd title "$hostString : '`pwd`'"

alias vim 'vim \!*; echo "Thanks for flying Vim"; cwdcmd'
alias v 'vim'
alias sa 'source $unihomeDir/rc/unihome.cshrc'
alias ip '/sbin/ifconfig -a'
alias skt 'netstat -anu -f inet'
alias dt '/usr/dt/bin/dtterm'
alias vv 'perl $unihomeDir/scripts/open.pl \!*; cwdcmd'
alias s 'source'
alias 0 'cat $unihomeDir/misc/clearScreen.txt'
alias l 'ls -lart1'
alias fdate "date '+%Y%m%d_%H%M%S'"
alias h history
alias sv 'cleartool setview'
alias append_bin_path 'setenv PATH `$unihomeDir/scripts/addpaths_csh $PATH \!*`'
alias prepend_bin_path 'setenv PATH `$unihomeDir/scripts/addpaths_csh \!* $PATH `'
alias append_lib_path 'setenv LD_LIBRARY_PATH `$unihomeDir/scripts/addpaths_csh $LD_LIBRARY_PATH \!*`'
alias prepend_lib_path 'setenv LD_LIBRARY_PATH `$unihomeDir/scripts/addpaths_csh \!* $LD_LIBRARY_PATH `'

# Files of interest for opening up together in an editor.
setenv unihomeFiles "README rc/unihome.sh rc/unihome.cshrc rc/.vimrc rc/.hgrc scripts/unihomeSetup.sh scripts/addpaths_csh scripts/open.pl"

