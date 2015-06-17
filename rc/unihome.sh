# Enable core files
ulimit -c unlimited

alias v='vim'

# Open GDB on the core file and its executable most recently generated in the subtree rooted at the ./ dir.
function gdb_recent() { recent_core=`find . -name "core.*" | xargs ls -dt | head -1` && cored_exec_file=`file $recent_core | sed "s/.*, from '\(.*\)'/\1/"` && gdb `find . -perm /111 -type f -name "$cored_exec_file*"` $recent_core ; }

if [[ `uname` == 'Darwin' ]]; then
  # Mac doesn't support --color, and -G means something different than on Linux.
  alias ls='ls -Gp'
  export CLICOLOR=1
  # Dark background colors:
  #
  # Default color for directory is too dark
  export LSCOLORS=GxFxCxDxBxegedabagaced
  # Light background colors:
  #export LSCOLORS=ExFxBxDxCxegedabagacad

  alias sa='source ~/.bash_profile'
  function e() {
    open -n -a ~/sw/emacs/live/nextstep/Emacs.app --args --reverse-video "$@"
  }
else
  alias ls='ls --color'
  alias sa='source ~/.bashrc'
  function e() {
    emacs --reverse-video "$@" &
    # So as I can close the shell and Emacs stays open
    disown
  }
fi
alias l='ls -lart'
alias c='cat'
alias m='mplayer -loop 0'
#citc=`g4 client -o | grep '^Client:' | sed 's|Client:\tboreilly:\([^:]*\):.*|\1|g'`
alias title='echo -n "]2;\!*"'
alias cwdcmd='title "$hostString : `pwd`"'
alias 0='cat ~/unihome/misc/clearScreen.txt'
alias fdate="date '+%Y%m%dT%H%M%S'"
alias h=history
alias gentags='etags `find . -name "*.h" -o -name "*.c" -o -name "*.cc" -o -name "*.cpp" -o -name "*.java" -o -name "*.py" -o -name "*.pl" -o -name "*.sh" -o -name "*.mk" -o -name "*akefile*" -o -name "*.f90"`'
alias acmd='cmd runas /noprofile /user:Administrator cmd'
PATH=~/bin:$PATH
export EDITOR=vim
export PAGER=less
export VISUAL=vim
# Vi bindings when arrowing up to previous commands
# Is this responsible for strange mutations to command history? History should be immutable.
#set -o vi
# Based on advice at: http://tldp.org/HOWTO/Xterm-Title-4.html
#
# Also used elements from a default Ubuntu bashrc
case $TERM in
    xterm*)
        export PS1="\[\033]0;\h \w\007\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\033[01;40m\]\h \w :\[\033[00m\] "
        ;;
    *)
        PS1="bash : "
        ;;
esac

# Command to make Windows symlinks
mklink(){
  mapfile -t ph < <(cygpath -aw "$@")
  [ -d "$2" ] && d=/d || d=
  runas /noprofile /user:Administrator cmd /c mklink $d "${ph[@]}"
}

