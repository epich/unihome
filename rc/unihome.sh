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
  # General purpose command to open apps at the command line, eg:
  #   o nextstep/Emacs.app -r /tmp/foo.txt
  function o() {
    # A hack to smuggle pwd to Emacs.app. It seems open undefines
    # PWD, even whilst other env vars are inherited.
    export APP_OPEN_PWD=`pwd`
    args=( $@ )
    open -n -a $1 --args ${args[@]:1}
  }
  function e() {
    o ~/sw/emacs/live/nextstep/Emacs.app -r "$@"
  }
else
  alias ls='ls --color'
  alias sa='source ~/.bashrc'
  function e() {
    emacs "$@" &
    # So as I can close the shell and Emacs stays open
    disown
  }
fi
alias .=pwd
alias l='ls -lart'
alias c='cat'
alias em='emacs -nw'
alias m='mplayer -loop 0'
#citc=`g4 client -o | grep '^Client:' | sed 's|Client:\tboreilly:\([^:]*\):.*|\1|g'`
alias title='echo -n "]2;\!*"'
alias cwdcmd='title "$hostString : `pwd`"'
alias 0='cat ~/unihome/misc/clearScreen.txt'
# Use date --iso-8601=ns ?
alias fdate="date '+%Y%m%dT%H%M%S'"
alias h=history
alias gentags='etags `find . -name "*.h" -o -name "*.c" -o -name "*.cc" -o -name "*.cpp" -o -name "*.java" -o -name "*.py" -o -name "*.pl" -o -name "*.sh" -o -name "*.mk" -o -name "*akefile*" -o -name "*.f90"`'
alias acmd='cmd runas /noprofile /user:Administrator cmd'
PATH=~/bin:~/.local/bin:$PATH
export EDITOR=vim
export PAGER='less -X'
export VISUAL=vim
export HISTTIMEFORMAT='%Y%m%dT%H%M%S '
# Vi bindings when arrowing up to previous commands
# Is this responsible for strange mutations to command history? History should be immutable.
#set -o vi

# Based on advice at: http://tldp.org/HOWTO/Xterm-Title-4.html
#
# Also used elements from a default Ubuntu bashrc
#
# Set MY_LOCALHOST_COLOR to an ANSI color code to customize it in the prompt.
# - 32 is green
# - 33 is yellow
case $TERM in
    xterm*)
	# With gray background:
        #export PS1="\[\033]0;\h \w\007\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\033[01;40m\]\h \w :\[\033[00m\] "
	PS1='${debian_chroot:+($debian_chroot)}\[\033[01;${MY_LOCALHOST_COLOR:-32}m\]\h\[\033[00m\] \[\033[01;36m\]\w\[\033[00m\] '
        ;;
    *)
        PS1="bash : "
        ;;
esac

# If the .xsession-errors file is not a symbolic link, delete it and create it as such
# From https://www.daniloaz.com/en/how-to-prevent-the-xsession-errors-file-from-growing-to-huge-size/
if [ ! -h $HOME/.xsession-errors ]; then
 /bin/rm $HOME/.xsession-errors
 ln -s /dev/null $HOME/.xsession-errors
fi

export LASTCORE=$(lscpu -p | tail -n 1 | cut -d, -f 1)
# Execute a command with affinity to one core. eg 'coreaff blaze build :all'
function coreaff() {
  taskset -c 2-$LASTCORE "$@"
}
export -f coreaff

# Command to make Windows symlinks
mklink(){
  mapfile -t ph < <(cygpath -aw "$@")
  [ -d "$2" ] && d=/d || d=
  runas /noprofile /user:Administrator cmd /c mklink $d "${ph[@]}"
}

