# Enable core files
ulimit -c 50000 

alias v='vim'
function e() { emacs --reverse-video "$@" & }
alias l='ls -lart'
alias c='cat'
alias m='mplayer -loop 0'
alias title='echo -n "]2;\!*"'
alias cwdcmd='title "$hostString : `pwd`"'
alias sa='source ~/unihome/rc/unihome.sh'
alias 0='cat $unihomeDir/misc/clearScreen.txt'
alias fdate="date '+%Y%m%d_%H%M%S'"
alias h=history
alias gentags='etags `find . -name "*.h" -o -name "*.cc" -o -name "*.java" -o -name "*.py" -o -name "*.pl" -o -name "*.sh" -o -name "*.mk" -o -name "*akefile*"`'
alias acmd='cmd runas /noprofile /user:Administrator cmd'
PATH=~/bin:$PATH
export EDITOR=vim
export PAGER=less
#export PS1="\e[0;36m\h \w :\e[m "
export PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "

# Command to make Windows symlinks
mklink(){
  mapfile -t ph < <(cygpath -aw "$@")
  [ -d "$2" ] && d=/d || d=
  runas /noprofile /user:Administrator cmd /c mklink $d "${ph[@]}"
}

