# Enable core files
ulimit -c 50000 

alias v='vim'
function e() { emacs --reverse-video "$@" & }
alias l='ls -lart'
alias c='cat'
alias m='mplayer -loop 0'
alias title='echo -n "]2;\!*"'
alias cwdcmd='title "$hostString : `pwd`"'
alias sa='source $unihomeDir/rc/unihome.sh'
alias 0='cat $unihomeDir/misc/clearScreen.txt'
alias fdate="date '+%Y%m%d_%H%M%S'"
alias h=history
alias gentags='etags `find . -name "*.h" -o -name "*.cc" -o -name "*.java" -o -name "*.py" -o -name "*.pl" -o -name "*.sh" -o -name "*.mk" -o -name "*akefile*"`'
PATH=~/bin:$PATH
export EDITOR=vim
export PAGER=less
export PS1="\e[0;36m\h \w :\e[m "

