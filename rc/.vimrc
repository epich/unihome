" Some notes on configuring jVi in NetBeans:
"   Set ignorecase
"   Mappings for:
"       -
"       ;
"       '
"   Bind Ctrl+[ to Esc

" Make searches case insensitive, unless they contain upper case letters
" Also, highlight the searches.
set ignorecase
set smartcase
set hls

" Don't need backwards compatability.
set nocompatible

" Indent policy
set autoindent
set nosmartindent
set tabstop=3
set shiftwidth=3
set expandtab
set softtabstop=3
set autoread

" When cursor is moved, the timestamp of the file is checked and 
" if it changed the changes are loaded in.
"source ~/unihome/rc/WatchForChanges.vimrc
"WatchForChanges!

set matchpairs +=<:> " % command matches angle brackets now.

" Show matching parens.
set noshowmatch
" For some odd reason, Suse Linux is not obeying set noshowmatch.
" This seems to work (thanks Google).
let loaded_matchparen = 1

" Show current vi mode
set showmode

" Displays helpful information in lower right
set ruler

" Backspace deletes new lines.
set backspace=2

set whichwrap-=h,l,w,e,b
"set nowrap

" No automatic linebreaks when inserting text.
set textwidth=0 wrapmargin=0
" /etc/vimrc undermines my .vimrc, so this corrects for it.
autocmd BufRead *.txt set tw=0

" smartindent doesn't work properly in python files
autocmd BufRead *.py set nosmartindent

" No beep
set vb
"set t_vb=

" When typing '#' as the first character in a new line, the indent for
" that line is removed, the '#' is put in the first column.  The indent
" is restored for the next line.  If you don't want this, use this
" mapping: ":inoremap # X^H#", where ^H is entered with CTRL-V CTRL-H.
" When using the ">>" command, lines starting with '#' are not shifted
" right.
"
" Note: I tried this to get # style comments to work properly with smartindent,
" but with smartindent on or off, it doesn't behave quite right.  Instead I
" ended up turning smartindent for Python.
"inoremap # X#

set laststatus=2
" now set it up to change the status line based on mode
"au InsertEnter * hi StatusLine term=reverse ctermbg=5 gui=undercurl guisp=Magenta
"au InsertLeave * hi StatusLine term=reverse ctermfg=0 ctermbg=2 gui=bold,reverse
"hi CursorLine ctermbg=green cterm=none
"au InsertEnter * set cursorline
"au InsertLeave * set nocursorline
"function! InsertStatuslineColor(mode)
"  if a:mode == 'i'
"    hi statusline guibg=magenta
"  elseif a:mode == 'r'
"    hi statusline guibg=blue
"  else
"    hi statusline guibg=red
"  endif
"endfunction
"au InsertEnter * call InsertStatuslineColor(v:insertmode)
"au InsertLeave * hi statusline guibg=green

" default the statusline to green when entering Vim
hi statusline guibg=green

" Well, can't get the map I want to Esc, this is the best that works.
" It's a little more convenient than no mapping.
imap <Delete> <Esc>
map <Delete> <Esc>
imap <C-j> <Esc>
map <C-a> k
map <C-Enter> k
map <C-Shift> k
map <C-Home> k
map <C-End> k
map <End> k
map <Insert> k
map <S-Enter> k
map <Shift-Enter> k
map <Space-Enter> k
map <Shift-Space> k
map <C-Enter> k

" vimdiff related
map <C-h> <C-w><Left>
map <C-l> <C-w><Right>

" More mappings.
"map q i<Space><Esc>l%a<Space><Esc>h%h " Add spaces inside parens when cursor is on the right paren.
"map Q hx%lxh%h " Take away spaces inside parens when cursor is on the right paren.
noremap o :n<Enter>
noremap O :N<Enter>
noremap \ :w<Enter>
map zl zL
map zh zH
map zzztr :1,$s/\([0-9]\{10,10\}\)\([0-9]\{3,3\}\)\([0-9]\{3,3\}\)\([0-9]\{3,3\}\)/\1\.\2_\3_\4/g<Enter>
noremap M M0
noremap L L0
noremap H H0
"noremap n nzz
"noremap N Nzz

" Buggy Putty only allows customization of F1 through F4.
imap <F1> // TODO: temporary for debug
" This displays the syntax group for setting in one's .vimrc file.
map <F1> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<CR>
map <F2> :set autoindent<Enter>
imap <F2> <Esc>:set noautoindent nosmartindent<Enter>a
"imap <F2> printf( // TODO: temporary for debug<Enter><Tab><Tab><Tab>""<Enter>"\n" );<Esc>ki
"imap <F3> std::cout << "" << std::endl; // TODO: temporary for debug<Esc>BBBBBBa
"imap <F3> COE_LOG( LM_EMERGENCY, <Enter><Tab><Tab><Tab>""<Enter>"\n" ); // TODO: temporary for debug<Esc>ki
imap <F3> org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // TODO: temporary for debug<Enter><Tab><Tab><Tab>"DEBUG: ",<Enter>new Object[]{} );<Esc>khi
"TODO: This causes cores for some reason.
"imap <F4> fprintf( stdout, // TODO: temporary for debug<Enter><Tab><Tab><Tab>"%s:%s:%s: "<Enter>"\n", __FILE__, __LINE__, __PRETTY_FUNCTION__ ); fflush(stdout); <Esc>ki
" This is for use in debugging isis-lanl code.  When in the RTN process, we use stdout and within the readout process stderr.  Annoying: yes.
imap <F4> fprintf( stderr, // TODO: temporary for debug<Enter><Tab><Tab><Tab>"DEBUG: "<Enter>"\n" ); fflush(stderr); fflush(stdout); <Esc>ki
"imap <F4> SORDS_Logger.getInstance().Log( SystemAlertLevel.DEBUG, // TODO: temporary for debug<Enter><Tab><Tab><Tab>"" );<Esc>3hi
"map <F9> :set autoindent smartindent<Enter>
"imap <F9> :set noautoindent nosmartindent<Enter>

map - $

noremap ; M08j8<C-E>
noremap ' M08k8<C-Y>
"map f <C-Y>
map s 8<C-E>
map f 8<C-Y>
"map F 8f
map S 8s
"map 9 <C-E> "Something more benign, I keep hitting 9 instead of 0.
imap <S-Backspace> <Esc>xa

" Without this, the wrong thing happens when using vim's visual mode
behave xterm

"let &titlestring = hostname() . "[vim(" . expand("%:t") . ")]"
"if &term == "screen"
"   set t_ts=k
"   set t_fs=\
"endif
"if &term == "screen" || &term == "xterm"
"   set title
"endif
"set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}

" Taken from http://www-vlsi.stanford.edu/~jsolomon/vim/
"autocmd BufEnter * let &titlestring = $HOSTNAME . ":" . expand("%:p:~")
"autocmd BufEnter * let &titlestring = $HOSTNAME . expand("%:t") . " @ " . expand("%:~")
autocmd BufEnter * let &titlestring = expand("%:t") . " @ " . expand("%:~")
"if &term == "xterm" || &term == "vt220"
   set title
   set t_ts=]2;
   set t_fs=
"endif

"Syntax highlighting
"source /usr/local/share/vim/vim63/vimrc_example.vim
"source /usr/local/share/vim/vim63/mswin.vim
"behave mswin
"syntax on
syntax enable

"  if has("terminfo")
"    let &t_Co=8
"    let &t_Sf="\<Esc>[3%p1%dm"
"    let &t_Sb="\<Esc>[4%p1%dm"
"  else
"    let &t_Co=8
"    let &t_Sf="\<Esc>[3%dm"
"    let &t_Sb="\<Esc>[4%dm"
"  endif

" Necessary to get syntax highlighting.  Taken from vim.wikia.com
if has("terminfo")
  let &t_Co=16
  let &t_AB="\<Esc>[%?%p1%{8}%<%t%p1%{40}%+%e%p1%{92}%+%;%dm"
  let &t_AF="\<Esc>[%?%p1%{8}%<%t%p1%{30}%+%e%p1%{82}%+%;%dm"
else
  let &t_Co=16
  let &t_Sf="\<Esc>[3%dm"
  let &t_Sb="\<Esc>[4%dm"
endif

runtime macros/matchit.vim

" Colors
set background=dark
highlight Statusline ctermfg=Black ctermbg=Green guifg=Black guibg=White term=none cterm=none gui=none
highlight Normal guibg=Black guifg=White term=none cterm=none gui=none
highlight cComment ctermfg=cyan guifg=cyan term=none cterm=none gui=none
highlight cTodo ctermfg=blue guifg=blue ctermbg=black guibg=black term=none cterm=none gui=none
highlight cInclude ctermfg=green guifg=green term=none cterm=none gui=none
highlight cError ctermfg=red guifg=red term=none cterm=none gui=none
highlight cFloat ctermfg=white guifg=white term=none cterm=none gui=none
highlight Character ctermfg=white guifg=white term=none cterm=none gui=none
highlight cNumber ctermfg=white guifg=white term=none cterm=none gui=none
highlight cppBoolean ctermfg=white guifg=white term=none cterm=none gui=none
highlight cString ctermfg=magenta guifg=magenta term=none cterm=none gui=none
highlight cType ctermfg=green guifg=green term=none cterm=none gui=none
" Regretably, bool is lumped in with virtual, explicit, etc. in cppType instead of cType. Lame.
highlight cppType ctermfg=green guifg=green term=none cterm=none gui=none
highlight perlIdentifier ctermfg=white guifg=white term=none cterm=none gui=none

highlight Todo ctermfg=blue guifg=blue ctermbg=black guibg=black term=none cterm=none gui=none

highlight String ctermfg=magenta guifg=magenta term=none cterm=none gui=none
highlight Constant ctermfg=white guifg=white term=none cterm=none gui=none
highlight Character ctermfg=white guifg=white term=none cterm=none gui=none
highlight Number ctermfg=white guifg=white term=none cterm=none gui=none
highlight Boolean ctermfg=white guifg=white term=none cterm=none gui=none
highlight Float ctermfg=white guifg=white term=none cterm=none gui=none

" XML syntax highlighting
highlight xmlTag ctermfg=yellow
highlight xmlTagName ctermfg=yellow
highlight xmlEndTag ctermfg=yellow

