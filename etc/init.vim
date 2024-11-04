"" ============================================================================
"" + File name:          vimrc
"" + Description:        Vim startup settings
"" + Author:             leaf@anardil.net
"" ============================================================================

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  set title
  set langmenu=en_US      " Sets languages
  set shell=bash          " So vim plays nice with fish
  set history=300         " Sets how many lines of history VIM has to remember
  set autoread            " Pick up changes from outside this vim session
  set fileformat=unix     " Use Unix line endings
  set confirm             " Ask to save instead of complaining
  set nocompatible
  set visualbell

  set splitbelow splitright
  set encoding=utf-8 termencoding=utf-8 fileencodings=utf-8
  set hidden nobackup nowritebackup noswapfile
  set ttimeout ttimeoutlen=20
  set tags=./tags;,tags;

  nnoremap <SPACE> <Nop>
  let g:mapleader = " "

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Vundle
    let has_vundle=1
    let vundle_url='https://github.com/VundleVim/Vundle.vim'

    if !filereadable(expand('~/.vim/bundle/Vundle.vim/README.md'))
      silent !mkdir -p ~/.vim/bundle
      silent exe "!git clone " . g:vundle_url . " ~/.vim/bundle/Vundle.vim"
      let has_vundle=0
    endif

    filetype off
    set runtimepath+=~/.vim/bundle/Vundle.vim

    try
      call vundle#begin()
        Plugin 'VundleVim/Vundle.vim'

        Plugin 'benmills/vimux'
        Plugin 'christoomey/vim-tmux-navigator'
        Plugin 'junegunn/fzf'
        Plugin 'junegunn/fzf.vim'

        Plugin 'ehamberg/vim-cute-python'
        Plugin 'Gandalf-/vim-sh-syntax'
        Plugin 'justinmk/vim-syntax-extra'

        Plugin 'tpope/vim-fugitive'
        Plugin 'tpope/vim-sleuth'
        Plugin 'simnalamburt/vim-mundo.git'
        Plugin 'rhysd/vim-clang-format'

        Plugin 'w0rp/ale'
        Plugin 'github/copilot.vim'

        if has_vundle == 0
          :PluginInstall
        endif
      call vundle#end()
    catch
    endtry
    filetype plugin indent on

  " vimux
    let g:VimuxOrientation = "h"
    let g:VimuxHeight = "33"

    let g:fzf_action = {
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

  " ALE
    let g:ale_fixers = {
    \   '*'      : ['remove_trailing_lines', 'trim_whitespace'],
    \   'haskell': ['hlint', 'stylish-haskell', 'remove_trailing_lines', 'trim_whitespace'],
    \   'python' : ['ruff', 'ruff_format', 'remove_trailing_lines', 'trim_whitespace'],
    \   'go'     : ['gofmt', 'remove_trailing_lines', 'trim_whitespace'],
    \   'rust'   : ['rustfmt', 'remove_trailing_lines', 'trim_whitespace'],
    \   'c'      : ['clang-format', 'remove_trailing_lines', 'trim_whitespace'],
    \   'cpp'    : ['clang-format', 'remove_trailing_lines', 'trim_whitespace']
    \}

    let g:ale_lint_on_text_changed = 'never'
    let g:ale_lint_on_save = 1
    let g:ale_lint_delay = 500
    let g:ale_linters = {
      \ 'html'   : ['tidy'],
      \ 'python' : ['ruff', 'mypy'],
      \ 'haskell': ['hlint', 'stack_build', 'stack_ghc'],
      \ 'c'      : [],
      \ 'cpp'    : []
      \ }

    let g:haskell_hlint_options = '-j'
    let g:ale_c_clang_options = "-std=c++11 -Wall -Wextra -D_DEFAULT_SOURCE -D_SVID_SOURCE"
    let g:ale_c_gcc_options = "-std=c++11 -Wall -Wextra -D_DEFAULT_SOURCE -D_SVID_SOURCE"

    let g:ale_fix_on_save = 1
    let g:ale_set_quickfix = 1
    let g:ale_set_highlights = 0

    highlight link ALEWarningSign Comment
    highlight link ALEWarning Comment

    highlight link ALEErrorSign String
    highlight link ALEError String

    let g:ale_sign_error = 'X' " '✘'
    let g:ale_sign_warning = '>' " '▶'
    let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

  " Search
    nnoremap \ :Rg<SPACE>
    vnoremap \ :<C-u>Rg <C-r><C-w><CR>

    set grepprg=rg\ --column
    set grepformat=%f:%l:%c%m

    command! -nargs=+ -complete=file -bar Search silent! grep! <args>|cwindow|redraw!

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => UI
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " General
    colorscheme desert
    set t_Co=256
    set number wrap textwidth=99 showcmd
    set scrolloff=4 showtabline=2 tabpagemax=30 laststatus=2 cmdheight=1

  " ui autocommands
    augroup ui_commands
      autocmd!

      " Resize vim windows when overall window size changes
      autocmd VimResized * wincmd =

      " cursor line
      autocmd InsertEnter,InsertLeave * set cursorline!
    augroup end

  " Be as wild as possible
    set wildmode=full wildmenu
    set wildignore=*.o,*~,*.pyc,/usr/include/*,*.class,*.bak,*.hi
    set wildignore+=.git\*,.svn\*
    set wildignorecase

  " Performance!?
    set lazyredraw ttyfast

  " Syntax, Column and cursor lines
    syntax on sync minlines=256
    set colorcolumn=+1           " Handy bar so we know when lines are too long
    set synmaxcol=200            " Limit column highlights to 200 columns
    highlight ColorColumn  guibg=#666666

  " Cursor line
    set cursorline              " Handy line so we know where we are
    highlight CursorLine   guibg=#666666

  " Line numbers
    highlight       LineNr guifg=yellow
    highlight CursorLineNr guifg=yellow guibg=#666666 gui=bold

  " Window split
    set fillchars+=vert:│
    highlight VertSplit    guifg=black guibg=#333333 gui=bold

  " Status line
    highlight StatusLine   guifg=black guibg=white     gui=bold
    highlight StatusLineNC guifg=black guibg=lightgrey
    set statusline=%f    " Path.
    set statusline+=%m   " Modified flag.
    set statusline+=%r   " Readonly flag.
    set statusline+=%w   " Preview window flag.
    set statusline+=\    " Space.
    set statusline+=%=   " Right align.

    try
      function! LinterStatus() abort
          let l:counts         = ale#statusline#Count(bufnr(''))
          let l:all_errors     = l:counts.error + l:counts.style_error
          let l:all_non_errors = l:counts.total - l:all_errors
          return l:counts.total == 0 ? '' : printf('%2dw %2de', all_non_errors, all_errors)
      endfunction
      set statusline+=%{LinterStatus()}
    catch
    endtry

    set statusline+=\    " Space.
    set statusline+=\ %4l\ %3c\ %3p%%


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Miscellaneous
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " virtual edit
  set virtualedit=block

  " misc autocommands
  augroup misc_commands
    autocmd!

  " make uses tabs
    autocmd FileType make setlocal noexpandtab

  " Searching
    set hlsearch
    set incsearch
    " set ignorecase
    " set smartcase
    " set infercase
    set showmatch
    set ruler

  " Search for visually selected text, forwards or backwards!
    vnoremap <silent> * :<C-U>
      \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
      \gvy/<C-R><C-R>=substitute(
      \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
      \gV:call setreg('"', old_reg, old_regtype)<CR>
    vnoremap <silent> # :<C-U>
      \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
      \gvy?<C-R><C-R>=substitute(
      \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
      \gV:call setreg('"', old_reg, old_regtype)<CR>

  " mouse!
    function! ToggleMouse()
        " check if mouse is enabled
        if &mouse == 'a'
            " disable mouse
            set mouse=
            echo "Mouse disabled"
        else
            " enable mouse everywhere
            set mouse=a
            echo "Mouse enabled"
        endif
    endfunc
    map <silent> <leader>b :call ToggleMouse()<cr>

    " scrolling like a GUI!
      map <ScrollWheelUp> 5<C-Y>
      map <ScrollWheelDown> 5<C-E>

  " Folding
    set foldtext=MyFoldText()
    function! MyFoldText()
      let nucolwidth = &fdc + &number*&numberwidth
      let winwd = winwidth(0) - nucolwidth - 5
      let foldlinecount = foldclosedend(v:foldstart) - foldclosed(v:foldstart) + 1
      if v:foldlevel == 1
        let prefix = " >>> "
      else
        let prefix = " >>> " . string(v:foldlevel) . ","
      endif
      let fdnfo = prefix . printf("%3s ", foldlinecount)
      " let line = strpart(getline(v:foldstart), 0 , winwd - len(fdnfo))
      let line =  "  ..."
      let fillcharcount = winwd - len(line) - len(fdnfo)
      return line . repeat(" ",fillcharcount) . fdnfo
    endfunction
    set foldmethod=indent
    set foldnestmax=10
    set nofoldenable
    set foldlevel=2

  " Hit enter in the file browser to open the selected
  " file with :vsplit to the right of browser
    let g:netrw_sort_sequence = '[\/]$,*'   " directories first
    let g:netrw_browse_split  = 3           " open files in new tab
    let g:netrow_altv         = 1
    let g:netrw_winsize       = -28         " thinner width
    let g:netrw_banner        = 0           " hide the help info
    let g:netrw_liststyle     = 3           " tree mode
    let g:netrw_list_hide     = '.*\.swp$,.*\.pyc'

  " Clear junk before running commands
    noremap :! :!clear;
    noremap :make :!clear; make


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  set backspace=indent,eol,start   " allow backspacing over everything
  set tabstop=4
  set showmatch                    " Show matched brackets


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Macros
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " fzf
    nnoremap <leader>[ :Buffers<CR>
    nnoremap <leader>] :Files<CR>

  " vimux
    nnoremap <C-b> :VimuxRunLastCommand<CR>

  " ale
    nnoremap <leader>a :ALENextWrap<CR>
    nnoremap <C-a> :ALEDetail<CR>

  " clear highlighting
    nnoremap <silent> <leader><Space> :nohl<CR>

  " Scroll to bottom for note taking
    nnoremap <silent> <leader>J 100<c-e>5<c-y>

  " super secret encryption algorithm
    nnoremap <silent> <leader>G ggg?G<C-o>

  " re hardwrap a paragraph
    nnoremap <silent> <leader>f gqip<cr>

  " split the screen and move there
    nnoremap <silent> <leader>V <C-w>v<C-w>l
    nnoremap <silent> <leader>H <C-w>s

  " tags
    nnoremap <leader>h :tselect<Space>
    let g:tagdirection = "forward"
    let g:tagprevious = ""

    function! TagTraverse()
      let l:tagcurrent = expand('<cword>')
      if g:tagprevious != l:tagcurrent
          " reset
          exe 'tag ' . l:tagcurrent
          let g:tagdirection = "forward"
          let g:tagprevious = l:tagcurrent
      else
        if g:tagdirection == "forward"
          try
            :silent tnext
          catch
            try | :silent tprevious | catch | :echo "No other entries for this tag" | endtry
            let g:tagdirection = "backwards"
          endtry
        else
          try
            :silent tprevious
          catch
            try | :silent tnext | catch | :echo "No other entries for this tag" | endtry
            let g:tagdirection = "forward"
          endtry
        endif
      endif
      :norm zz
    endfunction
    nnoremap <silent> <C-n>  :call TagTraverse()<CR>

  " windows
    nnoremap <silent> <leader>q :q<CR>
    nnoremap <silent> <leader>Q :qall<CR>
    command! Q :qall

  " line number toggle
    nnoremap <silent> <leader>n :set nu!<CR>

  " stay in visual mode while changing indentation
    vnoremap < <gv
    vnoremap > >gv

  " toggling
    nnoremap <silent> <leader>L :set list!<CR>
    nnoremap <silent> <leader>z :set spell!<CR>
    nnoremap <silent> <leader>R :set relativenumber!<CR>
    function! SmartPaste()
      setlocal nu!
      if &paste
        setlocal nopaste
        :ALEEnableBuffer
      else
        setlocal paste
        :ALEDisableBuffer
      endif
    endfunction
    nnoremap <leader>p :call SmartPaste() <cr>

  " close, jump to last, make new buffer
    nnoremap <silent> <leader>x :bp<bar>sp<bar>bn<bar>bd<CR>
    nnoremap <silent> <leader>j :w<bar>b#<CR>
    nnoremap <silent> <leader>t :tabe %<cr>

  " paste result of command
    nnoremap <leader>i :r!

  " open buffer, open tab, reload, save, sudo save current file
    nnoremap <leader>e :e<space>
    nnoremap <leader>E :tabe<space>
    nnoremap <leader>l :e % <cr>zz
    nnoremap <leader>k :w<CR>

  " Mundo Undo Tree
    nnoremap <silent> <leader>m :MundoToggle<CR>

  " visual block mode
    nnoremap <silent> <leader>v <C-S-v>

  " generic ':' command
    nnoremap m :
    vnoremap m :

  " substitute line, global
    vnoremap <leader>s :s/
    nnoremap <leader>s V:s/
    nnoremap <leader>S :%s/
    nnoremap <Leader>c :%s/\<<C-r><C-w>\>//gc<Left><Left><Left>

  " repeat last colon command
    nnoremap <leader>. q:k<cr>
    vnoremap <leader>. q:k<cr>

  " running programs in Vim!
    function! Compile(compile_command)
      if filereadable("./Makefile") || filereadable("./makefile")
        make
      else
        execute "!" . a:compile_command . " " . bufname("%")
      endif
    endfunction

  " Where are we?
    function! CurrentDir()
      :!clear;readlink -f %
    endfunction
    nnoremap <leader>N :call CurrentDir()<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Movement
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " recenter
    nnoremap <silent> Z zz
    nnoremap <silent> <C-]> <C-]>zz
    nnoremap <silent> n nzz
    nnoremap <silent> N Nzz
    nnoremap <silent> * *zz
    nnoremap <silent> # #zz
    nnoremap <silent> <C-o> <C-o>zz
    nnoremap <expr> ' printf("'%c zz", getchar())

  " Move through columns correctly
    nnoremap <silent> j gj
    nnoremap <silent> k gk

  " Switch buffers, tabs with parens, braces
    noremap  <silent> ( :bp<cr>zz
    noremap  <silent> ) :bn<cr>zz

  " List navigation with arrows
    noremap <silent> <Left>  :lprev<cr>zz
    noremap <silent> <Right> :lnext<cr>zz

    nnoremap <silent> <Up>   H5<C-y>5gk
    nnoremap <silent> <Down> L5<C-e>5gj

    nnoremap <silent> <Esc>[A ddkP
    nnoremap <silent> <Esc>[B ddp

  " repeat last macro
    nnoremap <silent> <C-@> @@

  " move to end and start of lines
    nnoremap B ^
    vnoremap B ^
    nnoremap E $
    vnoremap E $

  " Smarter [Y]anking
    map Y y$

  " Dictionary!
    let g:dict_toggle_is_on = 1
    function! DictToggle()
      if g:dict_toggle_is_on
        set dictionary+=/usr/share/dict/words
        set complete+=k
        echo "Dictionary on"
        let g:dict_toggle_is_on = 0
      else
        set dictionary-=/usr/share/dict/words
        set complete-=k
        echo "Dictionary off"
        let g:dict_toggle_is_on = 1
      endif
    endfunction
    nnoremap <leader>+ :call DictToggle()<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Abbreviations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  iab w/ with
  iab teh the
  iab hav have
  iab ona on a
  iab waht what
  iab tiem time
  iab alot a lot
  iab THis This
  iab THat That
  iab dont don't
  iab wont won't
  iab whcih which
  iab diong doing
  iab dont' don't
  iab wont' won't
  iab prety pretty
  iab realy really
  iab whould would
  iab relaly really
  iab shoudl should
  iab haven' haven't
  iab useable usable
  iab recieve receive
  iab similiar similar
  iab tomorow tomorrow
  iab shouldnt shouldn't
  iab finacial financial
  iab someting something
  iab somethign something
  iab diference difference
  iab informatino information
  iab opprotunity opportunity
  iab occasionaly occasionally
  iab explainations explanations

" work settings
if filereadable(expand('~/scripts/etc/work.vim'))
  source ~/scripts/etc/work.vim
endif
