"" ============================================================================
"" + File name:          vimrc
"" + Description:        Vim startup settings
"" + Author:             leaf
"" ============================================================================
 
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  set title
  set langmenu=en_US      " Sets languages
  set sh=bash             " So vim plays nice with fish
  set ff=unix             " No crazy DOS line endings please
  set history=300         " Sets how many lines of history VIM has to remember
  set autoread            " Pick up changes from outside this vim session
  set fileformat=unix     " Use Unix line endings
  set nocompatible        " Use Vim settings, because IMproved
  set confirm             " Ask to save instead of complaining

  set splitbelow splitright
  set encoding=utf-8 termencoding=utf-8 fileencodings=utf-8
  set hidden nobackup nowritebackup noswapfile
  set ttimeout ttimeoutlen=20

  let hostname = substitute(system('hostname'), '\n', '', '') " where are we?

  let g:tex_conceal = ""  " Don't hide LaTeX symbols
  nnoremap <SPACE> <Nop>
  let g:mapleader = " "

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Vundle 
    filetype off
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()
      Plugin 'vimwiki/vimwiki'
      Plugin 'VundleVim/Vundle.vim'
      Plugin 'Shougo/neocomplete.vim'
      Plugin 'Shougo/neosnippet.vim'
      Plugin 'Shougo/neosnippet-snippets'
      Plugin 'Shougo/neoinclude.vim'
      Plugin 'vim-syntastic/syntastic'
    call vundle#end()
    filetype plugin indent on

  " vimwiki
    autocmd TextChanged,TextChangedI *.md silent write
    let g:vimwiki_table_mappings = 0
    let g:vimwiki_global_ext     = 0
    let g:vimwiki_folding        = 'custom'

    if hostname == "wkstn-avoecks"
      let g:vimwiki_root = '~/vimwiki'
      let g:vimwiki_list = [{
            \ 'path': '~/vimwiki/', 
            \ 'syntax': 'markdown',
            \ 'ext': '.md'}]

    else
      let g:vimwiki_root = '~/google_drive'
      let g:vimwiki_list = [{
            \ "path": '~/google_drive/', 
            \ "path_html": '/tmp/html/', 
            \ "syntax": 'markdown', 
            \ "ext": '.md', 
            \ "css_file": '~/.vim/bundle/vimwiki/autoload/vimwiki/style.css', 
            \ "custom_wiki2html": '~/google_drive/code/shell/wiki2html.sh', }]
    endif
    nmap <Leader>wn <Plug>VimwikiNextLink
    nmap <Leader>wp <Plug>VimwikiPrevLink

    function! VimwikiLinkHandler(link)
      " Use Vim to open external files with the 'vfile:' scheme.  E.g.:
      "   1) [[vfile:~/Code/PythonProject/abc123.py]]
      "   2) [[vfile:./|Wiki Home]]
      let link = a:link
      if link =~# '^vfile:'
        let link = link[1:]
      else
        return 0
      endif
      let link_infos = vimwiki#base#resolve_link(link)
      if link_infos.filename == ''
        echomsg 'Vimwiki Error: Unable to resolve link!'
        return 0
      else
        exe 'Vexplore ' . fnameescape(link_infos.filename)
        return 1
      endif
    endfunction

  " neocomplete 
    if v:version >= 704 && has("lua")
      "let g:acp_enableAtStartup = 0
      let g:neocomplete#enable_at_startup = 1
      let g:neocomplete#enable_smart_case = 1
      let g:neocomplete#sources#syntax#min_keyword_length = 3

      if !exists('g:neocomplete#keyword_patterns')
          let g:neocomplete#keyword_patterns = {}
      endif

      let g:neocomplete#auto_completion_start_length = 2
      let g:neocomplete#keyword_patterns['default']  = '\h\w*'
      let g:neocomplete#max_list = 5

      inoremap <expr><C-g> neocomplete#undo_completion()
      inoremap <expr><C-l> neocomplete#complete_common_string()

      " <CR>: close popup and save indent.
        inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
        function! s:my_cr_function()
          return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
        endfunction

      " <TAB>: completion.
        inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

      " <BS>: close popup and delete backword char.
        inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

      autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    endif

  " neosnippet 
    imap <C-j> <Plug>(neosnippet_expand_or_jump)
    smap <C-j> <Plug>(neosnippet_expand_or_jump)
    xmap <C-j> <Plug>(neosnippet_expand_target)

    imap <expr><TAB>
     \ pumvisible() ? "\<C-n>" :
     \ neosnippet#expandable_or_jumpable() ?
     \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

    let g:neosnippet#snippets_directory = "~/.vim/bundle/neosnippet-snippets/neosnippets/"
    if has('conceal')
      set conceallevel=2 concealcursor=i
    endif

  " syntastic
    "let g:syntastic_cpp_compiler = 'clang++'
    "let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'
    let g:syntastic_error_symbol = '✘'
    let g:syntastic_warning_symbol = "▲"

    let g:syntastic_mode_map = { 'mode': 'active', 'active_filetypes': [], 'passive_filetypes': [] }
    nnoremap <leader>c :w <CR> :SyntasticCheck<CR>
    nnoremap <leader>C :SyntasticReset<CR>
    nnoremap <leader>T :SyntasticToggle<CR>
    "let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq   = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => UI
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " General
    colorscheme desert
    set t_Co=256
    set number wrap tw=79 showcmd
    set scrolloff=4 showtabline=2 tabpagemax=30 laststatus=2 cmdheight=1

  " Resize vim windows when overall window size changes
    autocmd VimResized * wincmd =

  " Be as wild as possible
    set wildmode=full wildmenu
    set wildignore=*.o,*~,*.pyc,/usr/include/*,*.class,*.bak,
    set wildignore+=.git\*,.svn\* 

  " Performance!?
    set lazyredraw ttyfast

  " Syntax, Column and cursor lines
    syntax on sync minlines=256

    set colorcolumn=80          " Handy bar so we know when lines are too long
    set synmaxcol=200           " Limit column highlights to 200 columns
    highlight ColorColumn ctermbg=234

  " Cursor line
    set cursorline              " Handy line so we know where we are
    highlight CursorLine cterm=NONE ctermbg=234 ctermfg=NONE
    autocmd InsertEnter,InsertLeave * set cul!

  " Window split
    set fillchars+=vert:│
    highlight VertSplit cterm=NONE ctermbg=0 ctermfg=NONE

  " Status line
    highlight StatusLine ctermfg=245 cterm=none
    set statusline=%f    " Path.
    set statusline+=%m   " Modified flag.
    set statusline+=%r   " Readonly flag.
    set statusline+=%w   " Preview window flag.
    set statusline+=\    " Space.
    set statusline+=%=   " Right align.
    set statusline+=\ %l:%03c\ %p%%

  " Colors
    highlight Pmenu      ctermbg=240
    highlight PmenuSel   ctermbg=234
    highlight PmenuSBar  ctermbg=238
    highlight PmenuThumb ctermbg=234

  " Be less obnoxious about 'Pattern not found' warnings
    highlight Error    None
    highlight ErrorMsg None

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Miscellaneous
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Strip whitespaces on save
    fun! <SID>StripTrailingWhitespaces()
        let l = line(".")
        let c = col(".")
        %s/\s\+$//e
        call cursor(l, c)
    endfun
    autocmd FileType sh,c,cpp,java,python autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
  " File specific 
    autocmd FileType make setlocal noexpandtab

  " Avoid dumb markdown extension
    autocmd BufNewFile,BufRead,BufEnter *.md set filetype=vimwiki

  " Searching
    set hlsearch incsearch showmatch ignorecase smartcase infercase ruler

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
      map <ScrollWheelUp> <C-Y>
      map <ScrollWheelDown> <C-E>

  " Folding
    set foldtext=MyFoldText()
    function! MyFoldText()
      let nucolwidth = &fdc + &number*&numberwidth
      let winwd = winwidth(0) - nucolwidth - 5
      let foldlinecount = foldclosedend(v:foldstart) - foldclosed(v:foldstart) + 1
      let prefix = " >>> "
      let fdnfo = prefix . string(v:foldlevel) . "," . string(foldlinecount)
      let line =  strpart(getline(v:foldstart), 0 , winwd - len(fdnfo))
      let fillcharcount = winwd - len(line) - len(fdnfo)
      return line . repeat(" ",fillcharcount) . fdnfo
    endfunction
    set foldmethod=indent   
    set foldnestmax=10
    set nofoldenable
    set foldlevel=2
    set foldcolumn=1

  " Cool file explorer stuff in a pane
    function! ToggleVExplorer()
        if exists("t:expl_buf_num")
            let expl_win_num = bufwinnr(t:expl_buf_num)
            if expl_win_num != -1
                let cur_win_nr = winnr()
                exec expl_win_num . 'wincmd w'
                close
                exec cur_win_nr . 'wincmd w'
                unlet t:expl_buf_num
            else
                unlet t:expl_buf_num
            endif
        else
            exec '1wincmd w'
            Vexplore .
            let t:expl_buf_num = bufnr("%")
        endif
    endfunction
    nnoremap <silent> <leader>d :call ToggleVExplorer()<CR>

  " Hit enter in the file browser to open the selected
  " file with :vsplit to the right of browser
    let g:netrw_sort_sequence = '[\/]$,*'   " directories first
    let g:netrw_browse_split  = 3           " open files in new tab
    let g:netrow_altv         = 1
    let g:netrw_winsize       = -28         " thinner width
    let g:netrw_banner        = 0           " hide the help info
    let g:netrw_liststyle     = 3           " tree mode

  " Clear junk before running commands
    noremap :! :!clear;
    noremap :make :!clear; make
    noremap <silent> <c-l> :nohlsearch<cr>
  
  " Make pylint happy
    autocmd BufNewFile,BufRead *.py setlocal tabstop=4

  " External tools
    call system('type ag')
		if v:shell_error == 0
			if exists('+grepprg')    | set grepprg=ag\ --vimgrep\ $* | endif
			if exists('+grepformat') | set grepformat=%f:%l:%c:%m    | endif
		endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  set backspace=indent,eol,start   " allow backspacing over everything
  set shiftwidth=2                 " Set 2 spaces for indenting
  set tabstop=2                    " Set 2 TAB stops
  set softtabstop=2
  set expandtab smarttab
  set autoindent smartindent nocindent
  set showmatch                    " Show matched brackets
  
  "set completeopt=menu,menuone
  "set pumheight=3
  "
  " Autocompletion with tab!
  "function! Tab_Or_Complete()
  "  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
  "    return "\<C-N>"
  "  else
  "    return "\<Tab>"
  "  endif
  "endfunction
  "inoremap <silent> <Tab> <C-R>=Tab_Or_Complete()<CR>

  " Auto complete whole lines with ctrl-l !
  "inoremap <C-L> <C-X><C-L>

  "autocmd InsertEnter * set noshowmode
  "autocmd InsertLeave * set showmode
  "
  "" automatic autocompletion
  "function! s:skinny_insert(char)
  "  if !pumvisible() && !exists('s:skinny_complete') && getline('.')[col('.') - 2].a:char =~# '\k\k'
  "    let s:skinny_complete = 1
  "      noautocmd call feedkeys("\<C-n>\<C-p>", "nt")
  "  endif
  "endfunction
  "
  "augroup SkinnyAutoComplete
  "  autocmd!
  "  autocmd InsertCharPre * silent! call <SID>skinny_insert(v:char)
  "  autocmd CompleteDone * silent! if exists('s:skinny_complete') | unlet s:skinny_complete | endif
  "augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Macros
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " List macros
    nnoremap <leader>? :echo "
          \  : clear highlighting        \n
          \+ : toggle dictionary         \n
          \1 : jump to tab number        \n
          \A :                           \| a :                           \n
          \B :                           \| b : toggle mouse              \n
          \C : syntastic reset           \| c : syntastic Check           \n
          \D :                           \| d : file explorer             \n
          \E :                           \| e : open in new buffer        \n
          \F :                           \| f : rewrap paragraph          \n
          \G :                           \| g :                           \n
          \H : horizontal split          \| h : tag select                \n
          \I :                           \| i : paste result of command   \n
          \J : scroll to bottom of screen\| j : jump to last buffer       \n
          \K : auto scroll down          \| k : save                      \n
          \L :                           \| l : reload file               \n
          \M : make in pane              \| m : make                      \n
          \N : print pwd                 \| n : cycle windows             \n
          \O : insert line above         \| o : insert line below         \n
          \P :                           \| p : toggle paste              \n
          \Q : close all windows         \| q : close window              \n
          \R : toggle relative numbering \| r : run buffer                \n
          \S : global substitute         \| s : line substitute           \n
          \T : toggle syntastic mode     \| t : tab current buffer        \n
          \U : source vimrc              \| u : open vimrc                \n
          \V : vertical split            \| v : visual block              \n
          \W : sudo save                 \| w :                           \n
          \X :                           \| x : close buffer              \n
          \Y :                           \| y : run in right pane         \n
          \Z :                           \| z : toggle spell check        \n
          \"<CR>

  " clear highlighting
    nnoremap <silent> <leader><Space> :nohl<CR>

  " Scroll to bottom for note taking
    nnoremap <silent> <leader>J 100<c-e>5<c-y>

  " super secret encryption algorithm
    nnoremap <silent> <leader>G ggg?G<C-o>

  " open vimrc, source vimrc
    nnoremap <silent> <leader>u :tabe $MYVIMRC<cr>
    nnoremap <silent> <leader>U :so $MYVIMRC<cr>

  " re hardwrap a paragraph
    nnoremap <silent> <leader>f gqip<cr>

  " split the screen and move there
    nnoremap <silent> <leader>V <C-w>v<C-w>l
    nnoremap <silent> <leader>H <C-w>s

  " tags
    nnoremap <leader>h :tselect<Space>

  " windows
    nnoremap <silent> <leader>q :q<CR>
    nnoremap <silent> <leader>Q :qall<CR>
    command! Q :qall
    nnoremap <silent> <leader>n <C-w><C-w>

  " stay in visual mode while changing indentation
    vnoremap < <gv
    vnoremap > >gv

  " shell + tmux pane commands
    function! Run_Command(command)
      execute "silent !tmux send-keys -t right '" . a:command . "' C-m"
      execute "redraw!"
    endfunction

  " toggling
    nnoremap <silent> <leader>z :set spell!<CR>
    nnoremap <silent> <leader>R :set relativenumber!<CR>
    nnoremap <leader>p          :setlocal paste!<cr>:set paste?<cr>

  " close, jump to last, make new buffer
    nnoremap <silent> <leader>x :bp<bar>sp<bar>bn<bar>bd<CR>
    nnoremap <silent> <leader>j :w<bar>b#<CR>
    nnoremap <silent> <leader>t :tabe %<cr>

  " paste result of command
    nnoremap          <leader>i :r!
    vnoremap <silent> <leader>T :!column -te<cr>

  " open buffer, open tab, reload, save, sudo save current file
    nnoremap <leader>E :e 
    nnoremap <leader>e :tabe 
    nnoremap <leader>l :e % <cr>zz
    nnoremap <leader>k :w<CR>

  " insert a blank line above,below the current line
    nnoremap <silent> <leader>O O<Esc>
    nnoremap <silent> <leader>o o<Esc>

  " make
    nnoremap <silent> <leader>M :!tmux send-keys -t right 
          \ 'make' C-m<cr><cr>
    nnoremap <silent> <leader>m :!clear;make<CR>

  " visual block mode
    nnoremap <silent> <leader>v <C-S-v>

  " generic ':' command
    nnoremap m :
    nnoremap <leader>; :
    vnoremap <leader>; :

  " substitute line, global
    vnoremap <leader>s :s/\v
    nnoremap <leader>s V:s/\v
    nnoremap <leader>S :%s/\v

  " repeat last colon command
    nnoremap <leader>. q:k<cr>
    vnoremap <leader>. q:k<cr>

  " go to tab by number
    noremap <leader>1 1gt
    noremap <leader>2 2gt
    noremap <leader>3 3gt
    noremap <leader>4 4gt
    noremap <leader>5 5gt
    noremap <leader>6 6gt
    noremap <leader>7 7gt
    noremap <leader>8 8gt
    noremap <leader>9 9gt
    noremap <leader>0 :tablast<cr>

  " running programs in Vim!
    function! Compile(compile_command)
      if filereadable("./Makefile") || filereadable("./makefile")
        make
      else
        execute "!" . a:compile_command . " " . bufname("%")
      endif
    endfunction
    
    autocmd FileType python  nmap <leader>r :!clear;python %<cr>
    autocmd FileType scheme  nmap <leader>r :!clear;racket %<cr>
    autocmd FileType sh      nmap <leader>r :!clear;bash %<cr>
    autocmd FileType perl    nmap <leader>r :!clear;perl %<cr>
    autocmd FileType haskell nmap <leader>r :!clear;runhaskell %<cr>
    
    autocmd FileType java    nmap <leader>r 
          \ :silent call Compile("javac")<cr> <bar>:!clear;java %:r<cr>
    autocmd FileType c       nmap <leader>r 
          \ :silent call Compile("gcc")  <cr> <bar>:!clear;./a.out <cr>

  " Run program in pane to the right
    autocmd FileType python 
          \ nnoremap <leader>y :!tmux send-keys -t right 
          \ 'python %' C-m <cr><cr>
    autocmd FileType sh
          \ nnoremap <leader>y :!tmux send-keys -t right 
          \ 'bash %' C-m<cr><cr>

  " Experiment
    nnoremap / /\v
    vnoremap / /\v

  " Search across all open buffers
    function! BuffersList()
      let all = range(0, bufnr('$'))
      let res = []
      for b in all
        if buflisted(b)
          call add(res, bufname(b))
        endif
      endfor
      return res
    endfunction

    function! GrepBuffers(expression)
      exec 'vimgrep/'.a:expression.'/ '.join(BuffersList())
    endfunction

    command! -nargs=+ GrepBufs call GrepBuffers(<q-args>)
    nnoremap <leader>/ :GrepBufs 
    nnoremap <leader>* 
          \ :call GrepBuffers("<C-R><C-W>")<CR> <bar> :copen 7<cr>

  " Where are we?
    function! CurrentDir()
      :!clear;readlink -f %
    endfunction
    nnoremap <leader>N :call CurrentDir()<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Movement
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " recenter
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
    nnoremap <silent> { :tabp<cr>
    nnoremap <silent> } :tabn<cr>

  " cycle through quickfix
    nnoremap <C-{> :cp<cr>
    nnoremap <C-}> :cn<cr>

  " List navigation with arrows
    noremap <silent> <Left>  :lprev<cr>zz
    noremap <silent> <Right> :lnext<cr>zz

  " scrolling Down, Up
    "nnoremap <c-j> 5<C-e>5gj
    "vnoremap <c-j> 5<C-e>5gj
    "nnoremap <c-k> 5<C-y>5gk
    "vnoremap <c-k> 5<C-y>5gk

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

  " Horizontal scrolling ?
    set scrollopt+=hor
    nnoremap <ScrollWheelLeft> :echo HELLO

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
