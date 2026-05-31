" Sourced by both init.lua and the plain-vim ~/.vimrc. Keep it
" plugin-independent and guard nvim-only bits with has('nvim').

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Folding
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
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
  let line =  "  ..."
  let fillcharcount = winwd - len(line) - len(fdnfo)
  return line . repeat(" ",fillcharcount) . fdnfo
endfunction
set foldtext=MyFoldText()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Status line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" diagnostic counts for the statusline
function! LinterStatus() abort
  if has('nvim') && luaeval('vim.diagnostic ~= nil')
    let l:errors = luaeval('#vim.diagnostic.get(0, {severity = vim.diagnostic.severity.ERROR})')
    let l:total  = luaeval('#vim.diagnostic.get(0)')
    if l:total == 0
      return ''
    endif
    return printf('%2dw %2de', l:total - l:errors, l:errors)
  endif
  return ''
endfunction

function! SetupStatusLine()
  highlight StatusLine   guifg=black guibg=white     gui=bold
  highlight StatusLineNC guifg=black guibg=lightgrey
  set statusline=%f    " Path.
  set statusline+=%m   " Modified flag.
  set statusline+=%r   " Readonly flag.
  set statusline+=%w   " Preview window flag.
  set statusline+=\    " Space.
  set statusline+=%=   " Right align.
  set statusline+=%{LinterStatus()}
  set statusline+=\    " Space.
  set statusline+=\ %4l\ %3c\ %3p%%
endfunction
call SetupStatusLine()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Tag traversal
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let s:tag_current = ""
let s:tag_index = 0

function! TagTraverse()
  let l:word = expand('<cword>')

  if empty(l:word)
    echo "No word under cursor"
    return
  endif

  if s:tag_current != l:word
    let s:tag_current = l:word
    let s:tag_index = 0

    try
      execute 'tag ' . l:word
      echo "Tag: " . l:word . " [1]"
    catch /^Vim\%((\a\+)\)\=:E426/
      echo "Tag not found: " . l:word
      return
    catch
      echo "Error jumping to tag: " . l:word
      return
    endtry
  else
    try
      silent tnext
      let s:tag_index += 1
      echo "Tag: " . l:word . " [" . (s:tag_index + 1) . "]"
    catch /^Vim\%((\a\+)\)\=:E428/
      try
        silent tfirst
        let s:tag_index = 0
        echo "Tag: " . l:word . " [1] (wrapped)"
      catch
        echo "Only one tag for: " . l:word
      endtry
    catch
      echo "Error navigating tags"
    endtry
  endif

  normal! zz
endfunction
nnoremap <silent> <C-n> :call TagTraverse()<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Smart paste (toggle paste mode + line numbers)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! SmartPaste()
  setlocal nu! paste!
endfunction
nnoremap <leader>p :call SmartPaste()<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Abbreviations
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
