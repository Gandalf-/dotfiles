-- plugin-independent maps. plugin maps live with their specs in lua/plugins/*;
-- maps that call custom functions live with those functions in legacy.vim.
local map = vim.keymap.set

map("n", "<SPACE>", "<Nop>")

-- search
map("n", [[\]], ":Rg<SPACE>")
map("v", [[\]], [[:<C-u>Rg <C-r><C-w><CR>]])
vim.api.nvim_create_user_command("Search", function(o)
  vim.cmd("silent! grep! " .. o.args)
  vim.cmd("cwindow")
  vim.cmd("redraw!")
end, { nargs = "+", complete = "file", bar = true })

-- search for the visual selection, forwards / backwards
map("v", "*", [[:<C-U>let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>gvy/<C-R><C-R>=substitute(escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>gV:call setreg('"', old_reg, old_regtype)<CR>]], { silent = true })
map("v", "#", [[:<C-U>let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>gvy?<C-R><C-R>=substitute(escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>gV:call setreg('"', old_reg, old_regtype)<CR>]], { silent = true })

-- scroll with the wheel
map("", "<ScrollWheelUp>", "5<C-Y>")
map("", "<ScrollWheelDown>", "5<C-E>")

-- clear the screen before shelling out
map("n", ":!", ":!clear;")
map("n", ":make", ":!clear; make")

map("n", "<leader><Space>", ":nohl<CR>", { silent = true })
map("n", "<leader>J", "100<c-e>5<c-y>", { silent = true }) -- scroll to bottom for notes
map("n", "<leader>G", "ggg?G<C-o>", { silent = true }) -- rot13 the buffer
map("n", "<leader>f", function() -- re-hardwrap a paragraph (bypass conform's formatexpr)
  local save = vim.bo.formatexpr
  vim.bo.formatexpr = ""
  vim.cmd("normal! gqip")
  vim.bo.formatexpr = save
end, { silent = true, desc = "re-hardwrap a paragraph" })

-- split and move there
map("n", "<leader>V", "<C-w>v<C-w>l", { silent = true })
map("n", "<leader>H", "<C-w>s", { silent = true })

map("n", "<leader>h", ":tselect<Space>")

map("n", "<leader>q", ":q<CR>", { silent = true })
map("n", "<leader>Q", ":qall<CR>", { silent = true })
vim.api.nvim_create_user_command("Q", "qall", {})

map("n", "<leader>n", ":set nu!<CR>", { silent = true })

-- keep the selection while re-indenting
map("v", "<", "<gv")
map("v", ">", ">gv")

map("n", "<leader>L", ":set list!<CR>", { silent = true })
map("n", "<leader>z", ":set spell!<CR>", { silent = true })
map("n", "<leader>R", ":set relativenumber!<CR>", { silent = true })

-- close, jump to last, new tab of current buffer
map("n", "<leader>x", ":bp<bar>sp<bar>bn<bar>bd<CR>", { silent = true })
map("n", "<leader>j", ":w<bar>b#<CR>", { silent = true })
map("n", "<leader>t", ":tabe %<cr>", { silent = true })

map("n", "<leader>i", ":r!") -- paste command output

map("n", "<leader>e", ":e<space>")
map("n", "<leader>E", ":tabe<space>")
map("n", "<leader>l", ":e % <cr>zz")
map("n", "<leader>k", ":w<CR>")

map("n", "<leader>v", "<C-v>", { silent = true }) -- visual block

-- ':' on the home row
map("n", "m", ":")
map("v", "m", ":")

-- substitute: line, line global, file global
map("v", "<leader>s", ":s/")
map("n", "<leader>s", "V:s/")
map("n", "<leader>S", ":%s/")
map("n", "<leader>c", [[:%s/\<<C-r><C-w>\>//gc<Left><Left><Left>]])

-- reopen the last colon command for editing
map("n", "<leader>.", "q:k<cr>")
map("v", "<leader>.", "q:k<cr>")

-- keep the cursor centered while jumping
map("n", "Z", "zz", { silent = true })
map("n", "<C-]>", "<C-]>zz", { silent = true })
map("n", "n", "nzz", { silent = true })
map("n", "N", "Nzz", { silent = true })
map("n", "*", "*zz", { silent = true })
map("n", "#", "#zz", { silent = true })
map("n", "<C-o>", "<C-o>zz", { silent = true })
map("n", "'", function() return "'" .. vim.fn.nr2char(vim.fn.getchar()) .. " zz" end, { expr = true })

-- move by display line
map("n", "j", "gj", { silent = true })
map("n", "k", "gk", { silent = true })

-- buffers on the parens (normal only; keep ( ) sentence motions for operators)
map("n", "(", ":bp<cr>zz", { silent = true })
map("n", ")", ":bn<cr>zz", { silent = true })

-- location list on the arrows
map("n", "<Left>", ":lprev<cr>zz", { silent = true })
map("n", "<Right>", ":lnext<cr>zz", { silent = true })
map("n", "<Up>", "H5<C-y>5gk", { silent = true })
map("n", "<Down>", "L5<C-e>5gj", { silent = true })

map("n", "<C-@>", "@@", { silent = true }) -- repeat last macro

-- line ends on B / E
map("n", "B", "^")
map("v", "B", "^")
map("n", "E", "$")
map("v", "E", "$")

map("", "Y", "y$")
