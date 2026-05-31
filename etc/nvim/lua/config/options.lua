local opt = vim.opt
local g = vim.g

-- general
opt.title = true
opt.langmenu = "en_US"
opt.shell = "bash" -- so vim plays nice with fish
opt.history = 300
opt.autoread = true
opt.fileformat = "unix"
opt.confirm = true
opt.visualbell = true

opt.splitbelow = true
opt.splitright = true
opt.encoding = "utf-8"
opt.fileencodings = "utf-8"
opt.hidden = true
opt.backup = false
opt.writebackup = false
opt.swapfile = false
opt.ttimeout = true
opt.ttimeoutlen = 20
opt.tags = "./tags;,tags;"

-- search
opt.hlsearch = true
opt.incsearch = true
opt.showmatch = true
opt.ruler = true
opt.grepprg = "rg --column"
opt.grepformat = "%f:%l:%c%m"

-- ui
opt.termguicolors = true
opt.number = true
opt.wrap = true
opt.textwidth = 99
opt.showcmd = true
opt.scrolloff = 4
opt.showtabline = 2
opt.tabpagemax = 30
opt.laststatus = 2
opt.cmdheight = 1
opt.cursorline = true

opt.wildmode = "full"
opt.wildmenu = true
opt.wildignore = "*.o,*~,*.pyc,/usr/include/*,*.class,*.bak,*.hi,.git*,.svn*"
opt.wildignorecase = true

opt.synmaxcol = 200

opt.fillchars:append({ vert = "│" })

-- folding (foldtext function lives in legacy.vim)
opt.foldmethod = "indent"
opt.foldnestmax = 10
opt.foldenable = false
opt.foldlevel = 2

-- text, tab and indent
opt.backspace = { "indent", "eol", "start" }
opt.tabstop = 4
opt.virtualedit = "block"

-- netrw
g.netrw_sort_sequence = [[[\/]$,*]]
g.netrw_browse_split = 3
g.netrw_altv = 1
g.netrw_winsize = -28
g.netrw_banner = 0
g.netrw_liststyle = 3
g.netrw_list_hide = [[.*\.swp$,.*\.pyc]]

vim.cmd("syntax on")
vim.cmd("syntax sync minlines=256")
-- colorscheme is set in lua/plugins/colorscheme.lua (after the theme loads)
