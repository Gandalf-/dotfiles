-- leader must be set before plugins load
vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("config.options")
require("config.keymaps")
require("config.autocmds")
require("config.lazy")

-- custom functions, statusline, and abbreviations, shared with plain vim
vim.cmd("runtime legacy.vim")

-- Qumulo-specific config (formerly work.vim)
local work_lua = vim.fn.expand("~/scripts/etc/work.lua")
if vim.fn.filereadable(work_lua) == 1 then
  dofile(work_lua)
end
