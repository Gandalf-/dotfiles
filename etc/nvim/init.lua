-- leader must be set before plugins load
vim.g.mapleader = " "
vim.g.maplocalleader = " "

require("config.options")
require("config.keymaps")
require("config.autocmds")
require("config.lazy")

-- custom functions, statusline, and abbreviations, shared with plain vim
vim.cmd("runtime legacy.vim")

local work = vim.fn.expand("~/scripts/etc/work.vim")
if vim.fn.filereadable(work) == 1 then
  vim.cmd("source " .. work)
end
