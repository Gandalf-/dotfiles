-- plugins install under stdpath("data")/lazy; the lockfile is written into
-- stdpath("config"), which is this repo, so versions stay pinned and committed
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({
    "git", "clone", "--filter=blob:none", "--branch=stable", repo, lazypath,
  })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = { { import = "plugins" } },
  defaults = { lazy = false },
  -- colorscheme for the first-run install screen; desert is a builtin fallback
  -- in case gruvbox isn't cloned yet
  install = { colorscheme = { "gruvbox", "desert" } },
  checker = { enabled = false },
  change_detection = { notify = false },
})
