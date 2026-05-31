return {
  "ellisonleao/gruvbox.nvim",
  priority = 1000, -- load before other plugins so highlights exist early
  lazy = false,
  config = function()
    vim.o.background = "dark"
    vim.cmd.colorscheme("gruvbox")
  end,
}
