return {
  -- fuzzy finder: :Rg, :Files, :Buffers and the \ / <leader>[ / <leader>] maps
  {
    "junegunn/fzf",
    build = "./install --bin",
  },
  {
    "junegunn/fzf.vim",
    dependencies = { "junegunn/fzf" },
    init = function()
      vim.g.fzf_action = {
        ["ctrl-x"] = "split",
        ["ctrl-v"] = "vsplit",
      }
    end,
    keys = {
      { "<leader>[", "<cmd>Buffers<cr>", desc = "Buffers" },
      { "<leader>]", "<cmd>Files<cr>", desc = "Files" },
    },
    cmd = { "Rg", "Files", "Buffers", "GFiles", "Lines", "BLines" },
  },

  -- detect per-file indentation
  { "tpope/vim-sleuth" },

  {
    "christoomey/vim-tmux-navigator",
    lazy = false,
  },
  {
    "preservim/vimux",
    init = function()
      vim.g.VimuxOrientation = "h"
      vim.g.VimuxHeight = "33"
    end,
    keys = {
      { "<C-b>", "<cmd>VimuxRunLastCommand<cr>", desc = "Vimux run last command" },
    },
    cmd = { "VimuxRunCommand", "VimuxRunLastCommand", "VimuxPromptCommand" },
  },
}
