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
    config = function()
      -- the plugin maps <C-h/j/k/l> in normal mode only; mirror them in
      -- visual mode so pane switching works without dropping to normal first
      local nav = {
        ["<C-h>"] = "TmuxNavigateLeft",
        ["<C-j>"] = "TmuxNavigateDown",
        ["<C-k>"] = "TmuxNavigateUp",
        ["<C-l>"] = "TmuxNavigateRight",
      }
      for key, cmd in pairs(nav) do
        vim.keymap.set("x", key, "<cmd>" .. cmd .. "<cr>", { silent = true })
      end
    end,
  },
}
